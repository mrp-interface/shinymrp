.make_grid_adj <- function(nx, ny) {
  stopifnot(nx >= 1, ny >= 1)
  if (nx * ny == 1) return(Matrix::Matrix(0, 1, 1, sparse = TRUE))
  
  Tx <- Matrix::bandSparse(
    nx,
    k = c(-1, 1),
    diag = list(
      rep.int(1, nx - 1),
      rep.int(1, nx - 1)
    )
  )
  Ty <- Matrix::bandSparse(
    ny,
    k = c(-1, 1),
    diag = list(
      rep.int(1, ny - 1),
      rep.int(1, ny - 1)
    )
  )
  
  W  <- Matrix::Diagonal(ny) %x% Tx + Ty %x% Matrix::Diagonal(nx)
  W
}

.make_disconnected_lattices <- function(
    components = list(c(20, 20)),
    n_isolates = 0
) {
  stopifnot(
    all(vapply(components, length, 1L) == 2L),
    n_isolates >= 0
  )
  
  # Build block matrix from adjacency matrices
  blocks <- lapply(components, function(x) .make_grid_adj(x[1], x[2]))
  W <- Matrix::bdiag(blocks)
  iso_block <- Matrix::Matrix(0, n_isolates, n_isolates, sparse = TRUE)
  W <- Matrix::bdiag(W, iso_block)
  
  # Ensure symmetric numeric sparse (dsCMatrix) and consistent dimnames
  W <- Matrix::forceSymmetric(W, uplo = "U")
  N <- nrow(W)
  lab <- sprintf("Z%04d", seq_len(N))
  dimnames(W) <- list(lab, lab)
  
  # Strict upper-triangle edge list (sparse-safe; no dense conversion)
  Up <- Matrix::triu(W, k = 1)
  E  <- Matrix::summary(Up)
  
  list(
    W      = W,
    labels = lab,
    node1  = as.integer(E$i),
    node2  = as.integer(E$j)
  )
}

# Split a global basis R into per-component blocks R_c
.split_basis_by_component <- function(basis) {
  R    <- basis$R
  comp <- basis$comp
  N    <- nrow(R)
  
  stopifnot(length(comp$comp_id) == N)
  
  N_comps <- comp$N_comps
  
  # Number of nodes in each component
  n_c <- tabulate(comp$comp_id, nbins = N_comps)
  # Number of basis columns per component (0 for singletons)
  p_c <- pmax(n_c - 1L, 0L)
  
  if (ncol(R) != sum(p_c)) {
    stop(
      "ncol(R) = ", ncol(R),
      " but sum(n_c - 1) = ", sum(p_c),
      "; basis dimensions don't match component sizes."
    )
  }
  
  # Column index ranges for each component
  # Example for p_c = c(399,399,0,0,0,0,0):
  # col_start = c(1, 400, 799, 799, 799, 799, 799)
  col_start <- cumsum(c(1L, head(p_c, -1L)))
  col_end   <- col_start + p_c - 1L
  
  out <- vector("list", N_comps)
  for (c in seq_len(N_comps)) {
    if (p_c[c] == 0L) {
      # singleton or degenerate component: no basis columns
      out[[c]] <- NULL
    } else {
      row_idx <- which(comp$comp_id == c)
      col_idx <- col_start[c]:col_end[c]
      out[[c]] <- R[row_idx, col_idx, drop = FALSE]
    }
  }
  out
}


# Check that each component block R_c has columns summing to (approx) zero
.check_basis_sum_to_zero <- function(basis, tol = 1e-6) {
  Rc_list <- .split_basis_by_component(basis)
  
  res <- data.frame(
    component       = integer(0),
    max_abs_colsum  = numeric(0)
  )
  
  # NOTE: use seq_along(Rc_list) to avoid any mismatch with comp$N_comps
  for (c in seq_along(Rc_list)) {
    Rc <- Rc_list[[c]]
    if (is.null(Rc)) next    # singleton: no columns, nothing to check
    
    col_sums <- colSums(Rc)
    max_abs  <- max(abs(col_sums))
    res <- rbind(res, data.frame(
      component      = c,
      max_abs_colsum = max_abs
    ))
  }
  
  attr(res, "ok") <- if (nrow(res) == 0L) TRUE else all(res$max_abs_colsum < tol)
  res
}


# Check that columns of R_c are (approximately) orthogonal
.check_basis_orthogonality <- function(basis, tol_offdiag = 1e-6) {
  Rc_list <- .split_basis_by_component(basis)
  
  res <- data.frame(
    component       = integer(0),
    max_abs_offdiag = numeric(0)
  )
  
  for (c in seq_along(Rc_list)) {
    Rc <- Rc_list[[c]]
    if (is.null(Rc)) next  # singleton or no basis columns
    
    G <- crossprod(Rc)     # p_c x p_c
    diag(G) <- 0           # ignore diagonal
    max_off <- if (length(G)) max(abs(G)) else 0
    
    res <- rbind(res, data.frame(
      component       = c,
      max_abs_offdiag = max_off
    ))
  }
  
  attr(res, "ok") <- if (nrow(res) == 0L) TRUE else all(res$max_abs_offdiag < tol_offdiag)
  res
}

# Check BYM2 scaling: GM(diag(R_c R_c^T)) ≈ 1 for each component
check_basis_scaling <- function(basis, tol_rel = 0.05) {
  Rc_list <- .split_basis_by_component(basis)
  
  res <- data.frame(
    component   = integer(0),
    gm_diag_RRt = numeric(0)
  )
  
  for (c in seq_along(Rc_list)) {
    Rc <- Rc_list[[c]]
    if (is.null(Rc)) next  # singleton: no basis, no scaling to check
    
    RRt_diag <- rowSums(Rc^2)
    gm       <- exp(mean(log(RRt_diag)))
    
    res <- rbind(res, data.frame(
      component   = c,
      gm_diag_RRt = gm
    ))
  }
  
  attr(res, "ok") <- if (nrow(res) == 0L) TRUE else all(abs(res$gm_diag_RRt - 1) <= tol_rel)
  res
}

# Check that R is consistent with the Laplacian Q:
# t(R_c) %*% Q_c %*% R_c should be approximately diagonal.
.check_basis_against_laplacian <- function(basis, node1, node2, N,
                                          tol_offdiag = 1e-6) {
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Please install the 'Matrix' package.")
  }
  
  R    <- basis$R
  comp <- basis$comp
  stopifnot(nrow(R) == N, length(comp$comp_id) == N)
  
  # Build Laplacian Q
  Wg <- Matrix::sparseMatrix(
    i    = c(node1, node2),
    j    = c(node2, node1),
    x    = 1,
    dims = c(N, N)
  )
  W  <- Matrix::forceSymmetric(Wg, uplo = "U")
  d  <- as.numeric(Matrix::rowSums(W))
  Q  <- Matrix::Diagonal(x = d) - W
  
  Rc_list <- .split_basis_by_component(basis)
  
  res <- data.frame(
    component       = integer(0),
    max_abs_offdiag = numeric(0)
  )
  
  for (c in seq_along(Rc_list)) {
    Rc <- Rc_list[[c]]
    if (is.null(Rc)) next  # singleton or no basis
    
    idx <- which(comp$comp_id == c)
    Qc  <- as.matrix(Q[idx, idx, drop = FALSE])
    
    M <- crossprod(Rc, Qc %*% Rc)  # p_c x p_c
    diag(M) <- 0
    max_off <- if (length(M)) max(abs(M)) else 0
    
    res <- rbind(res, data.frame(
      component       = c,
      max_abs_offdiag = max_off
    ))
  }
  
  attr(res, "ok") <- if (nrow(res) == 0L) TRUE else all(res$max_abs_offdiag < tol_offdiag)
  res
}

test_that("R construction works", {
  # connected graph
  adj <- .make_disconnected_lattices(
    components = list(c(3, 3)),
    n_isolates = 0
  )
  basis <- .compute_icar_basis(
    node1 = adj$node1,
    node2 = adj$node2,
    n_nodes = nrow(adj$W)
  )

  expect_true(attr(.check_basis_sum_to_zero(basis), "ok"))
  expect_true(attr(.check_basis_orthogonality(basis), "ok"))
  expect_true(attr(check_basis_scaling(basis), "ok"))
  expect_true(attr(.check_basis_against_laplacian(
    basis,
    node1 = adj$node1,
    node2 = adj$node2,
    N     = nrow(adj$W)
  ), "ok"))

  # disconnected graph
  adj <- .make_disconnected_lattices(
    components = list(c(3, 3), c(2, 2)),
    n_isolates = 0
  )
  basis <- .compute_icar_basis(
    node1 = adj$node1,
    node2 = adj$node2,
    n_nodes = nrow(adj$W)
  )

  expect_true(attr(.check_basis_sum_to_zero(basis), "ok"))
  expect_true(attr(.check_basis_orthogonality(basis), "ok"))
  expect_true(attr(check_basis_scaling(basis), "ok"))
  expect_true(attr(.check_basis_against_laplacian(
    basis,
    node1 = adj$node1,
    node2 = adj$node2,
    N     = nrow(adj$W)
  ), "ok"))

  # disconnected graph with isolates
  adj <- .make_disconnected_lattices(
    components = list(c(3, 3)),
    n_isolates = 2
  )
  basis <- .compute_icar_basis(
    node1 = adj$node1,
    node2 = adj$node2,
    n_nodes = nrow(adj$W)
  )

  expect_true(attr(.check_basis_sum_to_zero(basis), "ok"))
  expect_true(attr(.check_basis_orthogonality(basis), "ok"))
  expect_true(attr(check_basis_scaling(basis), "ok"))
  expect_true(attr(.check_basis_against_laplacian(
    basis,
    node1 = adj$node1,
    node2 = adj$node2,
    N     = nrow(adj$W)
  ), "ok"))
})
