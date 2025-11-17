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

.components_from_edges <- function(n, node1, node2) {
  # No nodes
  if (n == 0) {
    return(list(comp_id = integer(), N_comps = 0))
  }
  
  # Ensure integers (cheap + defensive)
  node1 <- as.integer(node1)
  node2 <- as.integer(node2)
  stopifnot(length(node1) == length(node2))
  
  # Build adjacency list
  adj <- rep(list(integer(0L)), n)
  
  if (length(node1) > 0) {
    # make edges undirected by adding both (a -> b) and (b -> a)
    all_from <- c(node1, node2)
    all_to   <- c(node2, node1)
    
    # Split neighbors by source node
    split_adj <- split(all_to, all_from)
    
    # Fill only existing entries
    # names(split_adj) are node indices as character
    idx <- as.integer(names(split_adj))
    adj[idx] <- split_adj
  }
  
  # Connected components via BFS
  comp_id <- integer(n)   # 0 = unvisited
  comp    <- 0L
  
  for (start in seq_len(n)) {
    if (comp_id[start] != 0L) next  # already assigned
    
    comp <- comp + 1L
    # BFS queue implemented as a vector with a head index
    queue <- start
    head  <- 1L
    
    while (head <= length(queue)) {
      v <- queue[head]
      head <- head + 1L
      
      if (comp_id[v] != 0L) next  # might have been assigned via another path
      
      comp_id[v] <- comp
      nbrs <- adj[[v]]
      
      if (length(nbrs)) {
        unvisited <- nbrs[comp_id[nbrs] == 0L]
        if (length(unvisited)) {
          queue <- c(queue, unvisited)
        }
      }
    }
  }
  
  list(
    comp_id = as.integer(comp_id),
    N_comps = as.integer(comp)
  )
}

bym2_scale_inla <- function(node1, node2, N) {
  if (!requireNamespace("INLA", quietly = TRUE))
    stop("Package 'INLA' is required.")
  if (!requireNamespace("Matrix", quietly = TRUE))
    stop("Package 'Matrix' is required.")
  
  node1 <- as.integer(node1)
  node2 <- as.integer(node2)
  stopifnot(length(node1) == length(node2), N >= 1L)
  if (N == 1L || length(node1) == 0L) return(1)  # trivial / no neighbors
  
  # Sparse symmetric adjacency
  W <- Matrix::sparseMatrix(
    i    = c(node1, node2),
    j    = c(node2, node1),
    x    = 1,
    dims = c(N, N)
  )
  W <- Matrix::forceSymmetric(W, uplo = "U")
  
  # ICAR structure matrix Q = D - W (tau = 1)
  d <- Matrix::rowSums(W)
  Q <- Matrix::Diagonal(x = d) - W
  
  # Scale Q using INLA (sum-to-zero constraint)
  Qs <- INLA::inla.scale.model(
    Q,
    constr = list(A = matrix(1, 1, N), e = 0)
  )
  
  # Extract the scalar scaling factor c such that Qs ≈ c * Q
  nz <- Matrix::summary(Q)
  if (nrow(nz) == 0L) return(1)
  i <- nz$i[1]; j <- nz$j[1]
  as.numeric(Qs[i, j] / Q[i, j])
}

