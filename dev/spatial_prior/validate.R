# Check per-component zero-sum (should be ~0 to floating-point)
check_component_constraints <- function(R, comp_id, tol = 1e-8) {
  tapply(seq_len(nrow(R)), comp_id, function(idx) {
    colSums(R[idx, , drop = FALSE])
  })
}
# Example: all entries of each returned vector should be ~ 0 (<= 1e-8).

# Check that the structured field has GM(marg var) ≈ 1 on each component
check_gmvar_one <- function(R, comp_id, B = 2000) {
  set.seed(1)
  eta <- matrix(rnorm(ncol(R) * B), ncol = B)
  phi <- R %*% eta
  out <- tapply(seq_len(nrow(R)), comp_id, function(idx) {
    if (length(idx) == 1L) return(NA_real_)
    vhat <- apply(phi[idx, , drop = FALSE], 1, var)
    exp(mean(log(vhat)))
  })
  unlist(out)
}


# Laplacian eigensystem
lap_eigs <- function(W) {
  L <- diag(rowSums(W)) - as.matrix(W)
  eig <- eigen(L, symmetric = TRUE); list(lam = eig$values, U = eig$vectors)
}

estimate_alpha <- function(phi, lam, U, tol = 1e-10) {
  pos <- lam > tol; lam <- lam[pos]; U <- U[, pos, drop = FALSE]
  coeff <- crossprod(U, phi)
  s2 <- coeff^2 + 1e-12
  as.numeric(-coef(lm(log(s2) ~ log(lam)))[2])
}

# Estimate α on multiple posterior draws of phi_zip
posterior_alpha <- function(fit, W, ndraws = 200) {
  E <- lap_eigs(W)
  mat <- posterior::as_draws_matrix(fit$draws("phi_zip"))  # draws x N_zip
  sel <- sample(seq_len(nrow(mat)), min(ndraws, nrow(mat)))
  apply(mat[sel, , drop = FALSE], 1, function(row) estimate_alpha(as.numeric(row), E$lam, E$U))
}

E <- lap_eigs(sim$W)
alpha_true_hat <- estimate_alpha(phi_true, E$lam, E$U)

alphas_fit <- posterior_alpha(fit, sim$W, ndraws = 200)
print(c(true = alpha_true_hat, mean_fit = mean(alphas_fit), median_fit = median(alphas_fit)))
print(quantile(alphas_fit, c(.1,.5,.9)))

# weighted means in the realized sample
mu_race <- with(sim$stan_data, mean(sim$true$a_race[J_race]))
mu_age  <- with(sim$stan_data, mean(sim$true$a_age[J_age]))
mu_time <- with(sim$stan_data, mean(sim$true$a_time[J_time]))
mu_zip  <- with(sim$stan_data, mean(sim$true$a_zip[J_zip]))
intercept_eff_true <- sim$true$intercept + mu_race + mu_age + mu_time + mu_zip
intercept_post <- posterior::summarize_draws(fit$draws("intercept"))
print(c(effective_true = intercept_eff_true, post_mean = intercept_post$mean))


post <- posterior::as_draws_matrix(
  fit$draws(c("lambda_race","lambda_age","lambda_time","lambda_zip","rho_zip"))
)
print(round(cor(post), 2))  # look for strong negatives

# Expand R to person-level; project each X column onto span(Rn)
confounding_index <- function(X, R, J_zip) {
  Rn  <- R[J_zip, , drop = FALSE]                 # N × N_pos
  RtR <- crossprod(Rn)
  L   <- chol(RtR + 1e-9 * diag(ncol(Rn)))        # stab
  
  proj_norm <- function(x) {
    y  <- forwardsolve(t(L), crossprod(Rn, x))
    w  <- backsolve(L, y)
    xp <- Rn %*% w                                 # projection of x onto span(Rn)
    sqrt(sum(xp^2) / sum(x^2))
  }
  apply(X, 2, proj_norm)
}

ci <- confounding_index(sim$stan_data$X, sim$stan_data$R, sim$stan_data$J_zip)
print(ci)  # values ≥ ~0.2 indicate meaningful overlap with the spatial basis


# X_tilde <- spatial_plus(sim$stan_data$X, sim$stan_data$R, sim$stan_data$J_zip)
# Refit using X_tilde both in simulation (to build y) and in Stan data.

# ZIP-level means of realized demo effects
N_zip <- sim$stan_data$N_zip
J_zip <- sim$stan_data$J_zip

zip_mean <- function(v) tapply(v, J_zip, mean)[seq_len(N_zip)]

m_race <- zip_mean(sim$true$a_race[sim$stan_data$J_race])
m_age  <- zip_mean(sim$true$a_age[sim$stan_data$J_age])
m_time <- zip_mean(sim$true$a_time[sim$stan_data$J_time])

a_zip_true <- sim$true$a_zip
a_zip_hat  <- colMeans(posterior::as_draws_matrix(fit$draws("a_zip")))

cor_true <- c(race=cor(m_race, a_zip_true), age=cor(m_age, a_zip_true), time=cor(m_time, a_zip_true))
cor_post <- c(race=cor(m_race, a_zip_hat),  age=cor(m_age, a_zip_hat),  time=cor(m_time, a_zip_hat))
View(rbind(cor_true, cor_post))

# Per-zip info
by_zip <- aggregate(list(n = sim$data$n_sample, y = sim$data$y),
                    list(z = sim$stan_data$J_zip), sum)
p_hat <- with(by_zip, y / pmax(1, n))
info  <- with(by_zip, n * p_hat * (1 - p_hat))
print(summary(info))
print(mean(info < 3))  # fraction of weakly informed ZIPs


# sum-to-zero contstraint
phi_draws <- posterior::as_draws_matrix(fit$draws("phi_zip"))
by_comp_sum <- tapply(seq_len(ncol(phi_draws)), sim$basis$comp_id, \(idx)
                      rowMeans(phi_draws[, idx, drop=FALSE])
)
print(sapply(by_comp_sum, mean))  # each near 0

# Jensen's curve
invlogit <- function(x) 1/(1+exp(-x)); gprime <- function(x) invlogit(x)*(1-invlogit(x))
g2 <- function(x) gprime(x)*(1-2*invlogit(x))
mu0 <- as.numeric(mean(sim$stan_data$X %*% sim$true$beta) + sim$true$intercept)
var_a <- with(sim$true, lambda_zip^2 + lambda_race^2 + lambda_age^2 + lambda_time^2)
delta <- -0.5 * var_a * g2(mu0) / gprime(mu0)
print(c(pred_bias = delta, post_minus_true = mean(fit$draws("intercept")) - sim$true$intercept))

# Ensure R rows align with zip levels used in J_zip
stopifnot(identical(levels(sim$data$zip), rownames(sim$stan_data$R) %||% levels(sim$data$zip)))




