#!/usr/bin/env Rscript
# Independent base-R stationary joint AR(1) construction for the diagnostic.
# R's chol is upper triangular; transpose explicitly for column-vector draws.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, !file.exists(args[[1L]]))
rho <- c(0.1, 0.5, -0.2)
sigma <- 0.36 * (0.5 * diag(3L) + 0.5 * matrix(1, 3L, 3L))
gamma <- sigma / (1 - outer(rho, rho))
innovation_lower <- t(chol(sigma))
stationary_lower <- t(chol(gamma))
initial_z <- c(0.25, -1, 0.75)
innovation_z <- matrix(c(
  1, 0, -0.5,
  -0.25, 0.8, 1.2,
  0.4, -0.6, 0.1,
  -1.1, 0.3, 0.7,
  0.2, 1.4, -0.9
), ncol = 3L, byrow = TRUE)
path <- matrix(NA_real_, 6L, 3L)
path[1L, ] <- stationary_lower %*% initial_z
for (i in 2:6) path[i, ] <- rho * path[i - 1L, ] + innovation_lower %*% innovation_z[i - 1L, ]
stopifnot(max(abs(gamma - diag(rho) %*% gamma %*% diag(rho) - sigma)) < 1e-14)
stopifnot(max(abs(innovation_lower %*% t(innovation_lower) - sigma)) < 1e-14)
stopifnot(max(abs(stationary_lower %*% t(stationary_lower) - gamma)) < 1e-14)
result <- list(
  schema = "voxelwise-rrg-parametric-r-oracle-v1",
  r_version = as.character(getRversion()), rho = rho,
  sigma = sigma, stationary_covariance = gamma,
  innovation_lower = innovation_lower, stationary_lower = stationary_lower,
  initial_z = initial_z, innovation_z = innovation_z, noise_path = path,
  lag_one_covariance = diag(rho) %*% gamma,
  lag_two_covariance = diag(rho^2) %*% gamma,
  convention = "Rows are physical times 0..5; lag covariance is Cov(u_t,u_(t-k)); no resets within a run."
)
jsonlite::write_json(result, args[[1L]], digits = 17L, auto_unbox = TRUE, pretty = TRUE)
