# Reproduce the ReducedRankGls conditional/bootstrap fixture used by ScalaFIM.
#
# The QR/SVD, conditional variance, and residual bootstrap follow
# fmrireg:::.rrr_fit_task_subspace() and .rrr_bootstrap_task_se(). Bootstrap
# row selection uses ScalaFIM's documented Park-Miller stream so JVM and
# Scala.js consume identical resamples.

options(digits = 17)

X <- rbind(
  c(-2, 1, 1), c(-1, 0, 1), c(0, 1, 1),
  c(1, 0, 1), c(2, 1, 1), c(3, 0, 1)
)
Y <- rbind(
  c(-1, 1.5, 0.2), c(0.2, 1, -0.3), c(1.4, 0.2, 0.6),
  c(2.1, -0.4, 0.4), c(3.7, -1.2, 1.1), c(4.2, -1.6, 0.7)
)

X_task <- X[, 1:2, drop = FALSE]
Z <- X[, 3, drop = FALSE]
qz <- qr(Z)
Qz <- qr.Q(qz)[, seq_len(qz$rank), drop = FALSE]
X0 <- X_task - Qz %*% crossprod(Qz, X_task)
Y0 <- Y - Qz %*% crossprod(Qz, Y)

fit_rrr <- function(design, response, rank = 1L) {
  qr_x <- qr(design)
  rank_x <- qr_x$rank
  Qx <- qr.Q(qr_x)[, seq_len(rank_x), drop = FALSE]
  R <- qr.R(qr_x)[seq_len(rank_x), seq_len(rank_x), drop = FALSE]
  piv <- qr_x$pivot[seq_len(rank_x)]
  G <- crossprod(Qx, response)
  eig <- eigen(tcrossprod(G), symmetric = TRUE)
  singular_values <- sqrt(pmax(eig$values, 0))
  tol <- max(dim(G)) * .Machine$double.eps * max(c(singular_values, 1))
  keep <- singular_values > tol
  U <- eig$vectors[, keep, drop = FALSE]
  d <- singular_values[keep]
  rank <- max(1L, min(as.integer(rank), length(d)))
  U_r <- U[, seq_len(rank), drop = FALSE]
  d_r <- d[seq_len(rank)]
  V_r <- sweep(t(G) %*% U_r, 2L, d_r, `/`)
  C_pivot <- backsolve(R, sweep(U_r, 2L, d_r, `*`))
  C_task <- matrix(0, nrow = ncol(design), ncol = rank)
  C_task[piv, ] <- C_pivot
  list(
    B = C_task %*% t(V_r),
    C = C_task,
    V = V_r,
    normalized_covariance = solve(crossprod(design))
  )
}

fit <- fit_rrr(X0, Y0)
df_residual <- 3
latent_residual <- Y0 %*% fit$V - X0 %*% fit$C
sigma_r <- crossprod(latent_residual) / df_residual
conditional_variance <- pmax(
  rowSums((fit$V %*% sigma_r) * fit$V),
  .Machine$double.eps
)
conditional_se <- outer(
  sqrt(diag(fit$normalized_covariance)),
  sqrt(conditional_variance)
)

park_miller <- local({
  state <- 7
  function(bound) {
    state <<- (state * 48271) %% 2147483647
    as.integer((state - 1) %% bound) + 1L
  }
})

blocks <- list(1:2, 3:4, 5:6)
fitted <- X0 %*% fit$B
residual <- Y0 - fitted
bootstrap_fits <- vector("list", 32)
for (replicate in seq_len(32)) {
  indices <- integer()
  while (length(indices) < nrow(X0)) {
    indices <- c(indices, blocks[[park_miller(length(blocks))]])
  }
  indices <- indices[seq_len(nrow(X0))]
  bootstrap_fits[[replicate]] <- fit_rrr(
    X0,
    fitted + residual[indices, , drop = FALSE]
  )$B
}

bootstrap_array <- array(unlist(bootstrap_fits), dim = c(2, 3, 32))
bootstrap_covariance <- lapply(
  seq_len(3),
  function(voxel) cov(t(bootstrap_array[, voxel, ]))
)
bootstrap_se <- sapply(bootstrap_covariance, function(value) sqrt(diag(value)))

print(list(
  task_normalized_covariance = fit$normalized_covariance,
  conditional_variance = conditional_variance,
  conditional_standard_errors = conditional_se,
  task_a_t = fit$B[1, ] / conditional_se[1, ],
  task_f = vapply(
    seq_len(ncol(Y)),
    function(voxel) {
      drop(t(fit$B[, voxel]) %*% solve(fit$normalized_covariance) %*% fit$B[, voxel]) /
        2 / conditional_variance[voxel]
    },
    numeric(1)
  ),
  bootstrap_standard_errors = bootstrap_se,
  bootstrap_task_a_t = fit$B[1, ] / bootstrap_se[1, ],
  bootstrap_covariance = bootstrap_covariance
))
