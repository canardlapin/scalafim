# Independent base-R population construction, shared by generation and moment QA.
# The covariance is derived algebraically, not estimated from generated samples.
rank_population <- function(n, p, q, rho, nuisance = "intercept") {
  stopifnot(length(rho) == min(p, q), all(is.finite(rho)), all(rho >= 0 & rho < 1),
            n > 3, p > 0, q > 0, nuisance %in% c("intercept", "three-column"))
  # Keep the established intercept stream and draw order unchanged.
  residual_x <- matrix(rnorm(n * p), n, p)
  innovation <- matrix(rnorm(n * q), n, q)
  residual_y <- innovation
  for (j in seq_along(rho))
    residual_y[, j] <- rho[[j]] * residual_x[, j] + sqrt(1 - rho[[j]]^2) * innovation[, j]
  residual_covariance <- diag(p + q)
  for (j in seq_along(rho)) residual_covariance[j, p+j] <- residual_covariance[p+j, j] <- rho[[j]]
  if (nuisance == "intercept") {
    x <- residual_x; y <- residual_y; z <- matrix(1, n, 1)
    joint <- cbind(x, y); covariance <- residual_covariance
    conditional_scale <- 1
  } else {
    z0 <- matrix(rnorm(n * 2), n, 2)
    shared <- .4 * rowSums(z0)
    conditional_scale <- sqrt(1 - 2 * .4^2)
    x <- conditional_scale * residual_x + shared
    y <- conditional_scale * residual_y + shared
    z <- cbind(1, z0)
    joint <- cbind(x, y, z0)
    covariance <- matrix(.4, p + q + 2, p + q + 2)
    covariance[seq_len(p+q), seq_len(p+q)] <- .32 + .68 * residual_covariance
    covariance[(p+q+1):(p+q+2), (p+q+1):(p+q+2)] <- diag(2)
  }
  list(x = x, y = y, nuisance = z, joint = joint, covariance = covariance,
       residual_x = residual_x, residual_y = residual_y,
       residual_covariance = residual_covariance, conditional_scale = conditional_scale)
}
