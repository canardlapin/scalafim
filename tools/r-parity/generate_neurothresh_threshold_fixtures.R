#!/usr/bin/env Rscript

# Generate small deterministic fixtures for the first ScalaFIM thresholding
# slice. The calculations intentionally mirror neurothresh's R algorithms using
# base R only, so the script can run even before the R package is installed.

score_set <- function(indices, z_vec, pi_vec, kappa = 1.0) {
  if (!length(indices)) return(-Inf)
  z <- z_vec[indices]
  w <- pi_vec[indices]
  valid <- is.finite(z) & is.finite(w)
  if (!any(valid)) return(-Inf)
  z <- z[valid]
  w <- w[valid]
  a <- kappa * z
  amax <- max(a[w > 0])
  amax + log(sum(w * exp(a - amax)))
}

score_set_stabilized <- function(indices, z_vec, pi_vec) {
  if (!length(indices)) return(c(U0 = -Inf, n_eff = 0))
  z <- z_vec[indices]
  w <- pi_vec[indices]
  valid <- is.finite(z) & is.finite(w)
  z <- z[valid]
  w <- w[valid]
  sum_wz <- sum(w * z)
  sum_w <- sum(w)
  sum_w2 <- sum(w * w)
  c(U0 = sum_wz / sqrt(sum_w2), n_eff = (sum_w * sum_w) / sum_w2)
}

wy_stepdown <- function(observed, null_matrix) {
  m <- length(observed)
  B <- nrow(null_matrix)
  ord <- order(observed, decreasing = TRUE)
  obs_sorted <- observed[ord]
  null_sorted <- null_matrix[, ord, drop = FALSE]
  successive <- null_sorted
  if (m > 1) {
    for (j in (m - 1):1) successive[, j] <- pmax(successive[, j], successive[, j + 1])
  }
  p_raw <- vapply(seq_len(m), function(j) {
    (1 + sum(successive[, j] >= obs_sorted[j])) / (B + 1)
  }, numeric(1))
  p_adj <- cummax(p_raw)
  out <- numeric(m)
  out[ord] <- p_adj
  out
}

z <- c(1, 2, 3)
pi <- c(1, 1, 2)
pi <- pi / sum(pi)
idx <- c(1, 3)

observed <- c(3.5, 2.1, 4.2)
nulls <- matrix(
  c(
    2, 1, 3,
    4, 1, 2,
    3, 3, 3,
    5, 0, 1
  ),
  nrow = 4,
  byrow = TRUE
)

out <- data.frame(
  fixture = c("softmax", "diffuse_u0", "diffuse_neff", "wy_p1", "wy_p2", "wy_p3"),
  value = c(
    score_set(idx, z, pi, kappa = 1),
    score_set_stabilized(idx, z, pi)[["U0"]],
    score_set_stabilized(idx, z, pi)[["n_eff"]],
    wy_stepdown(observed, nulls)
  )
)

write.csv(out, row.names = FALSE)
