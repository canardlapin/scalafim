#!/usr/bin/env Rscript

# Independent base-R oracle for SignedCrossRunRayleighReferenceFixtures.
# This intentionally materializes ordinary dense products and uses base R's
# eigen/solve path rather than any ScalaFIM or fmrireg.cca implementation.

X <- rbind(
  c(1, -1, -1), c(1, -1, -0.7), c(1, 1, -0.4), c(1, 1, -0.1),
  c(1, -1, 0.1), c(1, -1, 0.4), c(1, 1, 0.7), c(1, 1, 1)
)

base_response <- rbind(
  c(-0.9, 0.1, -0.15, -0.2), c(-1.14, 0.5, -0.09, -0.5),
  c(0.91, -0.85, 0.21, 0.5), c(0.67, -0.55, 0.47, 0.2),
  c(-1.02, 0.75, -0.47, -0.1), c(-0.56, 0.65, -0.56, -0.4),
  c(0.99, -0.2, 0.49, 0.6), c(1.05, -0.4, 0.1, 0.3)
)

runs <- lapply(0:2, function(run) {
  out <- base_response
  for (time in 0:7) {
    for (feature in 0:3) {
      out[time + 1, feature + 1] <- out[time + 1, feature + 1] +
        0.03 * run * (((time + feature) %% 3) - 1)
    }
  }
  out
})

cvec <- matrix(c(0, 1, 0), ncol = 1)
xtx_inv <- solve(crossprod(X))
contrast_variance <- drop(t(cvec) %*% xtx_inv %*% cvec)

moments <- lapply(runs, function(Z) {
  G <- crossprod(Z, X)
  u <- G %*% xtx_inv %*% cvec
  residual <- crossprod(Z) - G %*% xtx_inv %*% t(G)
  list(
    u = drop(u),
    effect = tcrossprod(u / sqrt(contrast_variance)),
    residual = 0.5 * (residual + t(residual))
  )
})

fit_fold <- function(held_out, ridge_fraction = 0.05) {
  training <- setdiff(seq_along(moments), held_out)
  effect <- Reduce(`+`, lapply(moments[training], `[[`, "effect"))
  residual <- Reduce(`+`, lapply(moments[training], `[[`, "residual"))
  ridge <- ridge_fraction * sum(diag(residual)) / nrow(residual)
  regularized <- residual + diag(ridge, nrow(residual))
  decomposition <- eigen(solve(regularized, effect))
  w <- Re(decomposition$vectors[, which.max(Re(decomposition$values))])
  w <- w / sqrt(drop(t(w) %*% regularized %*% w))
  anchor <- which.max(abs(w))
  if (w[anchor] < 0) w <- -w
  train_projection <- sum(vapply(training, function(index) sum(w * moments[[index]]$u), numeric(1)))
  held_projection <- sum(w * moments[[held_out]]$u)
  numerator <- train_projection * held_projection / contrast_variance
  denominator <- drop(t(w) %*% regularized %*% w)
  c(
    numerator = numerator,
    denominator = denominator,
    statistic = numerator / denominator,
    ridge = ridge,
    direction = w
  )
}

folds <- lapply(seq_along(moments), fit_fold)
print(do.call(rbind, folds), digits = 17)
cat("mean =", format(mean(vapply(folds, `[[`, numeric(1), "statistic")), digits = 17), "\n")
