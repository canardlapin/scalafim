#!/usr/bin/env Rscript

# Independent active-face oracle for the nonnegative one-shot canonical root.
# Reuse the frozen DGP from the ordinary canonical fixture generator, suppressing
# its Scala output, then enumerate every non-empty coordinate face. Within each
# face base R solves the symmetric-definite generalized eigenproblem directly.

invisible(capture.output(source("tools/r-parity/generate_cca_one_shot_fixtures.R", local = .GlobalEnv)))

nonnegative_direction <- function(effect, residual, ridge_fraction) {
  ridge <- ridge_fraction * mean(diag(residual))
  regularized <- residual + diag(ridge, nrow(residual))
  dimension <- nrow(effect)
  best <- NULL

  for (mask in seq_len(2^dimension - 1L)) {
    active <- which(as.logical(intToBits(mask)[seq_len(dimension)]))
    a <- effect[active, active, drop = FALSE]
    b <- regularized[active, active, drop = FALSE]
    upper <- chol(b)
    whitened <- solve(t(upper), a) %*% solve(upper)
    solved <- eigen((whitened + t(whitened)) / 2, symmetric = TRUE)
    candidate <- drop(solve(upper, solved$vectors[, 1, drop = FALSE]))
    if (all(candidate <= 1e-10)) candidate <- -candidate

    if (all(candidate >= -1e-10)) {
      candidate <- pmax(candidate, 0)
      direction <- numeric(dimension)
      direction[active] <- candidate
      direction <- direction / sqrt(drop(t(direction) %*% regularized %*% direction))
      root <- drop(t(direction) %*% effect %*% direction)
      if (is.null(best) || root > best$root) best <- list(direction = direction, root = root)
    }
  }

  stopifnot(!is.null(best), all(best$direction >= -1e-12))
  c(best, list(ridge = ridge))
}

constrained_folds <- lapply(seq_along(moments), function(held_out) {
  training <- setdiff(seq_along(moments), held_out)
  effect <- Reduce(`+`, lapply(moments[training], `[[`, "effect"))
  residual <- Reduce(`+`, lapply(moments[training], `[[`, "residual"))
  fit <- nonnegative_direction(effect, residual, ridge_fraction)
  held <- moments[[held_out]]
  heldout_root <- drop(t(fit$direction) %*% held$effect %*% fit$direction) /
    drop(t(fit$direction) %*% held$residual %*% fit$direction)
  c(fit$direction, fit$root, fit$ridge, heldout_root)
})

options(digits = 17)
for (index in seq_along(constrained_folds)) {
  cat(sprintf("fold%d <- c(%s)\n", index, paste(sprintf("%.17g", constrained_folds[[index]]), collapse = ", ")))
}
heldout <- vapply(constrained_folds, function(value) value[length(value)], numeric(1))
cat(sprintf("mean <- %.17g\n", mean(heldout)))
cat(sprintf("correlation <- %.17g\n", sqrt(mean(heldout) / (1 + mean(heldout)))) )
