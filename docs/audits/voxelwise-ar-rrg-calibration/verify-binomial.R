#!/usr/bin/env Rscript
# Independent exact-binomial oracle, including extreme counts.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L, !file.exists(args[[1L]]))
alpha <- 0.05 / (48 * 3)
k <- c(0, 1, 410, 475, 500, 1860, 1900, 2000)
n <- c(rep(500, 5), rep(2000, 3))
result <- lapply(seq_along(k), function(i) list(
  k = k[i], n = n[i], alpha = alpha,
  lower = if (k[i] == 0) 0 else qbeta(alpha / 2, k[i], n[i] - k[i] + 1),
  upper = if (k[i] == n[i]) 1 else qbeta(1 - alpha / 2, k[i] + 1, n[i] - k[i])))
jsonlite::write_json(result, args[[1L]], digits = 17L, auto_unbox = TRUE, pretty = TRUE)
