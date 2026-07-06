#!/usr/bin/env Rscript

# Regenerate constants for FmriArParitySuite.scala from the reference R package.
# Normal Scala tests do not call this script.

if (!requireNamespace("fmriAR", quietly = TRUE)) {
  stop("The fmriAR R package must be installed to regenerate parity fixtures.", call. = FALSE)
}

fmt <- function(x) {
  paste(sprintf("%.17g", as.numeric(x)), collapse = ", ")
}

fmt_vec <- function(name, x) {
  cat(name, " = Vector(", fmt(x), ")\n", sep = "")
}

fmt_mat <- function(name, m) {
  rows <- apply(as.matrix(m), 1, function(row) paste("Vector(", fmt(row), ")", sep = ""))
  cat(name, " = matrix(Vector(\n  ", paste(rows, collapse = ",\n  "), "\n))\n", sep = "")
}

kappa <- c(0.25, -0.1, 0.2)
phi <- fmriAR:::pacf_to_ar(kappa)
unstable <- fmriAR:::pacf_to_ar(c(1.4, -1.2, 0.7))
fmt_vec("pacfPhi", phi)
fmt_vec("pacfBack", fmriAR:::ar_to_pacf(phi))
fmt_vec("stablePhi", fmriAR:::enforce_stationary_ar(unstable, bound = 0.8))

arma_phi <- c(0.35, -0.1)
arma_theta <- 0.2
runs <- c(rep(1L, 4L), rep(2L, 4L))
censor <- c(2L, 7L)
y <- matrix(c(
  1, 0,
  2, 1,
  4, 1,
  7, 2,
  1, 1,
  3, 2,
  5, 3,
  8, 5
), nrow = 8, byrow = TRUE)
x <- cbind(1, seq_len(8), c(0, 1, 0, 1, 1, 0, 1, 0))
plan <- fmriAR:::new_whiten_plan(
  phi = list(arma_phi, arma_phi),
  theta = list(arma_theta, arma_theta),
  order = c(p = 2L, q = 1L),
  runs = runs,
  exact_first = FALSE,
  method = "arma",
  pooling = "run"
)
w <- fmriAR::whiten_apply(plan, x, y, runs = runs, censor = censor, parallel = FALSE)
fmt_mat("expectedX", w$X)
fmt_mat("expectedY", w$Y)

resid_none <- cbind(
  c(0, 1, 0, -1, 0, 1),
  c(1, 0, -1, 0, 1, 0)
)
none <- fmriAR::acorr_diagnostics(resid_none, max_lag = 3L, aggregate = "none")
fmt_mat("acfNone", none$acf)
cat("acfNoneCi = ", fmt(none$ci), "\n", sep = "")

resid_median <- cbind(
  c(0, 2, 0, -2, 0, 2),
  c(1, 1, -1, -1, 1, 1),
  c(-1, 0, 1, 0, -1, 0)
)
median <- fmriAR::acorr_diagnostics(resid_median, max_lag = 2L, aggregate = "median")
fmt_vec("acfMedian", median$acf)
cat("acfMedianCi = ", fmt(median$ci), "\n", sep = "")

n <- 24L
innov <- vapply(seq_len(n), function(i) {
  raw <- sin(i * 12.9898 + 78.233) * 43758.5453
  (raw - floor(raw)) * 2 - 1
}, numeric(1))
series <- numeric(n)
for (i in seq_len(n)) {
  series[i] <- innov[i] + if (i == 1L) 0 else 0.55 * series[i - 1L]
}
resid <- matrix(series, ncol = 1)
fit <- fmriAR::fit_noise(resid = resid, method = "ar", p = 1L, p_max = 1L, pooling = "global", exact_first = "none")
gamma <- fmriAR:::run_avg_acvf_cpp(resid, 1L)
fmt_vec("estimationSeries", series)
fmt_vec("fitNoiseGamma", gamma)
fmt_vec("fitNoisePhi", fit$phi[[1L]])
