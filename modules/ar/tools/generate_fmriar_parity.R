#!/usr/bin/env Rscript

# Regenerate constants for FmriArParitySuite.scala from fmriAR 0.3.3 source
# f563f2df20264ffa7db9be116f11d631551e9132.
# Normal Scala tests do not call this script.

if (!requireNamespace("fmriAR", quietly = TRUE)) {
  stop("The fmriAR R package must be installed to regenerate parity fixtures.", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to regenerate parity fixtures.", call. = FALSE)
}

out_file <- Sys.getenv(
  "FMRIAR_PARITY_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "ar.fmriar-parity.v1.r.json")
)
scala_out <- Sys.getenv(
  "FMRIAR_PARITY_SCALA_OUT",
  file.path(
    "modules", "ar", "shared", "src", "test", "scala", "scalafim",
    "fmri", "ar", "fixtures", "FmriArRFixture.scala"
  )
)
fmriar_pkg <- Sys.getenv("FMRIAR_R", file.path(path.expand("~"), "code", "fmriAR"))

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
fmt_vec("estimationSeries", series)
fixed_fits <- lapply(1:4, function(order) {
  fmriAR::fit_noise(
    resid = resid,
    method = "ar",
    p = order,
    p_max = 4L,
    pooling = "global",
    exact_first = "none"
  )
})
fmt_vec("fitNoiseGamma", fixed_fits[[1L]]$gamma[[1L]])
for (order in 1:4) {
  fmt_vec(paste0("fitNoisePhiAr", order), fixed_fits[[order]]$phi[[1L]])
}
auto_fit <- fmriAR::fit_noise(
  resid = resid,
  method = "ar",
  p = "auto",
  p_max = 4L,
  pooling = "global",
  exact_first = "none"
)
cat("fitNoiseAutoOrder = ", auto_fit$order[["p"]], "\n", sep = "")
fmt_vec("fitNoiseAutoPhi", auto_fit$phi[[1L]])

runs <- rep(c("run-b", "run-a"), each = 12L)
censor <- c(4L, 9L, 14L, 20L)
fit_run <- fmriAR::fit_noise(
  resid = resid,
  runs = runs,
  censor = censor,
  method = "ar",
  p = 1L,
  p_max = 1L,
  pooling = "run",
  exact_first = "none"
)
fit_global <- fmriAR::fit_noise(
  resid = resid,
  runs = runs,
  censor = censor,
  method = "ar",
  p = 1L,
  p_max = 1L,
  pooling = "global",
  exact_first = "none"
)
fmt_vec("censoredRunPhi", vapply(fit_run$phi, `[[`, numeric(1), 1L))
fmt_vec("censoredGlobalPhi", fit_global$phi[[1L]])

git_revision <- function(path) {
  value <- tryCatch(
    system2("git", c("-C", path, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(...) character(0)
  )
  if (length(value) == 0 || !nzchar(value[[1]])) "unresolved" else value[[1]]
}

package_version_at <- function(path) {
  description <- file.path(path, "DESCRIPTION")
  if (!file.exists(description)) return("unresolved")
  value <- tryCatch(read.dcf(description, fields = "Version"), error = function(...) NULL)
  if (is.null(value)) "unresolved" else as.character(value[[1]])
}

matrix_rows <- function(value) {
  lapply(seq_len(nrow(value)), function(index) unname(value[index, ]))
}

source <- list(
  fmriAR_revision = git_revision(normalizePath(fmriar_pkg, mustWork = FALSE)),
  fmriAR_version = package_version_at(normalizePath(fmriar_pkg, mustWork = FALSE)),
  r_version = as.character(getRversion()),
  producer = "modules/ar/tools/generate_fmriar_parity.R",
  reference = "fmriAR PACF, AR estimation, censor-aware pooling, ACF diagnostics, and ARMA whitening"
)
schema <- "scalafim-r-fmriar-parity-fixture/v1"
inputs <- list(
  pacf = kappa,
  unstable_pacf = c(1.4, -1.2, 0.7),
  arma_phi = arma_phi,
  arma_theta = arma_theta,
  whitening_runs = c(rep(1L, 4L), rep(2L, 4L)),
  whitening_censor = c(2L, 7L),
  estimation_series = series,
  estimation_runs = runs,
  estimation_censor = censor
)
outputs <- list(
  pacf_phi = phi,
  pacf_back = fmriAR:::ar_to_pacf(phi),
  stable_phi = fmriAR:::enforce_stationary_ar(unstable, bound = 0.8),
  whitened_design = matrix_rows(w$X),
  whitened_response = matrix_rows(w$Y),
  acf_none = matrix_rows(none$acf),
  acf_none_ci = none$ci,
  acf_median = unname(median$acf),
  acf_median_ci = median$ci,
  fit_noise_gamma = fixed_fits[[1L]]$gamma[[1L]],
  fit_noise_phi_by_order = lapply(fixed_fits, function(value) value$phi[[1L]]),
  fit_noise_auto_order = unname(auto_fit$order[["p"]]),
  fit_noise_auto_phi = auto_fit$phi[[1L]],
  censored_run_phi = vapply(fit_run$phi, `[[`, numeric(1), 1L),
  censored_global_phi = fit_global$phi[[1L]]
)
receipt <- list(
  schema_version = schema,
  producer_command = "bash tools/r-parity/regenerate_receipts.sh --r-only",
  conventions = list(
    dtype = "float64",
    censoring = "one-based fmriAR censor inputs; generated Scala fixture stores the resulting numerical oracle",
    pooling = "run and global pooling are recorded separately"
  ),
  accepted_differences = character(0),
  hashes = list(inputs_sha256 = "", outputs_sha256 = ""),
  source = source
)
payload <- list(
  schema_version = schema,
  scenario_id = "ar.fmriar-parity.v1",
  inputs = inputs,
  outputs = outputs,
  source = source,
  receipt = receipt
)

scala_number <- function(value) {
  if (abs(value) < 5e-16) return("0.0")
  rendered <- sprintf("%.17g", value)
  if (grepl("[.eE]", rendered)) rendered else paste0(rendered, ".0")
}
scala_doubles <- function(values) {
  paste0("Vector(", paste(vapply(as.numeric(values), scala_number, character(1)), collapse = ", "), ")")
}
scala_rows <- function(value) {
  paste0(
    "Vector(",
    paste(vapply(seq_len(nrow(value)), function(row) scala_doubles(value[row, ]), character(1)), collapse = ", "),
    ")"
  )
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, out_file, auto_unbox = TRUE, digits = 17, pretty = TRUE)
message("wrote ", out_file)

dir.create(dirname(scala_out), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    "package scalafim.fmri.ar.fixtures",
    "",
    "// Generated by modules/ar/tools/generate_fmriar_parity.R.",
    "object FmriArRFixture:",
    paste0("  val pacfPhi: Vector[Double] = ", scala_doubles(outputs$pacf_phi)),
    paste0("  val pacfBack: Vector[Double] = ", scala_doubles(outputs$pacf_back)),
    paste0("  val stablePhi: Vector[Double] = ", scala_doubles(outputs$stable_phi)),
    paste0("  val expectedXRows: Vector[Vector[Double]] = ", scala_rows(w$X)),
    paste0("  val expectedYRows: Vector[Vector[Double]] = ", scala_rows(w$Y)),
    paste0("  val acfNoneRows: Vector[Vector[Double]] = ", scala_rows(none$acf)),
    paste0("  val acfNoneCi: Double = ", scala_number(none$ci)),
    paste0("  val acfMedian: Vector[Double] = ", scala_doubles(median$acf)),
    paste0("  val acfMedianCi: Double = ", scala_number(median$ci)),
    paste0("  val estimationSeries: Vector[Double] = ", scala_doubles(series)),
    paste0("  val fitNoiseGamma: Vector[Double] = ", scala_doubles(outputs$fit_noise_gamma)),
    paste0(
      "  val fitNoisePhiByOrder: Vector[Vector[Double]] = Vector(",
      paste(vapply(outputs$fit_noise_phi_by_order, scala_doubles, character(1)), collapse = ", "),
      ")"
    ),
    paste0("  val fitNoiseAutoOrder: Int = ", as.integer(outputs$fit_noise_auto_order)),
    paste0("  val fitNoiseAutoPhi: Vector[Double] = ", scala_doubles(outputs$fit_noise_auto_phi)),
    paste0("  val censoredRunPhi: Vector[Double] = ", scala_doubles(outputs$censored_run_phi)),
    paste0("  val censoredGlobalPhi: Vector[Double] = ", scala_doubles(outputs$censored_global_phi)),
    paste0("  val fmriArRevision: String = \"", source$fmriAR_revision, "\""),
    paste0("  val fmriArVersion: String = \"", source$fmriAR_version, "\"")
  ),
  scala_out
)
message("wrote ", scala_out)
