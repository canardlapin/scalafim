#!/usr/bin/env Rscript

# Independent mixed-TR runwise and fixed-effects receipt for S13.
#
# From the ScalaFIM repository root:
#   LC_ALL=C LANG=C Rscript tools/r-parity/generate_mixed_tr_fixed_effects_receipt.R
#   python3 tools/r-parity/finalize_mixed_tr_fixed_effects_receipt.py

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate the mixed-TR receipt", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to generate the mixed-TR receipt", call. = FALSE)
}

r_pkg <- Sys.getenv("FMRIDESIGN_R", file.path(path.expand("~"), "code", "fmridesign"))
hrf_pkg <- Sys.getenv("FMRIHRF_R", file.path(path.expand("~"), "code", "fmrihrf"))
out_file <- Sys.getenv(
  "MIXED_TR_FIXED_EFFECTS_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "fit.mixed-tr-fixed-effects.v1.r.json")
)
scala_out <- Sys.getenv(
  "MIXED_TR_FIXED_EFFECTS_SCALA_OUT",
  file.path(
    "modules", "fit", "shared", "src", "test", "scala",
    "scalafim", "fmri", "fit", "fixtures", "MixedTrFixedEffectsRFixture.scala"
  )
)

pkgload::load_all(r_pkg, quiet = TRUE)

git_revision <- function(path) {
  value <- tryCatch(
    system2("git", c("-C", path, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(...) character(0)
  )
  if (length(value) == 0 || !nzchar(value[[1]])) "unresolved" else value[[1]]
}

matrix_rows <- function(value) {
  lapply(seq_len(nrow(value)), function(index) unname(value[index, ]))
}

events <- data.frame(
  onset = c(0, 4, 0, 4),
  run = factor(c("run-1", "run-1", "run-2", "run-2"), levels = c("run-1", "run-2")),
  cond = factor(c("A", "B", "A", "B"), levels = c("A", "B"))
)
sampling_frame <- fmrihrf::sampling_frame(
  blocklens = c(8, 12),
  TR = c(2, 0.8),
  start_time = c(0, 0),
  precision = 0.1
)
event_model_value <- event_model(
  onset ~ hrf(cond),
  data = events,
  block = ~run,
  sampling_frame = sampling_frame,
  precision = 0.1
)
event_design <- unname(as.matrix(design_matrix(event_model_value)))
design <- cbind(event_design, intercept = 1)
colnames(design) <- c("cond.A", "cond.B", "intercept")

run_rows <- list(1:8, 9:20)
run_betas <- list(
  matrix(c(1.20, -0.60, 0.50, -0.40, 0.80, -1.00), nrow = 3, ncol = 2),
  matrix(c(1.40, -0.90, 0.60, -0.60, 1.00, -0.80), nrow = 3, ncol = 2)
)
noise <- cbind(
  c(-0.08, 0.04, 0.11, -0.05, 0.07, -0.12, 0.03, 0.09,
    0.05, -0.09, 0.13, -0.04, 0.08, -0.11, 0.02, 0.10, -0.06, 0.07, -0.03, 0.12),
  c(0.06, -0.10, 0.04, 0.12, -0.07, 0.09, -0.03, -0.08,
    -0.04, 0.11, -0.06, 0.08, -0.12, 0.03, 0.09, -0.07, 0.05, -0.10, 0.13, -0.02)
)
response <- matrix(0, nrow = nrow(design), ncol = 2)
for (run in seq_along(run_rows)) {
  rows <- run_rows[[run]]
  response[rows, ] <- design[rows, , drop = FALSE] %*% run_betas[[run]] + noise[rows, ]
}
colnames(response) <- c("signal_a", "signal_b")

run_fits <- lapply(run_rows, function(rows) {
  x <- design[rows, , drop = FALSE]
  y <- response[rows, , drop = FALSE]
  fit <- stats::lm.fit(x = x, y = y)
  normalized_covariance <- solve(crossprod(x))
  residual_variance <- colSums(fit$residuals^2) / fit$df.residual
  list(
    coefficients = unname(fit$coefficients),
    residual_variance = unname(residual_variance),
    normalized_covariance = unname(normalized_covariance),
    residual_df = unname(fit$df.residual),
    covariance_by_voxel = lapply(residual_variance, function(value) unname(value * normalized_covariance))
  )
})

fixed_covariance <- vector("list", ncol(response))
fixed_coefficients <- matrix(0, nrow = ncol(design), ncol = ncol(response))
for (voxel in seq_len(ncol(response))) {
  precisions <- lapply(run_fits, function(run) solve(run$covariance_by_voxel[[voxel]]))
  total_precision <- Reduce(`+`, precisions)
  covariance <- solve(total_precision)
  weighted <- Reduce(
    `+`,
    Map(function(precision, run) precision %*% run$coefficients[, voxel], precisions, run_fits)
  )
  fixed_covariance[[voxel]] <- unname(covariance)
  fixed_coefficients[, voxel] <- unname(covariance %*% weighted)
}
fixed_standard_errors <- matrix(0, nrow = ncol(design), ncol = ncol(response))
for (voxel in seq_len(ncol(response))) {
  fixed_standard_errors[, voxel] <- sqrt(diag(fixed_covariance[[voxel]]))
}
fixed_residual_df <- sum(vapply(run_fits, `[[`, numeric(1), "residual_df"))
contrast <- c(1, -1, 0)
contrast_estimate <- drop(crossprod(contrast, fixed_coefficients))
contrast_standard_error <- vapply(
  fixed_covariance,
  function(covariance) sqrt(drop(crossprod(contrast, covariance %*% contrast))),
  numeric(1)
)
contrast_statistic <- contrast_estimate / contrast_standard_error

fmridesign_root <- normalizePath(r_pkg, mustWork = FALSE)
source <- list(
  fmridesign_revision = git_revision(fmridesign_root),
  fmrihrf_revision = git_revision(normalizePath(hrf_pkg, mustWork = FALSE)),
  fmridesign_version = as.character(utils::packageVersion("fmridesign")),
  fmrihrf_version = as.character(utils::packageVersion("fmrihrf")),
  r_version = as.character(getRversion()),
  stats_version = as.character(utils::packageVersion("stats")),
  producer = "tools/r-parity/generate_mixed_tr_fixed_effects_receipt.R",
  reference = "R fmridesign event_model plus base-R lm.fit and full inverse-covariance fixed effects"
)
accepted_differences <- c(
  "ScalaFIM preserves typed run and coefficient-axis provenance that is additional to the R numeric receipt.",
  "The fixed-effects receipt combines each voxel's full coefficient covariance; it does not use coefficientwise diagonal-only meta-analysis.",
  "Event onsets are exactly representable at 0 and 4 seconds so this receipt isolates mixed-TR and fixed-effects semantics from the separately tracked off-grid interpolation convention."
)
schema <- "scalafim-r-fixed-effects-fixture/v1"
inputs <- list(
  formula = "onset ~ hrf(cond)",
  events = list(
    onset = events$onset,
    run = as.character(events$run),
    cond = as.character(events$cond),
    cond_levels = levels(events$cond)
  ),
  sampling_frame = list(blocklens = c(8, 12), tr = c(2, 0.8), start_time = c(0, 0)),
  precision = 0.1,
  design = matrix_rows(design),
  response = matrix_rows(response),
  contrast = contrast
)
outputs <- list(
  run_coefficients = lapply(run_fits, function(run) matrix_rows(run$coefficients)),
  run_residual_variance = lapply(run_fits, `[[`, "residual_variance"),
  run_normalized_covariance = lapply(run_fits, function(run) matrix_rows(run$normalized_covariance)),
  run_residual_df = vapply(run_fits, `[[`, numeric(1), "residual_df"),
  fixed_coefficients = matrix_rows(fixed_coefficients),
  fixed_covariance_by_voxel = lapply(fixed_covariance, matrix_rows),
  fixed_standard_errors = matrix_rows(fixed_standard_errors),
  fixed_residual_df = fixed_residual_df,
  contrast_estimate = contrast_estimate,
  contrast_standard_error = contrast_standard_error,
  contrast_statistic = contrast_statistic
)
receipt <- list(
  schema_version = schema,
  producer_command = "LC_ALL=C LANG=C Rscript tools/r-parity/generate_mixed_tr_fixed_effects_receipt.R && python3 tools/r-parity/finalize_mixed_tr_fixed_effects_receipt.py",
  accepted_differences = accepted_differences,
  conventions = list(
    dtype = "float64",
    json_encoding = "sorted-key UTF-8 JSON",
    matrix_orientation = "rows are acquisition samples or coefficients; columns preserve declared condition/voxel order",
    sampling = "two runs with TR 2.0 and 0.8 seconds, zero scan start, and 0.1-second convolution precision",
    runwise_fit = "independent base-R lm.fit per run",
    fixed_effects = "sum full coefficient precision matrices and precision-weighted coefficient vectors independently per voxel",
    residual_df = "sum of the two runwise residual degrees of freedom"
  ),
  hashes = list(inputs_sha256 = "", outputs_sha256 = ""),
  source = source
)
payload <- list(
  schema_version = schema,
  scenario_id = "fit.mixed-tr-fixed-effects.v1",
  inputs = inputs,
  outputs = outputs,
  source = source,
  receipt = receipt
)

scala_string <- function(value) {
  paste0("\"", gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\\\"", value)), "\"")
}
scala_number <- function(value) {
  if (abs(value) < 5e-16) "0.0" else sprintf("%.17g", value)
}
scala_vector <- function(values, render) {
  if (length(values) == 0) "Vector.empty"
  else paste0("Vector(", paste(vapply(values, render, character(1)), collapse = ", "), ")")
}
scala_doubles <- function(values) scala_vector(as.numeric(values), scala_number)
scala_ints <- function(values) scala_vector(as.integer(values), as.character)
scala_strings <- function(values) scala_vector(as.character(values), scala_string)
scala_matrix <- function(value) {
  rows <- apply(value, 1, scala_doubles)
  paste0("Vector(\n    ", paste(rows, collapse = ",\n    "), "\n  )")
}
scala_matrices <- function(values) {
  rendered <- vapply(values, scala_matrix, character(1))
  paste0("Vector(\n    ", paste(rendered, collapse = ",\n    "), "\n  )")
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, out_file, auto_unbox = TRUE, digits = 17, pretty = TRUE)
message("wrote ", out_file)

dir.create(dirname(scala_out), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    "package scalafim.fmri.fit.fixtures",
    "",
    "// Generated by tools/r-parity/generate_mixed_tr_fixed_effects_receipt.R.",
    "object MixedTrFixedEffectsRFixture:",
    paste0("  val design: Vector[Vector[Double]] = ", scala_matrix(design)),
    paste0("  val response: Vector[Vector[Double]] = ", scala_matrix(response)),
    paste0("  val contrast: Vector[Double] = ", scala_doubles(contrast)),
    paste0("  val runCoefficients: Vector[Vector[Vector[Double]]] = ", scala_matrices(lapply(run_fits, `[[`, "coefficients"))),
    paste0("  val runResidualVariance: Vector[Vector[Double]] = Vector(", paste(vapply(run_fits, function(run) scala_doubles(run$residual_variance), character(1)), collapse = ", "), ")"),
    paste0("  val runNormalizedCovariance: Vector[Vector[Vector[Double]]] = ", scala_matrices(lapply(run_fits, `[[`, "normalized_covariance"))),
    paste0("  val runResidualDf: Vector[Int] = ", scala_ints(vapply(run_fits, `[[`, numeric(1), "residual_df"))),
    paste0("  val fixedCoefficients: Vector[Vector[Double]] = ", scala_matrix(fixed_coefficients)),
    paste0("  val fixedCovarianceByVoxel: Vector[Vector[Vector[Double]]] = ", scala_matrices(fixed_covariance)),
    paste0("  val fixedStandardErrors: Vector[Vector[Double]] = ", scala_matrix(fixed_standard_errors)),
    paste0("  val fixedResidualDf: Int = ", as.integer(fixed_residual_df)),
    paste0("  val contrastEstimate: Vector[Double] = ", scala_doubles(contrast_estimate)),
    paste0("  val contrastStandardError: Vector[Double] = ", scala_doubles(contrast_standard_error)),
    paste0("  val contrastStatistic: Vector[Double] = ", scala_doubles(contrast_statistic)),
    paste0("  val acceptedDifferences: Vector[String] = ", scala_strings(accepted_differences))
  ),
  scala_out
)
message("wrote ", scala_out)
