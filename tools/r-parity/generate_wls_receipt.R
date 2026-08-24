#!/usr/bin/env Rscript

# Independent base-R receipt for DVARS volume weighting and lawful WLS.
#
# From the ScalaFIM repository root:
#   LC_ALL=C LANG=C Rscript tools/r-parity/generate_wls_receipt.R
#   python3 tools/r-parity/finalize_wls_receipt.py

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to generate the WLS receipt", call. = FALSE)
}

out_file <- Sys.getenv(
  "WLS_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "fit.structural-s18-wls.v1.r.json")
)
scala_out <- Sys.getenv(
  "WLS_SCALA_OUT",
  file.path(
    "modules", "fit", "shared", "src", "test", "scala",
    "scalafim", "fmri", "fit", "fixtures", "WlsRFixture.scala"
  )
)

compute_dvars <- function(Y) {
  derivatives <- sqrt(rowMeans(diff(Y)^2))
  dvars <- c(stats::median(derivatives), derivatives)
  scale <- stats::median(dvars)
  if (scale > 0) dvars / scale else dvars
}

dvars_to_weights <- function(dvars, method, threshold = 1.5, steepness = 2) {
  raw <- switch(
    method,
    inverse_squared = 1 / (1 + dvars^2),
    soft_threshold = 1 / (1 + ((pmax(dvars, threshold) - threshold) / threshold)^steepness),
    tukey = {
      u <- dvars / (threshold * 2)
      ifelse(abs(u) <= 1, (1 - u^2)^2, 0)
    },
    stop("unknown DVARS weight method", call. = FALSE)
  )
  raw <- pmax(pmin(raw, 1), 0)
  raw / mean(raw)
}

dvars_response <- rbind(
  c(0, 0, 0),
  c(1, 1, 1),
  c(2, 2, 2),
  c(12, -8, 10),
  c(13, -7, 11),
  c(14, -6, 12)
)
dvars <- compute_dvars(dvars_response)
dvars_weights <- list(
  inverse_squared = dvars_to_weights(dvars, "inverse_squared"),
  soft_threshold = dvars_to_weights(dvars, "soft_threshold"),
  tukey = dvars_to_weights(dvars, "tukey")
)

x <- c(-2, -1, 0, 1, 2, -1.5, 0.5, 1.5, 2.5)
z <- c(0, 1, 0, 1, 0, 1, 1, 0, 1)
X <- cbind(x = x, z = z, intercept = 1)
Y <- cbind(
  signal_a = 1.25 + 0.8 * x - 0.4 * z + c(0.2, -0.1, 0.3, -0.2, 0.1, 0.4, -0.3, 0.15, -0.25),
  signal_b = -0.5 + 0.2 * x + 1.1 * z + c(-0.1, 0.25, -0.2, 0.35, -0.15, 0.05, 0.2, -0.3, 0.1)
)
weights <- c(1, 0.5, 0, 1.5, 2, 0.75, 1.25, 0.4, 1.6)

fit <- stats::lm.wfit(x = X, y = Y, w = weights)
positive <- weights > 0
weighted_X <- X[positive, , drop = FALSE] * sqrt(weights[positive])
weighted_Y <- Y[positive, , drop = FALSE] * sqrt(weights[positive])
normalized_covariance <- solve(crossprod(weighted_X))
residual_variance <- colSums(weights * fit$residuals^2) / fit$df.residual
standard_errors <- sqrt(outer(diag(normalized_covariance), residual_variance))
contrast <- c(1, -1, 0)
contrast_estimate <- drop(crossprod(contrast, fit$coefficients))
contrast_scale <- drop(crossprod(contrast, normalized_covariance %*% contrast))
contrast_standard_error <- sqrt(contrast_scale * residual_variance)
contrast_statistic <- contrast_estimate / contrast_standard_error

source <- list(
  r_version = as.character(getRversion()),
  stats_version = as.character(utils::packageVersion("stats")),
  producer = "tools/r-parity/generate_wls_receipt.R",
  reference = "base R stats::lm.wfit and direct weighted crossproduct"
)
accepted_differences <- c(
  "The receipt supplies explicit fixed weights to lm.wfit; ScalaFIM's typed source and alignment receipt is additional provenance.",
  "DVARS is estimated from the entire declared response block before voxel chunking; the fixture contains one run and no selection gap."
)
schema <- "scalafim-r-wls-fixture/v1"
inputs <- list(
  dvars_response = unname(split(dvars_response, row(dvars_response))),
  dvars_threshold = 1.5,
  dvars_steepness = 2,
  design = unname(split(X, row(X))),
  response = unname(split(Y, row(Y))),
  weights = weights,
  contrast = contrast
)
outputs <- list(
  dvars = dvars,
  dvars_weights = dvars_weights,
  retained_rows_zero_based = which(positive) - 1L,
  coefficients = unname(split(fit$coefficients, row(fit$coefficients))),
  residual_variance = residual_variance,
  normalized_covariance = unname(split(normalized_covariance, row(normalized_covariance))),
  standard_errors = unname(split(standard_errors, row(standard_errors))),
  residual_df = unname(fit$df.residual),
  rank = unname(fit$rank),
  contrast_estimate = contrast_estimate,
  contrast_standard_error = contrast_standard_error,
  contrast_statistic = contrast_statistic,
  weighted_design = unname(split(weighted_X, row(weighted_X))),
  weighted_response = unname(split(weighted_Y, row(weighted_Y)))
)
receipt <- list(
  schema_version = schema,
  producer_command = "LC_ALL=C LANG=C Rscript tools/r-parity/generate_wls_receipt.R && python3 tools/r-parity/finalize_wls_receipt.py",
  accepted_differences = accepted_differences,
  conventions = list(
    dtype = "float64",
    json_encoding = "sorted-key UTF-8 JSON",
    matrix_orientation = "rows are timepoints or predictors; columns preserve declared input order",
    weighting = "D = diag(sqrt(w)); exact zero-weight rows are excluded from rank and residual df",
    residual_variance = "sum(w * residual^2) / lm.wfit df.residual",
    dvars = "RMS temporal derivative across all response columns; first value is derivative median; median-normalized",
    weight_normalization = "each DVARS weight vector is divided by its arithmetic mean"
  ),
  hashes = list(inputs_sha256 = "", outputs_sha256 = ""),
  source = source
)
payload <- list(
  schema_version = schema,
  scenario_id = "fit.structural-s18-wls.v1",
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

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, out_file, auto_unbox = TRUE, digits = 17, pretty = TRUE)
message("wrote ", out_file)

dir.create(dirname(scala_out), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    "package scalafim.fmri.fit.fixtures",
    "",
    "// Generated by tools/r-parity/generate_wls_receipt.R from base R stats::lm.wfit.",
    "object WlsRFixture:",
    paste0("  val dvarsResponse: Vector[Vector[Double]] = ", scala_matrix(dvars_response)),
    paste0("  val dvars: Vector[Double] = ", scala_doubles(dvars)),
    paste0("  val inverseSquaredWeights: Vector[Double] = ", scala_doubles(dvars_weights$inverse_squared)),
    paste0("  val softThresholdWeights: Vector[Double] = ", scala_doubles(dvars_weights$soft_threshold)),
    paste0("  val tukeyWeights: Vector[Double] = ", scala_doubles(dvars_weights$tukey)),
    paste0("  val design: Vector[Vector[Double]] = ", scala_matrix(X)),
    paste0("  val response: Vector[Vector[Double]] = ", scala_matrix(Y)),
    paste0("  val weights: Vector[Double] = ", scala_doubles(weights)),
    paste0("  val contrast: Vector[Double] = ", scala_doubles(contrast)),
    paste0("  val retainedRows: Vector[Int] = ", scala_ints(which(positive) - 1L)),
    paste0("  val coefficients: Vector[Vector[Double]] = ", scala_matrix(fit$coefficients)),
    paste0("  val residualVariance: Vector[Double] = ", scala_doubles(residual_variance)),
    paste0("  val normalizedCovariance: Vector[Vector[Double]] = ", scala_matrix(normalized_covariance)),
    paste0("  val standardErrors: Vector[Vector[Double]] = ", scala_matrix(standard_errors)),
    paste0("  val residualDf: Int = ", as.integer(fit$df.residual)),
    paste0("  val rank: Int = ", as.integer(fit$rank)),
    paste0("  val contrastEstimate: Vector[Double] = ", scala_doubles(contrast_estimate)),
    paste0("  val contrastStandardError: Vector[Double] = ", scala_doubles(contrast_standard_error)),
    paste0("  val contrastStatistic: Vector[Double] = ", scala_doubles(contrast_statistic)),
    paste0("  val weightedDesign: Vector[Vector[Double]] = ", scala_matrix(weighted_X)),
    paste0("  val weightedResponse: Vector[Vector[Double]] = ", scala_matrix(weighted_Y)),
    paste0("  val acceptedDifferences: Vector[String] = ", scala_strings(accepted_differences))
  ),
  scala_out
)
message("wrote ", scala_out)
