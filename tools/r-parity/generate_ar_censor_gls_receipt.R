#!/usr/bin/env Rscript

# Independent two-run censor-aware AR(1)/GLS receipt for P3.4.
#
# The source series is generated wholly in R. Censored scans are row-deleted,
# contiguous retained stretches define whitening segments, and base R owns both
# the exact-first AR(1) transform and the QR fit of L X and L Y. The transform
# follows the fixed-AR convention used by fmrireg::ar_whiten_transform/fmriAR:
# sqrt(1-rho^2) at every segment start and y[t] - rho*y[t-1] within a segment.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to generate the AR/censor GLS receipt", call. = FALSE)
}

out_file <- Sys.getenv(
  "AR_CENSOR_GLS_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "fit.ar-censor-boundary-gls.v1.r.json")
)
scala_out <- Sys.getenv(
  "AR_CENSOR_GLS_SCALA_OUT",
  file.path(
    "modules", "fit", "shared", "src", "test", "scala", "scalafim",
    "fmri", "fit", "fixtures", "ArCensorGlsRFixture.scala"
  )
)
fmrireg_pkg <- Sys.getenv("FMRIREG_R", file.path(path.expand("~"), "code", "fmrireg"))

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

run_lengths <- c(12L, 12L)
n_timepoints <- sum(run_lengths)
timepoint <- 0:(n_timepoints - 1L)
run <- ifelse(timepoint < run_lengths[[1]], 0L, 1L)
censored <- c(3L, 7L, 8L, 14L, 15L, 20L)
selected <- setdiff(timepoint, censored)
rho <- 0.42
exact_first <- TRUE

task <- sin((timepoint + 1) * 0.37) + 0.35 * cos((timepoint + 1) * 0.13) +
  ifelse(timepoint %% 4L < 2L, -0.4, 0.45)
full_design <- cbind(
  task = task,
  run_1_intercept = as.numeric(run == 0L),
  run_2_intercept = as.numeric(run == 1L)
)
beta <- rbind(
  c(0.80, -0.45, 1.10, 0.25),
  c(1.20, -0.80, 0.30, 1.50),
  c(-0.40, 0.70, -1.10, -0.60)
)
voxel <- 1:4
noise <- outer(
  timepoint + 1,
  voxel,
  function(scan, column) 0.07 * sin(scan * (0.19 + column * 0.03)) +
    0.04 * cos(scan * (0.11 + column * 0.02))
)
full_response <- full_design %*% beta + noise

# Censored rows carry enormous values, while retained rows immediately before
# gaps/run boundaries carry a second canary. Any accidental row retention or
# AR state crossing is therefore numerically obvious.
for (index in seq_along(censored)) {
  full_response[censored[[index]] + 1L, ] <-
    (if (index %% 2L == 0L) -1 else 1) * (1e6 + index * 1e4) * c(1, -1, 0.5, -0.25)
}
boundary_canaries <- c(2L, 6L, 11L, 13L, 19L)
boundary_values <- c(250, -220, 310, -275, 240)
for (index in seq_along(boundary_canaries)) {
  full_response[boundary_canaries[[index]] + 1L, 4L] <-
    full_response[boundary_canaries[[index]] + 1L, 4L] + boundary_values[[index]]
}

selected_design <- full_design[selected + 1L, , drop = FALSE]
selected_response <- full_response[selected + 1L, , drop = FALSE]
selected_runs <- run[selected + 1L]
segment_start <- c(
  TRUE,
  diff(selected) != 1L | diff(selected_runs) != 0L
)
segment_id <- cumsum(segment_start)
segment_rows <- split(seq_along(selected), segment_id)

whiten <- function(value) {
  out <- matrix(0, nrow = nrow(value), ncol = ncol(value))
  first_scale <- sqrt(1 - rho^2)
  for (rows in segment_rows) {
    first <- rows[[1]]
    out[first, ] <- first_scale * value[first, ]
    if (length(rows) > 1L) {
      for (position in rows[-1L]) {
        out[position, ] <- value[position, ] - rho * value[position - 1L, ]
      }
    }
  }
  out
}

whitened_design <- whiten(selected_design)
whitened_response <- whiten(selected_response)
fit <- stats::lm.fit(whitened_design, whitened_response)
if (fit$rank != ncol(whitened_design)) {
  stop(sprintf("external whitened design rank %d < %d", fit$rank, ncol(whitened_design)), call. = FALSE)
}
coefficients <- qr.coef(fit$qr, whitened_response)
residuals <- whitened_response - whitened_design %*% coefficients
residual_df <- nrow(whitened_design) - fit$rank
residual_variance <- colSums(residuals^2) / residual_df
normalized_covariance <- chol2inv(qr.R(fit$qr))
standard_errors <- sqrt(outer(diag(normalized_covariance), residual_variance))

t_estimates <- unname(coefficients[1L, ])
t_standard_errors <- unname(standard_errors[1L, ])
t_statistics <- t_estimates / t_standard_errors
f_weights <- matrix(c(1, 0, 0), nrow = 1L)
f_statistics <- vapply(seq_len(ncol(coefficients)), function(column) {
  effect <- drop(f_weights %*% coefficients[, column])
  covariance <- residual_variance[[column]] * f_weights %*% normalized_covariance %*% t(f_weights)
  drop(crossprod(effect, solve(covariance, effect))) / nrow(f_weights)
}, numeric(1))

segments <- unname(lapply(segment_rows, function(rows) {
  source <- selected[rows]
  list(
    run_index = as.integer(selected_runs[rows[[1]]]),
    start_row = as.integer(rows[[1]] - 1L),
    end_row_exclusive = as.integer(rows[[length(rows)]]),
    start_timepoint = as.integer(source[[1]]),
    end_timepoint_exclusive = as.integer(source[[length(source)]] + 1L)
  )
}))
gaps <- list()
for (index in seq_len(length(segments) - 1L)) {
  left <- segments[[index]]
  right <- segments[[index + 1L]]
  if (left$run_index == right$run_index && right$start_timepoint > left$end_timepoint_exclusive) {
    gaps[[length(gaps) + 1L]] <- list(
      run_index = left$run_index,
      start_timepoint = left$end_timepoint_exclusive,
      end_timepoint_exclusive = right$start_timepoint
    )
  }
}

source <- list(
  fmrireg_revision = git_revision(normalizePath(fmrireg_pkg, mustWork = FALSE)),
  fmrireg_version = package_version_at(normalizePath(fmrireg_pkg, mustWork = FALSE)),
  r_version = as.character(getRversion()),
  stats_version = as.character(utils::packageVersion("stats")),
  producer = "tools/r-parity/generate_ar_censor_gls_receipt.R",
  reference = "independent base-R exact-first segmented AR(1) transform and direct QR, following the fmrireg/fmriAR fixed-whitening convention"
)
schema <- "scalafim-r-ar-censor-gls-fixture/v1"
accepted_differences <- c(
  "The receipt fixes rho=0.42 to isolate whitening, censor, and GLS semantics; estimated-AR policy is asserted separately against ScalaFIM diagnostics.",
  "Censored source scans are row-deleted before whitening. Legacy reset-after markers that remain in the fitted series are a separate compatibility surface."
)
inputs <- list(
  sampling_frame = list(blocklens = run_lengths, tr = c(1, 1), start_time = c(0, 0)),
  task = unname(task),
  full_response = matrix_rows(full_response),
  selected_timepoints = selected,
  censored_timepoints = censored,
  boundary_canary_timepoints = boundary_canaries,
  rho = rho,
  exact_first = exact_first,
  coefficient_names = c("task", "base_constant1_block_1", "base_constant1_block_2")
)
outputs <- list(
  selected_design = matrix_rows(selected_design),
  selected_response = matrix_rows(selected_response),
  segments = segments,
  censor_gaps = gaps,
  whitened_design = matrix_rows(whitened_design),
  whitened_response = matrix_rows(whitened_response),
  rank = fit$rank,
  residual_df = residual_df,
  coefficients = matrix_rows(coefficients),
  normalized_covariance = matrix_rows(normalized_covariance),
  residual_variance = unname(residual_variance),
  standard_errors = matrix_rows(standard_errors),
  t_task = list(estimates = t_estimates, standard_errors = t_standard_errors, statistics = t_statistics),
  f_task = list(numerator_df = 1L, denominator_df = residual_df, statistics = f_statistics)
)
receipt <- list(
  schema_version = schema,
  producer_command = "LC_ALL=C LANG=C Rscript tools/r-parity/generate_ar_censor_gls_receipt.R && python3 tools/r-parity/finalize_ar_censor_gls_receipt.py",
  conventions = list(
    dtype = "float64",
    matrix_orientation = "rows are retained scans and columns are coefficients or voxels as named",
    censoring = "zero-based source scans are removed before whitening; gaps and run changes start new segments",
    whitening = "global fixed AR(1); exact-first scale sqrt(1-rho^2) at every segment start; y[t]-rho*y[t-1] only inside a segment",
    fit = "base-R lm.fit direct QR on independently whitened L X and L Y"
  ),
  accepted_differences = accepted_differences,
  hashes = list(inputs_sha256 = "", outputs_sha256 = ""),
  source = source
)
payload <- list(
  schema_version = schema,
  scenario_id = "fit.ar-censor-boundary-gls.v1",
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
scala_rows <- function(value) {
  paste0("Vector(", paste(vapply(seq_len(nrow(value)), function(row) scala_doubles(value[row, ]), character(1)), collapse = ", "), ")")
}
scala_segment <- function(value) {
  paste0(
    "ArCensorSegmentExpected(", value$run_index, ", ", value$start_row, ", ",
    value$end_row_exclusive, ", ", value$start_timepoint, ", ", value$end_timepoint_exclusive, ")"
  )
}
scala_gap <- function(value) {
  paste0("ArCensorGapExpected(", value$run_index, ", ", value$start_timepoint, ", ", value$end_timepoint_exclusive, ")")
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, out_file, auto_unbox = TRUE, digits = 17, pretty = TRUE)
message("wrote ", out_file)

dir.create(dirname(scala_out), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    "package scalafim.fmri.fit.fixtures",
    "",
    "// Generated by tools/r-parity/generate_ar_censor_gls_receipt.R.",
    "final case class ArCensorSegmentExpected(runIndex: Int, startRow: Int, endRowExclusive: Int, startTimepoint: Int, endTimepointExclusive: Int)",
    "final case class ArCensorGapExpected(runIndex: Int, startTimepoint: Int, endTimepointExclusive: Int)",
    "final case class ArCensorTExpected(estimates: Vector[Double], standardErrors: Vector[Double], statistics: Vector[Double])",
    "final case class ArCensorFExpected(numeratorDf: Int, denominatorDf: Int, statistics: Vector[Double])",
    "",
    "object ArCensorGlsRFixture:",
    paste0("  val blockLengths: Vector[Int] = ", scala_ints(run_lengths)),
    paste0("  val task: Vector[Double] = ", scala_doubles(task)),
    paste0("  val responseRows: Vector[Vector[Double]] = ", scala_rows(full_response)),
    paste0("  val selectedTimepoints: Vector[Int] = ", scala_ints(selected)),
    paste0("  val censoredTimepoints: Vector[Int] = ", scala_ints(censored)),
    paste0("  val boundaryCanaryTimepoints: Vector[Int] = ", scala_ints(boundary_canaries)),
    paste0("  val rho: Double = ", scala_number(rho)),
    paste0("  val exactFirst: Boolean = ", tolower(as.character(exact_first))),
    paste0("  val selectedDesignRows: Vector[Vector[Double]] = ", scala_rows(selected_design)),
    paste0("  val selectedResponseRows: Vector[Vector[Double]] = ", scala_rows(selected_response)),
    paste0("  val segments: Vector[ArCensorSegmentExpected] = Vector(", paste(vapply(segments, scala_segment, character(1)), collapse = ", "), ")"),
    paste0("  val censorGaps: Vector[ArCensorGapExpected] = Vector(", paste(vapply(gaps, scala_gap, character(1)), collapse = ", "), ")"),
    paste0("  val whitenedDesignRows: Vector[Vector[Double]] = ", scala_rows(whitened_design)),
    paste0("  val whitenedResponseRows: Vector[Vector[Double]] = ", scala_rows(whitened_response)),
    paste0("  val rank: Int = ", as.integer(fit$rank)),
    paste0("  val residualDf: Int = ", as.integer(residual_df)),
    paste0("  val coefficients: Vector[Vector[Double]] = ", scala_rows(coefficients)),
    paste0("  val normalizedCovariance: Vector[Vector[Double]] = ", scala_rows(normalized_covariance)),
    paste0("  val residualVariance: Vector[Double] = ", scala_doubles(residual_variance)),
    paste0("  val standardErrors: Vector[Vector[Double]] = ", scala_rows(standard_errors)),
    paste0("  val taskT: ArCensorTExpected = ArCensorTExpected(", scala_doubles(t_estimates), ", ", scala_doubles(t_standard_errors), ", ", scala_doubles(t_statistics), ")"),
    paste0("  val taskF: ArCensorFExpected = ArCensorFExpected(1, ", residual_df, ", ", scala_doubles(f_statistics), ")"),
    paste0("  val acceptedDifferences: Vector[String] = ", scala_vector(accepted_differences, scala_string)),
    paste0("  val fmriregRevision: String = ", scala_string(source$fmrireg_revision)),
    paste0("  val fmriregVersion: String = ", scala_string(source$fmrireg_version))
  ),
  scala_out
)
message("wrote ", scala_out)
