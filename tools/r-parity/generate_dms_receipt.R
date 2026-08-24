#!/usr/bin/env Rscript

# Independent delayed-match-to-sample response and inference receipt.
#
# R fmridesign constructs the event regressors; base R owns centering,
# response synthesis, runwise QR fits, full-covariance fixed effects, and the
# semantic hypothesis calculations. No ScalaFIM design or fitted value is an
# input to this program.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate the DMS receipt", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to generate the DMS receipt", call. = FALSE)
}

r_pkg <- Sys.getenv("FMRIDESIGN_R", file.path(path.expand("~"), "code", "fmridesign"))
hrf_pkg <- Sys.getenv("FMRIHRF_R", file.path(path.expand("~"), "code", "fmrihrf"))
out_file <- Sys.getenv(
  "DMS_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "fit.dms-multiphase-dsl.v1.r.json")
)
scala_out <- Sys.getenv(
  "DMS_SCALA_OUT",
  file.path(
    "modules", "fit", "shared", "src", "test", "scala",
    "scalafim", "fmri", "fit", "fixtures", "DmsRFixture.scala"
  )
)

pkgload::load_all(hrf_pkg, quiet = TRUE)
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

# fmrihrf stores multi-condition HRF values in condition-major order. Reorder
# each term by its declared structural dimensions before assigning the
# basis-major semantic names used by the formula compiler.
basis_major <- function(value, n_conditions, n_basis) {
  stopifnot(ncol(value) == n_conditions * n_basis)
  source_columns <- unlist(lapply(
    seq_len(n_basis),
    function(basis) basis + (seq_len(n_conditions) - 1L) * n_basis
  ))
  value[, source_columns, drop = FALSE]
}

n_trials <- 60L
index <- 0:(n_trials - 1L)
run_index <- index %/% 30L
within_run <- index %% 30L
sample_onset <- 2.25 + within_run * 5.5
delay_duration <- c(4, 6, 8, 10)[(index %% 4L) + 1L]
observed_cells <- data.frame(
  match = c("match", "match", "match", "mismatch", "mismatch"),
  load = c("low", "medium", "high", "low", "medium")
)
cell <- observed_cells[(index %% nrow(observed_cells)) + 1L, ]
rt <- 0.45 + 0.015 * (index %% 20L)
rt[18L] <- NA_real_

events <- data.frame(
  onset = sample_onset,
  trial_id = sprintf("trial-%03d", index + 1L),
  run = factor(paste0("run-", run_index + 1L), levels = c("run-1", "run-2")),
  sample_onset = sample_onset,
  sample_duration = rep(0.5, n_trials),
  delay_onset = sample_onset + 1,
  delay_duration = delay_duration,
  probe_onset = sample_onset + 1 + delay_duration,
  probe_duration = rep(0.3, n_trials),
  stimulus = factor(ifelse(index %% 2L == 0L, "face", "scene"), levels = c("face", "scene")),
  load = factor(cell$load, levels = c("low", "medium", "high")),
  match = factor(cell$match, levels = c("match", "mismatch")),
  rt = rt
)
groups <- interaction(events$match, events$load, drop = TRUE)
events$rt_centered <- ave(events$rt, groups, FUN = function(value) value - mean(value, na.rm = TRUE))
events$rt_centered[is.na(events$rt_centered)] <- 0

precision <- 0.05
sampling_frame <- fmrihrf::sampling_frame(
  blocklens = c(180, 180),
  TR = c(1, 1),
  start_time = c(0, 0),
  precision = precision
)
formula_value <- onset ~
  hrf(stimulus, onsets = sample_onset, durations = sample_duration, basis = "spmg1", id = "sample") +
  hrf(load, onsets = delay_onset, durations = delay_duration, basis = "fir", nbasis = 8, id = "delay") +
  hrf(match, load, onsets = probe_onset, durations = probe_duration, basis = "spmg2", id = "probe") +
  hrf(match, load, rt_centered, onsets = probe_onset, durations = probe_duration, basis = "spmg2", id = "probe_rt")
run_sampling_frame <- fmrihrf::sampling_frame(
  blocklens = 180,
  TR = 1,
  start_time = 0,
  precision = precision
)
# Compile the public formula in fmridesign as an independent structural check.
# Numerical oracle ownership stays with explicit per-run fmrihrf coordinates:
# fmridesign 0.6.0's rendered multi-condition path has both a value/name-order
# defect and a repeated-local-schedule defect for multi-block matrices.
external_models <- lapply(levels(events$run), function(run_id) {
  run_events <- events[events$run == run_id, , drop = FALSE]
  run_events$run <- droplevels(run_events$run)
  event_model(
    formula_value,
    data = run_events,
    block = ~run,
    sampling_frame = run_sampling_frame,
    drop_empty = FALSE,
    precision = precision
  )
})
formula_matrices <- lapply(external_models, function(model) as.matrix(design_matrix(model)))
stopifnot(all(vapply(formula_matrices, nrow, integer(1)) == 180L))
stopifnot(all(vapply(formula_matrices, ncol, integer(1)) == 46L))
semantic_names <- colnames(formula_matrices[[1]])
stopifnot(all(vapply(formula_matrices[-1], function(value) identical(colnames(value), semantic_names), logical(1))))

cell_levels <- c(
  "match|low",
  "mismatch|low",
  "match|medium",
  "mismatch|medium",
  "match|high",
  "mismatch|high"
)
run_design <- function(run_id) {
  run_events <- events[events$run == run_id, , drop = FALSE]
  block <- rep(1L, nrow(run_events))
  sample <- fmrihrf::regressor_design(
    run_events$sample_onset,
    run_events$stimulus,
    block,
    run_sampling_frame,
    fmrihrf::HRF_SPMG1,
    duration = run_events$sample_duration,
    precision = precision
  )
  delay <- fmrihrf::regressor_design(
    run_events$delay_onset,
    run_events$load,
    block,
    run_sampling_frame,
    fmrihrf::hrf_fir_generator(nbasis = 8),
    duration = run_events$delay_duration,
    precision = precision
  )
  cells <- factor(
    paste(run_events$match, run_events$load, sep = "|"),
    levels = cell_levels
  )
  probe <- fmrihrf::regressor_design(
    run_events$probe_onset,
    cells,
    block,
    run_sampling_frame,
    fmrihrf::HRF_SPMG2,
    duration = run_events$probe_duration,
    precision = precision
  )
  probe_rt <- fmrihrf::regressor_design(
    run_events$probe_onset,
    cells,
    block,
    run_sampling_frame,
    fmrihrf::HRF_SPMG2,
    duration = run_events$probe_duration,
    amplitude = run_events$rt_centered,
    precision = precision
  )
  cbind(
    sample,
    basis_major(delay, n_conditions = 3, n_basis = 8),
    basis_major(probe[, 1:10, drop = FALSE], n_conditions = 5, n_basis = 2),
    basis_major(probe_rt[, 1:10, drop = FALSE], n_conditions = 5, n_basis = 2)
  )
}
design <- unname(do.call(rbind, lapply(levels(events$run), run_design)))
stopifnot(nrow(design) == 360L, ncol(design) == 46L)
colnames(design) <- semantic_names

beta <- setNames(rep(0, ncol(design)), colnames(design))
beta[c("sample_stimulus.face", "sample_stimulus.scene")] <- c(0.8, -0.4)
delay_shape <- c(0.05, 0.15, 0.30, 0.45, 0.35, 0.22, 0.12, 0.05)
load_scale <- c(low = 0.4, medium = 0.8, high = 1.2)
for (basis in seq_along(delay_shape)) {
  for (load in names(load_scale)) {
    beta[sprintf("delay_load.%s_b%02d", load, basis)] <- delay_shape[[basis]] * load_scale[[load]]
  }
}
beta[c(
  "probe_match.match_load.low_b01",
  "probe_match.mismatch_load.low_b01",
  "probe_match.match_load.medium_b01",
  "probe_match.mismatch_load.medium_b01",
  "probe_match.match_load.high_b01"
)] <- c(0.20, 0.60, 0.25, 1.05, 0.30)
beta[c(
  "probe_match.match_load.low_b02",
  "probe_match.mismatch_load.low_b02",
  "probe_match.match_load.medium_b02",
  "probe_match.mismatch_load.medium_b02",
  "probe_match.match_load.high_b02"
)] <- c(0.00, 0.10, 0.00, 0.25, 0.05)
beta[c(
  "probe_rt_match.match_load.low_rt_centered_b01",
  "probe_rt_match.mismatch_load.low_rt_centered_b01",
  "probe_rt_match.match_load.medium_rt_centered_b01",
  "probe_rt_match.mismatch_load.medium_rt_centered_b01",
  "probe_rt_match.match_load.high_rt_centered_b01"
)] <- c(0.10, 0.40, 0.15, 0.65, 0.20)
beta[c(
  "probe_rt_match.match_load.low_rt_centered_b02",
  "probe_rt_match.mismatch_load.low_rt_centered_b02",
  "probe_rt_match.match_load.medium_rt_centered_b02",
  "probe_rt_match.mismatch_load.medium_rt_centered_b02",
  "probe_rt_match.match_load.high_rt_centered_b02"
)] <- c(0.00, 0.05, 0.00, 0.10, 0.02)

# The scan noise is deterministic, non-polynomial, and declared directly. It
# is not calculated from either design matrix or fitted residuals.
noise <- 0.0035 * sin((0:359 + 1) * 0.37) + 0.002 * cos((0:359 + 1) * 0.19)
run_intercepts <- c(0.20, -0.15)
response <- drop(design %*% beta) + rep(run_intercepts, each = 180) + noise

run_rows <- list(1:180, 181:360)
run_fits <- lapply(run_rows, function(rows) {
  x <- cbind(design[rows, , drop = FALSE], intercept = 1)
  fit <- stats::lm.fit(x = x, y = response[rows])
  if (fit$rank != ncol(x)) stop("external DMS run design is rank deficient", call. = FALSE)
  normalized_covariance <- solve(crossprod(x))
  residual_variance <- sum(fit$residuals^2) / fit$df.residual
  task_covariance <- residual_variance * normalized_covariance[seq_len(ncol(design)), seq_len(ncol(design)), drop = FALSE]
  list(
    coefficients = unname(fit$coefficients[seq_len(ncol(design))]),
    covariance = unname(task_covariance),
    residual_variance = unname(residual_variance),
    residual_df = unname(fit$df.residual),
    rank = unname(fit$rank)
  )
})
precisions <- lapply(run_fits, function(run) solve(run$covariance))
fixed_covariance <- solve(Reduce(`+`, precisions))
fixed_coefficients <- drop(fixed_covariance %*% Reduce(
  `+`,
  Map(function(precision_matrix, run) precision_matrix %*% run$coefficients, precisions, run_fits)
))
names(fixed_coefficients) <- colnames(design)
fixed_df <- sum(vapply(run_fits, `[[`, numeric(1), "residual_df"))

point_weights <- function(hrf, lag) {
  drop(fmrihrf::evaluate(hrf, lag))
}
window_mean_weights <- function(hrf, from, until) {
  dimensions <- fmrihrf::nbasis(hrf)
  vapply(seq_len(dimensions), function(column) {
    stats::integrate(
      function(lag) {
        values <- fmrihrf::evaluate(hrf, lag)
        if (!is.null(dim(values))) values[, column]
        else if (dimensions == 1L) as.numeric(values)
        else values[[column]]
      },
      lower = from,
      upper = until,
      rel.tol = 1e-11,
      subdivisions = 1000L
    )$value / (until - from)
  }, numeric(1))
}

empty_contrast <- function() setNames(rep(0, ncol(design)), colnames(design))
add_weights <- function(weights, keys, values, sign = 1) {
  stopifnot(length(keys) == length(values), all(keys %in% names(weights)))
  weights[keys] <- weights[keys] + sign * values
  weights
}

spmg1_window <- window_mean_weights(fmrihrf::HRF_SPMG1, 4, 8)
fir_window <- window_mean_weights(fmrihrf::hrf_fir_generator(nbasis = 8), 3, 9)
spmg2_at_six <- point_weights(fmrihrf::HRF_SPMG2, 6)

sample_window <- empty_contrast()
sample_window <- add_weights(sample_window, "sample_stimulus.face", spmg1_window, 1)
sample_window <- add_weights(sample_window, "sample_stimulus.scene", spmg1_window, -1)

delay_window <- empty_contrast()
delay_window <- add_weights(delay_window, sprintf("delay_load.high_b%02d", 1:8), fir_window, 1)
delay_window <- add_weights(delay_window, sprintf("delay_load.low_b%02d", 1:8), fir_window, -1)

probe_at_six <- empty_contrast()
probe_at_six <- add_weights(probe_at_six, c(
  "probe_match.mismatch_load.medium_b01",
  "probe_match.mismatch_load.medium_b02"
), spmg2_at_six, 1)
probe_at_six <- add_weights(probe_at_six, c(
  "probe_match.match_load.medium_b01",
  "probe_match.match_load.medium_b02"
), spmg2_at_six, -1)

match_by_load <- empty_contrast()
for (basis in 1:2) {
  weight <- spmg2_at_six[[basis]]
  match_by_load[sprintf("probe_match.mismatch_load.medium_b%02d", basis)] <- weight
  match_by_load[sprintf("probe_match.match_load.medium_b%02d", basis)] <- -weight
  match_by_load[sprintf("probe_match.mismatch_load.low_b%02d", basis)] <- -weight
  match_by_load[sprintf("probe_match.match_load.low_b%02d", basis)] <- weight
}

rt_slope <- empty_contrast()
rt_slope["probe_rt_match.mismatch_load.medium_rt_centered_b01"] <- 1
rt_slope["probe_rt_match.match_load.medium_rt_centered_b01"] <- -1

t_contrasts <- list(
  `sample-face-minus-scene` = sample_window,
  `delay-high-minus-low` = delay_window,
  `probe-mismatch-at-six` = probe_at_six,
  `probe-match-by-load` = match_by_load,
  `probe-rt-slope` = rt_slope
)
t_results <- lapply(t_contrasts, function(contrast) {
  estimate <- drop(crossprod(contrast, fixed_coefficients))
  standard_error <- sqrt(drop(crossprod(contrast, fixed_covariance %*% contrast)))
  list(estimate = estimate, standard_error = standard_error, statistic = estimate / standard_error)
})

probe_shape <- matrix(0, nrow = 1, ncol = ncol(design), dimnames = list(NULL, colnames(design)))
probe_shape[1, "probe_match.mismatch_load.medium_b02"] <- 1
delay_omnibus <- matrix(0, nrow = 8, ncol = ncol(design), dimnames = list(NULL, colnames(design)))
for (basis in 1:8) delay_omnibus[basis, sprintf("delay_load.high_b%02d", basis)] <- 1
f_contrasts <- list(`probe-shape` = probe_shape, `delay-high-omnibus` = delay_omnibus)
f_results <- lapply(f_contrasts, function(contrast) {
  effect <- drop(contrast %*% fixed_coefficients)
  covariance <- contrast %*% fixed_covariance %*% t(contrast)
  numerator_df <- qr(contrast)$rank
  statistic <- drop(crossprod(effect, solve(covariance, effect))) / numerator_df
  list(numerator_df = numerator_df, denominator_df = fixed_df, statistic = statistic)
})

fmridesign_root <- normalizePath(r_pkg, mustWork = FALSE)
fmrihrf_root <- normalizePath(hrf_pkg, mustWork = FALSE)
source <- list(
  fmridesign_revision = git_revision(fmridesign_root),
  fmrihrf_revision = git_revision(fmrihrf_root),
  fmridesign_version = as.character(utils::packageVersion("fmridesign")),
  fmrihrf_version = as.character(utils::packageVersion("fmrihrf")),
  r_version = as.character(getRversion()),
  stats_version = as.character(utils::packageVersion("stats")),
  producer = "tools/r-parity/generate_dms_receipt.R",
  reference = "R fmridesign formula structure, per-run fmrihrf regressor_design coordinates, and base-R runwise QR/full-covariance fixed effects"
)
accepted_differences <- c(
  "Phase and parent-trial identity are ScalaFIM structural metadata; R receives the same single trial table and phase timing columns without those additional identities.",
  "The deliberately empty mismatch/high cell is represented by four typed zero columns in ScalaFIM and omitted from the 46-column estimable R design.",
  "R fmridesign validates the same formula structure, while numerical oracle coordinates are compiled per run with fmrihrf::regressor_design to avoid fmridesign 0.6.0 multi-condition value/name and repeated-block matrix defects.",
  "Within-cell RT centering is performed by base R with missing RT mapped to zero contribution; ScalaFIM performs the same policy inside the public compiler and records receipts."
)
schema <- "scalafim-r-dms-fixture/v1"
inputs <- list(
  formula = paste(deparse(formula_value), collapse = " "),
  trials = list(
    trial_id = events$trial_id,
    run = as.character(events$run),
    sample_onset = events$sample_onset,
    sample_duration = events$sample_duration,
    delay_onset = events$delay_onset,
    delay_duration = events$delay_duration,
    probe_onset = events$probe_onset,
    probe_duration = events$probe_duration,
    stimulus = as.character(events$stimulus),
    load = as.character(events$load),
    match = as.character(events$match),
    rt = ifelse(is.na(events$rt), "NaN", format(events$rt, digits = 17, scientific = FALSE, trim = TRUE))
  ),
  sampling_frame = list(blocklens = c(180, 180), tr = c(1, 1), start_time = c(0, 0)),
  precision = precision,
  planted_coefficients = as.list(beta),
  run_intercepts = run_intercepts,
  noise = noise
)
outputs <- list(
  design_column_keys = colnames(design),
  estimable_design = matrix_rows(design),
  response = response,
  run_rank = vapply(run_fits, `[[`, numeric(1), "rank"),
  run_residual_df = vapply(run_fits, `[[`, numeric(1), "residual_df"),
  run_coefficients = lapply(run_fits, `[[`, "coefficients"),
  run_covariance = lapply(run_fits, function(run) matrix_rows(run$covariance)),
  fixed_coefficients = fixed_coefficients,
  fixed_covariance = matrix_rows(fixed_covariance),
  fixed_residual_df = fixed_df,
  t_hypotheses = t_results,
  f_hypotheses = f_results
)
receipt <- list(
  schema_version = schema,
  producer_command = "LC_ALL=C LANG=C Rscript tools/r-parity/generate_dms_receipt.R && python3 tools/r-parity/finalize_dms_receipt.py",
  accepted_differences = accepted_differences,
  conventions = list(
    dtype = "float64",
    json_encoding = "sorted-key UTF-8 JSON",
    matrix_orientation = "rows are scans; columns are the 46 nonempty structural task coordinates",
    convolution = "Each run is compiled independently by R fmrihrf::regressor_design at 0.05-second precision; all fractional onsets lie on that grid",
    response = "declared planted task coefficients plus run intercepts and deterministic analytic noise",
    runwise_fit = "base-R lm.fit on 46 task columns plus one local intercept per run",
    fixed_effects = "full task-covariance inverse-precision combination across runs",
    response_functionals = "fmrihrf point evaluation and high-accuracy numerical window integration"
  ),
  hashes = list(inputs_sha256 = "", outputs_sha256 = ""),
  source = source
)
payload <- list(
  schema_version = schema,
  scenario_id = "fit.dms-multiphase-dsl.v1",
  inputs = inputs,
  outputs = outputs,
  source = source,
  receipt = receipt
)

scala_string <- function(value) {
  paste0("\"", gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\\\"", value)), "\"")
}
scala_number <- function(value) {
  if (is.na(value)) "Double.NaN"
  else if (abs(value) < 5e-16) "0.0"
  else sprintf("%.17g", value)
}
scala_vector <- function(values, render) {
  if (length(values) == 0) "Vector.empty"
  else paste0("Vector(", paste(vapply(values, render, character(1)), collapse = ", "), ")")
}
scala_doubles <- function(values) scala_vector(as.numeric(values), scala_number)
scala_strings <- function(values) scala_vector(as.character(values), scala_string)
scala_t_map <- function(values) {
  entries <- vapply(names(values), function(name) {
    value <- values[[name]]
    paste0(
      scala_string(name), " -> DmsTExpected(",
      scala_number(value$estimate), ", ", scala_number(value$standard_error), ", ",
      scala_number(value$statistic), ")"
    )
  }, character(1))
  paste0("Map(", paste(entries, collapse = ", "), ")")
}
scala_f_map <- function(values) {
  entries <- vapply(names(values), function(name) {
    value <- values[[name]]
    paste0(
      scala_string(name), " -> DmsFExpected(",
      as.integer(value$numerator_df), ", ", as.integer(value$denominator_df), ", ",
      scala_number(value$statistic), ")"
    )
  }, character(1))
  paste0("Map(", paste(entries, collapse = ", "), ")")
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, out_file, auto_unbox = TRUE, digits = 17, pretty = TRUE)
message("wrote ", out_file)

dir.create(dirname(scala_out), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    "package scalafim.fmri.fit.fixtures",
    "",
    "// Generated by tools/r-parity/generate_dms_receipt.R.",
    "final case class DmsTExpected(estimate: Double, standardError: Double, statistic: Double)",
    "final case class DmsFExpected(numeratorDf: Int, denominatorDf: Int, statistic: Double)",
    "",
    "object DmsRFixture:",
    paste0("  val trialIds: Vector[String] = ", scala_strings(events$trial_id)),
    paste0("  val runs: Vector[String] = ", scala_strings(as.character(events$run))),
    paste0("  val sampleOnsets: Vector[Double] = ", scala_doubles(events$sample_onset)),
    paste0("  val sampleDurations: Vector[Double] = ", scala_doubles(events$sample_duration)),
    paste0("  val delayOnsets: Vector[Double] = ", scala_doubles(events$delay_onset)),
    paste0("  val delayDurations: Vector[Double] = ", scala_doubles(events$delay_duration)),
    paste0("  val probeOnsets: Vector[Double] = ", scala_doubles(events$probe_onset)),
    paste0("  val probeDurations: Vector[Double] = ", scala_doubles(events$probe_duration)),
    paste0("  val stimulus: Vector[String] = ", scala_strings(as.character(events$stimulus))),
    paste0("  val load: Vector[String] = ", scala_strings(as.character(events$load))),
    paste0("  val matchStatus: Vector[String] = ", scala_strings(as.character(events$match))),
    paste0("  val rt: Vector[Double] = ", scala_doubles(events$rt)),
    paste0("  val response: Vector[Double] = ", scala_doubles(response)),
    paste0("  val runRank: Vector[Int] = Vector(", paste(vapply(run_fits, function(run) as.character(as.integer(run$rank)), character(1)), collapse = ", "), ")"),
    paste0("  val runResidualDf: Vector[Int] = Vector(", paste(vapply(run_fits, function(run) as.character(as.integer(run$residual_df)), character(1)), collapse = ", "), ")"),
    paste0("  val fixedResidualDf: Int = ", as.integer(fixed_df)),
    paste0("  val tExpected: Map[String, DmsTExpected] = ", scala_t_map(t_results)),
    paste0("  val fExpected: Map[String, DmsFExpected] = ", scala_f_map(f_results)),
    paste0("  val acceptedDifferences: Vector[String] = ", scala_strings(accepted_differences)),
    paste0("  val fmridesignRevision: String = ", scala_string(source$fmridesign_revision)),
    paste0("  val fmrihrfRevision: String = ", scala_string(source$fmrihrf_revision)),
    paste0("  val fmridesignVersion: String = ", scala_string(source$fmridesign_version)),
    paste0("  val fmrihrfVersion: String = ", scala_string(source$fmrihrf_version))
  ),
  scala_out
)
message("wrote ", scala_out)
