#!/usr/bin/env Rscript

# Independent sampled-nuisance and direct-QR receipt for P3.3 / S15.
#
# R fmrihrf owns event rendering, this script owns deterministic fMRIPrep-like
# scan regressors and response synthesis, and base R lm.fit owns the reference
# fit. No ScalaFIM matrix or fitted result is an input.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate the realistic-nuisance receipt", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to generate the realistic-nuisance receipt", call. = FALSE)
}

hrf_pkg <- Sys.getenv("FMRIHRF_R", file.path(path.expand("~"), "code", "fmrihrf"))
out_file <- Sys.getenv(
  "REALISTIC_NUISANCE_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "fit.realistic-nuisance.v1.r.json")
)
scala_out <- Sys.getenv(
  "REALISTIC_NUISANCE_SCALA_OUT",
  file.path(
    "modules", "fit", "shared", "src", "test", "scala", "scalafim",
    "fmri", "fit", "fixtures", "RealisticNuisanceRFixture.scala"
  )
)

pkgload::load_all(hrf_pkg, quiet = TRUE)

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

run_length <- 80L
n_runs <- 2L
precision <- 0.05
run_sampling_frame <- fmrihrf::sampling_frame(
  blocklens = run_length,
  TR = 1,
  start_time = 0,
  precision = precision
)

within_run_event <- rep(0:7, times = n_runs)
run_index <- rep(0:1, each = 8L)
events <- data.frame(
  trial_id = sprintf("nuisance-trial-%02d", seq_len(16L)),
  run = factor(paste0("run-", run_index + 1L), levels = c("run-1", "run-2")),
  onset = 4.25 + within_run_event * 9,
  condition = factor(
    rep(c("A", "B", "A", "A", "B", "B", "A", "B"), times = n_runs),
    levels = c("A", "B", "C")
  )
)

task_for_run <- function(run_id) {
  selected <- events[events$run == run_id, , drop = FALSE]
  raw <- fmrihrf::regressor_design(
    selected$onset,
    selected$condition,
    rep(1L, nrow(selected)),
    run_sampling_frame,
    fmrihrf::HRF_SPMG1,
    duration = 0,
    precision = precision
  )
  unname(raw[, 1:2, drop = FALSE])
}
task_design <- do.call(rbind, lapply(levels(events$run), task_for_run))
colnames(task_design) <- c("task_condition.A", "task_condition.B")

confounds_for_run <- function(run) {
  scan <- 0:(run_length - 1L)
  phase <- run * 0.31
  jitter <- function(multiplier, modulus) {
    0.08 * (((scan * multiplier + run) %% modulus) / modulus - 0.5)
  }
  trans_x <- sin((scan + 1) * 0.13 + phase) + 0.02 * scan / run_length + jitter(3, 17)
  trans_y <- cos((scan + 1) * 0.17 + phase) + jitter(5, 19)
  trans_z <- sin((scan + 1) * 0.07 + 0.4 + phase) + jitter(7, 23)
  rot_x <- cos((scan + 1) * 0.11 + 0.2 + phase) + jitter(11, 29)
  rot_y <- sin((scan + 1) * 0.19 + 0.1 + phase) + jitter(13, 31)
  rot_z <- cos((scan + 1) * 0.23 + 0.3 + phase) + jitter(17, 37)
  derivative <- function(value) c(0, diff(value))
  spike_a <- as.numeric(scan == (13L + run * 3L))
  spike_b <- as.numeric(scan == (51L - run * 4L))
  value <- cbind(
    trans_x = trans_x,
    trans_y = trans_y,
    trans_z = trans_z,
    rot_x = rot_x,
    rot_y = rot_y,
    rot_z = rot_z,
    trans_x_derivative1 = derivative(trans_x),
    trans_y_derivative1 = derivative(trans_y),
    trans_z_derivative1 = derivative(trans_z),
    rot_x_derivative1 = derivative(rot_x),
    rot_y_derivative1 = derivative(rot_y),
    rot_z_derivative1 = derivative(rot_z),
    trans_x_power2 = trans_x^2,
    trans_y_power2 = trans_y^2,
    trans_z_power2 = trans_z^2,
    a_comp_cor_00 = sin((scan + 1) * 0.29 + phase),
    a_comp_cor_01 = cos((scan + 1) * 0.37 + phase),
    motion_outlier_00 = spike_a,
    motion_outlier_01 = spike_b,
    constant_conf = 1,
    trans_x_dup = trans_x,
    rot_z_near = rot_z + 0.02 * sin((scan + 1) * 0.71)
  )
  unname(value)
}

raw_confounds <- lapply(0:1, confounds_for_run)
confound_names <- c(
  "trans_x", "trans_y", "trans_z", "rot_x", "rot_y", "rot_z",
  "trans_x_derivative1", "trans_y_derivative1", "trans_z_derivative1",
  "rot_x_derivative1", "rot_y_derivative1", "rot_z_derivative1",
  "trans_x_power2", "trans_y_power2", "trans_z_power2",
  "a_comp_cor_00", "a_comp_cor_01", "motion_outlier_00", "motion_outlier_01",
  "constant_conf", "trans_x_dup", "rot_z_near"
)
raw_confounds <- lapply(raw_confounds, function(value) {
  colnames(value) <- confound_names
  value
})
dropped_names <- c("constant_conf", "trans_x_dup")
retained_names <- setdiff(confound_names, dropped_names)
retained_confounds <- lapply(raw_confounds, function(value) {
  value[, retained_names, drop = FALSE]
})

block_diagonal <- function(matrices) {
  rows <- sum(vapply(matrices, nrow, integer(1)))
  cols <- sum(vapply(matrices, ncol, integer(1)))
  out <- matrix(0, nrow = rows, ncol = cols)
  row_offset <- 0L
  col_offset <- 0L
  for (value in matrices) {
    row_index <- row_offset + seq_len(nrow(value))
    col_index <- col_offset + seq_len(ncol(value))
    out[row_index, col_index] <- value
    row_offset <- row_offset + nrow(value)
    col_offset <- col_offset + ncol(value)
  }
  out
}

drift <- block_diagonal(lapply(seq_len(n_runs), function(...) stats::poly(seq_len(run_length), 2)))
intercepts <- block_diagonal(lapply(seq_len(n_runs), function(...) matrix(1, run_length, 1)))
nuisance <- block_diagonal(retained_confounds)
colnames(drift) <- unlist(lapply(seq_len(n_runs), function(run) paste0("drift_", run, "_", 1:2)))
colnames(intercepts) <- paste0("intercept_", seq_len(n_runs))
colnames(nuisance) <- unlist(lapply(seq_len(n_runs), function(run) paste0(retained_names, "_run_", run)))
full_design <- cbind(task_design, drift, intercepts, nuisance)

beta <- rep(0, ncol(full_design))
beta[1:2] <- c(0.75, -0.25)
beta[2L + ncol(drift) + 1:2] <- c(0.18, -0.12)
for (run in seq_len(n_runs)) {
  nuisance_offset <- 2L + ncol(drift) + ncol(intercepts) + (run - 1L) * length(retained_names)
  beta[nuisance_offset + match("trans_x", retained_names)] <- 0.11
  beta[nuisance_offset + match("a_comp_cor_00", retained_names)] <- -0.07
  beta[nuisance_offset + match("motion_outlier_00", retained_names)] <- 0.20
}
noise <- 0.004 * sin((seq_len(run_length * n_runs)) * 0.41) +
  0.0025 * cos((seq_len(run_length * n_runs)) * 0.17)
response <- drop(full_design %*% beta) + noise

fit <- stats::lm.fit(x = full_design, y = response)
decomposition <- svd(full_design)
singular_values <- decomposition$d
message(sprintf(
  "realistic-nuisance design: rank=%d/%d, singular range=[%.6g, %.6g]",
  fit$rank, ncol(full_design), min(singular_values), max(singular_values)
))
smallest <- decomposition$v[, length(singular_values)]
message(paste(
  "smallest singular direction:",
  paste(colnames(full_design)[order(abs(smallest), decreasing = TRUE)[1:6]], collapse = ", ")
))
if (fit$rank != ncol(full_design)) {
  stop(sprintf("external realistic-nuisance design rank %d < %d", fit$rank, ncol(full_design)), call. = FALSE)
}
residual_variance <- sum(fit$residuals^2) / fit$df.residual
covariance <- residual_variance * solve(crossprod(full_design))
task_coefficients <- unname(fit$coefficients[1:2])
task_covariance <- unname(covariance[1:2, 1:2, drop = FALSE])

t_weights <- c(1, -1)
t_estimate <- drop(crossprod(t_weights, task_coefficients))
t_standard_error <- sqrt(drop(crossprod(t_weights, task_covariance %*% t_weights)))
t_statistic <- t_estimate / t_standard_error
f_statistic <- drop(crossprod(task_coefficients, solve(task_covariance, task_coefficients))) / 2

source <- list(
  fmrihrf_revision = git_revision(normalizePath(hrf_pkg, mustWork = FALSE)),
  fmrihrf_version = as.character(utils::packageVersion("fmrihrf")),
  r_version = as.character(getRversion()),
  stats_version = as.character(utils::packageVersion("stats")),
  producer = "tools/r-parity/generate_realistic_nuisance_receipt.R",
  reference = "R fmrihrf event rendering, deterministic sampled confounds, and base-R direct QR"
)
accepted_differences <- c(
  "S12 mixed sustained/transient design construction is asserted separately through ScalaFIM structural provenance; this S15 receipt is an independent fit oracle.",
  "The confounds are deterministic fMRIPrep-shaped sampled columns rather than a claim about fMRIPrep TSV discovery or selection.",
  "R omits the declared empty condition C from the fitted task axis; ScalaFIM records and omits it through EmptyCellPolicy.Omit."
)
schema <- "scalafim-r-realistic-nuisance-fixture/v1"
inputs <- list(
  sampling_frame = list(blocklens = c(run_length, run_length), tr = c(1, 1), start_time = c(0, 0)),
  precision = precision,
  events = list(
    trial_id = events$trial_id,
    run = as.character(events$run),
    onset = events$onset,
    condition = as.character(events$condition),
    declared_condition_levels = levels(events$condition)
  ),
  nuisance_runs = lapply(raw_confounds, function(value) list(
    names = colnames(value),
    matrix = matrix_rows(value)
  )),
  nuisance_policy = list(
    check = "drop columns that do not increase rank",
    na_action = "drop",
    rank_tolerance = sqrt(.Machine$double.eps),
    duplicate_report_threshold = 0.999
  ),
  planted_coefficients = beta,
  noise = noise
)
outputs <- list(
  task_design = matrix_rows(task_design),
  response = response,
  retained_nuisance = lapply(seq_len(n_runs), function(...) retained_names),
  dropped_nuisance = lapply(seq_len(n_runs), function(...) dropped_names),
  design_rank = fit$rank,
  residual_df = fit$df.residual,
  task_coefficients = task_coefficients,
  task_covariance = matrix_rows(task_covariance),
  t_hypotheses = list(
    `task-a-minus-b` = list(
      estimate = t_estimate,
      standard_error = t_standard_error,
      statistic = t_statistic
    )
  ),
  f_hypotheses = list(
    `task-omnibus` = list(
      numerator_df = 2L,
      denominator_df = fit$df.residual,
      statistic = f_statistic
    )
  )
)
receipt <- list(
  schema_version = schema,
  producer_command = "LC_ALL=C LANG=C Rscript tools/r-parity/generate_realistic_nuisance_receipt.R && python3 tools/r-parity/finalize_realistic_nuisance_receipt.py",
  conventions = list(
    dtype = "float64",
    json_encoding = "JSON numbers are finite IEEE-754 binary64 values serialized with 17 significant digits",
    matrix_orientation = "rows are scans; nuisance columns retain their semantic fMRIPrep-style names",
    task_design = "each run is rendered independently with fmrihrf::regressor_design at 0.05-second precision and then concatenated",
    nuisance = "motion, first derivatives, declared squares, CompCor-like components, spikes, a constant alias, an exact duplicate, and a retained near duplicate",
    baseline = "per-run stats::poly degree two plus per-run intercepts",
    fit = "base-R lm.fit direct QR over the retained full design"
  ),
  accepted_differences = accepted_differences,
  hashes = list(inputs_sha256 = "", outputs_sha256 = ""),
  source = source
)
payload <- list(
  schema_version = schema,
  scenario_id = "fit.realistic-nuisance.v1",
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
scala_run <- function(value) {
  entries <- vapply(seq_along(confound_names), function(column) {
    paste0(
      scala_string(confound_names[[column]]), " -> ",
      scala_doubles(value[, column])
    )
  }, character(1))
  paste0("RealisticNuisanceRun(Vector(", paste(entries, collapse = ", "), "))")
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, out_file, auto_unbox = TRUE, digits = 17, pretty = TRUE)
message("wrote ", out_file)

dir.create(dirname(scala_out), recursive = TRUE, showWarnings = FALSE)
writeLines(
  c(
    "package scalafim.fmri.fit.fixtures",
    "",
    "// Generated by tools/r-parity/generate_realistic_nuisance_receipt.R.",
    "final case class RealisticNuisanceRun(columns: Vector[(String, Vector[Double])])",
    "final case class RealisticNuisanceTExpected(estimate: Double, standardError: Double, statistic: Double)",
    "final case class RealisticNuisanceFExpected(numeratorDf: Int, denominatorDf: Int, statistic: Double)",
    "",
    "object RealisticNuisanceRFixture:",
    paste0("  val trialIds: Vector[String] = ", scala_strings(events$trial_id)),
    paste0("  val runs: Vector[String] = ", scala_strings(as.character(events$run))),
    paste0("  val onsets: Vector[Double] = ", scala_doubles(events$onset)),
    paste0("  val conditions: Vector[String] = ", scala_strings(as.character(events$condition))),
    paste0("  val nuisanceRuns: Vector[RealisticNuisanceRun] = Vector(", paste(vapply(raw_confounds, scala_run, character(1)), collapse = ", "), ")"),
    paste0("  val response: Vector[Double] = ", scala_doubles(response)),
    paste0("  val retainedNuisance: Vector[String] = ", scala_strings(retained_names)),
    paste0("  val droppedNuisance: Vector[String] = ", scala_strings(dropped_names)),
    paste0("  val designRank: Int = ", as.integer(fit$rank)),
    paste0("  val residualDf: Int = ", as.integer(fit$df.residual)),
    paste0("  val taskCoefficients: Vector[Double] = ", scala_doubles(task_coefficients)),
    paste0("  val taskCovariance: Vector[Double] = ", scala_doubles(as.vector(t(task_covariance)))),
    paste0("  val taskDifference: RealisticNuisanceTExpected = RealisticNuisanceTExpected(", scala_number(t_estimate), ", ", scala_number(t_standard_error), ", ", scala_number(t_statistic), ")"),
    paste0("  val taskOmnibus: RealisticNuisanceFExpected = RealisticNuisanceFExpected(2, ", as.integer(fit$df.residual), ", ", scala_number(f_statistic), ")"),
    paste0("  val acceptedDifferences: Vector[String] = ", scala_strings(accepted_differences)),
    paste0("  val fmrihrfRevision: String = ", scala_string(source$fmrihrf_revision)),
    paste0("  val fmrihrfVersion: String = ", scala_string(source$fmrihrf_version))
  ),
  scala_out
)
message("wrote ", scala_out)
