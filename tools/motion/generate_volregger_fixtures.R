#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
volregger_home <- if (length(args) >= 1L) args[[1L]] else Sys.getenv("VOLREGGER_HOME", "~/code/volregger")
out_file <- if (length(args) >= 2L) args[[2L]] else ""
volregger_home <- normalizePath(path.expand(volregger_home), mustWork = TRUE)

source(file.path(volregger_home, "R", "utils.R"))
source(file.path(volregger_home, "R", "transform_metrics.R"))
source(file.path(volregger_home, "R", "fd_dvars.R"))

suppressPackageStartupMessages(library(Rcpp))
dyn.load(file.path(volregger_home, "src", "volregger.so"))
assign(
  "_volregger_cpp_volreg_apply",
  getNativeSymbolInfo("_volregger_cpp_volreg_apply", PACKAGE = "volregger"),
  envir = .GlobalEnv
)
assign(
  "_volregger_cpp_volreg_estimate",
  getNativeSymbolInfo("_volregger_cpp_volreg_estimate", PACKAGE = "volregger"),
  envir = .GlobalEnv
)
assign(
  "_volregger_cpp_fit_pose_spline",
  getNativeSymbolInfo("_volregger_cpp_fit_pose_spline", PACKAGE = "volregger"),
  envir = .GlobalEnv
)
source(file.path(volregger_home, "R", "RcppExports.R"))
source(file.path(volregger_home, "R", "adapters-neuroim2.R"))
source(file.path(volregger_home, "R", "control.R"))
source(file.path(volregger_home, "R", "profiles.R"))
source(file.path(volregger_home, "R", "apply_motion.R"))
source(file.path(volregger_home, "R", "estimate_motion.R"))
source(file.path(volregger_home, "R", "classes.R"))
source(file.path(volregger_home, "R", "reporting.R"))
source(file.path(volregger_home, "R", "cli_io.R"))
source(file.path(volregger_home, "R", "cli.R"))

git_commit <- tryCatch(
  system2("git", c("-C", volregger_home, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)[[1L]],
  error = function(e) "unknown"
)

fmt <- function(x) {
  ifelse(is.na(x), "NA", format(as.numeric(x), digits = 17, scientific = FALSE, trim = TRUE))
}

line <- function(key, value) {
  paste0(key, "=", paste(fmt(value), collapse = ","))
}

metadata <- function(key, value) {
  paste0("metadata.", key, "=", paste(as.character(value), collapse = ";"))
}

flag <- function(x) {
  as.numeric(isTRUE(x))
}

pose <- c(1.2, -0.5, 0.8, 0.1, -0.05, 0.08)
mat <- motion_matrices(matrix(pose, nrow = 1L))[, , 1L]
inv <- invert_motion(matrix(pose, nrow = 1L))[, , 1L]
fd_motion <- matrix(
  c(
    0, 0, 0, 0, 0, 0,
    0.5, 0, 0, 0.01, 0, 0,
    0.5, -0.25, 0, 0.01, 0.02, 0
  ),
  ncol = 6L,
  byrow = TRUE
)
dvars_values <- c(1, 2, 5)
robust_dvars_values <- c(0, 1, 11, 14)
apply_edge_input <- array(c(1, 0, 0), dim = c(3, 1, 1, 1))
estimator_identity_dims <- c(5L, 5L, 3L, 3L)
estimator_identity_frame <- array(0, dim = estimator_identity_dims[1:3])
estimator_identity_frame[3, 3, 2] <- 5
estimator_identity_run <- array(
  rep(as.numeric(estimator_identity_frame), estimator_identity_dims[[4L]]),
  dim = estimator_identity_dims
)
estimator_identity_control <- utils::modifyList(
  volreg_control(),
  list(
    max_iter = 2L,
    n_samples = prod(estimator_identity_dims[1:3]),
    robust_template = FALSE,
    capture_boost = FALSE,
    use_ic_stencil = FALSE,
    whiten_modes = FALSE,
    edge_exclude_frac = 0,
    use_overlap_taper = FALSE
  )
)
estimator_identity <- estimate_motion(estimator_identity_run, ref = "middle", control = estimator_identity_control)
estimator_translation_dims <- c(7L, 5L, 5L, 2L)
base_value_at <- function(x, y, z) {
  dx <- x - 3
  dy <- y - 2
  dz <- z - 2
  10 * exp(-(dx * dx / 5 + dy * dy / 3 + dz * dz / 4)) +
    0.4 * x +
    0.2 * y -
    0.15 * z +
    0.35 * dx * dy +
    0.12 * dx * dx -
    0.08 * dy * dy +
    0.05 * dx * dz
}
estimator_translation_fixed <- array(0, dim = estimator_translation_dims[1:3])
for (i in seq_len(estimator_translation_dims[[1L]])) {
  for (j in seq_len(estimator_translation_dims[[2L]])) {
    for (k in seq_len(estimator_translation_dims[[3L]])) {
      estimator_translation_fixed[i, j, k] <- base_value_at(i - 1, j - 1, k - 1)
    }
  }
}
estimator_translation_moving <- array(0, dim = estimator_translation_dims[1:3])
for (i in seq_len(estimator_translation_dims[[1L]])) {
  for (j in seq_len(estimator_translation_dims[[2L]])) {
    for (k in seq_len(estimator_translation_dims[[3L]])) {
      estimator_translation_moving[i, j, k] <- estimator_translation_fixed[min(estimator_translation_dims[[1L]], i + 1L), j, k]
    }
  }
}
estimator_translation_run <- array(0, dim = estimator_translation_dims)
estimator_translation_run[, , , 1L] <- estimator_translation_fixed
estimator_translation_run[, , , 2L] <- estimator_translation_moving
estimator_translation_mask <- array(TRUE, dim = estimator_translation_dims[1:3])
estimator_translation_mask[1, , ] <- FALSE
estimator_translation_mask[estimator_translation_dims[[1L]], , ] <- FALSE
estimator_translation_mask[, 1, ] <- FALSE
estimator_translation_mask[, estimator_translation_dims[[2L]], ] <- FALSE
estimator_translation_mask[, , 1] <- FALSE
estimator_translation_mask[, , estimator_translation_dims[[3L]]] <- FALSE
estimator_translation_control <- utils::modifyList(
  volreg_control(),
  list(
    use_pyramid = FALSE,
    max_iter = 12L,
    n_samples = prod(estimator_translation_dims[1:3]),
    robust_template = FALSE,
    capture_boost = TRUE,
    capture_trans_mm = 1,
    capture_rot_deg = 8,
    capture_topk = 4L,
    use_ic_stencil = FALSE,
    whiten_modes = FALSE,
    edge_exclude_frac = 0,
    use_overlap_taper = FALSE
  )
)
estimator_translation <- estimate_motion(
  estimator_translation_run,
  mask = estimator_translation_mask,
  ref = "index",
  ref_index = 1L,
  control = estimator_translation_control
)
translation_summary <- motion_displacement_summary(
  mask = array(TRUE, dim = c(3, 3, 3)),
  motion = matrix(c(1, 0, 0, 0, 0, 0), nrow = 1L),
  truth = matrix(c(0, 0, 0, 0, 0, 0), nrow = 1L),
  pixdim = c(2, 2, 2)
)
spline_pose <- matrix(
  c(
    0, 0, 0, 0, 0, 0,
    3, 0, 0, 0, 0, 0.3,
    0, 6, 0, 0.1, 0, 0,
    0, 0, 9, 0, 0.2, 0
  ),
  ncol = 6L,
  byrow = TRUE
)
spline_tr <- 1.25
spline_smooth_iter <- 2L
spline_slice_times <- c(0, 0.5, 0.25, 0.75)
spline_mb_groups <- c(1L, 2L, 1L, 3L)
spline_control <- list(spline_smooth_iter = spline_smooth_iter)
spline_by_slice <- cpp_fit_pose_spline(
  par = spline_pose,
  tr = spline_tr,
  slice_times = spline_slice_times,
  control = spline_control
)
spline_by_mb <- cpp_fit_pose_spline(
  par = spline_pose,
  tr = spline_tr,
  mb_groups = spline_mb_groups,
  control = spline_control
)

profile_flags <- function(name) {
  prof <- volreg_profile(name)
  ctl <- prof$control
  c(
    flag(ctl$use_ic_stencil),
    flag(ctl$whiten_modes),
    flag(ctl$parallel_frames),
    flag(ctl$use_slice_spline),
    flag(ctl$use_nuisance_marginalization),
    ctl$edge_exclude_frac,
    flag(ctl$template_refresh_valid_only)
  )
}
profile_fast_fmri <- volreg_profile("fast_fmri")
profile_fast_native_parallel <- volreg_profile("fast_native_parallel")
profile_ic_stencil <- volreg_profile("ic_stencil")
profile_ic_whiten <- volreg_profile("ic_whiten")
profile_slice_spline <- volreg_profile("slice_spline")

report_summary_stub <- list(
  engine = "rigid_robust",
  n_frames = 3L,
  motion_abs_max = c(tx = 0, ty = 0, tz = 0, rx = 0, ry = 0, rz = 0),
  fd_mean = 0,
  fd_p95 = 0,
  fd_max = 0,
  dvars_mean = 0,
  dvars_p95 = 0,
  robust_dvars_mean = 0,
  robust_dvars_p95 = 0,
  motion_spikes = 0L,
  fit_failures = 0L,
  censor_suggested = 0L,
  cost_drop_mean = 0,
  packet_correction_mag_mean = 0,
  packet_correction_mag_max = 0
)
report_summary_columns <- names(.volreg_report_summary(report_summary_stub, profile_fast_fmri$control))
report_components_fast_fmri <- .control_components(profile_fast_fmri$control)
cli_run_outputs <- sub(
  "^\\s*(<prefix>_[^[:space:]]+).*$",
  "\\1",
  grep("^  <prefix>", .volregger_usage("run"), value = TRUE)
)
benchmark_required_columns <- c(
  "estimate_sec",
  "apply_sec",
  "report_sec",
  "elapsed_sec",
  "tsnr_ratio",
  "disp_p95",
  "fd_error"
)
benchmark_truth_scenarios <- c("low_motion", "moderate_motion", "hard_motion_plus_nuisance")
benchmark_claim_families <- c("truth_displacement", "truth_pose", "truth_matrix")

lines <- c(
  "# Generated by tools/motion/generate_volregger_fixtures.R",
  "metadata.fixture_version=1",
  paste0("metadata.volregger_commit=", git_commit),
  "metadata.generated_by=tools/motion/generate_volregger_fixtures.R",
  metadata(
    "source_files",
    c(
      "R/utils.R",
      "R/transform_metrics.R",
      "R/fd_dvars.R",
      "R/apply_motion.R",
      "R/estimate_motion.R",
      "R/control.R",
      "R/profiles.R",
      "R/classes.R",
      "R/reporting.R",
      "R/cli.R",
      "src/api_apply.cpp",
      "src/api_estimate.cpp",
      "src/api_spline.cpp",
      "tests/testthat/test-synthetic-ablation-modes.R",
      "tests/testthat/test-ic-efficacy.R",
      "tests/testthat/test-reporting-cli.R",
      "tests/testthat/test-external-benchmark-guardrails.R"
    )
  ),
  metadata("profile.fast_fmri.components", profile_fast_fmri$components),
  metadata("profile.fast_native_parallel.components", profile_fast_native_parallel$components),
  metadata("profile.ic_stencil.components", profile_ic_stencil$components),
  metadata("profile.ic_whiten.components", profile_ic_whiten$components),
  metadata("profile.slice_spline.components", profile_slice_spline$components),
  metadata("profile.fast_fmri.engine", profile_fast_fmri$engine),
  metadata("profile.slice_spline.engine", profile_slice_spline$engine),
  metadata("spline.method", spline_by_slice$method),
  metadata("cli.commands", .volregger_commands()),
  metadata("cli.run.outputs", cli_run_outputs),
  metadata("report.summary_columns", report_summary_columns),
  metadata("report.fast_fmri.components", report_components_fast_fmri),
  metadata("benchmark.required_columns", benchmark_required_columns),
  metadata("benchmark.truth_scenarios", benchmark_truth_scenarios),
  metadata("benchmark.claim_families", benchmark_claim_families),
  line("pose", pose),
  line("matrix_row_major", as.vector(t(mat))),
  line("inverse_row_major", as.vector(t(inv))),
  line("fd_trace_row_major", as.vector(t(fd_motion))),
  line("fd", framewise_disp(fd_motion)),
  line("dvars_values", dvars_values),
  line("dvars_raw", dvars(array(dvars_values, dim = c(1, 1, 1, length(dvars_values))), robust = FALSE)),
  line("robust_dvars_values", robust_dvars_values),
  line("dvars_robust", dvars(array(robust_dvars_values, dim = c(1, 1, 1, length(robust_dvars_values))), robust = TRUE)),
  line(
    "translation_summary",
    c(
      translation_summary$disp_median,
      translation_summary$disp_p95,
      translation_summary$disp_max,
      translation_summary$fd_est,
      translation_summary$fd_truth,
      translation_summary$fd_error
    )
  ),
  line("radius_fd_pose", fd_from_transform(matrix(pose, nrow = 1L))),
  line("apply_edge_input", apply_edge_input),
  line(
    "apply_edge_plus_x_clamp",
    apply_motion(apply_edge_input, matrix(c(1, 0, 0, 0, 0, 0), nrow = 1L), interp = "linear", pad = "clamp")
  ),
  line(
    "apply_edge_plus_x_zero",
    apply_motion(apply_edge_input, matrix(c(1, 0, 0, 0, 0, 0), nrow = 1L), interp = "linear", pad = "zero")
  ),
  line("estimator_identity_dims", estimator_identity_dims),
  line("estimator_identity_motion_row_major", as.vector(t(estimator_identity$par))),
  line("estimator_identity_cost_init", estimator_identity$cost_init),
  line("estimator_identity_cost_final", estimator_identity$cost_final),
  line("estimator_identity_overlap", estimator_identity$overlap),
  line("estimator_translation_dims", estimator_translation_dims),
  line("estimator_translation_motion_row_major", as.vector(t(estimator_translation$par))),
  line("estimator_translation_cost_init", estimator_translation$cost_init),
  line("estimator_translation_cost_final", estimator_translation$cost_final),
  line("estimator_translation_overlap", estimator_translation$overlap),
  line("spline_pose_row_major", as.vector(t(spline_pose))),
  line("spline_tr", spline_tr),
  line("spline_smooth_iter", spline_smooth_iter),
  line("spline_slice_times", spline_slice_times),
  line("spline_mb_groups", spline_mb_groups),
  line("spline_smoothed_pose_row_major", as.vector(t(spline_by_slice$par_smooth))),
  line("spline_times", spline_by_slice$times),
  line("spline_packet_offsets_slice_times", spline_by_slice$packet_offsets),
  line("spline_packet_offsets_mb_groups", spline_by_mb$packet_offsets),
  line("profile_fast_fmri_flags", profile_flags("fast_fmri")),
  line("profile_fast_native_parallel_flags", profile_flags("fast_native_parallel")),
  line("profile_ic_stencil_flags", profile_flags("ic_stencil")),
  line("profile_ic_whiten_flags", profile_flags("ic_whiten")),
  line("profile_slice_spline_flags", profile_flags("slice_spline")),
  line("ic_whiten_noninferiority_gates", c(1.08, 1.05, 0.15, 1.12)),
  line("ic_nuisance_improvement_gates", c(0.98, 0.95, 0.10, 1.20)),
  line("expanded_capture_success_gates", c(-1, 2))
)

if (nzchar(out_file)) {
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, out_file, useBytes = TRUE)
} else {
  cat(paste0(lines, collapse = "\n"), "\n", sep = "")
}
