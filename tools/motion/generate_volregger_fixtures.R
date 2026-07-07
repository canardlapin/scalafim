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
source(file.path(volregger_home, "R", "RcppExports.R"))
source(file.path(volregger_home, "R", "adapters-neuroim2.R"))
source(file.path(volregger_home, "R", "control.R"))
source(file.path(volregger_home, "R", "profiles.R"))
source(file.path(volregger_home, "R", "apply_motion.R"))
source(file.path(volregger_home, "R", "estimate_motion.R"))

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

lines <- c(
  "# Generated by tools/motion/generate_volregger_fixtures.R",
  "metadata.fixture_version=1",
  paste0("metadata.volregger_commit=", git_commit),
  "metadata.generated_by=tools/motion/generate_volregger_fixtures.R",
  "metadata.source_files=R/utils.R;R/transform_metrics.R;R/fd_dvars.R",
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
  line("estimator_translation_overlap", estimator_translation$overlap)
)

if (nzchar(out_file)) {
  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, out_file, useBytes = TRUE)
} else {
  cat(paste0(lines, collapse = "\n"), "\n", sep = "")
}
