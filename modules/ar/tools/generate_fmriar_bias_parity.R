#!/usr/bin/env Rscript

source(file.path("tools", "r-parity", "receipt_serialization.R"))

# Regenerate constants for FmriArBiasParitySuite.scala: fmriAR's design-aware
# residual-bias correction (acvf_bias_matrix(), fit_noise(design =),
# noise_acvf(design =)).
#
# Reference: fmriAR 0.3.3 at ca27f27 (the "validate design-corrected residuals"
# commit; the correction itself predates the revision pinned for the
# non-design-aware fixtures, which this script does not touch).
#
# Every input is deterministic: designs and noise come from a sine hash, the
# residuals are the OLS residuals computed here in R, and both designs and
# residuals are rounded to the receipt's 13 significant digits BEFORE R fits
# anything. The Scala test therefore feeds fmriAR's own inputs, bit for bit,
# and no RNG runs at check time. Normal Scala tests do not call this script.

if (!requireNamespace("fmriAR", quietly = TRUE)) {
  stop("The fmriAR R package must be installed to regenerate parity fixtures.", call. = FALSE)
}

scala_out <- Sys.getenv(
  "FMRIAR_BIAS_SCALA_OUT",
  file.path(
    "modules", "ar", "shared", "src", "test", "scala", "scalafim",
    "fmri", "ar", "fixtures", "FmriArBiasRFixture.scala"
  )
)
fmriar_pkg <- Sys.getenv("FMRIAR_AR_R", file.path(path.expand("~"), "code", "fmriAR"))

fmrireg_pkg <- Sys.getenv("FMRIREG_AR_R", file.path(path.expand("~"), "code", "fmrireg"))
# Load the exact adapter source without installing fmrireg's unrelated IO/model
# dependencies. This is the reference function itself, not a rewritten formula.
fmrireg_reference <- new.env(parent = globalenv())
source(file.path(fmrireg_pkg, "R", "fmriAR_adapter.R"), local = fmrireg_reference)

# --- deterministic inputs -----------------------------------------------------

hash_unit <- function(i, seed) {
  raw <- sin(i * 12.9898 + seed * 78.233) * 43758.5453
  (raw - floor(raw)) * 2 - 1
}
noise_vec <- function(n, seed) vapply(seq_len(n), hash_unit, numeric(1), seed = seed)
round13 <- function(x) signif(x, RECEIPT_SIGNIFICANT_DIGITS)

# Regressor with a smooth drift part and a hashed part, so the design is
# neither polynomial nor periodic.
regressor <- function(n, k) {
  t <- seq_len(n)
  0.8 * sin(2 * pi * t * k / (n + 7) + 0.3 * k) + 0.5 * noise_vec(n, 100 + k)
}

# Intercept structure is explicit per case: "global", "per_run" or "none".
make_design <- function(lens, intercept, n_reg, linear = TRUE) {
  n <- sum(lens)
  runs <- rep(seq_along(lens), times = lens)
  cols <- list()
  if (intercept == "global") cols[["g"]] <- rep(1, n)
  if (intercept == "per_run") {
    for (r in seq_along(lens)) cols[[paste0("r", r)]] <- as.numeric(runs == r)
  }
  if (linear) cols[["lin"]] <- seq_len(n) / n
  for (k in seq_len(n_reg)) cols[[paste0("x", k)]] <- regressor(n, k)
  round13(do.call(cbind, unname(cols)))
}

# Append columns (rounded) to a design.
add_cols <- function(design, ...) round13(cbind(design, ...))

# AR(1) noise that restarts at each run boundary, one column per voxel.
make_resid <- function(design, lens, n_vox, rho = 0.45, seed = 1) {
  n <- nrow(design)
  runs <- rep(seq_along(lens), times = lens)
  Y <- matrix(0, n, n_vox)
  for (v in seq_len(n_vox)) {
    innov <- noise_vec(n, seed * 1000 + v)
    e <- numeric(n)
    for (i in seq_len(n)) {
      first <- i == 1L || runs[i] != runs[i - 1L]
      e[i] <- innov[i] + if (first) 0 else rho * e[i - 1L]
    }
    beta <- noise_vec(ncol(design), seed * 77 + v)
    Y[, v] <- design %*% beta + e
  }
  round13(qr.resid(qr(design), Y))
}

# --- number formatting --------------------------------------------------------

scala_number <- function(value) {
  if (abs(value) < 5e-16) return("0.0")
  rendered <- receipt_format_number(value)
  if (grepl("[.eE]", rendered)) rendered else paste0(rendered, ".0")
}
scala_doubles <- function(values) {
  paste0("Vector(", paste(vapply(as.numeric(values), scala_number, character(1)), collapse = ", "), ")")
}
scala_rows <- function(m) {
  m <- as.matrix(m)
  paste0("Vector(", paste(vapply(seq_len(nrow(m)), function(i) scala_doubles(m[i, ]), character(1)), collapse = ", "), ")")
}
# NA (an undefined innovation variance) is carried as NaN.
scala_doubles_na <- function(values) {
  paste0("Vector(", paste(vapply(as.numeric(values), function(v)
    if (is.na(v)) "Double.NaN" else scala_number(v), character(1)), collapse = ", "), ")")
}
scala_ints <- function(values) paste0("Vector(", paste(as.integer(values), collapse = ", "), ")")
scala_string <- function(x) paste0("\"", x, "\"")
scala_list <- function(items) paste0("Vector(", paste(items, collapse = ", "), ")")

# Run `expr`, returning its value and any warnings it raised.
with_warnings <- function(expr) {
  msgs <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = msgs)
}

# --- acvf_bias_matrix() cases -------------------------------------------------

bias_case <- function(name, design, lens, censor, max_lag) {
  runs <- rep(seq_along(lens), times = lens)
  got <- with_warnings(fmriAR::acvf_bias_matrix(
    design, runs = if (length(lens) > 1L) runs else NULL,
    censor = if (length(censor)) censor else NULL, max_lag = max_lag
  ))
  A <- unname(got$value)
  list(
    name = name, design = design, lens = lens, censor = censor, max_lag = max_lag,
    lag = nrow(A[[1L]]) - 1L, capped = length(got$warnings) > 0L,
    A = A, rcond = vapply(A, rcond, numeric(1))
  )
}

bias_cases <- list(
  # one run, intercept + drift + two regressors, no censoring
  bias_case("single_run_global_intercept",
            make_design(40L, "global", 2L), 40L, integer(0), 5L),
  # per-run intercepts, unequal run lengths, censoring (including adjacent frames)
  bias_case("two_runs_per_run_intercepts_censored",
            make_design(c(30L, 22L), "per_run", 2L), c(30L, 22L), c(5L, 6L, 40L), 6L),
  # a single shared intercept across two runs: per-run centering is NOT absorbed
  # by the design, so the centred projection differs from M
  bias_case("two_runs_shared_intercept_only",
            make_design(c(24L, 24L), "global", 2L), c(24L, 24L), integer(0), 4L),
  # no intercept at all, three unequal runs, censoring on a run edge and inside
  bias_case("three_runs_no_intercept_censored",
            make_design(c(16L, 20L, 14L), "none", 2L, linear = FALSE),
            c(16L, 20L, 14L), c(10L, 11L, 30L, 37L), 5L),
  # lag budget exceeds the residual degrees of freedom (n = 16, rank 10)
  bias_case("budget_exceeds_rdf",
            make_design(16L, "global", 8L), 16L, integer(0), 8L),
  # rank-deficient design: a duplicated regressor
  bias_case("rank_deficient_duplicate_column",
            {d <- make_design(c(30L, 26L), "global", 2L); add_cols(d, d[, ncol(d)])},
            c(30L, 26L), integer(0), 6L),
  # a column constant on run 1 and zero on run 2, next to per-run intercepts
  bias_case("run_constant_column_with_per_run_intercepts",
            {d <- make_design(c(30L, 26L), "per_run", 1L)
             add_cols(d, c(rep(0.7, 30L), rep(0, 26L)))},
            c(30L, 26L), integer(0), 6L),
  # run 2 entirely censored
  bias_case("run_two_fully_censored",
            make_design(c(30L, 20L), "per_run", 2L), c(30L, 20L), 31:50, 5L),
  # isolated single-row segments
  bias_case("isolated_single_row_segments",
            make_design(c(30L, 26L), "per_run", 2L), c(30L, 26L),
            c(2L, 4L, 6L, 8L, 10L, 12L, 14L), 5L),
  # a long run and a very short one
  bias_case("runs_70_and_14",
            make_design(c(70L, 14L), "per_run", 1L), c(70L, 14L), integer(0), 25L)
)

# --- fit_noise(design =) cases ------------------------------------------------

fit_one <- function(resid, design, lens, censor, p, p_max, pooling, corr_lag) {
  runs <- rep(seq_along(lens), times = lens)
  got <- with_warnings(fmriAR::fit_noise(
    resid = resid,
    runs = if (length(lens) > 1L) runs else NULL,
    censor = if (length(censor)) censor else NULL,
    method = "ar", p = p, p_max = p_max, pooling = pooling,
    exact_first = "none", design = design, correction_max_lag = corr_lag
  ))
  plan <- got$value
  list(
    p = if (identical(p, "auto")) "auto" else as.character(p), p_max = p_max,
    pooling = pooling, phi = lapply(plan$phi, as.numeric),
    gamma = lapply(plan$gamma, as.numeric),
    sigma2 = vapply(plan$sigma2, function(x) as.numeric(x), numeric(1)),
    warnings = got$warnings,
    rejected_runs = which(vapply(seq_along(lens), function(r)
      any(grepl(paste0("skipped for run ", r, ":"), got$warnings, fixed = TRUE)), logical(1))) - 1L
  )
}

fit_case <- function(name, design, resid, lens, censor, corr_lag, fits) {
  list(name = name, design = design, resid = resid, lens = lens, censor = censor,
       corr_lag = corr_lag, fits = fits)
}

# Single run, five voxels, drift + two regressors; every fixed order and auto.
d1 <- make_design(60L, "global", 2L)
r1 <- make_resid(d1, 60L, 5L, seed = 1)
f1 <- c(
  lapply(1:4, function(p) fit_one(r1, d1, 60L, integer(0), p, p, "global", 12L)),
  list(fit_one(r1, d1, 60L, integer(0), "auto", 4L, "global", 12L))
)

# Two runs of unequal length, per-run intercepts, censoring; global and run pooling.
l2 <- c(40L, 36L)
d2 <- make_design(l2, "per_run", 2L)
r2 <- make_resid(d2, l2, 5L, seed = 2)
c2 <- c(7L, 8L, 45L)
f2 <- c(
  lapply(c("global", "run"), function(pool) fit_one(r2, d2, l2, c2, 2L, 2L, pool, 10L)),
  lapply(c("global", "run"), function(pool) fit_one(r2, d2, l2, c2, 1L, 1L, pool, 10L)),
  lapply(c("global", "run"), function(pool) fit_one(r2, d2, l2, c2, "auto", 3L, pool, 10L))
)

# Three runs, no intercept at all (centring is not absorbed by the design).
l3 <- c(26L, 30L, 22L)
d3 <- make_design(l3, "none", 2L)
r3 <- make_resid(d3, l3, 4L, seed = 3)
c3 <- c(12L, 40L, 41L)
f3 <- list(
  fit_one(r3, d3, l3, c3, 2L, 2L, "global", 8L),
  fit_one(r3, d3, l3, c3, 2L, 2L, "run", 8L)
)

# Conditioning gate: a budget this close to the run length makes A numerically
# singular (rcond ~ 1e-7 < 1e-6), so R leaves the run uncorrected and warns.
d4 <- make_design(60L, "global", 0L)
r4 <- make_resid(d4, 60L, 3L, seed = 4)
f4 <- list(fit_one(r4, d4, 60L, integer(0), 2L, 2L, "global", 57L))
raw4 <- fmriAR::fit_noise(r4, method = "ar", p = 2L, p_max = 2L, pooling = "global",
                          exact_first = "none")
A4 <- fmriAR::acvf_bias_matrix(d4, max_lag = 57L)[[1L]]

# (a) rank-deficient design (duplicated column), global pooling, p = 2.
l5 <- c(30L, 26L)
d5 <- {d <- make_design(l5, "global", 2L); add_cols(d, d[, ncol(d)])}
r5 <- make_resid(d5, l5, 4L, seed = 5)
f5 <- list(fit_one(r5, d5, l5, integer(0), 2L, 2L, "global", 8L))

# (b) a column constant on run 1 and zero on run 2, with per-run intercepts, run pooling.
d6 <- add_cols(make_design(l5, "per_run", 1L), c(rep(0.7, 30L), rep(0, 26L)))
r6 <- make_resid(d6, l5, 4L, seed = 6)
f6 <- list(fit_one(r6, d6, l5, integer(0), 2L, 2L, "run", 8L))

# (c) run 2 entirely censored, run and global pooling, p = 1.
l7 <- c(30L, 20L)
d7 <- make_design(l7, "per_run", 2L)
r7 <- make_resid(d7, l7, 4L, seed = 7)
c7 <- 31:50
f7 <- lapply(c("run", "global"), function(pool) fit_one(r7, d7, l7, c7, 1L, 1L, pool, 8L))

# (d) censoring that leaves isolated single-row segments, global pooling, p = 2.
d8 <- make_design(l5, "per_run", 2L)
r8 <- make_resid(d8, l5, 4L, seed = 8)
c8 <- c(2L, 4L, 6L, 8L, 10L, 12L, 14L)
f8 <- list(fit_one(r8, d8, l5, c8, 2L, 2L, "global", 5L))

# (e) runs of 70 and 14 rows, auto order, budget 25, run pooling: only the short run's
# bias matrix is too ill-conditioned to solve against.
l9 <- c(70L, 14L)
d9 <- make_design(l9, "per_run", 1L)
r9 <- make_resid(d9, l9, 4L, seed = 9)
f9 <- list(fit_one(r9, d9, l9, integer(0), "auto", 4L, "run", 25L))

fit_cases <- list(
  fit_case("rank_deficient_duplicate_column", d5, r5, l5, integer(0), 8L, f5),
  fit_case("run_constant_column_run_pooling", d6, r6, l5, integer(0), 8L, f6),
  fit_case("run_two_fully_censored", d7, r7, l7, c7, 8L, f7),
  fit_case("isolated_single_row_segments", d8, r8, l5, c8, 5L, f8),
  fit_case("runs_70_and_14_auto", d9, r9, l9, integer(0), 25L, f9),
  fit_case("single_run", d1, r1, 60L, integer(0), 12L, f1),
  fit_case("two_runs_censored", d2, r2, l2, c2, 10L, f2),
  fit_case("three_runs_no_intercept", d3, r3, l3, c3, 8L, f3),
  fit_case("rcond_rejected", d4, r4, 60L, integer(0), 57L, f4)
)

# --- noise_acvf(design =) cases ----------------------------------------------

acvf_one <- function(resid, design, lens, censor, max_lag, pooling, corr_lag) {
  runs <- rep(seq_along(lens), times = lens)
  a <- fmriAR::noise_acvf(
    resid, runs = if (length(lens) > 1L) runs else NULL,
    censor = if (length(censor)) censor else NULL, max_lag = max_lag,
    pooling = pooling, design = design, correction_max_lag = corr_lag
  )
  list(max_lag = max_lag, pooling = pooling, acvf = lapply(unname(a$acvf), as.numeric),
       pairs = lapply(unname(a$pairs), as.numeric),
       n_segments = as.integer(unname(a$n_segments)), corrected = isTRUE(a$corrected))
}
acvf_cases <- list(
  list(fit = "two_runs_censored", result = acvf_one(r2, d2, l2, c2, 4L, "global", 10L)),
  list(fit = "two_runs_censored", result = acvf_one(r2, d2, l2, c2, 4L, "run", 10L)),
  list(fit = "single_run", result = acvf_one(r1, d1, 60L, integer(0), 3L, "global", 12L)),
  list(fit = "three_runs_no_intercept", result = acvf_one(r3, d3, l3, c3, 3L, "run", 8L))
)

# --- fmrireg adaptive lag budget ---------------------------------------------

adaptive_case <- function(name, design, lens, censor, order, ceiling) {
  ends <- cumsum(lens)
  idx <- lapply(seq_along(lens), function(r) seq.int(ends[r] - lens[r] + 1L, ends[r]))
  budget <- fmrireg_reference$.ar_correction_lag_budget(
    design, target_order = order, run_indices = idx,
    censor = if (length(censor)) censor else NULL, max_lag = ceiling)
  list(name = name, design = design, lens = lens, censor = censor, order = order,
       ceiling = ceiling, budget = as.integer(budget))
}
adaptive_cases <- list(
  adaptive_case("two_runs_censored", d2, l2, c2, 2L, 25L),
  adaptive_case("three_runs_no_intercept", d3, l3, c3, 4L, 25L),
  adaptive_case("short_runs_order_floor", make_design(c(24L, 18L), "per_run", 4L),
                c(24L, 18L), c(3L, 30L), 6L, 25L),
  adaptive_case("ceiling_binds", d1, 60L, integer(0), 1L, 3L),
  adaptive_case("heavy_censoring_one_run", make_design(c(30L, 30L), "per_run", 2L),
                c(30L, 30L), c(2:5, 9:20, 31:36), 3L, 25L),
  adaptive_case("rank_deficient", d5, l5, integer(0), 2L, 25L)
)

# --- serialisation ------------------------------------------------------------

git_revision <- function(path) {
  value <- tryCatch(
    system2("git", c("-C", path, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(...) character(0)
  )
  if (length(value) == 0 || !nzchar(value[[1]])) "unresolved" else value[[1]]
}

emit_bias <- function(case) {
  paste0(
    "    BiasCase(\n",
    "      name = ", scala_string(case$name), ",\n",
    "      design = ", scala_rows(case$design), ",\n",
    "      runLengths = ", scala_ints(case$lens), ",\n",
    "      censorOneBased = ", scala_ints(case$censor), ",\n",
    "      requestedLag = ", as.integer(case$max_lag), ",\n",
    "      lag = ", as.integer(case$lag), ",\n",
    "      capped = ", tolower(as.character(case$capped)), ",\n",
    "      matrices = ", scala_list(lapply(case$A, scala_rows)), ",\n",
    "      reciprocalCondition = ", scala_doubles(case$rcond), "\n",
    "    )"
  )
}

emit_fit <- function(fit) {
  paste0(
    "FitResult(order = ", scala_string(fit$p), ", pMax = ", as.integer(fit$p_max),
    ", pooling = ", scala_string(fit$pooling),
    ", phi = ", scala_list(lapply(fit$phi, scala_doubles)),
    ", gamma = ", scala_list(lapply(fit$gamma, scala_doubles)),
    ", sigma2 = ", scala_doubles_na(fit$sigma2),
    ", rejectedRuns = ", scala_ints(fit$rejected_runs), ")"
  )
}

emit_fit_case <- function(case) {
  paste0(
    "    FitCase(\n",
    "      name = ", scala_string(case$name), ",\n",
    "      design = ", scala_rows(case$design), ",\n",
    "      residuals = ", scala_rows(case$resid), ",\n",
    "      runLengths = ", scala_ints(case$lens), ",\n",
    "      censorOneBased = ", scala_ints(case$censor), ",\n",
    "      correctionMaxLag = ", as.integer(case$corr_lag), ",\n",
    "      fits = ", scala_list(lapply(case$fits, emit_fit)), "\n",
    "    )"
  )
}

emit_adaptive <- function(case) {
  paste0(
    "    AdaptiveCase(name = ", scala_string(case$name),
    ", design = ", scala_rows(case$design),
    ", runLengths = ", scala_ints(case$lens),
    ", censorOneBased = ", scala_ints(case$censor),
    ", order = ", as.integer(case$order), ", ceiling = ", as.integer(case$ceiling),
    ", budget = ", case$budget, ")"
  )
}

emit_acvf <- function(entry) {
  r <- entry$result
  paste0(
    "    AcvfResult(fit = ", scala_string(entry$fit), ", maxLag = ", as.integer(r$max_lag),
    ", pooling = ", scala_string(r$pooling),
    ", acvf = ", scala_list(lapply(r$acvf, scala_doubles)),
    ", pairs = ", scala_list(lapply(r$pairs, scala_doubles)),
    ", segments = ", scala_ints(r$n_segments),
    ", corrected = ", tolower(as.character(r$corrected)), ")"
  )
}

# Each case is its own lazy val: one Vector literal for every case would exceed the JVM's
# 64 KB limit on a single static initialiser.
lazy_group <- function(name, type, items) {
  parts <- vapply(seq_along(items), function(i) {
    paste0("  private lazy val ", name, i - 1L, ": ", type, " =\n", sub("^    ", "    ", items[[i]]))
  }, character(1))
  refs <- paste0(name, seq_along(items) - 1L, collapse = ", ")
  c(parts, "", paste0("  val ", name, ": Vector[", type, "] = Vector(", refs, ")"))
}

writeLines(
  c(
    "package scalafim.fmri.ar.fixtures",
    "",
    "// Generated by modules/ar/tools/generate_fmriar_bias_parity.R.",
    "// Inputs (designs and OLS residuals) are deterministic and rounded to 13 significant digits before",
    "// fmriAR sees them, so the Scala suite feeds fmriAR's inputs exactly.",
    "object FmriArBiasRFixture:",
    "",
    "  final case class BiasCase(",
    "      name: String,",
    "      design: Vector[Vector[Double]],",
    "      runLengths: Vector[Int],",
    "      censorOneBased: Vector[Int],",
    "      requestedLag: Int,",
    "      lag: Int,",
    "      capped: Boolean,",
    "      matrices: Vector[Vector[Vector[Double]]],",
    "      reciprocalCondition: Vector[Double]",
    "  )",
    "",
    "  final case class FitResult(",
    "      order: String,",
    "      pMax: Int,",
    "      pooling: String,",
    "      phi: Vector[Vector[Double]],",
    "      gamma: Vector[Vector[Double]],",
    "      sigma2: Vector[Double],",
    "      rejectedRuns: Vector[Int]",
    "  )",
    "",
    "  final case class FitCase(",
    "      name: String,",
    "      design: Vector[Vector[Double]],",
    "      residuals: Vector[Vector[Double]],",
    "      runLengths: Vector[Int],",
    "      censorOneBased: Vector[Int],",
    "      correctionMaxLag: Int,",
    "      fits: Vector[FitResult]",
    "  )",
    "",
    "  final case class AdaptiveCase(",
    "      name: String,",
    "      design: Vector[Vector[Double]],",
    "      runLengths: Vector[Int],",
    "      censorOneBased: Vector[Int],",
    "      order: Int,",
    "      ceiling: Int,",
    "      budget: Int",
    "  )",
    "",
    "  final case class AcvfResult(",
    "      fit: String,",
    "      maxLag: Int,",
    "      pooling: String,",
    "      acvf: Vector[Vector[Double]],",
    "      pairs: Vector[Vector[Double]],",
    "      segments: Vector[Int],",
    "      corrected: Boolean",
    "  )",
    "",
    lazy_group("biasCases", "BiasCase", lapply(bias_cases, emit_bias)),
    "",
    lazy_group("fitCases", "FitCase", lapply(fit_cases, emit_fit_case)),
    "",
    lazy_group("adaptiveCases", "AdaptiveCase", lapply(adaptive_cases, emit_adaptive)),
    "",
    lazy_group("acvfCases", "AcvfResult", lapply(acvf_cases, emit_acvf)),
    "",
    "  // The rejected-budget case: R's uncorrected fit on the same residuals, and the unrejected design's A.",
    paste0("  val rejectedRawPhi: Vector[Double] = ", scala_doubles(raw4$phi[[1L]])),
    paste0("  val rejectedReciprocalCondition: Double = ", scala_number(rcond(A4))),
    "",
    paste0("  val fmriArRevision: String = \"", git_revision(normalizePath(fmriar_pkg, mustWork = FALSE)), "\""),
    paste0("  val fmriArVersion: String = \"", as.character(utils::packageVersion("fmriAR")), "\"")
  ),
  scala_out
)
message("wrote ", scala_out)

receipt_out <- Sys.getenv("FMRIAR_BIAS_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "ar.fmriar-bias.v1.r.json"))
source_info <- list(
  fmriAR_revision = git_revision(fmriar_pkg),
  fmriAR_version = as.character(utils::packageVersion("fmriAR")),
  fmrireg_revision = git_revision(fmrireg_pkg),
  fmrireg_version = as.character(read.dcf(file.path(fmrireg_pkg, "DESCRIPTION"), fields = "Version")[[1]]),
  r_version = as.character(getRversion()),
  producer = "modules/ar/tools/generate_fmriar_bias_parity.R",
  reference = "fmriAR design-corrected ACVF and AR estimation; fmrireg adaptive lag budget from the pinned adapter source"
)
inputs <- list(
  bias_cases = lapply(bias_cases, function(x) x[c("name", "design", "lens", "censor", "max_lag")]),
  fit_cases = lapply(fit_cases, function(x) c(x[c("name", "design", "resid", "lens", "censor", "corr_lag")],
    list(recipes = lapply(x$fits, function(f) f[c("p", "p_max", "pooling")])))),
  adaptive_cases = lapply(adaptive_cases, function(x) x[c("name", "design", "lens", "censor", "order", "ceiling")])
)
outputs <- list(
  bias_cases = lapply(bias_cases, function(x) x[c("name", "lag", "capped", "A", "rcond")]),
  fit_cases = lapply(fit_cases, function(x) list(name = x$name, fits = x$fits)),
  adaptive_budgets = lapply(adaptive_cases, function(x) list(name = x$name, budget = x$budget)),
  acvf = acvf_cases,
  rejected_raw_phi = raw4$phi[[1L]], rejected_reciprocal_condition = rcond(A4)
)
payload <- list(
  schema_version = "scalafim-r-fmriar-bias-fixture/v1",
  inputs = canonicalize_receipt_numbers(inputs), outputs = canonicalize_receipt_numbers(outputs),
  source = source_info,
  receipt = list(source = source_info, conventions = list(reference_serialization = receipt_serialization_convention())))
dir.create(dirname(receipt_out), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, receipt_out, auto_unbox = TRUE, digits = RECEIPT_SIGNIFICANT_DIGITS,
  pretty = TRUE, na = "null")
message("wrote ", receipt_out)
