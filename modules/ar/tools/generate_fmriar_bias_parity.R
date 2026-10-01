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
fmriar_pkg <- Sys.getenv("FMRIAR_R", file.path(path.expand("~"), "code", "fmriAR"))

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
            make_design(16L, "global", 8L), 16L, integer(0), 8L)
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
    warnings = got$warnings
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

fit_cases <- list(
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
    ", sigma2 = ", scala_doubles(fit$sigma2),
    ", rejected = ", tolower(as.character(any(grepl("ill-conditioned", fit$warnings)))), ")"
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
    "      rejected: Boolean",
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
    paste0("  val biasCases: Vector[BiasCase] = Vector(\n", paste(vapply(bias_cases, emit_bias, character(1)), collapse = ",\n"), "\n  )"),
    "",
    paste0("  val fitCases: Vector[FitCase] = Vector(\n", paste(vapply(fit_cases, emit_fit_case, character(1)), collapse = ",\n"), "\n  )"),
    "",
    paste0("  val acvfCases: Vector[AcvfResult] = Vector(\n", paste(vapply(acvf_cases, emit_acvf, character(1)), collapse = ",\n"), "\n  )"),
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
