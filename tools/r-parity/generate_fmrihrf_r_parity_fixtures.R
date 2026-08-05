#!/usr/bin/env Rscript

# Regenerate the Scala golden fixtures used by the hrf module's R-parity suites.
#
# From the scalafim repository root:
#   Rscript tools/r-parity/generate_fmrihrf_r_parity_fixtures.R
#
# By default this loads the live R source at ~/code/fmrihrf with pkgload and
# writes modules/hrf/.../fixtures/HrfRParityFixtures.scala. Override with:
#   FMRIHRF_R=/path/to/fmrihrf HRF_RPARITY_OUT=/path/to/HrfRParityFixtures.scala \
#     Rscript tools/r-parity/generate_fmrihrf_r_parity_fixtures.R
#
# The corpus records R's output as REFERENCE DATA, not as an unconditional
# contract. scalafim deliberately diverges from R where R is wrong; see
# docs/plans/hrf-hardening.md section 6. The consuming suites assert exact
# agreement where the behavior is agreed, and assert the corrected value with
# R's number in the failure message where it is not.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to generate parity fixtures", call. = FALSE)
}

r_pkg <- Sys.getenv("FMRIHRF_R", file.path(path.expand("~"), "code", "fmrihrf"))
out_file <- Sys.getenv(
  "HRF_RPARITY_OUT",
  file.path(
    "modules", "hrf", "shared", "src", "test", "scala",
    "scalafim", "fmri", "hrf", "fixtures", "HrfRParityFixtures.scala"
  )
)

pkgload::load_all(r_pkg, quiet = TRUE)

`%||%` <- function(x, y) if (is.null(x)) y else x

scala_string <- function(x) {
  paste0("\"", gsub("\\\\", "\\\\\\\\", gsub("\"", "\\\\\"", x)), "\"")
}

scala_number <- function(x) {
  if (is.nan(x)) {
    "Double.NaN"
  } else if (is.infinite(x) && x > 0) {
    "Double.PositiveInfinity"
  } else if (is.infinite(x) && x < 0) {
    "Double.NegativeInfinity"
  } else {
    sprintf("%.12g", x)
  }
}

scala_vector_double <- function(xs) {
  if (length(xs) == 0) {
    "Vector.empty"
  } else {
    paste0("Vector(", paste(vapply(as.numeric(xs), scala_number, character(1)), collapse = ", "), ")")
  }
}

# --- the shared evaluation grid -------------------------------------------
# Deliberately starts below zero so that causality is part of the corpus.
kernel_times <- seq(-2, 32, by = 0.5)

as_value_matrix <- function(v, n_times) {
  m <- as.matrix(v)
  if (nrow(m) != n_times) m <- matrix(as.vector(v), nrow = n_times)
  m
}

kernel_fixture <- function(name, fn) {
  m <- as_value_matrix(fn(kernel_times), length(kernel_times))
  paste0(
    "    KernelFixture(name = ", scala_string(name),
    ", nbasis = ", ncol(m),
    ", times = ", scala_vector_double(kernel_times),
    ", values = ", scala_vector_double(as.vector(t(m))),
    ")"
  )
}

# Kernels are configured to match the scalafim `Hrfs.*` defaults exactly.
kernels <- list(
  spmg1       = function(t) evaluate(HRF_SPMG1, t),
  spmg2       = function(t) evaluate(HRF_SPMG2, t),
  spmg3       = function(t) evaluate(HRF_SPMG3, t),
  gamma       = function(t) hrf_gamma(t, shape = 6, rate = 1),
  gaussian    = function(t) hrf_gaussian(t, mean = 6, sd = 2),
  mexhat      = function(t) hrf_mexhat(t, mean = 6, sd = 2),
  inv_logit   = function(t) hrf_inv_logit(t, mu1 = 6, s1 = 1, mu2 = 16, s2 = 1, lag = 0),
  lwu         = function(t) hrf_lwu(t, tau = 6, sigma = 2.5, rho = 0.35, normalize = "none"),
  half_cosine = function(t) hrf_half_cosine(t, h1 = 1, h2 = 5, h3 = 7, h4 = 7, f1 = 0, f2 = 0),
  fourier     = function(t) hrf_fourier(t, span = 24, nbasis = 5),
  sine        = function(t) hrf_sine(t, span = 24, N = 5),
  bspline     = function(t) hrf_bspline(t, span = 24, N = 5, degree = 3),
  tent        = function(t) evaluate(hrf_tent_generator(nbasis = 5, span = 24), t),
  fir         = function(t) evaluate(hrf_fir_generator(nbasis = 12, span = 24), t),
  daguerre    = function(t) evaluate(hrf_daguerre_generator(nbasis = 3, scale = 4), t)
)

kernel_exprs <- vapply(
  names(kernels),
  function(nm) {
    res <- tryCatch(kernel_fixture(nm, kernels[[nm]]), error = function(e) {
      message(sprintf("  !! kernel '%s' failed in R: %s", nm, conditionMessage(e)))
      NA_character_
    })
    res
  },
  character(1)
)
kernel_exprs <- kernel_exprs[!is.na(kernel_exprs)]

# --- HRF-level duration evaluation (R's convergent trapezoid quadrature) ---
duration_grid <- seq(0, 40, by = 1)

duration_cases <- list(
  list(name = "spmg1_dur0_p0.5",   hrf = HRF_SPMG1, duration = 0, precision = 0.5,  summate = TRUE),
  list(name = "spmg1_dur1_p0.5",   hrf = HRF_SPMG1, duration = 1, precision = 0.5,  summate = TRUE),
  list(name = "spmg1_dur4_p1.0",   hrf = HRF_SPMG1, duration = 4, precision = 1.0,  summate = TRUE),
  list(name = "spmg1_dur4_p0.5",   hrf = HRF_SPMG1, duration = 4, precision = 0.5,  summate = TRUE),
  list(name = "spmg1_dur4_p0.1",   hrf = HRF_SPMG1, duration = 4, precision = 0.1,  summate = TRUE),
  list(name = "spmg1_dur4_p0.05",  hrf = HRF_SPMG1, duration = 4, precision = 0.05, summate = TRUE),
  list(name = "spmg1_dur4_p0.1_mass", hrf = HRF_SPMG1, duration = 4, precision = 0.1, summate = FALSE),
  list(name = "spmg3_dur4_p0.1",   hrf = HRF_SPMG3, duration = 4, precision = 0.1,  summate = TRUE),
  list(name = "gamma_dur6_p0.1",   hrf = HRF_GAMMA, duration = 6, precision = 0.1,  summate = TRUE)
)

duration_expr <- function(case) {
  m <- as_value_matrix(
    evaluate(case$hrf, duration_grid, duration = case$duration,
             precision = case$precision, summate = case$summate),
    length(duration_grid)
  )
  paste0(
    "    DurationFixture(name = ", scala_string(case$name),
    ", duration = ", scala_number(case$duration),
    ", precision = ", scala_number(case$precision),
    ", summate = ", if (case$summate) "true" else "false",
    ", nbasis = ", ncol(m),
    ", grid = ", scala_vector_double(duration_grid),
    ", values = ", scala_vector_double(as.vector(t(m))),
    ")"
  )
}

# --- regressor-level evaluation -------------------------------------------
regressor_grid <- seq(0, 118, by = 2)

regressor_cases <- list(
  list(name = "spmg1_aligned_impulse",
       onsets = c(10, 30, 60), duration = 0, amplitude = 1,
       hrf = HRF_SPMG1, precision = 0.33),
  list(name = "spmg1_unaligned_impulse",
       onsets = c(10, 23.5, 44.2), duration = 0, amplitude = 1,
       hrf = HRF_SPMG1, precision = 0.33),
  list(name = "spmg1_amplitude_modulated",
       onsets = c(10, 30, 60), duration = 0, amplitude = c(1, -0.5, 2.25),
       hrf = HRF_SPMG1, precision = 0.33),
  list(name = "spmg3_impulse",
       onsets = c(12, 40), duration = 0, amplitude = 1,
       hrf = HRF_SPMG3, precision = 0.33),
  list(name = "bspline_impulse",
       onsets = c(12, 40), duration = 0, amplitude = 1,
       hrf = HRF_BSPLINE, precision = 0.33)
)

regressor_expr <- function(case) {
  n <- length(case$onsets)
  durs <- rep(case$duration, length.out = n)
  amps <- rep(case$amplitude, length.out = n)
  r <- regressor(onsets = case$onsets, hrf = case$hrf, duration = durs, amplitude = amps)
  m <- as_value_matrix(
    evaluate(r, regressor_grid, precision = case$precision),
    length(regressor_grid)
  )
  paste0(
    "    RegressorFixture(name = ", scala_string(case$name),
    ", onsets = ", scala_vector_double(case$onsets),
    ", durations = ", scala_vector_double(durs),
    ", amplitudes = ", scala_vector_double(amps),
    ", precision = ", scala_number(case$precision),
    ", nbasis = ", ncol(m),
    ", grid = ", scala_vector_double(regressor_grid),
    ", values = ", scala_vector_double(as.vector(t(m))),
    ")"
  )
}

safe_map <- function(cases, f, label) {
  out <- vapply(cases, function(c) {
    tryCatch(f(c), error = function(e) {
      message(sprintf("  !! %s '%s' failed in R: %s", label, c$name, conditionMessage(e)))
      NA_character_
    })
  }, character(1))
  out[!is.na(out)]
}

duration_exprs <- safe_map(duration_cases, duration_expr, "duration case")
regressor_exprs <- safe_map(regressor_cases, regressor_expr, "regressor case")

r_version <- as.character(utils::packageVersion("fmrihrf"))

lines <- c(
  "package scalafim.fmri.hrf.fixtures",
  "",
  "// Generated by tools/r-parity/generate_fmrihrf_r_parity_fixtures.R",
  sprintf("// from the live R fmrihrf source (version %s). Do not edit by hand.", r_version),
  "//",
  "// This is REFERENCE DATA, not an unconditional contract. scalafim deliberately",
  "// diverges from R where R is wrong; see docs/plans/hrf-hardening.md section 6.",
  "// Consuming suites assert exact agreement where the behavior is agreed, and",
  "// assert the corrected value with R's number in the failure message where it",
  "// is not.",
  "object HrfRParityFixtures:",
  "",
  "  /** `values` is row-major: times.length rows by `nbasis` columns. */",
  "  final case class KernelFixture(",
  "      name: String,",
  "      nbasis: Int,",
  "      times: Vector[Double],",
  "      values: Vector[Double]",
  "  ):",
  "    def at(timeIndex: Int, basis: Int): Double = values(timeIndex * nbasis + basis)",
  "",
  "  /** R `evaluate.HRF(hrf, grid, duration, precision, summate)`. */",
  "  final case class DurationFixture(",
  "      name: String,",
  "      duration: Double,",
  "      precision: Double,",
  "      summate: Boolean,",
  "      nbasis: Int,",
  "      grid: Vector[Double],",
  "      values: Vector[Double]",
  "  )",
  "",
  "  /** R `evaluate.Reg(regressor(...), grid, precision)`. */",
  "  final case class RegressorFixture(",
  "      name: String,",
  "      onsets: Vector[Double],",
  "      durations: Vector[Double],",
  "      amplitudes: Vector[Double],",
  "      precision: Double,",
  "      nbasis: Int,",
  "      grid: Vector[Double],",
  "      values: Vector[Double]",
  "  )",
  "",
  "  val kernels: Vector[KernelFixture] = Vector(",
  paste(kernel_exprs, collapse = ",\n"),
  "  )",
  "",
  "  val durations: Vector[DurationFixture] = Vector(",
  paste(duration_exprs, collapse = ",\n"),
  "  )",
  "",
  "  val regressors: Vector[RegressorFixture] = Vector(",
  paste(regressor_exprs, collapse = ",\n"),
  "  )",
  "",
  "  def kernel(name: String): KernelFixture =",
  "    kernels.find(_.name == name).getOrElse(sys.error(s\"no R kernel fixture named '$name'\"))",
  "",
  "  def duration(name: String): DurationFixture =",
  "    durations.find(_.name == name).getOrElse(sys.error(s\"no R duration fixture named '$name'\"))",
  "",
  "  def regressor(name: String): RegressorFixture =",
  "    regressors.find(_.name == name).getOrElse(sys.error(s\"no R regressor fixture named '$name'\"))"
)

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
writeLines(lines, out_file)
message(sprintf(
  "wrote %s (%d kernels, %d duration cases, %d regressor cases)",
  out_file, length(kernel_exprs), length(duration_exprs), length(regressor_exprs)
))
