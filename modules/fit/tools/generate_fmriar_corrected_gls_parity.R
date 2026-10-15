#!/usr/bin/env Rscript
source(file.path("tools", "r-parity", "receipt_serialization.R"))
fmriar_root <- Sys.getenv("FMRIAR_AR_R")
fmrireg_root <- Sys.getenv("FMRIREG_AR_R")
stopifnot(nzchar(fmriar_root), nzchar(fmrireg_root))
reference <- new.env(parent = globalenv())
source(file.path(fmrireg_root, "R", "fmriAR_adapter.R"), local = reference)
round13 <- function(x) signif(x, RECEIPT_SIGNIFICANT_DIGITS)
lens <- c(48L, 72L)
n <- sum(lens)
runs <- rep(seq_along(lens), lens)
indices <- split(seq_len(n), runs)
censor <- c(5L, 6L, 24L, 51L, 80L, 119L)
row <- seq_len(n)
x <- round13(cbind(sin(row * 0.173) + cos(row * 0.071) * 0.3,
                   as.numeric(runs == 1L), as.numeric(runs == 2L)))
beta <- matrix(c(1.2, -0.7, 0.3, 1.8, 2.0, -1.0, 0.6, 0.2, -0.4, 1.5, 0.2, -1.2), 3L, byrow = TRUE)
noise <- matrix(0, n, 4L)
for (v in 1:4) {
  innovation <- sin(row * 12.9898 + v * 78.233) * 43758.5453
  innovation <- (innovation - floor(innovation)) * 2 - 1
  for (idx in indices) {
    for (j in seq_along(idx)) {
      i <- idx[j]
      noise[i, v] <- innovation[i] +
        (if (j > 1L) (0.25 + 0.08 * v) * noise[idx[j - 1L], v] else 0) -
        (if (j > 2L) 0.12 * noise[idx[j - 2L], v] else 0)
    }
  }
}
y <- round13(x %*% beta + noise)
residuals <- y - x %*% qr.solve(x, y)
fit_case <- function(order, pooling, voxelwise) {
  opts <- list(struct = paste0("ar", order), global = pooling == "global",
               exact_first = TRUE, shared_estimator = "pooled_acvf")
  columns <- if (voxelwise) lapply(1:ncol(y), function(v) v) else list(seq_len(ncol(y)))
  fits <- lapply(columns, function(cols) {
    r <- residuals[, cols, drop = FALSE]
    design <- reference$.shared_ar_correction_design(r, opts, x)
    plan <- reference$.estimate_ar_via_fmriAR(r, opts, indices, censor, design)
    raw <- reference$.estimate_ar_via_fmriAR(r, opts, indices, censor, NULL)
    white <- fmriAR::whiten_apply(plan, x, y[, cols, drop = FALSE],
                                 runs = runs, censor = censor, parallel = FALSE)
    coefficients <- qr.solve(white$X, white$Y)
    variance <- colSums((white$Y - white$X %*% coefficients)^2) / (n - ncol(x))
    covariance <- chol2inv(qr.R(qr(white$X)))
    list(phi = unname(plan$phi), raw_phi = unname(raw$phi), coefficients = coefficients,
         variance = variance, covariance = covariance,
         standard_errors = sqrt(outer(diag(covariance), variance)))
  })
  list(order = order, pooling = pooling, voxelwise = voxelwise,
       budget = reference$.ar_correction_lag_budget(x, order, indices, censor),
       phi = lapply(fits, function(f) f$phi), raw_phi = lapply(fits, function(f) f$raw_phi),
       coefficients = do.call(cbind, lapply(fits, function(f) f$coefficients)),
       variance = unlist(lapply(fits, function(f) f$variance)),
       covariance = lapply(fits, function(f) f$covariance),
       standard_errors = do.call(cbind, lapply(fits, function(f) f$standard_errors)),
       df = n - ncol(x))
}
cases <- list()
for (p in 1:2) {
  for (pooling in c("global", "run")) cases[[length(cases) + 1L]] <- fit_case(p, pooling, FALSE)
  cases[[length(cases) + 1L]] <- fit_case(p, "run", TRUE)
}
runwise_cases <- lapply(1:2, function(order) {
  per_run <- lapply(seq_along(indices), function(run) {
    idx <- indices[[run]]
    dx <- x[idx, c(1L, run + 1L), drop = FALSE]
    ry <- y[idx, , drop = FALSE]
    local_censor <- which(idx %in% censor)
    resid <- ry - dx %*% qr.solve(dx, ry)
    opts <- list(struct = paste0("ar", order), exact_first = TRUE)
    plan <- reference$.estimate_ar_via_fmriAR(resid, opts, list(seq_along(idx)), local_censor, dx)
    white <- fmriAR::whiten_apply(plan, dx, ry, censor = local_censor, parallel = FALSE)
    coefficients <- qr.solve(white$X, white$Y)
    variance <- colSums((white$Y - white$X %*% coefficients)^2) / (length(idx) - ncol(dx))
    covariance <- chol2inv(qr.R(qr(white$X)))
    list(phi = plan$phi[[1L]], coefficients = coefficients, variance = variance, covariance = covariance,
         df = length(idx) - ncol(dx))
  })
  list(order = order, runs = per_run)
})

number <- function(value) {
  if (abs(value) < 5e-16) return("0.0")
  text <- receipt_format_number(value)
  if (grepl("[.eE]", text)) text else paste0(text, ".0")
}
vec <- function(values) paste0("Vector(", paste(vapply(as.numeric(values), number, character(1)), collapse = ", "), ")")
ints <- function(values) paste0("Vector(", paste(as.integer(values), collapse = ", "), ")")
matrix_vec <- function(value) paste0("Vector(", paste(vapply(seq_len(nrow(value)), function(i) vec(value[i, ]), character(1)), collapse = ", "), ")")
phis <- function(value) paste0("Vector(", paste(vapply(value, function(plans) paste0("Vector(", paste(vapply(plans, vec, character(1)), collapse = ", "), ")"), character(1)), collapse = ", "), ")")
scala_path <- "modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/FmriArCorrectedGlsFixture.scala"
lines <- c("package scalafim.fmri.fit.fixtures", "", "// Generated from locked fmriAR and fmrireg sources by generate_fmriar_corrected_gls_parity.R.",
  "object FmriArCorrectedGlsFixture:",
  "  final case class RunFit(phi: Vector[Double], coefficients: Vector[Vector[Double]],",
  "      variance: Vector[Double], covariance: Vector[Vector[Double]], df: Int)",
  "  final case class RunwiseCase(order: Int, runs: Vector[RunFit])",
  "  final case class FitCase(order: Int, global: Boolean, voxelwise: Boolean, budget: Int,",
  "      phi: Vector[Vector[Vector[Double]]], rawPhi: Vector[Vector[Vector[Double]]],",
  "      coefficients: Vector[Vector[Double]], variance: Vector[Double],",
  "      covariance: Vector[Vector[Vector[Double]]], standardErrors: Vector[Vector[Double]], df: Int)",
  paste0("  val runLengths: Vector[Int] = ", ints(lens)),
  paste0("  val censoredTimepoints: Vector[Int] = ", ints(censor - 1L)),
  paste0("  lazy val design: Vector[Vector[Double]] = ", matrix_vec(x)),
  paste0("  lazy val response: Vector[Vector[Double]] = ", matrix_vec(y)))
for (i in seq_along(cases)) {
  c <- cases[[i]]
  lines <- c(lines, paste0("  private lazy val case", i, ": FitCase = FitCase(",
    c$order, ", ", tolower(as.character(c$pooling == "global")), ", ", tolower(as.character(c$voxelwise)), ", ", c$budget, ",\n",
    "    ", phis(c$phi), ", ", phis(c$raw_phi), ",\n",
    "    ", matrix_vec(c$coefficients), ", ", vec(c$variance), ",\n",
    "    Vector(", paste(vapply(c$covariance, matrix_vec, character(1)), collapse = ", "), "),\n",
    "    ", matrix_vec(c$standard_errors), ", ", c$df, ")"))
}
lines <- c(lines, paste0("  val cases: Vector[FitCase] = Vector(", paste0("case", seq_along(cases), collapse = ", "), ")"))
for (i in seq_along(runwise_cases)) {
  c <- runwise_cases[[i]]
  run_text <- vapply(c$runs, function(r) paste0("RunFit(", vec(r$phi), ", ", matrix_vec(r$coefficients),
    ", ", vec(r$variance), ", ", matrix_vec(r$covariance), ", ", r$df, ")"), character(1))
  lines <- c(lines, paste0("  private lazy val runwise", i, ": RunwiseCase = RunwiseCase(", c$order,
    ", Vector(", paste(run_text, collapse = ", "), "))"))
}
lines <- c(lines, "  val runwiseCases: Vector[RunwiseCase] = Vector(runwise1, runwise2)")
writeLines(lines, scala_path)
revision <- function(path) system2("git", c("-C", path, "rev-parse", "HEAD"), stdout = TRUE)[[1L]]
source_info <- list(fmriAR_revision = revision(fmriar_root), fmriAR_version = as.character(packageVersion("fmriAR")),
  fmrireg_revision = revision(fmrireg_root), fmrireg_version = read.dcf(file.path(fmrireg_root, "DESCRIPTION"), fields = "Version")[[1L]],
  r_version = as.character(getRversion()), producer = "modules/fit/tools/generate_fmriar_corrected_gls_parity.R",
  reference = "Pinned fmrireg shared-design selection, adaptive budget and fmriAR adapter, followed by fmriAR whitening and direct QR GLS")
payload <- list(schema_version = "scalafim-r-fmriar-corrected-gls-fixture/v1",
  inputs = list(design = x, response = y, run_lengths = lens, censor_one_based = censor, exact_first = "ar1",
    shared_recipes = lapply(cases, function(c) c[c("order", "pooling", "voxelwise")]), runwise_orders = 1:2),
  outputs = canonicalize_receipt_numbers(list(shared_cases = cases, runwise_cases = runwise_cases)), source = source_info,
  receipt = list(source = source_info, conventions = list(dtype = "float64", residual_df = "whitened rows minus design rank",
    pooling = "pooled within-run ACVF, observation-weighted run coefficients; voxelwise cases fit each column separately",
    reference_serialization = receipt_serialization_convention())))
jsonlite::write_json(payload, "docs/scenarios/fixtures/fit.fmriar-corrected-gls.v1.r.json", auto_unbox = TRUE,
  digits = RECEIPT_SIGNIFICANT_DIGITS, pretty = TRUE)
message("wrote corrected GLS JSON and Scala fixtures")
