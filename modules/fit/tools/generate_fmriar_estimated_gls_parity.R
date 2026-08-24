#!/usr/bin/env Rscript

# Regenerate the locked fmriAR estimated-GLS receipt and its portable Scala
# fixture. Normal Scala tests consume only the generated Scala source.

if (!requireNamespace("fmriAR", quietly = TRUE)) {
  stop("The fmriAR R package must be installed to regenerate parity fixtures.", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to regenerate parity fixtures.", call. = FALSE)
}

out_file <- Sys.getenv(
  "FMRIAR_ESTIMATED_GLS_RECEIPT_OUT",
  file.path("docs", "scenarios", "fixtures", "fit.fmriar-estimated-gls.v1.r.json")
)
scala_out <- Sys.getenv(
  "FMRIAR_ESTIMATED_GLS_SCALA_OUT",
  file.path(
    "modules", "fit", "shared", "src", "test", "scala", "scalafim",
    "fmri", "fit", "fixtures", "FmriArEstimatedGlsFixture.scala"
  )
)
fmriar_pkg <- Sys.getenv("FMRIAR_R", file.path(path.expand("~"), "code", "fmriAR"))

n <- 24L
row <- seq_len(n)
runs <- rep(c("run-b", "run-a"), each = n / 2L)
censor <- c(4L, 9L, 14L, 20L)
x <- cbind(
  task = ((row - 1L) %% 6L - 2.5) / 2 + ifelse(row > n / 2L, 0.3, 0),
  intercept = 1
)
beta <- matrix(c(1.25, -0.8, -0.45, 0.7), nrow = 2L, byrow = TRUE)
series <- c(
  0.4801696483973501, 0.39288356195620511, 0.12558448074114043,
  -0.38612706117446371, 0.43452753754339596, 0.74467469542360953,
  0.41708286304387504, 0.27627162953246176, 0.19550347314594618,
  -0.85268289486611282, -0.28429628811595231, 0.065872502651131981,
  -0.16112459073465857, 0.60950890905917421, 0.016836331954432016,
  -0.14592877149866851, -0.60464975504093599, 0.64206680041053854,
  0.77310149031943698, 1.1563170236945068, 1.3398933301660652,
  -0.17614675302192817, 0.15924145658477434, 0.95462884101509238
)
errors <- cbind(
  series,
  rev(series) * 0.7 + ifelse(row %% 2L == 0L, 0.1, -0.1)
)
y <- x %*% beta + errors

initial_beta <- qr.solve(x, y)
initial_residuals <- y - x %*% initial_beta
plan <- fmriAR::fit_noise(
  resid = initial_residuals,
  runs = runs,
  censor = censor,
  method = "ar",
  p = 2L,
  p_max = 2L,
  pooling = "global",
  exact_first = "none"
)
whitened <- fmriAR::whiten_apply(
  plan,
  x,
  y,
  runs = runs,
  censor = censor,
  parallel = FALSE
)
final_beta <- qr.solve(whitened$X, whitened$Y)
final_residuals <- whitened$Y - whitened$X %*% final_beta
residual_variance <- colSums(final_residuals^2) / (n - ncol(x))
normalized_covariance <- chol2inv(qr.R(qr(whitened$X)))

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

source <- list(
  fmriAR_revision = git_revision(normalizePath(fmriar_pkg, mustWork = FALSE)),
  fmriAR_version = package_version_at(normalizePath(fmriar_pkg, mustWork = FALSE)),
  r_version = as.character(getRversion()),
  producer = "modules/fit/tools/generate_fmriar_estimated_gls_parity.R",
  reference = "fmriAR global AR(2) estimation, censor-aware whitening, and direct GLS inference"
)
schema <- "scalafim-r-fmriar-estimated-gls-fixture/v1"
inputs <- list(
  design = matrix_rows(x),
  response = matrix_rows(y),
  run_labels = runs,
  censor_one_based = censor,
  ar_order = 2L,
  pooling = "global",
  exact_first = "none"
)
outputs <- list(
  estimated_phi = plan$phi[[1L]],
  coefficients = matrix_rows(final_beta),
  residual_variance = residual_variance,
  normalized_covariance = matrix_rows(normalized_covariance),
  residual_degrees_of_freedom = n - ncol(x)
)
receipt <- list(
  schema_version = schema,
  producer_command = "bash tools/r-parity/regenerate_receipts.sh --r-only",
  conventions = list(
    dtype = "float64",
    censoring = "fmriAR receives one-based censor rows; Scala stores zero-based source timepoints",
    residual_df = "selected row count minus the final whitened design rank"
  ),
  accepted_differences = character(0),
  hashes = list(inputs_sha256 = "", outputs_sha256 = ""),
  source = source
)
payload <- list(
  schema_version = schema,
  scenario_id = "fit.fmriar-estimated-gls.v1",
  inputs = inputs,
  outputs = outputs,
  source = source,
  receipt = receipt
)

scala_number <- function(value) {
  if (abs(value) < 5e-16) return("0.0")
  rendered <- sprintf("%.17g", value)
  if (grepl("[.eE]", rendered)) rendered else paste0(rendered, ".0")
}
scala_doubles <- function(values) {
  paste0("Vector(", paste(vapply(as.numeric(values), scala_number, character(1)), collapse = ", "), ")")
}
scala_ints <- function(values) {
  paste0("Vector(", paste(as.integer(values), collapse = ", "), ")")
}
scala_matrix <- function(value, row_expr, col_expr) {
  rows <- paste(
    vapply(seq_len(nrow(value)), function(index) scala_doubles(value[index, ]), character(1)),
    collapse = ", "
  )
  c(
    paste0("    Matrix.tabulate(", row_expr, ", ", col_expr, ") { (row, column) =>"),
    paste0("      Vector(", rows, ")(row)(column)"),
    "    }"
  )
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(payload, out_file, auto_unbox = TRUE, digits = 17, pretty = TRUE)
message("wrote ", out_file)

scala_lines <- c(
  "package scalafim.fmri.fit.fixtures",
  "",
  "import gale.linalg.{DMat, Matrix}",
  "",
  "// Generated by modules/fit/tools/generate_fmriar_estimated_gls_parity.R.",
  "object FmriArEstimatedGlsFixture:",
  paste0("  val rows: Int = ", n),
  paste0("  val runLengths: Vector[Int] = ", scala_ints(rle(runs)$lengths)),
  paste0("  val censoredTimepoints: Vector[Int] = ", scala_ints(censor - 1L)),
  "",
  "  val design: DMat ="
)
scala_lines <- c(scala_lines, scala_matrix(x, "rows", ncol(x)))
scala_lines <- c(scala_lines, "", "  val response: DMat =")
scala_lines <- c(scala_lines, scala_matrix(y, "rows", ncol(y)))
scala_lines <- c(
  scala_lines,
  "",
  paste0("  val estimatedPhi: Vector[Double] = ", scala_doubles(outputs$estimated_phi)),
  "",
  "  val coefficients: DMat ="
)
scala_lines <- c(scala_lines, scala_matrix(final_beta, nrow(final_beta), ncol(final_beta)))
scala_lines <- c(
  scala_lines,
  "",
  paste0("  val residualVariance: Vector[Double] = ", scala_doubles(residual_variance)),
  "",
  "  val normalizedCovariance: DMat ="
)
scala_lines <- c(scala_lines, scala_matrix(normalized_covariance, nrow(normalized_covariance), ncol(normalized_covariance)))
scala_lines <- c(
  scala_lines,
  "",
  paste0("  val residualDegreesOfFreedom: Int = ", n - ncol(x)),
  paste0("  val fmriArRevision: String = \"", source$fmriAR_revision, "\""),
  paste0("  val fmriArVersion: String = \"", source$fmriAR_version, "\"")
)

dir.create(dirname(scala_out), recursive = TRUE, showWarnings = FALSE)
writeLines(scala_lines, scala_out)
message("wrote ", scala_out)
