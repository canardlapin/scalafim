#!/usr/bin/env Rscript

# Generate fitted-projection reference fixtures for scalafim multivar tests.
#
# The compatibility formulas mirror these multivarious sources at commit
# d44b7d0104a8647aefb61c3217c069c247a27b3d:
#   R/projector.R       project.projector, partial_project.projector
#   R/bi_projector.R    project_vars.bi_projector
#   R/twoway_projector.R partial_project.cross_projector, transfer.cross_projector
#
# Geometry-general formulas use only base R solve/crossprod and are independent
# of the Scala implementation. This script prints the complete Scala object to
# stdout. Regenerate with:
#   Rscript tools/r-parity/generate_multivar_projection_fixtures.R

fmt_num <- function(x) {
  if (is.nan(x)) return("Double.NaN")
  if (is.infinite(x)) {
    return(if (x > 0) "Double.PositiveInfinity" else "Double.NegativeInfinity")
  }
  if (x == 0) return("0.0")
  formatC(x, digits = 17, format = "fg", flag = "#")
}

fmt_vec <- function(x) {
  paste0("Vector(", paste(vapply(x, fmt_num, character(1)), collapse = ", "), ")")
}

fmt_int_vec <- function(x) {
  paste0("Vector(", paste(x, collapse = ", "), ")")
}

fmt_matrix <- function(x, indent = 6) {
  x <- as.matrix(x)
  pad <- paste(rep(" ", indent), collapse = "")
  row_pad <- paste(rep(" ", indent + 2), collapse = "")
  rows <- apply(x, 1, function(row) paste0(row_pad, fmt_vec(row)))
  paste0(
    "GaleNumerics.matrixFromRows(\n",
    pad, "Vector(\n",
    paste(rows, collapse = ",\n"),
    "\n", pad, ")\n",
    paste(rep(" ", indent - 2), collapse = ""), ")"
  )
}

center_scale <- function(x, center, scale) {
  sweep(sweep(as.matrix(x), 2, center, "-"), 2, scale, "/")
}

inverse_center_scale <- function(x, center, scale) {
  sweep(sweep(as.matrix(x), 2, scale, "*"), 2, center, "+")
}

ridge_recovery <- function(x_working, weights, metric, ridge) {
  contribution <- x_working %*% weights
  gram <- crossprod(weights, metric %*% weights) + diag(ridge, ncol(weights))
  contribution %*% solve(gram)
}

right_pseudoinverse <- function(weights) {
  solve(crossprod(weights), t(weights))
}

center <- c(1.0, -0.5, 2.0, 0.25)
scale <- c(2.0, 0.5, 1.5, 2.5)
weights <- matrix(
  c(
    0.8, -0.2,
    0.3, 0.7,
    -0.4, 0.5,
    0.6, 0.1
  ),
  ncol = 2,
  byrow = TRUE
)

new_raw <- matrix(
  c(
    1.4, -0.25, 2.3, 1.0,
    0.2, -0.8, 1.1, -0.5,
    2.6, 0.1, 3.5, 0.75
  ),
  ncol = 4,
  byrow = TRUE
)
new_working <- center_scale(new_raw, center, scale)
full_scores <- new_working %*% weights

subset_zero_based <- c(0L, 2L)
subset_r <- subset_zero_based + 1L
partial_raw <- new_raw[, subset_r, drop = FALSE]
partial_working <- center_scale(partial_raw, center[subset_r], scale[subset_r])
partial_weights <- weights[subset_r, , drop = FALSE]
partial_contribution <- partial_working %*% partial_weights
ridge <- 0.15
partial_least_squares <- ridge_recovery(
  partial_working,
  partial_weights,
  diag(length(subset_r)),
  ridge
)
partial_metric <- matrix(c(1.4, 0.25, 0.25, 0.9), ncol = 2, byrow = TRUE)
partial_metric_least_squares <- ridge_recovery(
  partial_working,
  partial_weights,
  partial_metric,
  ridge
)

training_working <- matrix(
  c(
    -1.0, 0.5, 0.2, 1.2,
    0.4, -0.8, 1.1, -0.3,
    0.7, 0.6, -0.5, 0.2,
    -0.2, -0.1, 0.9, -1.0,
    0.1, -0.2, -1.7, -0.1
  ),
  ncol = 4,
  byrow = TRUE
)
training_scores <- training_working %*% weights
singular_values <- sqrt(diag(crossprod(training_scores)))
supplementary_raw <- matrix(
  c(
    2.0, 1.0,
    1.5, -0.5,
    -1.0, 0.2,
    0.0, 1.4,
    3.0, -1.1
  ),
  ncol = 2,
  byrow = TRUE
)
supplementary_centered <- sweep(
  supplementary_raw,
  2,
  colMeans(supplementary_raw),
  "-"
)
supplementary_compatibility <-
  crossprod(supplementary_centered, training_scores) %*%
  diag(1 / singular_values^2, ncol(training_scores)) /
  (nrow(training_scores) - 1)

row_weights <- c(0.10, 0.20, 0.25, 0.15, 0.30)
row_metric <- diag(row_weights)
supplementary_metric_least_squares <-
  crossprod(supplementary_centered, row_metric %*% training_scores) %*%
  solve(crossprod(training_scores, row_metric %*% training_scores) +
    diag(ridge, ncol(training_scores)))

block_one_contribution <- new_working[, 1:2, drop = FALSE] %*%
  weights[1:2, , drop = FALSE]
block_two_contribution <- new_working[, 3:4, drop = FALSE] %*%
  weights[3:4, , drop = FALSE]

decoder <- right_pseudoinverse(weights)
reconstruction_working <- full_scores %*% decoder
reconstruction_raw <- inverse_center_scale(reconstruction_working, center, scale)

paired_target_weights <- matrix(
  c(
    0.5, 0.2,
    -0.3, 0.8,
    0.7, -0.4
  ),
  ncol = 2,
  byrow = TRUE
)
paired_target_center <- c(10.0, -2.0, 0.5)
paired_target_scale <- c(2.0, 0.5, 1.5)
paired_target_decoder <- right_pseudoinverse(paired_target_weights)
transfer_working <- full_scores %*% paired_target_decoder
transfer_raw <- inverse_center_scale(
  transfer_working,
  paired_target_center,
  paired_target_scale
)

matrix_fields <- list(
  newRaw = new_raw,
  newWorking = new_working,
  weights = weights,
  fullScores = full_scores,
  partialRaw = partial_raw,
  partialWorking = partial_working,
  partialContribution = partial_contribution,
  partialLeastSquares = partial_least_squares,
  partialMetric = partial_metric,
  partialMetricLeastSquares = partial_metric_least_squares,
  trainingScores = training_scores,
  supplementaryRaw = supplementary_raw,
  supplementaryCompatibility = supplementary_compatibility,
  supplementaryMetricLeastSquares = supplementary_metric_least_squares,
  blockOneContribution = block_one_contribution,
  blockTwoContribution = block_two_contribution,
  decoder = decoder,
  reconstructionWorking = reconstruction_working,
  reconstructionRaw = reconstruction_raw,
  pairedTargetWeights = paired_target_weights,
  pairedTargetDecoder = paired_target_decoder,
  transferWorking = transfer_working,
  transferRaw = transfer_raw
)

vector_fields <- list(
  center = center,
  scale = scale,
  singularValues = singular_values,
  rowWeights = row_weights,
  pairedTargetCenter = paired_target_center,
  pairedTargetScale = paired_target_scale
)

lines <- c(
  "package scalafim.multivar",
  "",
  "import gale.linalg.DMat",
  "",
  "/** Golden fitted-projection fixtures generated by",
  "  * tools/r-parity/generate_multivar_projection_fixtures.R.",
  paste0("  * Provenance: R ", getRversion(), "; base stats; multivarious d44b7d0104a8647aefb61c3217c069c247a27b3d."),
  "  */",
  "object ProjectionParityReferenceFixtures:",
  paste0("  val ridge: Double = ", fmt_num(ridge)),
  paste0("  val subset: Vector[Int] = ", fmt_int_vec(subset_zero_based))
)

for (name in names(vector_fields)) {
  lines <- c(lines, paste0("  val ", name, ": Vector[Double] = ", fmt_vec(vector_fields[[name]])))
}

for (name in names(matrix_fields)) {
  lines <- c(lines, paste0("  val ", name, ": DMat = ", fmt_matrix(matrix_fields[[name]], 4)))
}

cat(paste(lines, collapse = "\n"), "\n", sep = "")
