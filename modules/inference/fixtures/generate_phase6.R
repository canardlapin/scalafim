#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L) normalizePath(args[[1L]], mustWork = TRUE) else normalizePath(".", mustWork = TRUE)
fixture_dir <- file.path(repo, "modules", "inference", "fixtures", "v1")
scala_file <- file.path(
  repo,
  "modules", "inference", "shared", "src", "test", "scala",
  "scalafim", "inference", "Phase6RReferenceFixtures.scala"
)

fmt <- function(x) {
  out <- sprintf("%.17g", x)
  out[x == 0] <- "0.0"
  out
}

vec <- function(x) paste0("Vector(", paste(fmt(as.vector(t(x))), collapse = ", "), ")")
num_vec <- function(x) paste0("Vector(", paste(fmt(x), collapse = ", "), ")")
matrix_scala <- function(x) {
  sprintf("MatrixData(%d, %d, %s)", nrow(x), ncol(x), vec(x))
}

cca_x <- matrix(c(
  1, 0,
  2, 1,
  3, 1,
  4, 2,
  5, 3,
  6, 5,
  7, 8,
  8, 13
), ncol = 2, byrow = TRUE)
cca_y <- matrix(c(
  0, 1,
  1, 1,
  1, 2,
  2, 3,
  3, 5,
  5, 8,
  8, 13,
  13, 21
), ncol = 2, byrow = TRUE)
cca_ridge <- 1e-8
cca_xc <- scale(cca_x, center = TRUE, scale = FALSE)
cca_yc <- scale(cca_y, center = TRUE, scale = FALSE)
cca_denom <- nrow(cca_x) - 1
cca_cxx <- crossprod(cca_xc) / cca_denom + diag(cca_ridge, ncol(cca_x))
cca_cyy <- crossprod(cca_yc) / cca_denom + diag(cca_ridge, ncol(cca_y))
cca_cxy <- crossprod(cca_xc, cca_yc) / cca_denom
inverse_sqrt <- function(value) {
  eig <- eigen(value, symmetric = TRUE)
  eig$vectors %*% diag(1 / sqrt(eig$values), length(eig$values)) %*% t(eig$vectors)
}
cca_roots <- svd(inverse_sqrt(cca_cxx) %*% cca_cxy %*% inverse_sqrt(cca_cyy), nu = 0, nv = 0)$d

gen_a <- diag(c(9, 4, 1))
gen_b <- diag(c(1, 2, 4))
gen_roots <- sort(Re(eigen(solve(gen_b, gen_a), only.values = TRUE)$values), decreasing = TRUE)

rrr_x <- matrix(c(-3, -2, -1, 0, 1, 2, 3, 4), ncol = 1)
rrr_y <- matrix(c(-5.8, -4.2, -1.9, 0.2, 2.1, 3.9, 6.2, 8.1), ncol = 1)
rrr_train <- 1:6
rrr_test <- 7:8
x_train <- rrr_x[rrr_train, , drop = FALSE]
y_train <- rrr_y[rrr_train, , drop = FALSE]
x_test <- rrr_x[rrr_test, , drop = FALSE]
y_test <- rrr_y[rrr_test, , drop = FALSE]
x_mean <- colMeans(x_train)
y_mean <- colMeans(y_train)
coefficient <- solve(crossprod(sweep(x_train, 2, x_mean)), crossprod(sweep(x_train, 2, x_mean), sweep(y_train, 2, y_mean)))
prediction <- sweep(sweep(x_test, 2, x_mean) %*% coefficient, 2, y_mean, "+")
model_sse <- sum((y_test - prediction)^2)
baseline_sse <- sum((y_test - matrix(y_mean, nrow(y_test), ncol(y_test), byrow = TRUE))^2)
rrr_gain <- (baseline_sse - model_sse) / baseline_sse

cpca_z <- matrix(c(
  2, 0, 1, 0,
  1, 1, 0, 1,
  3, 1, 1, 0,
  0, 2, 1, 1,
  1, 3, 0, 2,
  2, 4, 1, 3
), ncol = 4, byrow = TRUE)
cpca_row_design <- cbind(1, c(-1, -1, -1, 1, 1, 1))
cpca_col_design <- cbind(c(1, 1, 0, 0), c(0, 0, 1, 1))
q_row <- qr.Q(qr(cpca_row_design))
q_col <- qr.Q(qr(cpca_col_design))
cpca_block <- q_row %*% crossprod(q_row, cpca_z) %*% q_col %*% t(q_col)
cpca_rank <- min(ncol(q_row), ncol(q_col))
cpca_roots <- svd(cpca_block, nu = 0, nv = 0)$d[seq_len(cpca_rank)]^2

multiblock <- matrix(c(
  -2, -1, -1.8, -0.8,
  -1, 0, -1.1, 0.2,
  0, 1, 0.1, 0.9,
  1, 0, 0.8, 0.1,
  2, 1, 2.2, 1.2,
  3, 2, 2.9, 2.1
), ncol = 4, byrow = TRUE)
centered <- scale(multiblock, center = TRUE, scale = FALSE)
block1 <- centered[, 1:2, drop = FALSE]
block2 <- centered[, 3:4, drop = FALSE]
consensus <- tcrossprod(block1) / sum(block1^2) + tcrossprod(block2) / sum(block2^2)
multiblock_roots <- pmax(0, eigen(consensus, symmetric = TRUE, only.values = TRUE)$values)

feature_observed <- c(3.0, 2.0, 1.0, 0.5)
feature_null <- rbind(
  c(0.2, 2.5, 0.8, 0.7),
  c(1.0, 1.5, 1.2, 0.1),
  c(2.0, 2.1, 0.5, 0.9),
  c(3.5, 0.5, 1.5, 0.4),
  c(0.1, 3.0, 0.1, 0.6),
  c(2.9, 1.9, 1.1, 0.2),
  c(1.5, 2.2, 0.9, 0.8),
  c(0.7, 0.4, 1.0, 0.3),
  c(3.1, 1.8, 0.7, 1.0)
)
feature_raw <- vapply(seq_along(feature_observed), function(index) {
  (1 + sum(abs(feature_null[, index]) >= abs(feature_observed[[index]]))) / (nrow(feature_null) + 1)
}, numeric(1))
feature_bh <- p.adjust(feature_raw, method = "BH")
feature_holm <- p.adjust(feature_raw, method = "holm")

records <- rbind(
  data.frame(family = "cca", statistic = "canonical_correlation", index = seq_along(cca_roots), value = cca_roots),
  data.frame(family = "generalized_eigen", statistic = "root", index = seq_along(gen_roots), value = gen_roots),
  data.frame(family = "rrr", statistic = c("gain", "model_sse", "baseline_sse"), index = 1:3, value = c(rrr_gain, model_sse, baseline_sse)),
  data.frame(family = "cpca", statistic = "constrained_root", index = seq_along(cpca_roots), value = cpca_roots),
  data.frame(family = "multiblock", statistic = "consensus_root", index = seq_along(multiblock_roots), value = multiblock_roots),
  data.frame(family = "feature", statistic = "raw_p", index = seq_along(feature_raw), value = feature_raw),
  data.frame(family = "feature", statistic = "bh_p", index = seq_along(feature_bh), value = feature_bh),
  data.frame(family = "feature", statistic = "holm_p", index = seq_along(feature_holm), value = feature_holm)
)
write.table(records, file.path(fixture_dir, "phase6_families.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)

scala <- c(
  "package scalafim.inference",
  "",
  "/** Generated by modules/inference/fixtures/generate_phase6.R using base R.",
  "  * Do not hand-edit: regenerate from the documented analytic/reference script.",
  "  */",
  "object Phase6RReferenceFixtures:",
  "  final case class MatrixData(rows: Int, cols: Int, values: Vector[Double]):",
  "    def toRows: Vector[Vector[Double]] =",
  "      Vector.tabulate(rows)(row => Vector.tabulate(cols)(col => values(row * cols + col)))",
  sprintf("  val rVersion: String = \"%s\"", R.version.string),
  sprintf("  val ccaX: MatrixData = %s", matrix_scala(cca_x)),
  sprintf("  val ccaY: MatrixData = %s", matrix_scala(cca_y)),
  sprintf("  val ccaRidge: Double = %s", fmt(cca_ridge)),
  sprintf("  val ccaRoots: Vector[Double] = %s", num_vec(cca_roots)),
  sprintf("  val generalizedA: MatrixData = %s", matrix_scala(gen_a)),
  sprintf("  val generalizedB: MatrixData = %s", matrix_scala(gen_b)),
  sprintf("  val generalizedRoots: Vector[Double] = %s", num_vec(gen_roots)),
  sprintf("  val rrrX: MatrixData = %s", matrix_scala(rrr_x)),
  sprintf("  val rrrY: MatrixData = %s", matrix_scala(rrr_y)),
  "  val rrrTrainingZeroBased: Vector[Int] = Vector(0, 1, 2, 3, 4, 5)",
  "  val rrrTestZeroBased: Vector[Int] = Vector(6, 7)",
  sprintf("  val rrrGain: Double = %s", fmt(rrr_gain)),
  sprintf("  val rrrModelSse: Double = %s", fmt(model_sse)),
  sprintf("  val rrrBaselineSse: Double = %s", fmt(baseline_sse)),
  sprintf("  val cpcaData: MatrixData = %s", matrix_scala(cpca_z)),
  sprintf("  val cpcaRowDesign: MatrixData = %s", matrix_scala(cpca_row_design)),
  sprintf("  val cpcaColumnDesign: MatrixData = %s", matrix_scala(cpca_col_design)),
  sprintf("  val cpcaRoots: Vector[Double] = %s", num_vec(cpca_roots)),
  sprintf("  val multiblockData: MatrixData = %s", matrix_scala(multiblock)),
  sprintf("  val multiblockRoots: Vector[Double] = %s", num_vec(multiblock_roots)),
  sprintf("  val featureObserved: Vector[Double] = %s", num_vec(feature_observed)),
  sprintf("  val featureNull: MatrixData = %s", matrix_scala(feature_null)),
  sprintf("  val featureRawP: Vector[Double] = %s", num_vec(feature_raw)),
  sprintf("  val featureBhP: Vector[Double] = %s", num_vec(feature_bh)),
  sprintf("  val featureHolmP: Vector[Double] = %s", num_vec(feature_holm))
)
writeLines(scala, scala_file)
