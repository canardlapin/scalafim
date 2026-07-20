#!/usr/bin/env Rscript

# Generate paired latent-method reference fixtures for scalafim multivar tests.
#
# The computations mirror the current shared Scala contracts:
#   - PLSC: SVD of centered cross-covariance X'Y / (n - 1).
#   - CCA: SVD of ridge-regularized, whitened cross-covariance.
#   - RRR: OLS x-to-y coefficient map followed by SVD of the fitted response.
#
# This script prints the Scala fixture object to stdout. Regenerate with:
#   Rscript tools/r-parity/generate_multivar_paired_r_parity_fixtures.R

fmt_num <- function(x) {
  if (is.nan(x)) {
    return("Double.NaN")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "Double.PositiveInfinity" else "Double.NegativeInfinity")
  }
  if (x == 0) {
    return("0.0")
  }
  formatC(x, digits = 17, format = "fg", flag = "#")
}

fmt_vec <- function(x, indent = 6) {
  paste0("Vector(", paste(vapply(x, fmt_num, character(1)), collapse = ", "), ")")
}

fmt_matrix <- function(x, indent = 4) {
  x <- as.matrix(x)
  pad <- paste(rep(" ", indent), collapse = "")
  row_pad <- paste(rep(" ", indent + 4), collapse = "")
  rows <- apply(x, 1, function(row) paste0(row_pad, fmt_vec(row)))
  paste0(
    "DoubleMatrix.fromRows(\n",
    pad, "Vector(\n",
    paste(rows, collapse = ",\n"),
    "\n",
    pad, ")\n",
    paste(rep(" ", indent - 2), collapse = ""), ")"
  )
}

fmt_dvec <- function(x, indent = 4) {
  paste0("DoubleVector.fromSeq(", fmt_vec(x, indent), ")")
}

center <- function(x) {
  sweep(as.matrix(x), 2, colMeans(x), "-")
}

inverse_sqrt <- function(x, tolerance = 1e-12) {
  eig <- eigen(x, symmetric = TRUE)
  if (any(eig$values <= tolerance)) {
    stop("matrix is not positive definite")
  }
  eig$vectors %*% diag(1 / sqrt(eig$values), nrow = length(eig$values)) %*% t(eig$vectors)
}

leading_svd <- function(x, k) {
  out <- svd(x, nu = k, nv = k)
  list(u = out$u[, seq_len(k), drop = FALSE], d = out$d[seq_len(k)], v = out$v[, seq_len(k), drop = FALSE])
}

canonicalize_pair <- function(x_weights, y_weights, x_scores, y_scores) {
  x_weights <- as.matrix(x_weights)
  y_weights <- as.matrix(y_weights)
  x_scores <- as.matrix(x_scores)
  y_scores <- as.matrix(y_scores)
  for (col in seq_len(ncol(x_weights))) {
    best <- which.max(abs(x_weights[, col]))
    if (x_weights[best, col] < 0) {
      x_weights[, col] <- -x_weights[, col]
      y_weights[, col] <- -y_weights[, col]
      x_scores[, col] <- -x_scores[, col]
      y_scores[, col] <- -y_scores[, col]
    }
  }
  list(
    x_weights = x_weights,
    y_weights = y_weights,
    x_scores = x_scores,
    y_scores = y_scores
  )
}

paired_fixture <- function(x, y, components, d, x_weights, y_weights, x_scores, y_scores) {
  canon <- canonicalize_pair(x_weights, y_weights, x_scores, y_scores)
  list(
    x = as.matrix(x),
    y = as.matrix(y),
    components = components,
    singular_values = d,
    x_weights = canon$x_weights,
    y_weights = canon$y_weights,
    x_scores = canon$x_scores,
    y_scores = canon$y_scores
  )
}

plsc_reference <- function(x, y, components) {
  xp <- center(x)
  yp <- center(y)
  cross <- crossprod(xp, yp) / max(1, nrow(x) - 1)
  decomp <- leading_svd(cross, components)
  paired_fixture(x, y, components, decomp$d, decomp$u, decomp$v, xp %*% decomp$u, yp %*% decomp$v)
}

cca_reference <- function(x, y, components, x_ridge, y_ridge) {
  xp <- center(x)
  yp <- center(y)
  denom <- 1 / max(1, nrow(x) - 1)
  cxx <- crossprod(xp) * denom + diag(x_ridge, ncol(x))
  cyy <- crossprod(yp) * denom + diag(y_ridge, ncol(y))
  cxy <- crossprod(xp, yp) * denom
  wx <- inverse_sqrt(cxx)
  wy <- inverse_sqrt(cyy)
  decomp <- leading_svd(wx %*% cxy %*% wy, components)
  x_weights <- wx %*% decomp$u
  y_weights <- wy %*% decomp$v
  out <- paired_fixture(x, y, components, decomp$d, x_weights, y_weights, xp %*% x_weights, yp %*% y_weights)
  out$x_ridge <- x_ridge
  out$y_ridge <- y_ridge
  out
}

rrr_reference <- function(x, y, components, ridge = 0) {
  xp <- center(x)
  yp <- center(y)
  gram <- crossprod(xp) + diag(ridge, ncol(x))
  coefficient <- solve(gram, crossprod(xp, yp))
  fitted <- xp %*% coefficient
  decomp <- leading_svd(fitted, components)
  response_loadings <- decomp$v
  encoder_weights <- coefficient %*% response_loadings
  low_rank_coefficient <- encoder_weights %*% t(response_loadings)
  paired <- paired_fixture(
    x,
    y,
    components,
    decomp$d,
    encoder_weights,
    response_loadings,
    xp %*% encoder_weights,
    yp %*% response_loadings
  )
  paired$ridge <- ridge
  paired$full_coefficient <- coefficient
  paired$working_coefficient <- low_rank_coefficient
  paired$predicted_working <- xp %*% low_rank_coefficient
  paired$predicted <- sweep(paired$predicted_working, 2, colMeans(y), "+")
  paired
}

emit_fixture <- function(name, fixture, extra_lines = character()) {
  c(
    paste0("  val ", name, ": PairedReference ="),
    "    PairedReference(",
    paste0("      x = ", fmt_matrix(fixture$x, 8), ","),
    paste0("      y = ", fmt_matrix(fixture$y, 8), ","),
    paste0("      components = ", fixture$components, ","),
    paste0("      singularValues = ", fmt_dvec(fixture$singular_values, 8), ","),
    paste0("      xWeights = ", fmt_matrix(fixture$x_weights, 8), ","),
    paste0("      yWeights = ", fmt_matrix(fixture$y_weights, 8), ","),
    paste0("      xScores = ", fmt_matrix(fixture$x_scores, 8), ","),
    paste0("      yScores = ", fmt_matrix(fixture$y_scores, 8), if (length(extra_lines) == 0) "" else ","),
    extra_lines,
    "    )"
  )
}

plsc_x <- matrix(
  c(
    1.2, -0.3, 0.7,
    0.1, 1.4, -1.1,
    -0.8, 0.5, 0.2,
    1.9, -1.2, 1.3,
    -1.1, 0.0, -0.4
  ),
  ncol = 3,
  byrow = TRUE
)
plsc_y <- matrix(
  c(
    0.4, 1.1,
    1.2, -0.7,
    -0.6, 0.3,
    1.5, 0.9,
    -1.0, -0.5
  ),
  ncol = 2,
  byrow = TRUE
)

cca_x <- matrix(
  c(
    0.2, 1.1, -0.4,
    1.0, 0.3, 0.8,
    -0.5, 1.4, 0.1,
    1.7, -0.8, 1.2,
    -1.2, 0.6, -0.7,
    0.4, -1.1, 0.5,
    1.3, 0.9, -1.0
  ),
  ncol = 3,
  byrow = TRUE
)
cca_y <- matrix(
  c(
    1.10, -0.20,
    0.35, 1.30,
    1.45, -0.75,
    -0.90, 0.40,
    0.25, -1.20,
    -1.10, 0.95,
    0.80, 0.10
  ),
  ncol = 2,
  byrow = TRUE
)

rrr_x <- matrix(
  c(
    1.0, 0.2, -0.4,
    0.5, 1.3, 0.7,
    -0.8, 0.4, 1.1,
    1.7, -1.0, 0.3,
    -1.4, -0.6, -0.9,
    0.2, 0.8, -1.2
  ),
  ncol = 3,
  byrow = TRUE
)
rrr_y <- matrix(
  c(
    1.58, -0.32,
    0.82, 1.44,
    -1.56, 1.07,
    1.20, -1.70,
    -1.32, -0.38,
    1.65, 0.42
  ),
  ncol = 2,
  byrow = TRUE
)

plsc <- plsc_reference(plsc_x, plsc_y, components = 2)
cca <- cca_reference(cca_x, cca_y, components = 2, x_ridge = 0.07, y_ridge = 0.11)
cca_base_correlations <- cancor(cca_x, cca_y)$cor[seq_len(2)]
rrr <- rrr_reference(rrr_x, rrr_y, components = 1)

stats_version <- as.character(packageVersion("stats"))

lines <- c(
  "package scalafim.multivar",
  "",
  "import scalafim.linalg.DoubleMatrix",
  "import scalafim.linalg.DoubleVector",
  "",
  "/** Golden paired latent-method fixtures generated by",
  "  * tools/r-parity/generate_multivar_paired_r_parity_fixtures.R.",
  paste0("  * Provenance: R ", getRversion(), "; stats ", stats_version, "; base svd/eigen/solve/cancor."),
  "  * Component signs are canonicalized so the largest-magnitude x weight is positive.",
  "  */",
  "object PairedLatentRReferenceFixtures:",
  "  final case class PairedReference(",
  "      x: DoubleMatrix,",
  "      y: DoubleMatrix,",
  "      components: Int,",
  "      singularValues: DoubleVector,",
  "      xWeights: DoubleMatrix,",
  "      yWeights: DoubleMatrix,",
  "      xScores: DoubleMatrix,",
  "      yScores: DoubleMatrix,",
  "      xRidge: Double = 0.0,",
  "      yRidge: Double = 0.0,",
  "      baseCancorCorrelations: Option[DoubleVector] = None,",
  "      fullCoefficient: Option[DoubleMatrix] = None,",
  "      workingCoefficient: Option[DoubleMatrix] = None,",
  "      predictedWorking: Option[DoubleMatrix] = None,",
  "      predicted: Option[DoubleMatrix] = None",
  "  )",
  "",
  emit_fixture("plsc", plsc),
  "",
  emit_fixture(
    "cca",
    cca,
    c(
      paste0("      xRidge = ", fmt_num(cca$x_ridge), ","),
      paste0("      yRidge = ", fmt_num(cca$y_ridge), ","),
      paste0("      baseCancorCorrelations = Some(", fmt_dvec(cca_base_correlations, 8), ")")
    )
  ),
  "",
  emit_fixture(
    "rrr",
    rrr,
    c(
      paste0("      fullCoefficient = Some(", fmt_matrix(rrr$full_coefficient, 8), "),"),
      paste0("      workingCoefficient = Some(", fmt_matrix(rrr$working_coefficient, 8), "),"),
      paste0("      predictedWorking = Some(", fmt_matrix(rrr$predicted_working, 8), "),"),
      paste0("      predicted = Some(", fmt_matrix(rrr$predicted, 8), ")")
    )
  )
)

cat(paste(lines, collapse = "\n"))
cat("\n")
