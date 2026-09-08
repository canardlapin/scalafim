#!/usr/bin/env Rscript

# Independent base-R oracle for the held-out MANOVA root-spectrum estimand.
# The generator uses ordinary dense products, Cholesky whitening, and eigen;
# it does not call the ScalaFIM or Multivar implementation under test.

fmt_num <- function(value) {
  if (value == 0) return("0.0")
  sprintf("%.17g", value)
}

fmt_vec <- function(values, indent = 0, width = 120) {
  tokens <- vapply(values, fmt_num, character(1))
  lines <- character()
  current <- paste0(paste(rep(" ", indent), collapse = ""), "Vector(")
  entries <- 0L
  for (index in seq_along(tokens)) {
    separator <- if (entries == 0L) "" else ", "
    closing <- if (index == length(tokens)) ")" else ""
    candidate <- paste0(current, separator, tokens[[index]], closing)
    if (nchar(candidate, type = "bytes") > width && entries > 0L) {
      lines <- c(lines, paste0(current, ","))
      current <- paste0(
        paste(rep(" ", indent + 2L), collapse = ""),
        tokens[[index]],
        closing
      )
      entries <- 1L
    } else {
      current <- candidate
      entries <- entries + 1L
    }
  }
  paste(c(lines, current), collapse = "\n")
}

fmt_matrix <- function(values, indent = 6) {
  values <- as.matrix(values)
  pad <- paste(rep(" ", indent), collapse = "")
  rows <- vapply(seq_len(nrow(values)), function(row) {
    width <- if (row < nrow(values)) 119 else 120
    fmt_vec(values[row, ], indent + 2L, width)
  }, character(1))
  paste0(
    "fromRows(\n",
    pad, "Vector(\n",
    paste(rows, collapse = ",\n"),
    "\n", pad, ")\n",
    paste(rep(" ", indent - 2), collapse = ""), ")"
  )
}

design <- cbind(
  intercept = 1,
  a = c(-1, 0, 1, 0, -1, 0, 1, 0, -1, 1),
  b = c(0, -1, 0, 1, 0, -1, 0, 1, 0, 0),
  drift = seq(-1, 1, length.out = 10)
)

coefficients <- list(
  matrix(c(
    0.2, 0.1, -0.1, 0.0, 0.3,
    1.2, -0.4, 0.7, 0.2, -0.3,
    -0.2, 1.0, 0.5, -0.6, 0.4,
    0.1, -0.1, 0.2, 0.0, 0.1
  ), nrow = 4, byrow = TRUE),
  matrix(c(
    0.1, 0.2, 0.0, -0.1, 0.2,
    1.0, -0.2, 0.8, 0.3, -0.4,
    -0.1, 1.1, 0.4, -0.5, 0.5,
    0.0, -0.2, 0.1, 0.1, 0.0
  ), nrow = 4, byrow = TRUE),
  matrix(c(
    0.3, 0.0, -0.2, 0.1, 0.4,
    1.1, -0.3, 0.6, 0.4, -0.2,
    -0.3, 0.9, 0.6, -0.4, 0.3,
    0.2, 0.0, 0.3, -0.1, 0.2
  ), nrow = 4, byrow = TRUE)
)

noise <- list(
  matrix(c(
    0.10, -0.05, 0.02, 0.04, -0.03,
    -0.08, 0.03, -0.01, 0.02, 0.05,
    0.04, 0.06, -0.03, -0.02, 0.01,
    -0.02, -0.04, 0.05, 0.03, -0.06,
    0.06, 0.01, 0.04, -0.05, 0.02,
    -0.05, 0.02, 0.01, 0.06, -0.04,
    0.03, -0.06, 0.02, -0.01, 0.04,
    -0.01, 0.05, -0.04, 0.02, 0.03,
    0.02, -0.02, 0.03, -0.04, 0.01,
    -0.04, 0.00, -0.02, 0.05, -0.03
  ), nrow = 10, byrow = TRUE),
  matrix(c(
    -0.03, 0.04, 0.01, -0.02, 0.05,
    0.05, -0.02, 0.03, 0.01, -0.04,
    -0.01, 0.06, -0.04, 0.03, 0.02,
    0.04, -0.05, 0.02, -0.01, 0.03,
    -0.02, 0.01, 0.05, -0.04, 0.00,
    0.03, -0.04, -0.01, 0.05, 0.02,
    -0.05, 0.02, 0.04, 0.00, -0.03,
    0.01, 0.03, -0.02, 0.04, -0.05,
    0.02, -0.01, 0.00, -0.03, 0.04,
    -0.04, 0.05, -0.03, 0.02, 0.01
  ), nrow = 10, byrow = TRUE),
  matrix(c(
    0.02, 0.01, -0.04, 0.05, -0.02,
    -0.04, 0.05, 0.02, -0.03, 0.01,
    0.06, -0.03, 0.01, 0.02, -0.05,
    -0.01, -0.02, 0.05, -0.04, 0.03,
    0.03, 0.04, -0.02, 0.01, -0.06,
    -0.05, 0.00, 0.03, 0.04, -0.01,
    0.01, -0.05, 0.04, -0.02, 0.02,
    0.04, 0.02, -0.03, 0.00, -0.04,
    -0.02, 0.03, 0.00, -0.05, 0.05,
    0.00, -0.04, -0.01, 0.03, 0.02
  ), nrow = 10, byrow = TRUE)
)
noise <- lapply(noise, function(values) 10 * values)

responses <- Map(function(beta, epsilon) design %*% beta + epsilon, coefficients, noise)
contrast <- rbind(c(0, 1, 0, 0), c(0, 0, 1, 0))
inverse <- solve(crossprod(design))
contrast_covariance <- contrast %*% inverse %*% t(contrast)
q <- inverse %*% t(contrast) %*% solve(chol(contrast_covariance))

moments <- lapply(responses, function(response) {
  cross <- crossprod(response, design)
  transformed <- cross %*% q
  list(
    hypothesis = tcrossprod(transformed),
    error = crossprod(response) - cross %*% inverse %*% t(cross)
  )
})

generalized <- function(hypothesis, error, rank) {
  upper <- chol(error)
  whitened <- solve(t(upper), hypothesis) %*% solve(upper)
  solved <- eigen((whitened + t(whitened)) / 2, symmetric = TRUE)
  roots <- pmax(solved$values[seq_len(rank)], 0)
  frame <- solve(upper, solved$vectors[, seq_len(rank), drop = FALSE])
  list(roots = roots, frame = frame)
}

statistics <- function(roots) c(
  roy = roots[1],
  wilks = prod(1 / (1 + roots)),
  pillai = sum(roots / (1 + roots)),
  hotelling = sum(roots)
)

folds <- lapply(seq_along(moments), function(held_out) {
  training <- moments[-held_out]
  fit <- generalized(
    Reduce(`+`, lapply(training, `[[`, "hypothesis")),
    Reduce(`+`, lapply(training, `[[`, "error")),
    2
  )
  held_hypothesis <- crossprod(
    fit$frame,
    moments[[held_out]]$hypothesis %*% fit$frame
  )
  held_error <- crossprod(
    fit$frame,
    moments[[held_out]]$error %*% fit$frame
  )
  held <- generalized(held_hypothesis, held_error, 2)
  list(roots = held$roots, statistics = statistics(held$roots))
})

mean_statistics <- Reduce(`+`, lapply(folds, `[[`, "statistics")) /
  length(folds)

cat("package scalafim.fmri.mvpa\n\n")
cat("import gale.linalg.{DMat, Matrix}\n\n")
cat("object ManovaReferenceFixtures:\n")
cat("  val design: DMat = ", fmt_matrix(design, 4), "\n\n", sep = "")
cat("  val responses: Vector[DMat] = Vector(\n")
for (index in seq_along(responses)) {
  suffix <- if (index < length(responses)) "," else ""
  cat("    ", fmt_matrix(responses[[index]], 6), suffix, "\n", sep = "")
}
cat("  )\n\n")
cat("  final case class Fold(\n")
cat("      roots: Vector[Double],\n")
cat("      roy: Double,\n")
cat("      wilks: Double,\n")
cat("      pillai: Double,\n")
cat("      hotelling: Double\n")
cat("  )\n\n")
cat("  val folds: Vector[Fold] = Vector(\n")
for (index in seq_along(folds)) {
  fold <- folds[[index]]
  suffix <- if (index < length(folds)) "," else ""
  cat("    Fold(\n")
  cat("      ", fmt_vec(fold$roots), ",\n", sep = "")
  cat("      ", paste(vapply(fold$statistics, fmt_num, character(1)), collapse = ",\n      "), "\n", sep = "")
  cat("    )", suffix, "\n", sep = "")
}
cat("  )\n\n")
cat("  val mean: Vector[Double] = ", fmt_vec(mean_statistics), "\n\n", sep = "")
cat("  private def fromRows(rows: Vector[Vector[Double]]): DMat =\n")
cat("    Matrix.tabulate(rows.length, rows.head.length): (row, column) =>\n")
cat("      rows(row)(column)\n")
