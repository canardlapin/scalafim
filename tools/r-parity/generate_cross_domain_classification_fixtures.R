#!/usr/bin/env Rscript

# Independent base-R oracle for cross-domain correlation-centroid
# classification. Training uses source-domain class means; assessment uses
# Pearson decision scores in the declared class-axis order. No probability
# conversion is part of this estimand.

fmt_num <- function(value) {
  if (value == 0) return("0.0")
  sprintf("%.17g", value)
}

fmt_strings <- function(values) {
  paste0("Vector(", paste(sprintf('"%s"', values), collapse = ", "), ")")
}

fmt_ints <- function(values) {
  paste0("Vector(", paste(values, collapse = ", "), ")")
}

fmt_matrix <- function(values, indent) {
  rows <- apply(values, 1, function(row) {
    paste0("Vector(", paste(vapply(row, fmt_num, character(1)), collapse = ", "), ")")
  })
  pad <- paste(rep(" ", indent), collapse = "")
  row_pad <- paste(rep(" ", indent + 2), collapse = "")
  paste0(
    "GaleTestMatrix.fromRows(\n",
    pad, "Vector(\n",
    row_pad, paste(rows, collapse = paste0(",\n", row_pad)), "\n",
    pad, ")\n",
    paste(rep(" ", indent - 2), collapse = ""), ")"
  )
}

pearson <- function(left, right) {
  centered_left <- left - mean(left)
  centered_right <- right - mean(right)
  denominator <- sqrt(sum(centered_left ^ 2) * sum(centered_right ^ 2))
  stopifnot(is.finite(denominator), denominator > 0)
  sum(centered_left * centered_right) / denominator
}

fit_and_score <- function(source, target, source_labels, feature_sets) {
  classes <- sort(unique(source_labels))
  lapply(feature_sets, function(feature_set) {
    columns <- feature_set$ordinals + 1L
    prototypes <- t(vapply(classes, function(label) {
      colMeans(source[source_labels == label, columns, drop = FALSE])
    }, numeric(length(columns))))
    scores <- matrix(0, nrow = nrow(target), ncol = length(classes))
    for (row in seq_len(nrow(target))) {
      for (class_index in seq_along(classes)) {
        scores[row, class_index] <- pearson(
          target[row, columns],
          prototypes[class_index, ]
        )
      }
    }
    list(
      ordinal = feature_set$ordinal,
      label = feature_set$label,
      feature_ordinals = feature_set$ordinals,
      classes = classes,
      scores = scores,
      predicted = classes[max.col(scores, ties.method = "first")]
    )
  })
}

source_rows <- matrix(
  c(
    2.0, 1.0, -1.0, 0.0, 0.6,
    -1.2, 2.1, 0.8, -0.5, 1.0,
    0.1, -1.0, 2.2, 1.0, -0.7,
    2.3, 0.9, -0.8, 0.2, 0.4,
    -0.9, 2.4, 1.1, -0.4, 0.8,
    -0.1, -0.8, 2.0, 1.2, -0.5
  ),
  ncol = 5,
  byrow = TRUE
)

target_rows <- matrix(
  c(
    0.0, -1.3, 2.8, 1.4, -0.8,
    2.8, 1.4, -1.3, 0.1, 0.7,
    -1.4, 2.8, 1.5, -0.7, 1.1,
    0.2, -1.1, 2.6, 1.1, -0.6,
    -1.2, 2.6, 1.2, -0.3, 0.9,
    2.6, 1.1, -1.2, 0.2, 0.5
  ),
  ncol = 5,
  byrow = TRUE
)

source_labels <- c("a", "b", "c", "a", "b", "c")
target_labels <- c("c", "a", "b", "c", "b", "a")
regional_sets <- list(
  list(ordinal = 101L, label = "regional_front", ordinals = c(0L, 1L, 2L)),
  list(ordinal = 102L, label = "regional_back", ordinals = c(2L, 3L, 4L))
)
searchlight_sets <- list(
  list(ordinal = 201L, label = "sl_left", ordinals = c(0L, 1L, 4L)),
  list(ordinal = 202L, label = "sl_mid", ordinals = c(1L, 2L, 3L)),
  list(ordinal = 203L, label = "sl_right", ordinals = c(0L, 3L, 4L))
)

regional <- fit_and_score(source_rows, target_rows, source_labels, regional_sets)
searchlight <- fit_and_score(source_rows, target_rows, source_labels, searchlight_sets)

emit_cases <- function(cases, indent = 6) {
  pad <- paste(rep(" ", indent), collapse = "")
  case_text <- vapply(cases, function(value) {
    accuracy <- mean(value$predicted == target_labels)
    paste0(
      "MeasurementCase(\n",
      pad, "  measurementOrdinal = ", value$ordinal, ",\n",
      pad, "  label = \"", value$label, "\",\n",
      pad, "  featureOrdinals = ", fmt_ints(value$feature_ordinals), ",\n",
      pad, "  expectedClasses = ", fmt_strings(value$classes), ",\n",
      pad, "  expectedPredicted = ", fmt_strings(value$predicted), ",\n",
      pad, "  expectedAccuracy = ", fmt_num(accuracy), ",\n",
      pad, "  expectedDecisionScores = ", fmt_matrix(value$scores, indent + 4), "\n",
      pad, ")"
    )
  }, character(1))
  paste(case_text, collapse = paste0(",\n", pad))
}

cat("package scalafim.fmri.mvpa\n\n")
cat("import gale.linalg.DMat\n\n")
cat("object CrossDomainClassificationReferenceFixtures:\n")
cat("  final case class MeasurementCase(\n")
cat("      measurementOrdinal: Int,\n")
cat("      label: String,\n")
cat("      featureOrdinals: Vector[Int],\n")
cat("      expectedClasses: Vector[String],\n")
cat("      expectedPredicted: Vector[String],\n")
cat("      expectedAccuracy: Double,\n")
cat("      expectedDecisionScores: DMat\n")
cat("  )\n\n")
cat("  object CorrelationCentroid:\n")
cat("    val sourceRows: DMat = ", fmt_matrix(source_rows, 6), "\n", sep = "")
cat("    val targetRows: DMat = ", fmt_matrix(target_rows, 6), "\n", sep = "")
cat("    val sourceLabels: Vector[String] = ", fmt_strings(source_labels), "\n", sep = "")
cat("    val targetLabels: Vector[String] = ", fmt_strings(target_labels), "\n", sep = "")
cat("    val regionalCases: Vector[MeasurementCase] = Vector(\n")
cat("      ", emit_cases(regional, 6), "\n", sep = "")
cat("    )\n")
cat("    val searchlightCases: Vector[MeasurementCase] = Vector(\n")
cat("      ", emit_cases(searchlight, 6), "\n", sep = "")
cat("    )\n")
