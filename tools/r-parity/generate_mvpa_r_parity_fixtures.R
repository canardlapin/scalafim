#!/usr/bin/env Rscript

# Generate tiny MVPA reference fixtures for scalafim shared JVM/JS tests.
#
# The fixture computations mirror the small rMVPA-style kernels that scalafim
# intentionally implements without runtime R dependencies:
#   - feature_rsa core: fold-local standardized ridge maps from features to
#     neural patterns and the reverse decoding direction;
#   - naive_xdec_model core: source prototypes, row correlations, and softmax
#     probabilities over target-domain rows.

fmt_num <- function(x) {
  if (is.nan(x)) {
    return("Double.NaN")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "Double.PositiveInfinity" else "Double.NegativeInfinity")
  }
  paste0(formatC(x, digits = 17, format = "fg", flag = "#"), "d")
}

fmt_vec <- function(x, indent = 6) {
  pad <- paste(rep(" ", indent), collapse = "")
  paste0("Vector(", paste(vapply(x, fmt_num, character(1)), collapse = ", "), ")")
}

fmt_string_vec <- function(x) {
  paste0("Vector(", paste(sprintf('"%s"', x), collapse = ", "), ")")
}

fmt_matrix <- function(x, indent = 8) {
  rows <- apply(x, 1, function(row) fmt_vec(row, indent + 2))
  pad <- paste(rep(" ", indent), collapse = "")
  inner <- paste0(pad, rows, collapse = ",\n")
  paste0("DoubleMatrix.fromRows(\n", pad, "Vector(\n", inner, "\n", pad, ")\n", paste(rep(" ", indent - 2), collapse = ""), ")")
}

row_major <- function(x) {
  as.vector(t(as.matrix(x)))
}

sample_scale <- function(x) {
  n <- length(x)
  if (n <= 1) {
    return(1)
  }
  m <- mean(x)
  s <- sqrt(sum((x - m) ^ 2) / (n - 1))
  if (!is.finite(s) || s <= 1e-12) 1 else s
}

standardize <- function(x, means, scales) {
  sweep(sweep(as.matrix(x), 2, means, "-"), 2, scales, "/")
}

ridge_fit <- function(source, target, lambda) {
  source <- as.matrix(source)
  target <- as.matrix(target)
  source_means <- colMeans(source)
  target_means <- colMeans(target)
  source_scales <- apply(source, 2, sample_scale)
  target_scales <- apply(target, 2, sample_scale)
  x <- standardize(source, source_means, source_scales)
  y <- standardize(target, target_means, target_scales)
  coefs <- solve(crossprod(x) + diag(lambda, ncol(x)), crossprod(x, y))
  list(
    source_means = source_means,
    source_scales = source_scales,
    target_means = target_means,
    target_scales = target_scales,
    coefficients = coefs
  )
}

ridge_predict <- function(fit, source) {
  x <- standardize(source, fit$source_means, fit$source_scales)
  pred <- x %*% fit$coefficients
  sweep(sweep(pred, 2, fit$target_scales, "*"), 2, fit$target_means, "+")
}

cross_validate_ridge <- function(source, target, folds, lambda, items) {
  source <- as.matrix(source)
  target <- as.matrix(target)
  test_rows <- sort(unique(unlist(lapply(folds, function(fold) fold$test))))
  predicted <- matrix(0, nrow = length(test_rows), ncol = ncol(target))
  observed <- matrix(0, nrow = length(test_rows), ncol = ncol(target))
  counts <- integer(length(test_rows))
  row_to_output <- setNames(seq_along(test_rows), as.character(test_rows))

  for (fold in folds) {
    fit <- ridge_fit(source[fold$train, , drop = FALSE], target[fold$train, , drop = FALSE], lambda)
    fold_pred <- ridge_predict(fit, source[fold$test, , drop = FALSE])
    for (local_row in seq_along(fold$test)) {
      out_row <- row_to_output[[as.character(fold$test[[local_row]])]]
      predicted[out_row, ] <- predicted[out_row, ] + fold_pred[local_row, ]
      observed[out_row, ] <- target[fold$test[[local_row]], ]
      counts[out_row] <- counts[out_row] + 1L
    }
  }

  predicted <- predicted / counts
  list(
    items = items[test_rows],
    predicted = predicted,
    observed = observed
  )
}

pearson <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  xm <- mean(x)
  ym <- mean(y)
  num <- sum((x - xm) * (y - ym))
  den <- sqrt(sum((x - xm) ^ 2) * sum((y - ym) ^ 2))
  if (den <= 0) NaN else num / den
}

row_cor <- function(left, left_row, right, right_row) {
  pearson(left[left_row, ], right[right_row, ])
}

pattern_metrics <- function(predicted, observed) {
  n <- nrow(predicted)
  if (n < 2 || ncol(predicted) < 2) {
    return(list(pattern_correlation = NaN, pattern_discrimination = NaN, pattern_rank_percentile = NaN))
  }
  cors <- matrix(NaN, nrow = n, ncol = n)
  for (row in seq_len(n)) {
    for (col in seq_len(n)) {
      cors[row, col] <- row_cor(predicted, row, observed, col)
    }
  }
  diag_cors <- diag(cors)
  pattern_correlation <- mean(diag_cors[is.finite(diag_cors)])
  off <- cors[row(cors) != col(cors)]
  off_mean <- mean(off[is.finite(off)])
  ranks <- numeric(0)
  for (row in seq_len(n)) {
    values <- cors[row, ]
    diag_value <- values[[row]]
    finite <- is.finite(values)
    if (is.finite(diag_value) && sum(finite) > 1) {
      ranks <- c(ranks, (sum(values[finite] <= diag_value) - 1) / (sum(finite) - 1))
    }
  }
  list(
    pattern_correlation = pattern_correlation,
    pattern_discrimination = pattern_correlation - off_mean,
    pattern_rank_percentile = mean(ranks)
  )
}

rdm_correlation_vector <- function(matrix) {
  n <- nrow(matrix)
  out <- numeric(n * (n - 1) / 2)
  p <- 1L
  for (col in seq_len(n)) {
    if (col < n) {
      for (row in seq.int(col + 1L, n)) {
        out[[p]] <- 1 - row_cor(matrix, row, matrix, col)
        p <- p + 1L
      }
    }
  }
  out
}

average_ranks <- function(x) {
  rank(x, ties.method = "average")
}

feature_metrics <- function(predicted, observed) {
  pm <- pattern_metrics(predicted, observed)
  pred_rdm <- rdm_correlation_vector(predicted)
  obs_rdm <- rdm_correlation_vector(observed)
  flat_pred <- row_major(predicted)
  flat_obs <- row_major(observed)
  rss <- sum((flat_obs - flat_pred) ^ 2)
  tss <- sum((flat_obs - mean(flat_obs)) ^ 2)
  col_cors <- vapply(seq_len(ncol(predicted)), function(col) pearson(predicted[, col], observed[, col]), numeric(1))
  c(
    PatternCorrelation = pm$pattern_correlation,
    PatternDiscrimination = pm$pattern_discrimination,
    PatternRankPercentile = pm$pattern_rank_percentile,
    RdmCorrelation = pearson(average_ranks(pred_rdm), average_ranks(obs_rdm)),
    TargetCorrelation = pearson(flat_pred, flat_obs),
    Mse = mean((flat_pred - flat_obs) ^ 2),
    RSquared = 1 - rss / tss,
    MeanTargetwiseCorrelation = mean(col_cors[is.finite(col_cors)]),
    Observations = nrow(predicted),
    TargetColumns = ncol(predicted),
    RidgeLambda = feature_lambda
  )
}

feature_rows <- matrix(
  c(
    0.0, 1.0, 0.2,
    1.0, 0.0, -0.1,
    0.5, 1.5, 0.7,
    2.0, 0.2, 0.4,
    1.2, 2.4, -0.3,
    2.5, 1.1, 1.0
  ),
  ncol = 3,
  byrow = TRUE
)

pattern_rows <- cbind(
  1.0 + 1.7 * feature_rows[, 1] - 0.4 * feature_rows[, 2] + 0.8 * feature_rows[, 3] + c(0.00, 0.05, -0.02, 0.07, -0.04, 0.03),
  -0.5 + 0.2 * feature_rows[, 1] + 1.4 * feature_rows[, 2] - 0.6 * feature_rows[, 3] + c(0.04, -0.03, 0.01, -0.02, 0.05, -0.01),
  0.3 - 0.9 * feature_rows[, 1] + 0.6 * feature_rows[, 2] + 0.5 * feature_rows[, 3] + c(-0.02, 0.03, 0.04, -0.01, 0.02, -0.05),
  2.0 + 0.5 * feature_rows[, 1] - 0.3 * feature_rows[, 2] + 1.2 * feature_rows[, 3] + c(0.01, -0.04, 0.02, 0.03, -0.02, 0.04)
)

items <- paste0("item_", 0:5)
feature_names <- c("shape", "color", "motion")
feature_lambda <- 0.75
feature_folds <- list(
  list(name = "fold_a", train = c(3L, 4L, 5L, 6L), test = c(1L, 2L)),
  list(name = "fold_b", train = c(1L, 2L, 5L, 6L), test = c(3L, 4L)),
  list(name = "fold_c", train = c(1L, 2L, 3L, 4L), test = c(5L, 6L))
)

feature_encode <- cross_validate_ridge(feature_rows, pattern_rows, feature_folds, feature_lambda, items)
feature_decode <- cross_validate_ridge(pattern_rows, feature_rows, feature_folds, feature_lambda, items)
feature_search_cols <- c(1L, 3L, 4L)
feature_search_encode <- cross_validate_ridge(feature_rows, pattern_rows[, feature_search_cols, drop = FALSE], feature_folds, feature_lambda, items)

prototype_means <- function(x, labels) {
  labels <- factor(labels)
  levels <- levels(labels)
  out <- matrix(0, nrow = length(levels), ncol = ncol(x))
  rownames(out) <- levels
  for (i in seq_along(levels)) {
    out[i, ] <- colMeans(x[labels == levels[[i]], , drop = FALSE])
  }
  out
}

row_correlation_scores <- function(test, prototypes) {
  scores <- matrix(0, nrow = nrow(test), ncol = nrow(prototypes))
  for (row in seq_len(nrow(test))) {
    for (klass in seq_len(nrow(prototypes))) {
      scores[row, klass] <- row_cor(test, row, prototypes, klass)
    }
  }
  colnames(scores) <- rownames(prototypes)
  scores
}

softmax <- function(scores) {
  out <- scores
  for (row in seq_len(nrow(scores))) {
    shifted <- scores[row, ] - max(scores[row, ])
    z <- exp(shifted)
    out[row, ] <- z / sum(z)
  }
  out
}

naive_xdec <- function(source, target, source_labels, target_labels, feature_sets) {
  lapply(feature_sets, function(feature_set) {
    cols <- feature_set$indices + 1L
    prototypes <- prototype_means(source[, cols, drop = FALSE], source_labels)
    scores <- row_correlation_scores(target[, cols, drop = FALSE], prototypes)
    probs <- softmax(scores)
    classes <- colnames(probs)
    predicted <- classes[max.col(probs, ties.method = "first")]
    list(
      id = feature_set$id,
      label = feature_set$label,
      indices = feature_set$indices,
      classes = classes,
      predicted = predicted,
      probabilities = probs,
      accuracy = mean(predicted == target_labels),
      tested_samples = nrow(target),
      source_samples = nrow(source)
    )
  })
}

xdec_source <- matrix(
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
xdec_target <- matrix(
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
xdec_source_labels <- c("a", "b", "c", "a", "b", "c")
xdec_target_labels <- c("c", "a", "b", "c", "b", "a")
xdec_regional_sets <- list(
  list(id = 101L, label = "regional_front", indices = c(0L, 1L, 2L)),
  list(id = 102L, label = "regional_back", indices = c(2L, 3L, 4L))
)
xdec_searchlight_sets <- list(
  list(id = 201L, label = "sl_left", indices = c(0L, 1L, 4L)),
  list(id = 202L, label = "sl_mid", indices = c(1L, 2L, 3L)),
  list(id = 203L, label = "sl_right", indices = c(0L, 3L, 4L))
)
xdec_regional <- naive_xdec(xdec_source, xdec_target, xdec_source_labels, xdec_target_labels, xdec_regional_sets)
xdec_searchlight <- naive_xdec(xdec_source, xdec_target, xdec_source_labels, xdec_target_labels, xdec_searchlight_sets)

emit_metrics <- function(metrics, indent = 8) {
  pad <- paste(rep(" ", indent), collapse = "")
  lines <- sprintf('"%s" -> %s', names(metrics), vapply(metrics, fmt_num, character(1)))
  paste0("MetricVector(\n", pad, paste(lines, collapse = ",\n"), "\n", paste(rep(" ", indent - 2), collapse = ""), ")")
}

emit_xdec_cases <- function(cases, indent = 6) {
  pad <- paste(rep(" ", indent), collapse = "")
  case_text <- vapply(cases, function(case) {
    paste0(
      "NaiveXdecCase(\n",
      pad, "  roiId = ", case$id, ",\n",
      pad, "  label = \"", case$label, "\",\n",
      pad, "  featureIndices = Vector(", paste(case$indices, collapse = ", "), "),\n",
      pad, "  expectedClasses = ", fmt_string_vec(case$classes), ",\n",
      pad, "  expectedPredicted = ", fmt_string_vec(case$predicted), ",\n",
      pad, "  expectedAccuracy = ", fmt_num(case$accuracy), ",\n",
      pad, "  expectedProbabilities = ", fmt_matrix(case$probabilities, indent + 4), "\n",
      pad, ")"
    )
  }, character(1))
  paste(case_text, collapse = ",\n")
}

cat("object FeatureRsa:\n")
cat("  val items: Vector[String] = ", fmt_string_vec(items), "\n", sep = "")
cat("  val featureNames: Vector[String] = ", fmt_string_vec(feature_names), "\n", sep = "")
cat("  val lambda: Double = ", fmt_num(feature_lambda), "\n", sep = "")
cat("  val featureRows: DoubleMatrix = ", fmt_matrix(feature_rows, 4), "\n", sep = "")
cat("  val patternRows: DoubleMatrix = ", fmt_matrix(pattern_rows, 4), "\n", sep = "")
cat("  val searchlightPatternIndices: Vector[Int] = Vector(", paste(feature_search_cols - 1L, collapse = ", "), ")\n", sep = "")
cat("  object EncodeRegional:\n")
cat("    val predicted: DoubleMatrix = ", fmt_matrix(feature_encode$predicted, 6), "\n", sep = "")
cat("    val observed: DoubleMatrix = ", fmt_matrix(feature_encode$observed, 6), "\n", sep = "")
cat("    val metrics: MetricVector = ", emit_metrics(feature_metrics(feature_encode$predicted, feature_encode$observed), 8), "\n", sep = "")
cat("  object DecodeRegional:\n")
cat("    val predicted: DoubleMatrix = ", fmt_matrix(feature_decode$predicted, 6), "\n", sep = "")
cat("    val observed: DoubleMatrix = ", fmt_matrix(feature_decode$observed, 6), "\n", sep = "")
cat("    val metrics: MetricVector = ", emit_metrics(feature_metrics(feature_decode$predicted, feature_decode$observed), 8), "\n", sep = "")
cat("  object EncodeSearchlight:\n")
cat("    val predicted: DoubleMatrix = ", fmt_matrix(feature_search_encode$predicted, 6), "\n", sep = "")
cat("    val observed: DoubleMatrix = ", fmt_matrix(feature_search_encode$observed, 6), "\n", sep = "")
cat("    val metrics: MetricVector = ", emit_metrics(feature_metrics(feature_search_encode$predicted, feature_search_encode$observed), 8), "\n", sep = "")
cat("\n")
cat("object NaiveXdec:\n")
cat("  val sourceRows: DoubleMatrix = ", fmt_matrix(xdec_source, 4), "\n", sep = "")
cat("  val targetRows: DoubleMatrix = ", fmt_matrix(xdec_target, 4), "\n", sep = "")
cat("  val sourceLabels: Vector[String] = ", fmt_string_vec(xdec_source_labels), "\n", sep = "")
cat("  val targetLabels: Vector[String] = ", fmt_string_vec(xdec_target_labels), "\n", sep = "")
cat("  val regionalCases: Vector[NaiveXdecCase] = Vector(\n")
cat("    ", emit_xdec_cases(xdec_regional, 4), "\n", sep = "")
cat("  )\n")
cat("  val searchlightCases: Vector[NaiveXdecCase] = Vector(\n")
cat("    ", emit_xdec_cases(xdec_searchlight, 4), "\n", sep = "")
cat("  )\n")
