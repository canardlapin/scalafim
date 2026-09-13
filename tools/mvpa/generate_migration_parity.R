#!/usr/bin/env Rscript

# Independent base-R oracle for the Unified MVPA migration baselines.
# It deliberately does not load ScalaFIM or translate its implementation helpers.

args <- commandArgs(trailingOnly = TRUE)
check_mode <- identical(args, "--check")
if (!check_mode && length(args) != 1L) {
  stop("usage: generate_migration_parity.R OUTPUT.json | --check", call. = FALSE)
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required to write the fixture", call. = FALSE)
}
output_path <- if (check_mode) tempfile(fileext = ".json") else args[[1L]]

zscore_fit <- function(x) {
  center <- colMeans(x)
  scale <- apply(x, 2L, stats::sd)
  scale[!is.finite(scale) | scale <= 1e-12] <- 1
  list(center = center, scale = scale)
}

zscore_apply <- function(x, fitted) {
  sweep(sweep(x, 2L, fitted$center, "-"), 2L, fitted$scale, "/")
}

swift_fit <- function(x, labels) {
  classes <- unique(labels)
  fitted_z <- zscore_fit(x)
  scaled <- zscore_apply(x, fitted_z)
  counts <- vapply(classes, function(label) sum(labels == label), integer(1L))
  centroids <- do.call(rbind, lapply(classes, function(label) {
    colMeans(scaled[labels == label, , drop = FALSE])
  }))
  list(
    classes = classes,
    counts = counts,
    priors = counts / length(labels),
    center = fitted_z$center,
    scale = fitted_z$scale,
    centroids = centroids
  )
}

swift_predict <- function(model, x) {
  scaled <- zscore_apply(x, list(center = model$center, scale = model$scale))
  scores <- scaled %*% t(model$centroids)
  scores <- sweep(scores, 2L, 0.5 * rowSums(model$centroids^2), "-")
  scores <- sweep(scores, 2L, log(model$priors), "+")
  shifted <- sweep(scores, 1L, apply(scores, 1L, max), "-")
  weights <- exp(shifted)
  probabilities <- weights / rowSums(weights)
  predicted <- model$classes[max.col(probabilities, ties.method = "first")]
  list(
    scaled = scaled,
    scores = scores,
    probabilities = probabilities,
    predicted = predicted
  )
}

train_x <- rbind(
  c(2, 0, 10),
  c(0, 2, 8),
  c(3, 1, 9),
  c(-1, 4, 7),
  c(1, 3, 11),
  c(4, -2, 6),
  c(0, 5, 12)
)
train_labels <- c("zeta", "alpha", "zeta", "beta", "alpha", "zeta", "beta")
test_x <- rbind(
  c(2, 1, 8.5),
  c(-0.5, 4.5, 11),
  c(3.5, -1, 6.5),
  c(1, 2.5, 10)
)
full_model <- swift_fit(train_x, train_labels)
full_prediction <- swift_predict(full_model, test_x)

cv_x <- rbind(
  c(2, 1, 0),
  c(-2, -1, 0),
  c(-1.5, -1, 1),
  c(1.5, 1, -1),
  c(0.8, 0.5, 0),
  c(2.2, 0.7, 1),
  c(-2, -0.4, -1),
  c(-0.7, -0.2, 0.5),
  c(-1.8, -1.2, 0)
)
cv_labels <- c("b", "a", "a", "b", "a", "b", "a", "b", "a")
cv_blocks <- c(0L, 0L, 1L, 1L, 1L, 2L, 2L, 2L, 2L)
cv_classes <- unique(cv_labels)
cv_probabilities <- matrix(NA_real_, nrow(cv_x), length(cv_classes))
cv_folds <- lapply(unique(cv_blocks), function(block) {
  train <- which(cv_blocks != block)
  test <- which(cv_blocks == block)
  model <- swift_fit(cv_x[train, , drop = FALSE], cv_labels[train])
  prediction <- swift_predict(model, cv_x[test, , drop = FALSE])
  columns <- match(cv_classes, model$classes)
  probabilities <- prediction$probabilities[, columns, drop = FALSE]
  cv_probabilities[test, ] <<- probabilities
  predicted <- cv_classes[max.col(probabilities, ties.method = "first")]
  list(
    block = block,
    train_indices = train - 1L,
    test_indices = test - 1L,
    classes = model$classes,
    probabilities = unname(probabilities),
    predicted = predicted,
    correct = sum(predicted == cv_labels[test]),
    tested_samples = length(test),
    accuracy = mean(predicted == cv_labels[test])
  )
})
cv_predicted <- cv_classes[max.col(cv_probabilities, ties.method = "first")]

condition_names <- c("a", "b", "c")
partition_means <- array(0, dim = c(3L, 3L, 3L))
partition_means[, 2L, ] <- rbind(
  c(-1, 0, -1),
  c(1, 0, -1),
  c(-0.5, 0, 1)
)
partition_means[, 3L, ] <- rbind(
  c(0, -2, 0),
  c(0, -1, -1),
  c(0, -3, 1)
)
condition_pairs <- rbind(c(2L, 1L), c(3L, 1L), c(3L, 2L))
ordered_partition_pairs <- do.call(rbind, lapply(seq_len(dim(partition_means)[1L]), function(left) {
  rights <- setdiff(seq_len(dim(partition_means)[1L]), left)
  cbind(left, rights)
}))
rdm_raw <- apply(condition_pairs, 1L, function(pair) {
  deltas <- partition_means[, pair[1L], ] - partition_means[, pair[2L], ]
  mean(apply(ordered_partition_pairs, 1L, function(partitions) {
    sum(deltas[partitions[1L], ] * deltas[partitions[2L], ])
  }))
})

fixture <- list(
  schema = "scalafim.mvpa.migration-parity.v1",
  source = list(
    repository_revision = "528c302e454697055bc9af31c9a6eca684f019e3",
    generator = "tools/mvpa/generate_migration_parity.R",
    oracle = "independent base-R sample SD, class means, linear discriminant scores, stable softmax, and explicit ordered partition-pair dot products",
    zero_based_indices = TRUE
  ),
  tolerances = list(
    absolute = 1e-12,
    relative = 1e-12,
    rationale = "small finite dense fixtures with moderate feature scales and no iterative solver; errors should be roundoff-scale"
  ),
  swift_fit = list(
    training_patterns = unname(train_x),
    training_labels = train_labels,
    test_patterns = unname(test_x),
    expected = list(
      classes = full_model$classes,
      counts = full_model$counts,
      priors = full_model$priors,
      zscore_center = full_model$center,
      zscore_sample_sd = full_model$scale,
      scaled_centroids = unname(full_model$centroids),
      scaled_test = unname(full_prediction$scaled),
      scores = unname(full_prediction$scores),
      probabilities = unname(full_prediction$probabilities),
      predicted = full_prediction$predicted
    )
  ),
  swift_cross_validation = list(
    patterns = unname(cv_x),
    labels = cv_labels,
    blocks = cv_blocks,
    expected = list(
      classes = cv_classes,
      folds = cv_folds,
      sample_indices = seq_len(nrow(cv_x)) - 1L,
      probabilities = unname(cv_probabilities),
      predicted = cv_predicted,
      correct = sum(cv_predicted == cv_labels),
      tested_samples = length(cv_labels),
      pooled_accuracy = mean(cv_predicted == cv_labels),
      unweighted_mean_fold_accuracy = mean(vapply(cv_folds, `[[`, numeric(1L), "accuracy"))
    )
  ),
  identity_metric_rdm = list(
    estimand = "mean over all ordered distinct partition pairs of signed squared-Euclidean cross-products",
    condition_names = condition_names,
    partition_means = lapply(seq_len(dim(partition_means)[1L]), function(fold) unname(partition_means[fold, , ])),
    pair_order = lapply(seq_len(nrow(condition_pairs)), function(index) {
      unname(condition_names[condition_pairs[index, ]])
    }),
    ordered_partition_pairs = split(unname(ordered_partition_pairs - 1L), row(ordered_partition_pairs)),
    expected_raw = unname(rdm_raw),
    feature_count = dim(partition_means)[3L],
    expected_feature_normalized = unname(rdm_raw / dim(partition_means)[3L]),
    claim_exclusion = "identity-metric baseline only; not estimated-noise crossnobis"
  ),
  failure_policy = list(
    non_finite_input = "typed failure",
    missing_training_class = "typed failure",
    ill_conditioned_case = "not present in this baseline; use method-specific conditioning court"
  )
)

jsonlite::write_json(
  fixture,
  path = output_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  digits = 17,
  null = "null"
)

if (check_mode) {
  canonical_path <- "docs/scenarios/fixtures/mvpa.migration-parity.v1.r.json"
  generated <- readBin(output_path, what = "raw", n = file.info(output_path)$size)
  canonical <- readBin(canonical_path, what = "raw", n = file.info(canonical_path)$size)
  unlink(output_path)
  if (!identical(generated, canonical)) {
    stop("generated fixture differs from the checked-in receipt", call. = FALSE)
  }
  message("MVPA migration parity fixture is current")
}
