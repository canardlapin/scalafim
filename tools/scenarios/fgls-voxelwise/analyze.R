# Independent analysis of retained FGLS voxelwise study records.
# Usage: Rscript analyze.R records.jsonl outdir protocol.json [other-platform-records.jsonl] [candidate]
suppressMessages(library(jsonlite))
args <- commandArgs(TRUE)
records <- args[1]; outdir <- args[2]; protocol <- fromJSON(args[3], simplifyVector = FALSE)
other <- if (length(args) >= 4 && nzchar(args[4]) && args[4] != "-") args[4] else NA
candidate <- if (length(args) >= 5) args[5] else NA
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
acc <- protocol$acceptance
read_records <- function(path) {
  lines <- readLines(path)
  trials <- sub("^FGLS_STUDY_TRIAL ", "", grep("^FGLS_STUDY_TRIAL ", lines, value = TRUE))
  failures <- grep("^FGLS_STUDY_FAILURE ", lines, value = TRUE)
  rows <- lapply(trials, function(line) { x <- fromJSON(line)
    data.frame(cell = x$cell, profile = x$profile, engine = x$engine, replicate = x$replicate,
               phiError1 = x$phiError[1], phiError2 = x$phiError[2], phiSquaredError = x$phiSquaredError,
               whiteness = x$whiteness, nullEstimate = x$nullEstimate, nullVariance = x$nullVariance,
               tDf = x$tDf, fDf = x$fDf, signalEstimate = x$signalEstimate, signalVariance = x$signalVariance,
               tPValue = if (is.null(x$tPValue)) NA else x$tPValue, fPValue = if (is.null(x$fPValue)) NA else x$fPValue,
               coverPValue = if (is.null(x$coverPValue)) NA else x$coverPValue,
               tRejected = x$tRejected, fRejected = x$fRejected, covered = x$covered, stringsAsFactors = FALSE) })
  list(trials = do.call(rbind, rows), failures = length(failures))
}
data <- read_records(records)
d <- data$trials
cp <- function(k, n, conf) { a <- (1 - conf) / 2
  c(if (k == 0) 0 else qbeta(a, k, n - k + 1), if (k == n) 1 else qbeta(1 - a, k + 1, n - k)) }
set.seed(protocol$seed + protocol$profiles$bootstrap$domain)
B <- protocol$profiles$bootstrap$resamples
cells <- unique(d$cell); engines <- unique(d$engine)
expected_n <- protocol$profiles[[unique(d$profile)]]$replicates
boot_idx <- lapply(cells, function(cell) { n <- length(unique(d$replicate[d$cell == cell])); replicate(B, sample.int(n, n, TRUE)) })
names(boot_idx) <- cells
evaluate <- function(cell, engine) {
  a <- d[d$cell == cell & d$engine == engine, ]; a <- a[order(a$replicate), ]; n <- nrow(a)
  if (n == 0) return(NULL)
  conf <- acc$equivalence_interval_confidence
  ti <- cp(sum(a$tRejected), n, conf); fi <- cp(sum(a$fRejected), n, conf); ci <- cp(sum(a$covered), n, conf)
  ratio <- mean(a$nullVariance) / var(a$nullEstimate)
  idx <- boot_idx[[cell]]
  boots <- apply(idx, 2, function(i) mean(a$nullVariance[i]) / var(a$nullEstimate[i]))
  ri <- unname(quantile(boots, c(0.05, 0.95)))
  rmse <- sqrt(mean(a$phiSquaredError))
  half <- function(x) qt(0.95, n - 1) * sd(x) / sqrt(n)
  b1 <- mean(a$phiError1); b2 <- mean(a$phiError2)
  known <- d[d$cell == cell & d$engine == (if (grepl("^existing", engine)) "existing-known-restart" else "known-continuous"), ]
  wexcess <- mean(a$whiteness) - mean(known$whiteness)
  std <- mean(a$nullEstimate) / sd(a$nullEstimate)
  estimated <- !grepl("known", engine)
  crit <- list(
    no_refusals = data$failures == 0,
    complete = n == expected_n,
    null_t = ti[1] >= acc$null_rejection_interval[[1]] && ti[2] <= acc$null_rejection_interval[[2]],
    null_f = fi[1] >= acc$null_rejection_interval[[1]] && fi[2] <= acc$null_rejection_interval[[2]],
    coverage = ci[1] >= acc$coverage_interval[[1]] && ci[2] <= acc$coverage_interval[[2]],
    variance_ratio = ri[1] >= acc$variance_ratio_interval[[1]] && ri[2] <= acc$variance_ratio_interval[[2]],
    effect_bias = abs(std) + qt(0.95, n - 1) / sqrt(n) <= acc$standardized_effect_bias_interval[[2]],
    ar_rmse = if (estimated) rmse <= acc$AR_RMSE_max else TRUE,
    ar_bias_1 = if (estimated) abs(b1) + half(a$phiError1) <= acc$AR_mean_bias_interval[[2]] else TRUE,
    ar_bias_2 = if (estimated) abs(b2) + half(a$phiError2) <= acc$AR_mean_bias_interval[[2]] else TRUE,
    whiteness = mean(a$whiteness) <= acc$whiteness_mean_abs_max && (!estimated || wexcess <= acc$whiteness_excess_over_known_max))
  list(cell = cell, engine = engine, n = n,
       t_rejections = sum(a$tRejected), t_interval = ti, f_rejections = sum(a$fRejected), f_interval = fi,
       covered = sum(a$covered), coverage_interval = ci, variance_ratio = ratio, variance_ratio_interval = ri,
       ar_rmse = rmse, ar_bias = c(b1, b2), whiteness = mean(a$whiteness), whiteness_excess = wexcess,
       standardized_effect_bias = std, mean_t_df = mean(a$tDf), mean_f_df = mean(a$fDf),
       mean_reported_variance = mean(a$nullVariance), estimator_variance = var(a$nullEstimate),
       criteria = crit, passed = sum(unlist(crit)), all_pass = all(unlist(crit)))
}
results <- list(); for (cell in cells) for (engine in engines) { r <- evaluate(cell, engine); if (!is.null(r)) results[[length(results) + 1]] <- r }
# Paired decomposition against the known-continuous comparator (same datasets).
paired <- list()
for (cell in cells) {
  k <- d[d$cell == cell & d$engine == "known-continuous", ]; k <- k[order(k$replicate), ]
  idx <- boot_idx[[cell]]
  for (engine in setdiff(engines, "known-continuous")) {
    a <- d[d$cell == cell & d$engine == engine, ]; a <- a[order(a$replicate), ]
    if (nrow(a) != nrow(k) || nrow(a) == 0) next
    rep_ratio <- function(i) mean(a$nullVariance[i]) / mean(k$nullVariance[i])
    est_ratio <- function(i) var(a$nullEstimate[i]) / var(k$nullEstimate[i])
    br <- apply(idx, 2, rep_ratio); be <- apply(idx, 2, est_ratio)
    paired[[length(paired) + 1]] <- list(cell = cell, engine = engine,
      reported_variance_ratio = rep_ratio(seq_len(nrow(a))), reported_interval = unname(quantile(br, c(0.05, 0.95))),
      estimator_variance_ratio = est_ratio(seq_len(nrow(a))), estimator_interval = unname(quantile(be, c(0.05, 0.95))))
  }
}
admission <- vapply(protocol$cells, function(c) if (c$role == "admission") c$id else NA_character_, "")
admission <- admission[!is.na(admission)]
candidates <- c("run-continuous-conditional", "run-continuous-kr", "pooled-continuous-conditional", "pooled-continuous-kr")
score <- sapply(candidates, function(e) sum(sapply(results, function(r) if (r$engine == e && r$cell %in% admission) r$passed else 0)))
selected <- candidates[which.max(score)] # ties resolve to the declared order
summary <- list(profile = unique(d$profile), records = records, failures = data$failures, results = results, paired = paired,
                candidate_scores = as.list(score), selection_rule_choice = selected)
if (!is.na(candidate)) {
  verdicts <- sapply(admission, function(cell) { r <- Filter(function(r) r$cell == cell && r$engine == candidate, results)
    length(r) == 1 && r[[1]]$all_pass })
  summary$candidate <- candidate; summary$candidate_cell_pass <- as.list(verdicts)
  summary$scientific_pass <- all(verdicts)
}
if (!is.na(other)) {
  o <- read_records(other)$trials
  key <- function(x) paste(x$cell, x$engine, x$replicate)
  m <- merge(d, o, by = c("cell", "engine", "replicate"), suffixes = c("", ".o"))
  num <- c("phiSquaredError", "whiteness", "nullEstimate", "nullVariance", "tDf", "fDf", "signalEstimate", "signalVariance")
  diffs <- sapply(num, function(v) max(abs(m[[v]] - m[[paste0(v, ".o")]]) / (1 + abs(m[[v]]))))
  same <- all(m$tRejected == m$tRejected.o & m$fRejected == m$fRejected.o & m$covered == m$covered.o)
  summary$cross_platform <- list(matched = nrow(m), this = nrow(d), other = nrow(o), max_scaled_difference = as.list(diffs),
                                 identical_decisions = same)
}
writeLines(toJSON(summary, auto_unbox = TRUE, digits = NA, pretty = TRUE), file.path(outdir, "summary.json"))
tab <- do.call(rbind, lapply(results, function(r) data.frame(cell = r$cell, engine = r$engine, n = r$n, tRej = r$t_rejections,
  fRej = r$f_rejections, cov = r$covered, vratio = round(r$variance_ratio, 4), vlo = round(r$variance_ratio_interval[1], 4),
  rmse = round(r$ar_rmse, 4), white = round(r$whiteness, 4), df = round(r$mean_t_df, 1), passed = r$passed, all = r$all_pass)))
write.csv(tab, file.path(outdir, "table.csv"), row.names = FALSE)
print(tab, row.names = FALSE)
if (!is.na(candidate)) quit(status = if (isTRUE(summary$scientific_pass)) 0 else 1)
