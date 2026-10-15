# Known-control qualification only. Original eleven criteria and margins retained.
# Usage: Rscript analyze.R records.jsonl outdir protocol.json [other-platform-records.jsonl]
suppressMessages(library(jsonlite))
args <- commandArgs(TRUE)
records <- args[1]; outdir <- args[2]; protocol <- fromJSON(args[3], simplifyVector = FALSE)
other <- if (length(args) >= 4 && nzchar(args[4]) && args[4] != "-") args[4] else NA
expected_cells <- vapply(fromJSON(file.path(dirname(args[3]), "cells.json"), simplifyVector=FALSE), function(x) x$cell, "")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
acc <- protocol$control_acceptance
read_records <- function(path) {
  lines <- readLines(path)
  trials <- sub("^FGLS_STUDY_TRIAL ", "", grep("^FGLS_STUDY_TRIAL ", lines, value = TRUE))
  failures <- grep("^FGLS_STUDY_FAILURE ", lines, value = TRUE)
  rows <- lapply(trials, function(line) { x <- fromJSON(line)
    stopifnot(identical(x$role, "independent-cmrg-known-control-qualification"))
    data.frame(cell = x$cell, profile = x$profile, engine = x$engine, replicate = x$replicate,
               phiError1 = x$phiError[1], phiError2 = x$phiError[2], phiSquaredError = x$phiSquaredError,
               whiteness = x$whiteness, nullEstimate = x$nullEstimate, nullVariance = x$nullVariance,
               tDf = x$tDf, fDf = x$fDf, signalEstimate = x$signalEstimate, signalVariance = x$signalVariance,
               tPValue = if (is.null(x$tPValue)) NA else x$tPValue, fPValue = if (is.null(x$fPValue)) NA else x$fPValue,
               coverPValue = if (is.null(x$coverPValue)) NA else x$coverPValue,
               tRejected = x$tRejected, fRejected = x$fRejected, covered = x$covered, stringsAsFactors = FALSE) })
  d <- do.call(rbind, rows)
  stopifnot(!is.null(d), nrow(d)==length(expected_cells)*protocol$replicates,
            !anyDuplicated(d[c("cell", "engine", "replicate")]),
            setequal(unique(d$cell), expected_cells),
            identical(unique(d$profile), "control"), identical(unique(d$engine), "known-continuous"),
            all(vapply(split(d$replicate,d$cell), function(x) identical(sort(as.integer(x)),0:(protocol$replicates-1L)), FALSE)),
            all(vapply(d[vapply(d,is.numeric,FALSE)], function(x) all(is.finite(x)), FALSE)))
  list(trials = d, failures = length(failures))
}
data <- read_records(records)
d <- data$trials
cp <- function(k, n, conf) { a <- (1 - conf) / 2
  c(if (k == 0) 0 else qbeta(a, k, n - k + 1), if (k == n) 1 else qbeta(1 - a, k + 1, n - k)) }
set.seed(protocol$bootstrap_seed)
B <- protocol$bootstrap$resamples
cells <- unique(d$cell); engines <- unique(d$engine)
stopifnot(identical(unique(d$profile), "control"),identical(unique(d$engine), "known-continuous"))
expected_n <- protocol$replicates
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
stopifnot(length(results)==5L,all(vapply(results,function(r)r$n,0)==2048))
summary <- list(role="known-control qualification; no candidate or confirmation",results=results,
  control_pass=all(vapply(results,function(r)r$all_pass,FALSE)),scientific_admission=FALSE)
if(!is.na(other)) {
 o <- read_records(other); stopifnot(o$failures==0)
 m <- merge(d,o$trials,by=c("cell","engine","replicate"),suffixes=c("", ".o"))
 stopifnot(nrow(m)==10240,nrow(d)==10240,nrow(o$trials)==10240)
 fields <- c("nullEstimate","nullVariance","signalEstimate","signalVariance","tPValue","fPValue","coverPValue","whiteness")
 gaps <- sapply(fields,function(f)max(abs(m[[f]]-m[[paste0(f,".o")]])/(1+abs(m[[f]]))))
 same <- all(m$tRejected==m$tRejected.o & m$fRejected==m$fRejected.o & m$covered==m$covered.o)
 summary$platform_parity <- list(max_scaled_difference=as.list(gaps),identical_decisions=same)
 summary$control_pass <- summary$control_pass && same && max(gaps)<=protocol$numeric_parity_tolerance
}
writeLines(toJSON(summary,auto_unbox=TRUE,pretty=TRUE,digits=NA),file.path(outdir,"summary.json"))
print(data.frame(cell=vapply(results,function(r)r$cell,""),passed=vapply(results,function(r)r$passed,0),all_pass=vapply(results,function(r)r$all_pass,FALSE)))
if(!summary$control_pass)quit(status=1)
