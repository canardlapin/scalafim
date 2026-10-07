#!/usr/bin/env Rscript
# External, independent rate/interval adjudicator. No production statistic calls.
# Input counts are independent DATASETS per predeclared member/cell, never folds
# or pooled voxels. Bootstrap input is one metric value per independent dataset.
args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default = NULL) {
  at <- match(name, args)
  if (is.na(at)) default else args[[at + 1L]]
}
counts_path <- option("--counts")
output <- option("--output")
if (is.null(counts_path) || is.null(output)) stop("need --counts and --output")
counts <- read.delim(counts_path, check.names = FALSE, stringsAsFactors = FALSE)
needed <- c("cell", "member", "metric", "successes", "datasets", "expected_datasets",
            "failures", "missing", "estimand_contradictions")
if (!all(needed %in% names(counts))) stop("missing independent-dataset counts/provenance columns")
cp <- function(k, n, confidence, one_sided = FALSE) {
  if (n <= 0 || k < 0 || k > n || k != floor(k) || n != floor(n)) stop("invalid binomial counts")
  tail <- if (one_sided) 1-confidence else (1-confidence)/2
  c(if (k == 0) 0 else qbeta(tail, k, n-k+1),
    if (k == n) 1 else qbeta(1-tail, k+1, n-k))
}
rows <- vector("list", nrow(counts))
for (i in seq_len(nrow(counts))) {
  row <- counts[i, ]
  if (row$datasets < 0 || row$successes < 0 || row$successes > row$datasets ||
      row$datasets != floor(row$datasets) || row$successes != floor(row$successes))
    stop("invalid retained dataset counts")
  blocked <- row$datasets != row$expected_datasets || row$failures != 0 || row$missing != 0 || row$estimand_contradictions != 0
  if (row$metric %in% c("type-i", "fwer", "coverage")) {
    if (row$expected_datasets != 10000) stop("null/coverage requires exactly10000 independent datasets")
    interval <- if (blocked) c(NA_real_,NA_real_) else cp(row$successes, row$datasets, .90)
    bounds <- if (row$metric == "coverage") c(.935, .965) else c(.035, .065)
    passes <- !blocked && interval[[1L]] >= bounds[[1L]] && interval[[2L]] <= bounds[[2L]]
  } else if (row$metric == "standard-power") {
    if (row$expected_datasets != 5000) stop("alternative power requires exactly5000 independent datasets")
    interval <- if (blocked) c(NA_real_,NA_real_) else cp(row$successes, row$datasets, .95, TRUE)
    bounds <- c(.80, 1); passes <- !blocked && interval[[1L]] >= .80
  } else if (row$metric == "unsupported-refusal") {
    if (row$expected_datasets != 10000) stop("refusal requires exactly10000 datasets")
    interval <- if (blocked) c(NA_real_,NA_real_) else c(row$successes/row$datasets, row$successes/row$datasets)
    bounds <- c(1, 1); passes <- !blocked && row$successes == 10000
  } else stop("unsupported metric; do not silently adjudicate a different claim")
  rows[[i]] <- data.frame(cell=row$cell, member=row$member, metric=row$metric,
    datasets=row$datasets, successes=row$successes, lower=interval[[1L]], upper=interval[[2L]],
    required_lower=bounds[[1L]], required_upper=bounds[[2L]], blocked=blocked, passes=passes)
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write.table(do.call(rbind, rows), output, sep="\t", quote=FALSE, row.names=FALSE,na="unavailable")

metrics_path <- option("--dataset-metrics")
if (!is.null(metrics_path)) {
  metrics <- read.delim(metrics_path, check.names=FALSE, stringsAsFactors=FALSE)
  state_path <- option("--bootstrap-state")
  if (is.null(state_path)) stop("bootstrap requires its recorded named child R state")
  states <- read.delim(state_path,check.names=FALSE,stringsAsFactors=FALSE)
  if (!all(c("cell","metric",paste0("r_state",1:6)) %in% names(states)))
    stop("bootstrap requires one named child state per cell/metric")
  if (!all(c("cell","metric","dataset_index","value") %in% names(metrics))) stop("invalid dataset-level bootstrap table")
  groups <- split(metrics, interaction(metrics$cell,metrics$metric,drop=TRUE))
  results <- list()
  for (name in names(groups)) {
    group <- groups[[name]]
    bound <- states[states$cell == group$cell[[1L]] & states$metric == group$metric[[1L]],]
    if (nrow(bound) != 1) stop("missing/duplicate cell-specific bootstrap child state")
    state <- as.numeric(bound[1L,paste0("r_state",1:6)])
    if (any(state <= 0) || any(state > 2147483646)) stop("invalid bootstrap R state")
    RNGkind("L'Ecuyer-CMRG",normal.kind="Inversion",sample.kind="Rejection")
    assign(".Random.seed",c(10407L,as.integer(state)),envir=.GlobalEnv)
    if (anyDuplicated(group$dataset_index) || !all(is.finite(group$value))) stop("duplicate/nonfinite dataset metrics")
    n <- nrow(group)
    if (!n %in% c(5000L,10000L)) stop("bootstrap population must have frozen dataset count")
    draws <- replicate(9999L, mean(group$value[sample.int(n,n,replace=TRUE)]))
    metric <- group$metric[[1L]]
    if (metric == "fdp") {
      interval <- c(0,unname(quantile(draws,.95,type=1)))
      passes <- interval[[2L]] <= .060
    } else if (metric == "standardized-bias") {
      interval <- unname(quantile(draws,c(.05,.95),type=1))
      passes <- interval[[1L]] >= -.05 && interval[[2L]] <= .05
    } else if (metric == "strong-minus-weak") {
      interval <- unname(quantile(draws,c(.025,.975),type=1))
      passes <- interval[[1L]] > 0
    } else stop("variance-ratio/effective-df bootstrap needs paired effect/variance columns; not silently substituted by a mean bootstrap")
    results[[name]] <- data.frame(cell=group$cell[[1L]],metric=metric,datasets=n,
                                  bootstrap_draws=9999,lower=interval[[1L]],upper=interval[[2L]],passes=passes)
  }
    write.table(do.call(rbind,results),paste0(output,".bootstrap.tsv"),sep="\t",quote=FALSE,row.names=FALSE)
}
cat("Independent adjudication receipt written; failures/blocked cells remain explicit\n")
