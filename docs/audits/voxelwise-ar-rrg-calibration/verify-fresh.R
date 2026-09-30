#!/usr/bin/env Rscript
# Independently recompute all fresh coverage counts and exact simultaneous limits.
# Usage: Rscript verify-fresh.R NEW_OUTPUT.json INPUT.jsonl[.gz] ...
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 2L, !file.exists(args[[1L]]))
read_rows <- function(path) {
  con <- if (grepl("\\.gz$", path)) gzfile(path) else file(path)
  on.exit(close(con))
  lapply(readLines(con), jsonlite::fromJSON, simplifyVector = TRUE)
}
rows <- unlist(lapply(args[-1L], read_rows), recursive = FALSE)
scenarios <- c("baseline", "strong_ar", "multiple_runs", "censor_continuous")
labels <- c(as.vector(t(outer(0:1, 0:2, function(r,v) paste0("coefficient_",r,"_voxel_",v)))),
            paste0("difference_voxel_",0:2), paste0("average_voxel_",0:2))
alpha <- 0.05 / (48 * 3)
counts <- table(vapply(rows, function(r) r$scenario, character(1)))
stopifnot(setequal(names(counts), scenarios), length(unique(counts)) == 1L)
n <- as.integer(counts[1L]); stopifnot(n %in% c(500L,1000L,2000L))
stopifnot(all(vapply(rows, function(r) r$status == "ok" && r$completed_replicates == 999L, logical(1))))
result <- list()
for (scenario in scenarios) {
  group <- Filter(function(r) r$scenario == scenario, rows)
  stopifnot(setequal(vapply(group, function(r) r$dataset, integer(1)), 100000L + 0:(n-1L)))
  for (j in seq_along(labels)) {
    covered <- sum(vapply(group, function(r) r$lower[j] <= r$truth[j] && r$truth[j] <= r$upper[j], logical(1)))
    lower <- if (covered == 0L) 0 else qbeta(alpha/2, covered, n-covered+1)
    upper <- if (covered == n) 1 else qbeta(1-alpha/2, covered+1, n-covered)
    result[[length(result)+1L]] <- list(scenario=scenario, coordinate=labels[j], covered=covered,
      attempts=n, coverage=covered/n, simultaneous_interval=c(lower,upper),
      outside_band=upper < .92 || lower > .98, inside_band=lower >= .92 && upper <= .98)
  }
}
rejected <- any(vapply(result,function(x) x$outside_band,logical(1)))
accepted <- n == 2000L && all(vapply(result,function(x) x$inside_band,logical(1)))
verdict <- if (rejected) "prespecified_calibration_rejection" else if (accepted) "accepted_declared_approximate_band" else if (n==2000L) "inconclusive" else "continue_to_next_checkpoint"
jsonlite::write_json(list(r_version=as.character(getRversion()),verdict=verdict, coordinates=result),args[[1L]],digits=17L,auto_unbox=TRUE,pretty=TRUE)
