# Pilot description only. This is not the frozen confirmation adjudicator.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args) == 3L)
counts <- read.delim(args[[1L]], stringsAsFactors=FALSE, check.names=FALSE)
stopifnot(all(counts$datasets == 200), all(counts$successes >= 0),
          all(counts$successes <= 200), all(counts$successes == floor(counts$successes)))
rows <- lapply(seq_len(nrow(counts)), function(i) {
  row <- counts[i, ]; k <- row$successes; n <- row$datasets
  tail <- if (row$sidedness == "lower-one-sided") 1-row$confidence else (1-row$confidence)/2
  lower <- if (k == 0) 0 else qbeta(tail,k,n-k+1)
  upper <- if (row$sidedness == "lower-one-sided" || k == n) 1 else qbeta(1-tail,k+1,n-k)
  data.frame(row, rate=k/n, lower=lower, upper=upper,
             scope="pilot-descriptive-only", confirmation_qualified=FALSE)
})
options(digits=17)
write.table(do.call(rbind,rows),args[[2L]],sep="\t",quote=FALSE,row.names=FALSE)
writeLines(c("Independent R dataset-binomial description; no confirmation pass/fail decision.",
             capture.output(sessionInfo())),args[[3L]])
