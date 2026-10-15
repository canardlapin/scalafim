# Independent base-R fixtures for the group summary adapter, no native helpers.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
out <- args[[1L]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)

y <- c(-1, 1, 3)
v <- c(1, 1, 1)
w <- 1 / v
mean_fe <- sum(w * y) / sum(w)
q_fe <- sum(w * (y - mean_fe)^2)
df <- length(y) - 1
# Equal within-subject variance gives the exact PM root analytically.
tau_squared <- max(0, sum((y - mean(y))^2) / df - v[[1L]])
wr <- 1 / (v + tau_squared)
mean_re <- sum(wr * y) / sum(wr)
q_re <- sum(wr * (y - mean_re)^2)
mkh_scale <- max(1, q_re / df)
expected <- data.frame(
  key = c("subject_count", "mean", "sample_variance", "fixed_se", "q_fixed",
          "tau_squared_PM", "i_squared", "mixed_se_mKH", "mixed_df",
          "equal_subject_loss", "pooled_row_loss"),
  value = c(length(y), mean_fe, var(y), sqrt(1/sum(w)), q_fe,
            tau_squared, max(0, (q_fe-df)/q_fe), sqrt(mkh_scale/sum(wr)), df,
            mean(c(.25, 2.25)), weighted.mean(c(.25, 2.25), c(2,6))))
stopifnot(abs(tau_squared-3) < 1e-12, abs(q_fe-8) < 1e-12,
          abs(q_re-2) < 1e-12)
write.table(expected, file.path(out, "group-oracle.tsv"), sep="\t", quote=FALSE,
            row.names=FALSE)
capture.output(sessionInfo(), file=file.path(out,"group-oracle-R-session.txt"))
cat("PASS: independent equal-variance group and subject-weighting arithmetic\n")
