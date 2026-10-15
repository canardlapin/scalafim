# Third, package-based reference for the two hand-written M5.03 group oracles.
# metafor is independent of ScalaFIM and of both base-R oracle scripts.
# Usage: Rscript metafor-crosscheck.R [OUTPUT_DIR]   (needs metafor >= 4.0)
args <- commandArgs(trailingOnly = TRUE)
destination <- if (length(args)) args[[1]] else "."
suppressMessages(library(metafor))
options(digits = 17)
cases <- list()
add <- function(id, y, v) cases[[id]] <<- list(y = y, v = v)
# Main oracle (docs/verification/umvpa-group-summaries-20261007/oracle.R).
effects <- cbind(c(1, 3, 5), c(2, 4, 6)); variances <- cbind(c(1, 4, 1), c(4, 1, 1))
for (feature in 1:2) for (component in 1:2)
  add(sprintf("main/%d/%d", feature, component),
      effects[, component] * c(1, 10)[feature], variances[, component] * c(1, 9)[feature])
# Unmerged branch oracle (47e14319 group-oracle.R): equal known variances.
# The salvage test applies it to two measurements and two task coordinates:
# effects ((-1, 1, 3) + k) * (1, 10)[feature], variances (1, 9)[feature].
for (feature in 1:2) for (component in 1:2)
  add(sprintf("branch/%d/%d", feature, component),
      (c(-1, 1, 3) + component - 1) * c(1, 10)[feature], rep(c(1, 9)[feature], 3))
rows <- list()
for (id in names(cases)) {
  y <- cases[[id]]$y; v <- cases[[id]]$v
  fe <- rma(yi = y, vi = v, method = "FE")
  pm <- rma(yi = y, vi = v, method = "PM", test = "adhoc", control = list(tol = 1e-14))
  dl <- rma(yi = y, vi = v, method = "DL")
  rows[[id]] <- data.frame(case = id,
    fe_mean = fe$b[[1]], fe_se = fe$se, fe_p = fe$pval, q = fe$QE, i2_fe = fe$I2 / 100,
    pm_tau2 = pm$tau2, pm_mean = pm$b[[1]], pm_se_mkh = pm$se, pm_p_mkh = pm$pval,
    pm_i2_metafor = pm$I2 / 100, dl_tau2 = dl$tau2, dl_i2_metafor = dl$I2 / 100)
}
out <- do.call(rbind, rows)
write.table(out, file.path(destination, "metafor-crosscheck.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
writeLines(capture.output(sessionInfo()), file.path(destination, "metafor-R-session.txt"))
print(out, row.names = FALSE)
