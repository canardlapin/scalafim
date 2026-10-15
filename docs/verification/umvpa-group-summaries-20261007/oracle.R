# Independent base-R reference: weighted means, scalar PM root, and dense OLS.
# No Scala implementation helpers or fitted coefficients are imported.
args <- commandArgs(trailingOnly = TRUE)
destination <- if (length(args)) args[[1]] else "."
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
options(digits = 17)
records <- list()
record <- function(name, values) {
  records[[length(records) + 1L]] <<- data.frame(name, index = seq_along(values), value = as.vector(t(values)))
}
effects <- cbind(c(1, 3, 5), c(2, 4, 6))
variances <- cbind(c(1, 4, 1), c(4, 1, 1))
meta <- function(y, v, random) {
  n <- length(y)
  moments <- function(tau) {
    w <- 1 / (v + tau)
    mu <- sum(w * y) / sum(w)
    c(mean = mu, q = sum(w * (y - mu)^2), variance = 1 / sum(w))
  }
  fixed <- moments(0)
  tau <- 0
  if (random && fixed[["q"]] > n - 1) {
    hi <- max(v, var(y))
    while (moments(hi)[["q"]] > n - 1) hi <- hi * 2
    tau <- uniroot(function(t) moments(t)[["q"]] - (n - 1), c(0, hi), tol = 1e-12)$root
  }
  fitted <- moments(tau)
  se <- sqrt(fitted[["variance"]] * if (random) max(1, fitted[["q"]] / (n - 1)) else 1)
  statistic <- fitted[["mean"]] / se
  c(mean = fitted[["mean"]], se = se, tau2 = tau, q = fixed[["q"]],
    i2 = max(0, (fixed[["q"]] - (n - 1)) / fixed[["q"]]),
    p = if (random) 2 * pt(-abs(statistic), n - 1) else 2 * pnorm(-abs(statistic)))
}
for (random in c(FALSE, TRUE)) {
  mode <- if (random) "PM-mKH" else "known-FE"
  for (feature in 1:2) for (component in 1:2) {
    values <- meta(effects[, component] * c(1, 10)[feature], variances[, component] * c(1, 9)[feature], random)
    for (name in names(values)) record(paste(mode, feature, component, name, sep = "/"), values[[name]])
  }
}
record("empirical-mean", colMeans(effects))
record("empirical-variance", apply(effects, 2, var))
u <- c(1, 1, 1, 1, -1, -1, -1, -1)
v <- c(1, 1, -1, -1, 1, 1, -1, -1)
e <- c(1, -1, 1, -1, 1, -1, 1, -1)
f <- c(1, 1, -1, -1, -1, -1, 1, 1)
drift <- c(1, -1, -1, 1, 1, -1, -1, 1)
x <- cbind(u, .5 * u + sqrt(.75) * v)
y <- cbind(7 + 2*x[, 1] + 3*x[, 2] + 4*drift, -3 - x[, 1] + 2*x[, 2] - 2*drift)
truth <- y + cbind(.5 * e, .25 * f)
design <- cbind(1, drift, x)
prediction <- design %*% solve(crossprod(design), crossprod(design, y))
loss <- function(prediction) rowSums(sweep((prediction - truth)^2, 2, c(1, 2), "*"))
full <- loss(prediction)
reduced <- sapply(3:4, function(k) {
  z <- design[, -k]
  loss(z %*% solve(crossprod(z), crossprod(z, y)))
})
improvement <- reduced - full
subjects <- c(rep("A", 6), rep("B", 2))
by.subject <- function(values) t(vapply(c("A", "B"), function(s) colMeans(as.matrix(values)[subjects == s, , drop=FALSE]), numeric(NCOL(values))))
subject.improvement <- by.subject(improvement)
record("subject-full-loss", by.subject(full))
record("subject-reduced-loss", by.subject(reduced))
record("subject-improvements", subject.improvement)
record("group-improvements", colMeans(subject.improvement))
record("group-improvement-variance", apply(subject.improvement, 2, var))
record("pooled-row-improvements", colMeans(improvement))
write.table(do.call(rbind, records), file.path(destination, "expected.tsv"), sep = "\t", row.names = FALSE, quote = FALSE)
writeLines(capture.output(sessionInfo()), file.path(destination, "R-session.txt"))
