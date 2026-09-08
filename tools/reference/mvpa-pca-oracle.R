#!/usr/bin/env Rscript

# Independent base-R oracle for GlobalDecompositionSuite. The measurement
# retains x2 then x1; PCA is centered, unscaled, and requests two components.
x <- rbind(
  c(-3, 1, 5),
  c(-1, -1, 5),
  c(1, -1, 5),
  c(3, 1, 5)
)
measured <- x[, c(2, 1), drop = FALSE]
fit <- prcomp(measured, center = TRUE, scale. = FALSE, rank. = 2)
singular_values <- fit$sdev * sqrt(nrow(measured) - 1)

stopifnot(isTRUE(all.equal(unname(singular_values), c(sqrt(20), 2), tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(abs(unname(fit$rotation)), rbind(c(0, 1), c(1, 0)), tolerance = 1e-12)))

writeLines(c(
  paste(format(singular_values, digits = 16), collapse = "\t"),
  apply(abs(fit$rotation), 1, function(row) paste(format(row, digits = 16), collapse = "\t"))
))
