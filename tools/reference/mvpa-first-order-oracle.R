#!/usr/bin/env Rscript

# Independent base-R oracle for FirstOrderAnalysisSuite. Rows are effects,
# columns are neural coordinates, Q is query-by-effect, and the measurement
# preserves coordinates z then x.
b1 <- rbind(
  a = c(1, 2, 3),
  b = c(4, 5, 6),
  unused = c(99, 99, 99)
)
b2 <- rbind(
  a = c(3, 4, 5),
  b = c(8, 8, 10),
  unused = c(-99, -99, -99)
)
q <- rbind(
  `b-minus-a` = c(-1, 1, 0),
  `a-level` = c(1, 0, 0)
)
weights <- c(1, 3) / 4
measurement <- c(3, 1)

answer <- (weights[[1]] * q %*% b1 + weights[[2]] * q %*% b2)[, measurement]
stopifnot(isTRUE(all.equal(unname(answer), rbind(c(4.5, 4.5), c(4.5, 2.5)))))
write.table(answer, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
