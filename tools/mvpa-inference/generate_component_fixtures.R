#!/usr/bin/env Rscript
# Independent base-R oracle for ComponentConfirmationSuite. No production code.
u <- rep(c(1, -1), each = 4)
v <- rep(c(1, -1), each = 2, times = 2)
e <- rep(c(1, -1), 4)
f <- u * v
drift <- v * e
x1 <- u
x2 <- .5 * u + sqrt(.75) * v
train <- data.frame(x1, x2, drift,
  y1 = 7 + 2 * x1 + 3 * x2 + 4 * drift,
  y2 = -3 - x1 + 2 * x2 - 2 * drift)
test <- train
test$y1 <- test$y1 + .5 * e
test$y2 <- test$y2 + .25 * f
full <- lm(cbind(y1, y2) ~ drift + x1 + x2, train)
reduced <- list(lm(cbind(y1, y2) ~ drift + x2, train),
                lm(cbind(y1, y2) ~ drift + x1, train))
loss <- function(model) {
  residual <- predict(model, test) - as.matrix(test[c('y1', 'y2')])
  drop(residual^2 %*% c(1, 2))
}
full_loss <- loss(full)
improvement <- vapply(reduced, function(model) loss(model) - full_loss, numeric(8))
unit <- c(rep(1, 6), rep(2, 2))
equal_units <- apply(improvement, 2, function(delta) mean(tapply(delta, unit, mean)))
association <- c(cor(resid(lm(x1 ~ drift, test)), resid(lm(y1 ~ drift, test))),
                 cor(resid(lm(x2 ~ drift, test)), resid(lm(y2 ~ drift, test))))
stopifnot(max(abs(colMeans(improvement) - c(4.5, 12.75))) < 1e-12,
          abs(mean(full_loss) - .375) < 1e-12,
          max(abs(equal_units - c(3.3066243270259355, 12.172649730810374))) < 1e-12,
          max(abs(association - c(3.5 / sqrt(19.25), 1.5 / sqrt(3.0625)))) < 1e-12)
options(digits = 17)
print(list(R = R.version.string, full_mean_loss = mean(full_loss),
  equal_row_improvement = colMeans(improvement), equal_unit_improvement = equal_units,
  association = association, reduced_coefficients = lapply(reduced, coef)))
