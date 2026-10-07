#!/usr/bin/env Rscript
# Algebraic/stream regression checks, not the 10,000-dataset simulator QA.
source("tools/mvpa-inference/rank_population.R")
set.seed(2741)
x <- matrix(rnorm(80 * 6), 80, 6)
innovation <- matrix(rnorm(80 * 4), 80, 4)
y <- innovation
rho <- c(.5, .3, .2, 0)
for (j in 1:4) y[,j] <- rho[j]*x[,j] + sqrt(1-rho[j]^2)*innovation[,j]
state <- .Random.seed
set.seed(2741)
old <- rank_population(80, 6, 4, rho)
stopifnot(identical(x, old$x), identical(y, old$y), identical(state, .Random.seed))
set.seed(2741)
with_nuisance <- rank_population(80, 6, 4, rho, "three-column")
stopifnot(identical(old$x, with_nuisance$residual_x), identical(old$y, with_nuisance$residual_y))
cov <- with_nuisance$covariance
conditional <- cov[1:10,1:10] - cov[1:10,11:12] %*% solve(cov[11:12,11:12], cov[11:12,1:10])
stopifnot(max(abs(conditional - .68 * old$covariance)) < 1e-14,
          max(abs(diag(cov) - 1)) < 1e-14,
          max(abs(cov[1:10,11:12] - .4)) < 1e-14,
          min(eigen(cov, symmetric = TRUE)$values) > 0)
resid <- function(a) qr.resid(qr(with_nuisance$nuisance), a)
stopifnot(max(abs(resid(with_nuisance$x) - sqrt(.68)*resid(old$x))) < 1e-13,
          max(abs(resid(with_nuisance$y) - sqrt(.68)*resid(old$y))) < 1e-13)
cat("PASS: intercept draws unchanged, nuisance covariance/conditional truth and residual construction agree.\n")
