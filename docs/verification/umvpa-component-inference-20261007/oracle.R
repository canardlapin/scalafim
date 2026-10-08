#!/usr/bin/env Rscript
# Independent base-R linear-loss reference; no Scala or production helper calls.
options(digits=17)
set.seed(20261007)
u <- rep(c(1,-1), each=4)
v <- rep(c(1,-1), each=2, times=2)
e <- rep(c(1,-1), 4)
drift <- v*e
d <- data.frame(x1=u, x2=.5*u+sqrt(.75)*v, drift=drift)
d$y1 <- 7+2*d$x1+3*d$x2+4*drift
d$y2 <- -3-d$x1+2*d$x2-2*drift
full <- predict(lm(cbind(y1,y2)~drift+x1+x2,d), d)
reduced <- list(predict(lm(cbind(y1,y2)~drift+x2,d),d),
                predict(lm(cbind(y1,y2)~drift+x1,d),d))
observed <- as.matrix(d[c('y1','y2')])+cbind(.5*e,.25*u*v)
metric <- c(1,2)
response_cov <- matrix(c(.25,.06,.06,.0625),2,2)
gamma <- kronecker(diag(8),response_cov)
reference <- function(unit) {
  counts <- table(unit)
  weights <- 1/(length(counts)*as.numeric(counts[as.character(unit)]))
  gradients <- t(vapply(reduced,function(pred) as.vector(t(2*sweep(sweep(full-pred,2,metric,'*'),1,weights,'*'))),numeric(16)))
  constants <- vapply(reduced,function(pred) sum(weights*drop((pred^2-full^2)%*%metric)),numeric(1))
  improvements <- vapply(reduced,function(pred) sum(weights*drop(((observed-pred)^2-(observed-full)^2)%*%metric)),numeric(1))
  covariance <- gradients%*%gamma%*%t(gradients)
  stopifnot(max(abs(improvements-(constants+gradients%*%as.vector(t(observed)))))<1e-12)
  # An actual response mean with BOTH affine improvement means exactly zero.
  null_mean <- drop(-t(gradients)%*%solve(gradients%*%t(gradients),constants))
  stopifnot(max(abs(constants+gradients%*%null_mean))<1e-11)
  responses <- matrix(null_mean,16,10000)+t(chol(gamma))%*%matrix(rnorm(160000),16,10000)
  null_improvements <- constants+gradients%*%responses
  se <- sqrt(diag(covariance))
  z <- sweep(null_improvements,1,se,'/')
  rejection <- rowSums(z>qnorm(.95))
  coverage <- rowSums(abs(z)<=qnorm(.975))
  print(list(weights=weights,improvement=improvements,gradient=gradients,
    covariance=covariance,standard_error=se,confidence_lower=improvements-qnorm(.975)*se,
    confidence_upper=improvements+qnorm(.975)*se,candidate_reject=improvements>qnorm(.95)*se,
    null_rejections=rejection,null_interval_coverage=coverage,
    variance_ratio=apply(null_improvements,1,var)/diag(covariance)))
}
print(list(R=R.version.string,seed=20261007,null_datasets=10000,
  one_sided_critical=qnorm(.95),two_sided_critical=qnorm(.975)))
reference(1:8)
unequal_units <- c(rep(1,6),rep(2,2))
unequal <- reference(unequal_units)
known_block_row_cov <- diag(8)+.0625*outer(unequal_units,unequal_units,'==')
known_block_response_cov <- kronecker(known_block_row_cov,response_cov)
block_mean_cov <- unequal$gradient%*%known_block_response_cov%*%t(unequal$gradient)
print(list(within_unit_row_factor=.25,within_unit_mean_covariance=block_mean_cov,
  standard_error=sqrt(diag(block_mean_cov)),
  lower95=unequal$improvement-qnorm(.975)*sqrt(diag(block_mean_cov)),
  upper95=unequal$improvement+qnorm(.975)*sqrt(diag(block_mean_cov))))
full_offset <- 1
reduced_offset <- 0
outcome_offset <- 1e8
rounded_difference <- (outcome_offset-reduced_offset)^2-(outcome_offset-full_offset)^2
affine_difference <- (full_offset-reduced_offset)*((outcome_offset-reduced_offset)+(outcome_offset-full_offset))
stopifnot(affine_difference == 199999999, rounded_difference == 200000000)
print(list(offset_outcome=outcome_offset,rounded_difference=rounded_difference,
  exact_affine_difference=affine_difference,known_variance=4,
  lower95=affine_difference-2*qnorm(.975),upper95=affine_difference+2*qnorm(.975)))
averaged_variance <- sum(rep(2/8,8)^2)
print(list(offset_pipeline_rows=8,known_variance=averaged_variance,
  standard_error=sqrt(averaged_variance),z=affine_difference/sqrt(averaged_variance),
  lower95=affine_difference-qnorm(.975)*sqrt(averaged_variance),
  upper95=affine_difference+qnorm(.975)*sqrt(averaged_variance),
  negative_offset_improvement=-200000001))
