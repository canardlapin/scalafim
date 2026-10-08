#!/usr/bin/env Rscript
# Exact numerical integration for the simple Gaussian likelihood-ratio oracle.
# No datasets or RNG streams. This is an upper bound for a level-alpha rank test
# when H0 is R2 and H1 is R3, with all nuisance parameters/directions known.
weighted_chisq_tail <- function(cutoff, n, a, b, tolerance=1e-10, route='density') {
  stopifnot(n>0, a>0, b>0, tolerance>0)
  if (route=='density') {
    upper <- qchisq(1-1e-12,n)
    value <- integrate(function(v) dchisq(v,n)*pchisq(pmax(0,(cutoff+b*v)/a),n,lower.tail=FALSE),
                       0,upper,abs.tol=tolerance,rel.tol=tolerance,subdivisions=1000L)
    return(c(probability=value$value,error=value$abs.error+1e-12))
  }
  stopifnot(route=='quantile')
  value <- integrate(function(u) pchisq(pmax(0,(cutoff+b*qchisq(u,n))/a),n,lower.tail=FALSE),
                     0,1,abs.tol=tolerance,rel.tol=tolerance,subdivisions=1000L)
  c(probability=value$value,error=value$abs.error)
}
rank_power_bound <- function(n,rho,alpha=.05,tolerance=1e-10,route='density') {
  stopifnot(length(n)==1L,n>=1,n==floor(n),rho>0,rho<1,alpha>0,alpha<1)
  # Under H0: W=(1-rho) A-(1+rho) B. Under H1: W=(1-rho^2)(A-B).
  # A,B are independent chi-squared(n); W is monotone in log(f1/f0).
  tail0 <- function(c) weighted_chisq_tail(c,n,1-rho,1+rho,tolerance,route)[['probability']]
  bracket <- c(-1,1)*qchisq(1-1e-12,n)*(1+rho)
  cutoff <- uniroot(function(c) tail0(c)-alpha,bracket,tol=1e-9)$root
  power <- weighted_chisq_tail(cutoff/(1-rho^2),n,1,1,tolerance,route)
  data.frame(n=n,rho=rho,alpha=alpha,cutoff=cutoff,size=tail0(cutoff),
             optimal_power=power[['probability']],quadrature_error=power[['error']],route=route)
}
args <- commandArgs(trailingOnly=TRUE)
if (sys.nframe()==0L) {
  if(length(args)!=1L) stop('usage: rank_power_bound.R OUTPUT_DIRECTORY')
  dir.create(args[[1]],recursive=TRUE,showWarnings=FALSE)
  cells <- expand.grid(n=c(80L,160L),alpha=c(.05,.065),route=c('density','quantile'),stringsAsFactors=FALSE)
  result <- do.call(rbind,lapply(seq_len(nrow(cells)),function(i)
    rank_power_bound(cells$n[[i]],.2,cells$alpha[[i]],route=cells$route[[i]])))
  for(n in c(80L,160L)) for(alpha in c(.05,.065)) {
    x <- result[result$n==n & result$alpha==alpha,]
    stopifnot(abs(diff(x$optimal_power))<2e-9,max(abs(x$size-alpha))<1e-9)
  }
  # Symmetry check supplies a distribution identity independent of root finding.
  for(n in c(1L,5L,80L)) stopifnot(abs(weighted_chisq_tail(0,n,1,1)[['probability']]-.5)<1e-9)
  lower <- 1L; upper <- 512L
  while(upper-lower>1L) {
    middle <- as.integer(floor((upper+lower)/2))
    if(rank_power_bound(middle,.2)$optimal_power>=.8) upper <- middle else lower <- middle
  }
  boundary <- rbind(rank_power_bound(lower,.2),rank_power_bound(upper,.2))
  stopifnot(boundary$optimal_power[[1]]<.8,boundary$optimal_power[[2]]>=.8)
  write.table(result,file.path(args[[1]],'oracle-power-bound.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
  write.table(boundary,file.path(args[[1]],'oracle-minimum-n.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
  print(result);print(boundary)
  print(sessionInfo())
}
