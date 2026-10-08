#!/usr/bin/env Rscript
# Exploratory read-only re-analysis of retained inputs, never a new rate study.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=3L) stop('usage: diagnose_rank.R INDEX.tsv POWER-BOUND.tsv OUTPUT-DIRECTORY')
index <- read.delim(args[[1]],check.names=FALSE,stringsAsFactors=FALSE)
bounds <- read.delim(args[[2]],check.names=FALSE,stringsAsFactors=FALSE)
stopifnot(nrow(index)==12200L,all(index$ordinal %in% 0:199),!anyDuplicated(paste(index$cell,index$ordinal)))
output <- args[[3]]; dir.create(output,recursive=TRUE,showWarnings=FALSE)
read_case <- function(path) {
  lines <- strsplit(readLines(path,warn=FALSE),'\t',fixed=TRUE)
  matrix_named <- function(name) {
    row <- lines[[which(vapply(lines,function(x) x[[1]]==name,TRUE))]]
    matrix(as.numeric(strsplit(row[[4]],',',fixed=TRUE)[[1]]),as.integer(row[[2]]),as.integer(row[[3]]),byrow=TRUE)
  }
  stopifnot(lines[[1]][[2]]=='pilot')
  list(x=matrix_named('X'),y=matrix_named('Y'),z=matrix_named('Z'))
}
orth <- function(x) qr.Q(qr(x))[,seq_len(ncol(x)),drop=FALSE]
roots <- function(x,y) svd(crossprod(orth(x),orth(y)),nu=0,nv=0)$d
complete <- function(w) {
  if(nrow(w)==ncol(w)) w else cbind(w,qr.Q(qr(w),complete=TRUE)[,(ncol(w)+1):nrow(w),drop=FALSE])
}
observed <- function(x,y) {
  qx <- orth(x); qy <- orth(y); s <- svd(crossprod(qx,qy))
  a <- qr.solve(x,qx %*% s$u); b <- qr.solve(y,qy %*% s$v)
  u <- x %*% complete(a); v <- y %*% complete(b)
  w <- vapply(seq_len(min(ncol(x),ncol(y))),function(k)
    -sum(log1p(-roots(u[,k:ncol(u),drop=FALSE],v[,k:ncol(v),drop=FALSE])^2)),0.0)
  list(roots=s$d,wilks=w)
}
rows <- vector('list',nrow(index))
for(i in seq_len(nrow(index))) {
  meta <- index[i,]; d <- read_case(meta$path); zqr <- qr(d$z)
  x <- qr.resid(zqr,d$x);y <- qr.resid(zqr,d$y);m <- nrow(x)-zqr$rank
  fit <- observed(x,y)
  # H4 has one variable on one side and three on the other. This exact beta
  # reference is calibrated for EXTERNALLY KNOWN axes, not fitted tail axes.
  oracle_r2 <- roots(x[,4:ncol(x),drop=FALSE],y[,4:ncol(y),drop=FALSE])[[1]]^2
  oracle_h4 <- pbeta(oracle_r2,3/2,(m-3)/2,lower.tail=FALSE)
  fitted_r2 <- -expm1(-fit$wilks[[4]])
  fitted_beta_reference <- pbeta(fitted_r2,3/2,(m-3)/2,lower.tail=FALSE)
  r3 <- sum(x[,3]*y[,3])/sqrt(sum(x[,3]^2)*sum(y[,3]^2))
  known_pair <- pt(r3*sqrt((m-1)/(1-r3^2)),m-1,lower.tail=FALSE)
  # Oracle likelihood ratio gets true nuisance coefficients, scale, signs and
  # directions, hence supplies extra information unavailable to fitted CCA.
  mean <- if(meta$nuisance=='intercept') rep(0,meta$n) else .4*rowSums(d$z[,2:3,drop=FALSE])
  scale <- if(meta$nuisance=='intercept') 1 else sqrt(.68)
  ox <- (d$x[,3]-mean)/scale;oy <- (d$y[,3]-mean)/scale
  W <- .8*sum((ox+oy)^2/2)-1.2*sum((ox-oy)^2/2)
  cutoff <- bounds$cutoff[bounds$n==meta$n & bounds$alpha==.05 & bounds$route=='density']
  stopifnot(length(cutoff)==1L)
  row <- data.frame(cell=meta$cell,ordinal=meta$ordinal,rank=meta$rank,rate_class=meta$rate_class,
    n=meta$n,p=meta$p,q=meta$q,nuisance=meta$nuisance,residual_rows=m,
    raw_h3=meta$raw3,closed_h3=meta$closed3,raw_h4=meta$raw4,closed_h4=meta$closed4,
    known_axes_h4_p=oracle_h4,fitted_h4_beta_reference=fitted_beta_reference,
    known_third_pair_p=known_pair,oracle_lr_reject=W>cutoff,
    known_h4_r_squared=oracle_r2,fitted_h4_r_squared=fitted_r2)
  for(k in 1:4) { row[[paste0('root',k)]] <- fit$roots[[k]];row[[paste0('wilks',k)]] <- fit$wilks[[k]] }
  rows[[i]] <- row
}
result <- do.call(rbind,rows)
write.table(result,gzfile(file.path(output,'independent-observed.tsv.gz')),sep='\t',quote=FALSE,row.names=FALSE)
summary <- lapply(split(result,result$cell),function(x) data.frame(cell=x$cell[[1]],datasets=nrow(x),
  rank=x$rank[[1]],rate_class=x$rate_class[[1]],
  raw_h3=sum(x$raw_h3<=.05),closed_h3=sum(x$closed_h3<=.05),
  raw_h4=sum(x$raw_h4<=.05),closed_h4=sum(x$closed_h4<=.05),
  known_axes_h4=sum(x$known_axes_h4_p<=.05),known_third_pair=sum(x$known_third_pair_p<=.05),
  oracle_lr=sum(x$oracle_lr_reject),
  mean_known_h4_r_squared=mean(x$known_h4_r_squared),mean_fitted_h4_r_squared=mean(x$fitted_h4_r_squared),
  mean_raw_h4_minus_beta=mean(x$raw_h4-x$fitted_h4_beta_reference),
  rms_raw_h4_minus_beta=sqrt(mean((x$raw_h4-x$fitted_h4_beta_reference)^2))))
write.table(do.call(rbind,summary),file.path(output,'diagnostic-summary.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
cat('Re-analysed',nrow(result),'retained datasets; no generator, new randomization or confirmation stream\n')
print(sessionInfo())
