args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L) stop('Usage: Rscript generate-probes.R <0.4.1-library> <0.3.3-source-root> <output.json>')
reference_library <- args[1L]
old_source_root <- args[2L]
output_json <- args[3L]
library(fmriAR, lib.loc = reference_library)
ns <- asNamespace('fmriAR')
old <- new.env(parent = ns)
for (f in list.files(file.path(old_source_root, 'R'), full.names = TRUE)) {
  if (basename(f) != 'RcppExports.R') sys.source(f, envir = old)
}
mat <- function(x) list(rows=nrow(x), cols=ncol(x), data=as.numeric(t(x)))
sim <- function(n, v, phi, theta=numeric()) vapply(seq_len(v), function(j) as.numeric(arima.sim(list(ar=phi,ma=theta),n,n.start=200L)),numeric(n))
result <- list(r_version=R.version.string, fmriar_version=as.character(packageVersion('fmriAR', lib.loc=reference_library)), old_revision='ca27f2772bb4675573fae717a992c993c2f986c8', new_revision='de42cd9dc3e290d0653c5145749c27e03085f7d6', old_reference_method='Old R estimator source; unchanged Yule-Walker/PACF C++ exports supplied by isolated new package. Old whitening is not called.')
set.seed(102)
for (i in 1:100) {
  y <- sim(200,1,0.5)
  op <- old$fit_noise(y,p='auto')$phi[[1]]
  np <- fit_noise(y,p='auto')$phi[[1]]
  if (length(op) != length(np)) {
    result$bic <- list(seed=102,draw=i,input=mat(y),old_order=length(op),new_order=length(np),old_phi=op,new_phi=np)
    break
  }
}
stopifnot(!is.null(result$bic))
set.seed(9121)
n <- 80; v <- 6
g <- as.numeric(stats::filter(rnorm(n),0.9,method='recursive'))
y <- matrix(rnorm(n*v),n,v)+0.5*g
runs <- rep(1:2,each=40)
result$diagnostics <- list(input=mat(y),old_mean=old$acorr_diagnostics(y,max_lag=3)$acf,new_mean=acorr_diagnostics(y,max_lag=3)$acf,old_median=old$acorr_diagnostics(y,max_lag=3,aggregate='median')$acf,new_median=acorr_diagnostics(y,max_lag=3,aggregate='median')$acf,new_none=mat(acorr_diagnostics(y,max_lag=3,aggregate='none')$acf),runs=runs,old_run_none=mat(old$acorr_diagnostics(y,runs=runs,max_lag=3,aggregate='none')$acf),new_run_none=mat(acorr_diagnostics(y,runs=runs,max_lag=3,aggregate='none')$acf))
set.seed(9117)
lens <- c(40,60,30); runs <- rep(1:3,lens)
y <- do.call(rbind,lapply(lens,function(L) sim(L,4,c(0.5,0.2))))
result$pooling <- list(input=mat(y),runs=runs,old_phi=old$fit_noise(y,runs=runs,p=2)$phi[[1]],new_phi=fit_noise(y,runs=runs,p=2)$phi[[1]],old_ar1=old$fit_noise(y,runs=runs,p=1)$phi[[1]],new_ar1=fit_noise(y,runs=runs,p=1)$phi[[1]])
whitening <- function(phi,theta=numeric()) {
  n <- 12; starts <- c(0L,6L)
  gamma <- fmriAR:::arma_acvf_cpp(phi,theta,5L)
  covariance <- matrix(0,n,n)
  covariance[1:6,1:6] <- covariance[7:12,7:12] <- toeplitz(gamma)
  exact <- fmriAR:::arma_whiten_inplace(diag(n),matrix(0,n,0),phi,theta,starts,exact_first=TRUE,parallel=FALSE)$Y
  dense <- solve(t(chol(covariance)))
  stopifnot(max(abs(exact-dense))<1e-10)
  list(phi=phi,theta=theta,covariance=mat(covariance),exact=mat(exact),dense_max_error=max(abs(exact-dense)))
}
result$ar2_whitening <- whitening(c(1.2,-0.5))
result$arma_whitening <- whitening(0.6,0.5)
result$ar1_whitening <- whitening(0.7)
set.seed(9204)
n <- 300; runs <- rep(1:2,each=150)
dct <- function(L,k) sapply(1:k,function(j) cos(pi*j*(seq_len(L)-0.5)/L))
X <- cbind(model.matrix(~factor(runs)-1),rbind(cbind(dct(150,6),matrix(0,150,6)),cbind(matrix(0,150,6),dct(150,6))),rep(rep(c(0,1),each=10),length.out=n))
for (i in 1:20) {
  E <- sim(n,3,0.4)
  R <- E-X%*%qr.solve(X,E)
  cen <- sort(sample(n,30))
  op <- old$fit_noise(R,runs=runs,p=1,censor=cen,design=X)$phi[[1]]
  np <- fit_noise(R,runs=runs,p=1,censor=cen,design=X)$phi[[1]]
  if (abs(op-np)>0.1) {
    a <- acvf_bias_matrix(X,runs=runs,censor=cen,max_lag=25)
    result$bias <- list(seed=9204,draw=i,input=mat(R),design=mat(X),runs=runs,censor_zero=cen-1L,old_phi=op,new_phi=np,old_lag5_phi=old$fit_noise(R,runs=runs,p=1,censor=cen,design=X,correction_max_lag=5)$phi[[1]],new_lag5_phi=fit_noise(R,runs=runs,p=1,censor=cen,design=X,correction_max_lag=5)$phi[[1]],matrices=lapply(a,mat),singular_ratio=lapply(a,function(A) {d<-svd(A)$d; list(rcond=rcond(A),smallest_over_median=min(d)/median(d),anchored_directions=sum(d<=0.1*median(d))) }))
    break
  }
}
stopifnot(!is.null(result$bias))
jsonlite::write_json(result,output_json,auto_unbox=TRUE,digits=17,pretty=TRUE)
cat('BIC old/new orders:', result$bic$old_order, result$bic$new_order, '\n')
cat('ACF lag1 old/new:', result$diagnostics$old_mean[1], result$diagnostics$new_mean[1], '\n')
cat('Pooling AR2 old/new:', result$pooling$old_phi, '/', result$pooling$new_phi, '\n')
cat('Bias old/new lag25:', result$bias$old_phi, result$bias$new_phi, '\n')
cat('Bias old/new lag5:', result$bias$old_lag5_phi, result$bias$new_lag5_phi, '\n')
cat('Dense whitening error:', result$ar2_whitening$dense_max_error, result$arma_whitening$dense_max_error, '\n')
