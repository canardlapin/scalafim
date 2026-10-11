#!/usr/bin/env Rscript
source(file.path('tools','r-parity','receipt_serialization.R'))
library(fmriAR)
reference <- Sys.getenv('FMRIAR_041_R')
revision <- if (nzchar(Sys.which('git'))) trimws(system2('git',c('-c',paste0('safe.directory=',reference),'-C',reference,'rev-parse','HEAD'),stdout=TRUE)) else trimws(readLines(file.path(reference,'.git','HEAD')))
stopifnot(grepl('^[0-9a-f]{40}$',revision))
stopifnot(revision == 'de42cd9dc3e290d0653c5145749c27e03085f7d6', as.character(packageVersion('fmriAR')) == '0.4.1')
raw <- jsonlite::fromJSON(gzfile('docs/verification/ar-fmriar-041-cross-reference-20261009/inputs.json.gz'),simplifyVector=FALSE)
read_matrix <- function(x) matrix(unlist(x$data),unlist(x$rows),unlist(x$cols),byrow=TRUE)
rows <- function(x) unname(lapply(seq_len(nrow(x)),function(i) as.numeric(x[i,])))
round_matrix <- function(x) matrix(as.numeric(receipt_format_number(x)),nrow(x),ncol(x))
bic_y <- round_matrix(read_matrix(raw$bic$input))
diag_y <- round_matrix(read_matrix(raw$diagnostics$input))
runs <- rep(1:2,each=40)
bic <- fit_noise(bic_y,p='auto',p_max=6)
legacy <- jsonlite::fromJSON('docs/scenarios/fixtures/ar.fmriar-parity.v1.r.json')$inputs$estimation_series
legacy_auto <- fit_noise(matrix(legacy,ncol=1),p='auto',p_max=4)
legacy_median_y <- cbind(c(0,2,0,-2,0,2),c(1,1,-1,-1,1,1),c(-1,0,1,0,-1,0))
models <- list(list('iid',numeric(),numeric()),list('ar1',0.7,numeric()),list('ar2',c(1.2,-0.5),numeric()),list('ar3',c(0.3,0.2,0.1),numeric()),list('ma1',numeric(),0.7),list('ma2',numeric(),c(0.3,-0.2)),list('arma11',0.6,0.5),list('arma22',c(0.6,0.2),c(0.3,-0.2)))
starts <- c(0L,1L,4L); lengths <- c(1L,3L,8L); n <- sum(lengths)
whitening <- lapply(models,function(model) {
 phi <- model[[2]]; theta <- model[[3]]
 g0 <- if (!length(phi) && !length(theta)) 1 else sum(c(1,ARMAtoMA(phi,theta,5000))^2)
 gamma <- if (!length(phi) && !length(theta)) c(1,rep(0,n-1)) else ARMAacf(ar=phi,ma=theta,lag.max=n-1)*g0
 sigma <- matrix(0,n,n)
 for (i in seq_along(starts)) {idx <- seq.int(starts[i]+1L,starts[i]+lengths[i]); sigma[idx,idx] <- toeplitz(gamma[seq_along(idx)])}
 exact <- fmriAR:::arma_whiten_inplace(diag(n),matrix(0,n,0),phi,theta,starts,exact_first=TRUE,parallel=FALSE)$Y
 oracle <- solve(t(chol(sigma)))
 stopifnot(max(abs(exact-oracle))<1e-10)
 list(name=model[[1]],phi=phi,theta=theta,lengths=lengths,covariance=rows(sigma),operator=rows(exact))
})
bias_receipt <- jsonlite::fromJSON('docs/scenarios/fixtures/ar.fmriar-bias.v1.r.json',simplifyVector=FALSE)
short_case <- Filter(function(x) x$name == 'runs_70_and_14_auto',bias_receipt$outputs$fit_cases)[[1]]$fits[[1]]
short_gamma <- lapply(short_case$gamma,unlist)
short_standard <- Map(function(g,n) {
 candidates <- lapply(0:min(4,floor(n/5)),function(p) {
  f <- fmriAR:::yw_from_acvf_fast(fmriAR:::.shrink_to_pd(g[seq_len(p+1)]),p)
  phi <- fmriAR:::enforce_stationary_ar(f$phi,0.99)
  list(phi=phi,bic=n*log(max(f$sigma2,1e-12))+(p+1)*log(n),sigma2=g[1]-sum(phi*g[seq_along(phi)+1]))
 })
 candidates[[which.min(vapply(candidates,function(x) x$bic,0))]]
},short_gamma,c(70,14))
tail_y <- round_matrix(read_matrix(raw$bias$input))
tail_x <- round_matrix(read_matrix(raw$bias$design))
tail_runs <- rep(1:2,each=150)
tail_censor <- as.integer(unlist(raw$bias$censor_zero)+1L)
tail_fit <- fit_noise(tail_y,runs=tail_runs,censor=tail_censor,p=1,design=tail_x,correction_max_lag=25)
consumer_x <- cbind(tail_x[,3:15],tail_x[,1:2])
consumer_y <- round_matrix(tail_y + consumer_x %*% matrix(seq_len(45)/100,15,3))
consumer_residuals <- consumer_y-consumer_x%*%qr.solve(consumer_x,consumer_y)
consumer_cases <- lapply(c('restart','estimate-only'),function(treatment) {
 voxel <- lapply(1:3,function(v) {
  noise <- fit_noise(consumer_residuals[,v,drop=FALSE],runs=tail_runs,censor=tail_censor,p=2,pooling='run',design=consumer_x,correction_max_lag=25)
  whitening_noise <- noise
  # Empty censor inputs normalize to NULL and trigger the plan fallback; clear
  # the copied plan's censor set to express estimation-only continuity.
  if (treatment=='estimate-only') whitening_noise$censor <- NULL
  w <- whiten_apply(whitening_noise,consumer_x,consumer_y[,v,drop=FALSE],runs=tail_runs,censor=if(treatment=='restart') tail_censor else NULL,parallel=FALSE)
  fit <- lm.fit(w$X,w$Y)
  variance <- sum(fit$residuals^2)/(nrow(consumer_x)-fit$rank)
  covariance <- chol2inv(chol(crossprod(w$X)))
  list(coefficients=as.numeric(fit$coefficients),variance=variance,se=sqrt(diag(covariance)*variance),covariance=rows(covariance))
 })
 list(treatment=treatment,coefficients=rows(do.call(cbind,lapply(voxel,function(v) v$coefficients))),variance=vapply(voxel,function(v) v$variance,0),se=rows(do.call(cbind,lapply(voxel,function(v) v$se))),covariance=lapply(voxel,function(v) v$covariance))
})
inputs <- list(consumer_x=rows(consumer_x),consumer_y=rows(consumer_y),tail_y=rows(tail_y),tail_x=rows(tail_x),tail_runs=tail_runs,tail_censor=tail_censor,short_run_gamma=short_gamma,short_run_lengths=c(70,14),bic_y=rows(bic_y),diag_y=rows(diag_y),runs=runs,legacy_series=legacy,legacy_median_y=rows(legacy_median_y),models=lapply(models,function(x) list(name=x[[1]],phi=x[[2]],theta=x[[3]],lengths=lengths)))
outputs <- list(consumer_cases=consumer_cases,tail_phi=tail_fit$phi[[1]],tail_gamma=tail_fit$gamma[[1]],tail_sigma2=tail_fit$sigma2[[1]],short_run_phi=lapply(short_standard,function(x) x$phi),short_run_sigma2=vapply(short_standard,function(x) x$sigma2,0),bic_order=length(bic$phi[[1]]),bic_phi=bic$phi[[1]],legacy_auto_order=length(legacy_auto$phi[[1]]),legacy_auto_phi=legacy_auto$phi[[1]],legacy_median=acorr_diagnostics(legacy_median_y,max_lag=2,aggregate='median')$acf,diag_mean=acorr_diagnostics(diag_y,max_lag=3)$acf,diag_median=acorr_diagnostics(diag_y,max_lag=3,aggregate='median')$acf,diag_none=rows(acorr_diagnostics(diag_y,max_lag=3,aggregate='none')$acf),diag_run_none=rows(acorr_diagnostics(diag_y,runs=runs,max_lag=3,aggregate='none')$acf),whitening=whitening)
source <- list(r_version=as.character(getRversion()),fmriAR_revision=revision,fmriAR_version=as.character(packageVersion('fmriAR')),producer='modules/ar/tools/generate_fmriar_041_parity.R',reference='fmriAR 0.4.1 standard BIC, voxel ACF aggregation, and stationary AR/ARMA whitening checked against independent dense Cholesky')
outputs <- canonicalize_receipt_numbers(outputs)
payload <- list(schema_version='scalafim-r-fmriar-041-fixture/v1',inputs=inputs,outputs=outputs,source=source,receipt=list(source=source,conventions=list(reference_serialization=receipt_serialization_convention(),constant_columns='R reports NA; Scala reports zero and omits constants from aggregation',short_run_bic='Hold the locked 0.3.3 bias-corrected ACVF fixed, select standard BIC with the 0.4.1 Yule-Walker kernel; no tail-anchoring change is claimed'),hashes=list()))
jsonlite::write_json(payload,'docs/scenarios/fixtures/ar.fmriar-041.v1.r.json',auto_unbox=TRUE,digits=RECEIPT_SIGNIFICANT_DIGITS,pretty=TRUE)
num <- function(x) {s<-receipt_format_number(x);ifelse(grepl('[.eE]',s),s,paste0(s,'.0'))}
vec <- function(x) paste0('Vector(',paste(num(as.numeric(x)),collapse=', '),')')
ints <- function(x) paste0('Vector(',paste(as.integer(x),collapse=', '),')')
mat <- function(x) paste0('Vector(',paste(vapply(x,vec,''),collapse=', '),')')
csv_matrix <- function(x) paste0('csvMatrix(',nrow(x),', ',ncol(x),', "',paste(receipt_format_number(as.numeric(t(x))),collapse=','),'")')
lines <- c('package scalafim.fmri.ar.fixtures','','// Generated by modules/ar/tools/generate_fmriar_041_parity.R.','object FmriAr041RFixture:', '  private def csvMatrix(rows: Int, cols: Int, text: String): Vector[Vector[Double]] =', '    val data = text.split(",").map(_.toDouble)', '    Vector.tabulate(rows)(r => Vector.tabulate(cols)(c => data(r * cols + c)))', paste0('  val tailResiduals = ',csv_matrix(tail_y)),paste0('  val tailDesign = ',csv_matrix(tail_x)),paste0('  val tailCensor = ',ints(tail_censor-1L)),paste0('  val tailPhi = ',vec(outputs$tail_phi)),paste0('  val tailGamma = ',vec(outputs$tail_gamma)),paste0('  val tailSigma2 = ',num(outputs$tail_sigma2)),'  final case class WhiteningCase(name: String, phi: Vector[Double], theta: Vector[Double], lengths: Vector[Int], covariance: Vector[Vector[Double]], operator: Vector[Vector[Double]])',paste0('  val shortRunPhi = ',mat(outputs$short_run_phi)),paste0('  val shortRunSigma2 = ',vec(outputs$short_run_sigma2)),paste0('  val bicSeries = ',vec(bic_y)),paste0('  val bicOrder = ',length(bic$phi[[1]])),paste0('  val bicPhi = ',vec(outputs$bic_phi)),paste0('  val legacyAutoOrder = ',outputs$legacy_auto_order),paste0('  val legacyAutoPhi = ',vec(outputs$legacy_auto_phi)),paste0('  val legacyMedian = ',vec(outputs$legacy_median)),paste0('  val diagnosticRows = ',mat(rows(diag_y))),paste0('  val diagnosticMean = ',vec(outputs$diag_mean)),paste0('  val diagnosticMedian = ',vec(outputs$diag_median)),paste0('  val diagnosticNone = ',mat(outputs$diag_none)),paste0('  val diagnosticRunNone = ',mat(outputs$diag_run_none)),paste0('  val whiteningCases = Vector(',paste(vapply(outputs$whitening,function(x) paste0('WhiteningCase("',x$name,'", ',vec(x$phi),', ',vec(x$theta),', ',ints(x$lengths),', ',mat(x$covariance),', ',mat(x$operator),')'),''),collapse=',\n    '),')'))
writeLines(lines,'modules/ar/shared/src/test/scala/scalafim/fmri/ar/fixtures/FmriAr041RFixture.scala')

fit_lines <- c('package scalafim.fmri.fit.fixtures','','// Generated by modules/ar/tools/generate_fmriar_041_parity.R.','object FmriAr041GlsFixture:', '  private def csvMatrix(rows: Int, cols: Int, text: String): Vector[Vector[Double]] =', '    val data = text.split(",").map(_.toDouble)', '    Vector.tabulate(rows)(r => Vector.tabulate(cols)(c => data(r * cols + c)))','  final case class FitCase(treatment: String, coefficients: Vector[Vector[Double]], variance: Vector[Double], se: Vector[Vector[Double]], covariance: Vector[Vector[Vector[Double]]])',paste0('  val design = ',csv_matrix(consumer_x)),paste0('  val response = ',csv_matrix(consumer_y)),paste0('  val censor = ',ints(tail_censor-1L)),paste0('  val cases = Vector(',paste(vapply(outputs$consumer_cases,function(x) paste0('FitCase("',x$treatment,'", ',mat(x$coefficients),', ',vec(x$variance),', ',mat(x$se),', Vector(',paste(vapply(x$covariance,mat,''),collapse=', '),'))'),''),collapse=',\n    '),')'))
writeLines(fit_lines,'modules/fit/shared/src/test/scala/scalafim/fmri/fit/fixtures/FmriAr041GlsFixture.scala')
