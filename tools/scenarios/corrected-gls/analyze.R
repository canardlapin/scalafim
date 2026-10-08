#!/usr/bin/env Rscript
# Independent base-R summaries, exact CP intervals and dataset-level bootstrap.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("usage: Rscript analyze.R LOG OUTPUT_DIR [paired-platform LOG]")
log_path <- args[[1L]]
out <- args[[2L]]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
protocol <- jsonlite::read_json("tools/scenarios/corrected-gls/protocol.json", simplifyVector = FALSE)
read_records <- function(path, marker) {
  lines <- readLines(path, warn = FALSE)
  selected <- lines[grepl(marker, lines, fixed = TRUE)]
  lapply(selected, function(line) jsonlite::fromJSON(sub(paste0("^.*", marker), "", line)))
}
rows <- read_records(log_path, "GLS_STUDY_TRIAL ")
failures <- read_records(log_path, "GLS_STUDY_FAILURE ")
if (!length(rows)) stop("log contains no fitted study records")
profile <- unique(vapply(rows, `[[`, character(1), "profile"))
if (length(profile) != 1L) stop("mixed study profiles")
n_expected <- protocol$profiles[[profile]]$replicates
key <- function(r) paste(r$cell, r$engine, r$replicate, sep = "/")
keys <- vapply(rows, key, character(1))
if (anyDuplicated(keys)) stop("duplicate fitted dataset records")
cp <- function(count, n, confidence = .90) {
  alpha <- 1 - confidence
  c(if (count == 0L) 0 else qbeta(alpha/2, count, n-count+1),
    if (count == n) 1 else qbeta(1-alpha/2, count+1, n-count))
}
equivalent <- function(interval, bounds) interval[[1L]] >= bounds[[1L]] && interval[[2L]] <= bounds[[2L]]
results <- list()
for (i in seq_along(protocol$cells)) {
  cell <- protocol$cells[[i]]
  for (engine in protocol$engines) {
    selected <- Filter(function(r) identical(r$cell, cell$id) && identical(r$engine, engine), rows)
    n <- length(selected)
    if (!n) {
      results[[length(results)+1L]] <- list(cell=cell$id, engine=engine, replicates=0L, complete=FALSE, scientific_pass=FALSE)
      next
    }
    getnum <- function(name) vapply(selected, `[[`, numeric(1), name)
    theta <- getnum("nullEstimate")
    vhat <- getnum("nullVariance")
    signal <- getnum("signalEstimate")
    t_reject <- sum(vapply(selected, `[[`, logical(1), "tRejected"))
    f_reject <- sum(vapply(selected, `[[`, logical(1), "fRejected"))
    covered <- sum(vapply(selected, `[[`, logical(1), "covered"))
    sample_var <- var(theta)
    ratio <- mean(vhat) / sample_var
    phi_matrix <- do.call(rbind, lapply(selected, `[[`, "phi"))
    truth <- unlist(cell$phi)
    phi_bias <- colMeans(phi_matrix) - truth
    phi_half <- qt(.95, n-1) * apply(phi_matrix, 2L, sd) / sqrt(n)
    standardized_bias <- mean(theta)/sqrt(sample_var)
    bias_interval <- standardized_bias + c(-1,1)*qt(.95,n-1)/sqrt(n)
    interval_t <- cp(t_reject,n)
    interval_f <- cp(f_reject,n)
    interval_coverage <- cp(covered,n)
    ratio_interval <- c(NA_real_,NA_real_)
    if (profile == "confirmation" && n == n_expected) {
      # This stream is disjoint from all Gaussian-data streams; never reseed it
      # from observed uncertainty or a desired qualification result.
      boot_seed <- ((selected[[1L]]$root + 4*10000019 + i*1000003 + match(engine,protocol$engines)*7919) %% 2147483646) + 1
      set.seed(as.integer(boot_seed))
      bootstrap <- replicate(protocol$acceptance$bootstrap_resamples, {
        sample <- sample.int(n,n,replace=TRUE)
        mean(vhat[sample])/var(theta[sample])
      })
      ratio_interval <- unname(quantile(bootstrap,c(.05,.95)))
    }
    reciprocal <- sample_var*mean(1/vhat)
    d_inv <- if (reciprocal>1) 2*reciprocal/(reciprocal-1) else NA_real_
    gates <- list(complete=n==n_expected,
      mean_AR_bias=all(abs(phi_bias)+phi_half <= .04),
      AR_RMSE=sqrt(mean(getnum("phiSquaredError"))) <= .10,
      null_t=equivalent(interval_t,c(.02,.08)),
      null_f=equivalent(interval_f,c(.02,.08)),
      interval_coverage=equivalent(interval_coverage,c(.92,.98)),
      variance_ratio=all(is.finite(ratio_interval)) && equivalent(ratio_interval,c(.80,1.20)),
      standardized_bias=equivalent(bias_interval,c(-.10,.10)),
      residual_whiteness=mean(getnum("whiteness")) <= .10)
    results[[length(results)+1L]] <- list(cell=cell$id, engine=engine, replicates=n,
      complete=n==n_expected, phi_bias=phi_bias, phi_bias_halfwidth=phi_half,
      AR_RMSE=sqrt(mean(getnum("phiSquaredError"))), whiteness=mean(getnum("whiteness")),
      null_t=list(count=t_reject,rate=t_reject/n,interval=interval_t),
      null_f=list(count=f_reject,rate=f_reject/n,interval=interval_f),
      coverage=list(count=covered,rate=covered/n,interval=interval_coverage),
      variance_ratio=list(estimate=ratio,interval=ratio_interval),
      standardized_bias=list(estimate=standardized_bias,interval=bias_interval),
      d_eff=2*mean(vhat)^2/var(vhat),d_inv=d_inv,
      effect_variance_dependence=cor(theta^2,vhat),
      timing_ms=list(mean=mean(getnum("milliseconds")),median=median(getnum("milliseconds"))),
      gates=gates,scientific_pass=profile=="confirmation" && engine!="raw" && all(unlist(gates)))
  }
}
for(i in seq_along(results)) {
  r <- results[[i]]
  if (r$engine == "corrected" && r$replicates>0L) {
    known <- Filter(function(x) x$cell==r$cell && x$engine=="known-phi",results)[[1L]]
    results[[i]]$gates$whiteness_excess <- r$whiteness-known$whiteness <= .025
    results[[i]]$scientific_pass <- r$scientific_pass && results[[i]]$gates$whiteness_excess
  }
}
parity <- NULL
if (length(args)>=3L) {
  other <- read_records(args[[3L]],"GLS_STUDY_TRIAL ")
  named <- setNames(other,vapply(other,key,character(1)))
  same <- length(other)==length(rows) && !anyDuplicated(names(named)) && setequal(names(named),keys)
  worst <- 0
  facts_same <- TRUE
  for(r in rows) {
    q <- named[[key(r)]]
    if (is.null(q)) {same<-FALSE;next}
    a <- unlist(r[c("phi","phiSquaredError","whiteness","nullEstimate","nullVariance","signalEstimate","signalVariance")])
    b <- unlist(q[c("phi","phiSquaredError","whiteness","nullEstimate","nullVariance","signalEstimate","signalVariance")])
    if(length(a)!=length(b)) {same<-FALSE;next}
    worst <- max(worst,max(abs(a-b)/(1+abs(a))))
    facts_same <- facts_same && identical(r[c("tRejected","fRejected","covered")],q[c("tRejected","fRejected","covered")])
  }
  parity <- list(same_dataset_keys=same,max_scaled_numeric_error=worst,tolerance=1e-9,same_decisions=facts_same,
    pass=same && worst<=1e-9 && facts_same)
}
science <- if (profile!="confirmation") NULL else !length(failures) &&
  all(vapply(Filter(function(r) r$engine!="raw",results),function(r) isTRUE(r$scientific_pass),logical(1)))
summary <- list(schema_version="scalafim-corrected-gls-qualification-summary/v1",profile=profile,
  seed=unique(vapply(rows,`[[`,numeric(1),"root")),r_version=as.character(getRversion()),
  independent_sampling_units="replicate datasets; response columns are not counted as independent tests",
  replicates_per_cell=n_expected,failures=failures,results=results,cross_platform=parity,
  scientific_pass=science,limits=protocol$scope_limits)
jsonlite::write_json(summary,file.path(out,"summary.json"),auto_unbox=TRUE,digits=16L,pretty=TRUE,na="null",null="null")
jsonlite::write_json(list(profile=profile,independent_rows=rows,failures=failures),file.path(out,"replicate-records.json"),auto_unbox=TRUE,digits=16L,na="null")
nonraw <- Filter(function(r) r$engine!="raw" && r$replicates>0L,results)
if(length(nonraw)) {
  png(file.path(out,"qualification.png"),width=1500,height=1050,res=140)
  par(mfrow=c(2,2),mar=c(9,4,3,1))
  labels <- vapply(nonraw,function(r) paste(r$cell,r$engine,sep="\n"),character(1))
  x <- seq_along(nonraw)
  draw <- function(y,lo,hi,target,bounds,title) {
    plot(x,y,xaxt="n",pch=19,col=rep(c("steelblue","darkorange"),length.out=length(x)),
      xlab="",ylab="",main=title,ylim=range(c(y,lo,hi,target,bounds),finite=TRUE))
    axis(1,at=x,labels=labels,las=2,cex.axis=.58)
    interval <- is.finite(lo) & is.finite(hi) & hi > lo
    arrows(x[interval],lo[interval],x[interval],hi[interval],angle=90,code=3,length=.025)
    abline(h=target,lty=2);abline(h=bounds,lty=3,col="gray50")
  }
  draw(vapply(nonraw,function(r) r$coverage$rate,numeric(1)),vapply(nonraw,function(r) r$coverage$interval[1],numeric(1)),
    vapply(nonraw,function(r) r$coverage$interval[2],numeric(1)),.95,c(.92,.98),"95% interval coverage (90% exact CP)")
  draw(vapply(nonraw,function(r) r$null_t$rate,numeric(1)),vapply(nonraw,function(r) r$null_t$interval[1],numeric(1)),
    vapply(nonraw,function(r) r$null_t$interval[2],numeric(1)),.05,c(.02,.08),"Null t rejection (90% exact CP)")
  draw(vapply(nonraw,function(r) r$variance_ratio$estimate,numeric(1)),vapply(nonraw,function(r) r$variance_ratio$interval[1],numeric(1)),
    vapply(nonraw,function(r) r$variance_ratio$interval[2],numeric(1)),1,c(.8,1.2),"Reported / empirical variance (90% bootstrap)")
  draw(vapply(nonraw,function(r) r$phi_bias[1],numeric(1)),vapply(nonraw,function(r) r$phi_bias[1]-r$phi_bias_halfwidth[1],numeric(1)),
    vapply(nonraw,function(r) r$phi_bias[1]+r$phi_bias_halfwidth[1],numeric(1)),0,c(-.04,.04),"Lag-1 coefficient bias (90% mean interval)")
  dev.off()
}
cat("Profile:",profile,"scientificPass:",if(is.null(science))"not-admitted" else science,"\n")
if(!is.null(parity))cat("Cross-platform parity:",parity$pass,"\n")
