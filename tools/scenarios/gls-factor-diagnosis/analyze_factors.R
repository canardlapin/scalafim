#!/usr/bin/env Rscript
# Paired dataset bootstrap and exact variance-ratio decomposition.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)<2L)stop("usage: Rscript analyze_factors.R RUN_DIR OUTPUT_DIR")
run <- args[[1L]]; out <- args[[2L]]
dir.create(out,recursive=TRUE,showWarnings=FALSE)
protocol <- jsonlite::read_json(file.path(run,"analysis-protocol.json"),simplifyVector=FALSE)
read_records <- function(path) {
  lines <- readLines(path,warn=FALSE)
  selected <- lines[startsWith(lines,"GLS_STUDY_TRIAL ")]
  lapply(selected,function(line)jsonlite::fromJSON(sub("^GLS_STUDY_TRIAL ","",line)))
}
rows <- read_records(file.path(run,"records-jvm.jsonl"))
other <- read_records(file.path(run,"records-js.jsonl"))
key <- function(r)paste(r$cell,r$engine,r$replicate,sep="/")
named <- setNames(other,vapply(other,key,character(1)))
stopifnot(length(rows)==length(other),!anyDuplicated(names(named)),
 setequal(vapply(rows,key,character(1)),names(named)))
worst <- 0
for(r in rows) {
 q <- named[[key(r)]]
 for(field in c("varianceScale","covarianceGeometry","residualDf")) {
  stopifnot(is.finite(r[[field]]),is.finite(q[[field]]))
  worst <- max(worst,abs(r[[field]]-q[[field]])/(1+abs(r[[field]])))
 }
}
stopifnot(worst<=1e-9)
profile <- unique(vapply(rows,`[[`,character(1),"profile"))
stopifnot(length(profile)==1L)
n <- protocol$profiles[[profile]]$replicates
ids <- vapply(protocol$cells,`[[`,character(1),"id")
series <- lapply(ids,function(id) {
 engines <- lapply(c("known-phi","corrected"),function(engine) {
  selected <- Filter(function(r)r$cell==id && r$engine==engine,rows)
  selected <- selected[order(vapply(selected,`[[`,numeric(1),"replicate"))]
  stopifnot(length(selected)==n,identical(vapply(selected,`[[`,numeric(1),"replicate"),as.numeric(0:(n-1L))))
  fields <- c("nullEstimate","nullVariance","phiSquaredError","whiteness","varianceScale","covarianceGeometry")
  values <- lapply(fields,function(name)vapply(selected,`[[`,numeric(1),name));names(values)<-fields
  for(field in c("tRejected","fRejected","covered"))values[[field]]<-as.numeric(vapply(selected,`[[`,logical(1),field))
  stopifnot(all(is.finite(unlist(values))))
  values
 });names(engines)<-c("known","corrected");engines
});names(series)<-ids
metrics <- function(s,index) {
 k<-s$known;c<-s$corrected
 vk<-var(k$nullEstimate[index]);vc<-var(c$nullEstimate[index])
 rk<-mean(k$nullVariance[index]);rc<-mean(c$nullVariance[index])
 c(calibration=rc/vc,knownCalibration=rk/vk,reportedVarianceRatio=rc/rk,
   samplingVarianceRatio=vc/vk,scaleRatio=mean(c$varianceScale[index])/mean(k$varianceScale[index]),
   geometryRatio=mean(c$covarianceGeometry[index])/mean(k$covarianceGeometry[index]),
   scaleGeometryCovariance=cov(c$varianceScale[index],c$covarianceGeometry[index]),
   AR_RMSE=sqrt(mean(c$phiSquaredError[index])),tRejection=mean(c$tRejected[index]),
   fRejection=mean(c$fRejected[index]),coverage=mean(c$covered[index]),whiteness=mean(c$whiteness[index]))
}
point <- vapply(series,metrics,numeric(12),index=seq_len(n))
stopifnot(max(abs(point["calibration",]-point["knownCalibration",]*point["reportedVarianceRatio",]/point["samplingVarianceRatio",]))<1e-10)
set.seed(as.integer((protocol$seed+4*10000019+9000001)%%2147483646+1))
b <- protocol$acceptance$bootstrap_resamples
bootstrap <- array(NA_real_,c(nrow(point),length(ids),b))
for(i in seq_len(b)) {
 index <- sample.int(n,n,replace=TRUE)
 bootstrap[,,i] <- vapply(series,metrics,numeric(12),index=index)
}
effects <- list()
add_effect <- function(name,pairs) {
 if(!length(pairs))return(invisible(NULL))
 estimate <- Reduce(`+`,lapply(pairs,function(p)point[,p[[2L]]]-point[,p[[1L]]]))/length(pairs)
 samples <- Reduce(`+`,lapply(pairs,function(p)bootstrap[,p[[2L]],]-bootstrap[,p[[1L]],]))/length(pairs)
 intervals <- t(apply(samples,1L,quantile,probs=c(.05,.95)))
 effects[[length(effects)+1L]] <<- list(name=name,paired_cells=length(pairs),
  metrics=lapply(seq_along(estimate),function(j)list(name=names(estimate)[j],estimate=estimate[j],interval=unname(intervals[j,]))))
}
if(profile!="confirmation") {
 specs <- protocol$cells
 same <- function(a,b,except)all(vapply(setdiff(c("nuisance","censored","pooling"),except),function(k)identical(a[[k]],b[[k]]),logical(1))) && (except=="order" || length(a$phi)==length(b$phi))
 for(factor in c("order","nuisance","censored","voxelwise","run")) {
  pairs <- list()
  for(i in seq_along(specs))for(j in seq_along(specs)) {
   a<-specs[[i]];z<-specs[[j]]
   match <- if(factor=="order")length(a$phi)==1L&&length(z$phi)==2L&&same(a,z,"order")
    else if(factor=="nuisance")a$nuisance==0&&z$nuisance==12&&same(a,z,"nuisance")
    else if(factor=="censored")!a$censored&&z$censored&&same(a,z,"censored")
    else a$pooling=="global"&&z$pooling==factor&&same(a,z,"pooling")
   if(match)pairs[[length(pairs)+1L]]<-c(i,j)
  }
  add_effect(factor,pairs)
 }
 target <- which(vapply(specs,function(s)length(s$phi)==2L&&s$nuisance==12&&s$censored&&s$pooling=="voxelwise",logical(1)))
 for(factor in c("order","nuisance","censored","pooling-global","pooling-run")) {
  a<-specs[[target]]
  j<-which(vapply(specs,function(s) {
   if(factor=="order")length(s$phi)==1L&&same(s,a,"order")
   else if(factor=="nuisance")s$nuisance==0&&same(s,a,"nuisance")
   else if(factor=="censored")!s$censored&&same(s,a,"censored")
   else s$pooling==sub("pooling-","",factor)&&same(s,a,"pooling")
  },logical(1)))
  stopifnot(length(j)==1L)
  add_effect(paste0("target-",factor),list(c(j,target)))
 }
}
decomposition <- lapply(seq_along(ids),function(i) {
 intervals<-t(apply(bootstrap[,i,],1L,quantile,probs=c(.05,.95)))
 list(cell=ids[i],metrics=lapply(seq_len(nrow(point)),function(j)list(name=rownames(point)[j],estimate=point[j,i],interval=unname(intervals[j,]))))
})
summary <- list(schema_version="scalafim-gls-factor-analysis/v1",profile=profile,replicates_per_cell=n,
 pairing=protocol$pairing,scientific_admission=FALSE,variance_identity_max_error=max(abs(point["calibration",]-point["knownCalibration",]*point["reportedVarianceRatio",]/point["samplingVarianceRatio",])),
 additional_platform_max_scaled_error=worst,decomposition=decomposition,paired_effects=effects)
jsonlite::write_json(summary,file.path(out,"factors.json"),auto_unbox=TRUE,digits=16,pretty=TRUE)
png(file.path(out,"variance-decomposition.png"),width=1600,height=1100,res=140)
par(mfrow=c(2,2),mar=c(9,4,3,1))
for(metric in c("calibration","reportedVarianceRatio","samplingVarianceRatio","AR_RMSE")) {
 j<-match(metric,rownames(point));intervals<-t(apply(bootstrap[j,,],1L,quantile,probs=c(.05,.95)))
 plot(seq_along(ids),point[j,],xaxt="n",pch=19,xlab="",ylab="",main=metric,
  ylim=range(c(point[j,],intervals,if(metric=="AR_RMSE")c(0,.1) else 1)))
 arrows(seq_along(ids),intervals[,1],seq_along(ids),intervals[,2],angle=90,code=3,length=.02)
 axis(1,at=seq_along(ids),labels=ids,las=2,cex.axis=.5)
 abline(h=if(metric=="AR_RMSE").1 else 1,lty=2)
}
dev.off()
cat("Paired factor analysis retained; independent units per cell:",n,"\n")
