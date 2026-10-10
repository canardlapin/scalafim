#!/usr/bin/env Rscript
# Rendering only: numeric estimates and intervals come from retained factors.json.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L)
s <- jsonlite::read_json(args[[1L]],simplifyVector=FALSE)
cells <- s$decomposition
ids <- vapply(cells,`[[`,character(1),"cell")
labels <- if(s$profile=="confirmation")c("Global\n96/144","Run\n96/144","Voxelwise\n192/288") else {
 vapply(strsplit(ids,"-"),function(parts)paste0(sub("ar","",parts[1]),
  if(parts[2]=="high")"H" else "L",if(parts[3]=="censored")"C" else "U","-",
  switch(parts[4],global="G",run="R",voxelwise="V")),character(1))
}
png(args[[2L]],width=1600,height=1050,res=150)
par(mfrow=c(2,2),mar=c(6,4,3,1),oma=c(3,0,1,0))
titles <- c(calibration="Reported / empirical variance",AR_RMSE="AR coefficient recovery RMSE",
 reportedVarianceRatio="Reported variance / known-filter reported variance",
 samplingVarianceRatio="Estimator variance / known-filter estimator variance")
for(metric in names(titles)) {
 values <- lapply(cells,function(cell)Filter(function(m)m$name==metric,cell$metrics)[[1L]])
 y <- vapply(values,`[[`,numeric(1),"estimate")
 lo <- vapply(values,function(m)unlist(m$interval)[1],numeric(1))
 hi <- vapply(values,function(m)unlist(m$interval)[2],numeric(1))
 reference <- if(metric=="AR_RMSE").1 else 1
 bounds <- if(metric=="calibration")c(.8,1.2) else reference
 color <- if(s$profile=="confirmation")c("steelblue4","darkgreen","darkorange3") else
  rep(c("steelblue4","darkgreen","darkorange3"),length.out=length(ids))
 plot(seq_along(ids),y,xaxt="n",xlab="",ylab="",pch=19,col=color,main=titles[[metric]],
  ylim=range(c(y,lo,hi,reference,bounds)))
 arrows(seq_along(ids),lo,seq_along(ids),hi,angle=90,code=3,length=.02,col=color)
 axis(1,at=seq_along(ids),labels=labels,las=2,cex.axis=.8)
 abline(h=reference,lty=2)
 if(metric=="calibration")abline(h=bounds,lty=3,col="gray50")
}
mtext(paste("Profile:",s$profile,";",s$replicates_per_cell,"paired datasets per cell; 90% dataset-bootstrap intervals"),
 outer=TRUE,side=3,line=0,cex=.9)
if(s$profile!="confirmation") {
 mtext("1: AR(1) rho=.5; 2: AR(2) phi=(.45,-.10); L/H: 0/12 nuisance; U/C: complete/censored; G/R/V: global/run/voxelwise",
  outer=TRUE,side=1,line=1,cex=.8)
} else {
 mtext("Confirmation verdicts require the complete exact-interval gate table; these four endpoints alone do not establish admission.",
  outer=TRUE,side=1,line=1,cex=.8)
}
dev.off()
