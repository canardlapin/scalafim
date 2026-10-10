#!/usr/bin/env Rscript
# Presentation only: reads the independently adjudicated summary without recomputing decisions.
args <- commandArgs(trailingOnly=TRUE)
if(length(args)!=2L) stop("usage: plot.R SUMMARY OUTPUT_PNG")
s <- jsonlite::read_json(args[[1L]], simplifyVector=FALSE)
r <- s$results
labels <- vapply(r, function(x) {
  case <- if(grepl("long",x$cell)) "AR2 censored 192/288 *" else
    if(grepl("ar1",x$cell)) "AR1 censored 96/144" else
    if(grepl("complete",x$cell)) "AR2 complete 96/144" else "AR2 censored 96/144"
  paste(case, if(x$engine=="known-phi") "known filter" else "estimated filter")
}, character(1))
colors <- vapply(r,function(x) if(x$engine=="known-phi") "#286090" else "#b95713",character(1))
y <- rev(seq_along(r))
interval <- function(x) if(is.null(x)) c(NA_real_,NA_real_) else unlist(x)
point <- function(x) if(is.null(x)) NA_real_ else x
png(args[[2L]],width=1800,height=1100,res=150)
par(mfrow=c(2,2),mar=c(4,14,4,1),oma=c(2,0,2,0))
draw <- function(metric,bounds,title) {
  x <- vapply(r,function(z) point(z[[metric]]$estimate),numeric(1))
  if(metric %in% c("null_f","null_t","coverage")) x <- vapply(r,function(z) point(z[[metric]]$rate),numeric(1))
  ci <- t(vapply(r,function(z) interval(z[[metric]]$interval),numeric(2)))
  lim <- range(c(x,ci,bounds),finite=TRUE)
  span <- diff(lim); if(span==0) span <- .01
  plot(x,y,xlim=lim+c(-.08,.08)*span,ylim=c(.5,length(r)+.5),yaxt="n",
    pch=19,col=colors,xlab="",ylab="",main=title)
  axis(2,at=y,labels=labels,las=1,cex.axis=.72)
  abline(v=bounds,lty=3,col="gray50")
  finite <- is.finite(ci[,1]) & is.finite(ci[,2]) & ci[,2]>ci[,1]
  arrows(ci[finite,1],y[finite],ci[finite,2],y[finite],angle=90,code=3,length=.03,col=colors[finite])
}
draw("null_f",c(.02,.08),"Joint F rejection: 90% exact interval")
draw("coverage",c(.92,.98),"95% interval coverage: 90% exact interval")
draw("null_t",c(.02,.08),"t rejection: 90% exact interval")
draw("variance_ratio",c(.8,1.2),"Reported / empirical variance: 90% bootstrap")
mtext(paste("Continuous stationary whitening:",s$profile,";",s$replicates_per_cell,"datasets per case"),outer=TRUE,side=3,line=.5)
mtext("* Only the declared long AR2 cell is eligible for admission; all short-run results remain diagnostics.",outer=TRUE,side=1,line=.2,cex=.8)
dev.off()
