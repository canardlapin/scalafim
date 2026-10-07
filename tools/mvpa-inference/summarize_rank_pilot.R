#!/usr/bin/env Rscript
# Descriptive independent-dataset intervals only. No admission decisions.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L)
x <- read.delim(args[[1]],stringsAsFactors=FALSE,check.names=FALSE)
stopifnot(all(x$datasets==200L),all(x$successes>=0L),all(x$successes<=200L),all(x$missing==0L),all(x$failures==0L))
x$rate <- x$successes/x$datasets
x$cp90_lower <- mapply(function(k,n) if(k==0) 0 else qbeta(.05,k,n-k+1),x$successes,x$datasets)
x$cp90_upper <- mapply(function(k,n) if(k==n) 1 else qbeta(.95,k+1,n-k),x$successes,x$datasets)
x$cp95_one_sided_lower <- mapply(function(k,n) if(k==0) 0 else qbeta(.05,k,n-k+1),x$successes,x$datasets)
x$scientific_release <- 'unavailable-pilot-only'
write.table(x,args[[2]],sep='\t',quote=FALSE,row.names=FALSE)
cat('Descriptive pilot CP90 intervals and one-sided CP95 lower bounds; no qualification\n')
print(sessionInfo())
