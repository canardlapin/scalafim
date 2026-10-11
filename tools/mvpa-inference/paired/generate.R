# Generate each declared exploratory dataset exactly once for all three methods.
suppressMessages(library(jsonlite))
args <- commandArgs(TRUE)
stopifnot(length(args)==4L)
manifest <- fromJSON(args[1],simplifyVector=FALSE)
cell <- Filter(function(c)c$id==args[2],manifest$cells)[[1L]]
records <- read.delim(args[3],colClasses='character',check.names=FALSE)
out <- args[4]
stopifnot(manifest$role=='exploratory-pilot-only',nrow(records)==200L,
          all(records$phase=='pilot'),all(records$scenario_id==cell$id),
          identical(as.integer(records$dataset_index),0:199),!anyDuplicated(records$root_seed64),
          cell$draws==199L,cell$datasets==200L,!dir.exists(out))
source('tools/mvpa-inference/rank_population.R')
dir.create(out,recursive=TRUE)
number_text <- function(x)paste(format(x,digits=17,scientific=TRUE,trim=TRUE),collapse=',')
line <- function(name,x)paste(name,nrow(x),ncol(x),number_text(as.vector(t(x))),sep='\t')
RNGkind("L'Ecuyer-CMRG",normal.kind='Inversion',sample.kind='Rejection')
for(i in 1:200) {
  assign('.Random.seed',c(10407L,as.integer(records[i,paste0('r_state',1:6)])),envir=.GlobalEnv)
  x <- rank_population(cell$n,cell$p,cell$q,unlist(cell$rho),cell$nuisance)
  path <- file.path(out,sprintf('dataset-%05d.tsv',i-1L))
  stopifnot(!file.exists(path))
  writeLines(c(paste('case','pilot',cell$id,i-1L,records$root_seed64[i],199L,cell$n,sep='\t'),
    line('X',x$x),line('Y',x$y),line('Z',x$nuisance),paste('rho',number_text(unlist(cell$rho)),sep='\t')),
    path,useBytes=TRUE)
}
capture.output(sessionInfo(),file=file.path(out,'R-session.txt'))
cat('generated exactly 200 archived pilot inputs; no fitting or selection\n')
