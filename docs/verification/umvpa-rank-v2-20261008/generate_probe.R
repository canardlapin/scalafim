# Eight independently generated fixture datasets, only after source freeze.
args <- commandArgs(trailingOnly=TRUE)
root <- args[[1]]; assignments <- args[[2]]; output <- args[[3]]
source(file.path(root,'tools/mvpa-inference/rank_population.R'))
records <- read.delim(assignments,colClasses='character',check.names=FALSE)
stopifnot(nrow(records)==8L,!anyDuplicated(records$id))
dir.create(output,recursive=TRUE,showWarnings=FALSE)
RNGkind("L'Ecuyer-CMRG",normal.kind='Inversion',sample.kind='Rejection')
text <- function(x) paste(format(x,digits=17,scientific=TRUE,trim=TRUE),collapse=',')
matrix_line <- function(name,x) paste(name,nrow(x),ncol(x),text(as.vector(t(x))),sep='\t')
for(i in seq_len(nrow(records))) {
  .Random.seed <- c(10407L,as.integer(records[i,paste0('r_state',1:6)]))
  n <- as.integer(records$n[i]);p <- as.integer(records$p[i]);q <- as.integer(records$q[i])
  data <- rank_population(n,p,q,c(.5,.3,.2,0),records$nuisance[i])
  path <- file.path(output,paste0(records$id[i],'.tsv'))
  stopifnot(!file.exists(path))
  writeLines(c(paste('case','fixture',records$id[i],0,records$root_seed64[i],199,n,sep='\t'),
    matrix_line('X',data$x),matrix_line('Y',data$y),matrix_line('Z',data$nuisance),
    paste('rho',text(c(.5,.3,.2,0)),sep='\t')),path,useBytes=TRUE)
}
cat('Generated exactly eight resource fixtures; no pilot or confirmation.\n')
print(sessionInfo())
