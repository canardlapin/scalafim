#!/usr/bin/env Rscript
# The production basis fixes the coordinate action. Its geometry is checked
# independently; observed statistics also use R residuals without that basis.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L)
index <- read.delim(args[[1]],check.names=FALSE,stringsAsFactors=FALSE)
read_matrices <- function(path) {
  lines <- strsplit(readLines(path,warn=FALSE),'\t',fixed=TRUE)
  function(name) {
    row <- lines[[which(vapply(lines,function(x) x[[1]]==name,TRUE))]]
    matrix(as.numeric(strsplit(row[[4]],',',fixed=TRUE)[[1]]),as.integer(row[[2]]),as.integer(row[[3]]),byrow=TRUE)
  }
}
orth <- function(x) qr.Q(qr(x))[,seq_len(ncol(x)),drop=FALSE]
roots <- function(x,y) svd(crossprod(orth(x),orth(y)),nu=0,nv=0)$d
complete <- function(w) {
  if(nrow(w)==ncol(w)) w else cbind(w,qr.Q(qr(w),complete=TRUE)[,(ncol(w)+1):nrow(w),drop=FALSE])
}
reference <- function(x,y,actions) {
  qx <- orth(x); qy <- orth(y); s <- svd(crossprod(qx,qy))
  u <- x %*% complete(qr.solve(x,qx %*% s$u))
  v <- y %*% complete(qr.solve(y,qy %*% s$v))
  statistics <- t(vapply(actions,function(action) vapply(seq_len(min(ncol(x),ncol(y))),function(k)
    -sum(log1p(-roots(u[action,k:ncol(u),drop=FALSE],v[,k:ncol(v),drop=FALSE])^2)),0.0),numeric(4)))
  list(roots=s$d,statistics=statistics)
}
checks <- vector('list',nrow(index))
for(i in seq_len(nrow(index))) {
  meta <- index[i,]; d <- read_matrices(meta$input); e <- read_matrices(meta$export)
  x <- d('X');y <- d('Y');z <- d('Z');q <- e('Q'); n <- nrow(x);m <- ncol(q)
  zqr <- qr(z); residual <- diag(n)-tcrossprod(qr.Q(zqr)[,seq_len(zqr$rank),drop=FALSE])
  stopifnot(m==n-zqr$rank)
  compact <- reference(crossprod(q,x),crossprod(q,y),c(list(seq_len(m)),lapply(1:3,function(j) e('actions')[j,])))
  independent <- reference(qr.resid(zqr,x),qr.resid(zqr,y),list(seq_len(n)))
  checks[[i]] <- data.frame(platform=meta$platform,cell=meta$cell,
    orthogonality=max(abs(crossprod(q)-diag(m))),projector=max(abs(tcrossprod(q)-residual)),
    nuisance=max(abs(crossprod(q,z))),
    compact_roots=max(abs(compact$roots-e('roots'))),
    compact_observed=max(abs(compact$statistics[1,]-e('wilks'))),
    original_space_roots=max(abs(independent$roots-e('roots'))),
    original_space_observed=max(abs(independent$statistics[1,]-e('wilks'))),
    action_statistics=max(abs(compact$statistics[-1,,drop=FALSE]-e('nulls'))))
}
result <- do.call(rbind,checks)
write.table(result,file.path(args[[2]],'replay-checks.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
maximum <- max(as.matrix(result[,-c(1,2)]))
stopifnot(nrow(result)==32L,all(table(result$platform)==16L),maximum<1e-10)
cat('PASS:',nrow(result),'platform/case exports, 3 fixed actions and 4 hypotheses each; maximum absolute discrepancy',format(maximum,digits=17),'\n')
print(sessionInfo())
