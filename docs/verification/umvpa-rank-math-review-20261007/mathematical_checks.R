#!/usr/bin/env Rscript
# Deterministic mathematical counterexamples, not a calibration campaign.
# No RNG, new pilot populations or confirmation streams are used.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==1L)
out <- args[[1]]; dir.create(out,recursive=TRUE,showWarnings=FALSE)
options(digits=17)
permutations <- function(x) {
  if(length(x)==1L) return(matrix(x,1L))
  do.call(rbind,lapply(seq_along(x),function(i) cbind(x[[i]],permutations(x[-i]))))
}
orth <- function(x) qr.Q(qr(x))[,seq_len(ncol(x)),drop=FALSE]
cca <- function(x,y) svd(crossprod(orth(x),orth(y)))
projector <- function(x) tcrossprod(orth(x))
complete_coefficients <- function(w) {
  if(nrow(w)==ncol(w)) w else cbind(w,qr.Q(qr(w),complete=TRUE)[,(ncol(w)+1L):nrow(w),drop=FALSE])
}
complete_scores <- function(q,w) q %*% complete_coefficients(w)
reference <- function(x,y,actions,completion) {
  qx <- orth(x);qy <- orth(y);s <- svd(crossprod(qx,qy))
  if(completion=='coefficient-euclidean') {
    u <- x %*% complete_coefficients(qr.solve(x,qx %*% s$u))
    v <- y %*% complete_coefficients(qr.solve(y,qy %*% s$v))
  } else {
    stopifnot(completion=='score-orthogonal')
    u <- complete_scores(qx,s$u);v <- complete_scores(qy,s$v)
  }
  bases <- lapply(seq_len(min(ncol(x),ncol(y))),function(k)
    list(x=orth(u[,k:ncol(u),drop=FALSE]),y=orth(v[,k:ncol(v),drop=FALSE])))
  statistic <- function(action) vapply(bases,function(b) {
    roots <- svd(crossprod(b$x[action,,drop=FALSE],b$y),nu=0,nv=0)$d
    stopifnot(all(roots<1-1e-10))
    -sum(log1p(-roots^2))
  },0.0)
  observed <- statistic(seq_len(nrow(x)))
  null <- t(apply(actions,1L,statistic))
  # Includes the identity. The tolerance only stabilizes algebraic equalities.
  counts <- colSums(sweep(null,2L,observed,`-`) >= -1e-12)
  list(roots=s$d,observed=observed,null=null,counts=counts,
       last_projector=projector(v[,2:ncol(v),drop=FALSE]))
}

# A fully specified six-dimensional rotation makes every action nondegenerate.
rotation <- diag(6)
for(j in 1:5) for(k in (j+1):6) {
  angle <- (j+2*k)/17
  g <- diag(6);g[j,j] <- g[k,k] <- cos(angle)
  g[j,k] <- -sin(angle);g[k,j] <- sin(angle)
  rotation <- rotation %*% g
}
x0 <- diag(6)[,1:2]
y0 <- cbind(.8*diag(6)[,1]+.6*diag(6)[,3],
            .2*diag(6)[,2]+sqrt(.96)*diag(6)[,4],
            .7*diag(6)[,5]+sqrt(.51)*diag(6)[,6])
x <- rotation %*% x0;y <- rotation %*% y0
shear <- diag(3);shear[1,3] <- 2
actions <- permutations(1:6)
stopifnot(nrow(actions)==720L,max(abs(crossprod(rotation)-diag(6)))<1e-12)
results <- list();details <- list()
for(completion in c('coefficient-euclidean','score-orthogonal')) {
  a <- reference(x,y,actions,completion)
  b <- reference(x,y %*% shear,actions,completion)
  results[[completion]] <- data.frame(completion=completion,
    max_root_difference=max(abs(a$roots-b$roots)),
    max_observed_difference=max(abs(a$observed-b$observed)),
    max_null_h1_difference=max(abs(a$null[,1]-b$null[,1])),
    max_null_h2_difference=max(abs(a$null[,2]-b$null[,2])),
    tail_projector_difference=max(abs(a$last_projector-b$last_projector)),
    base_h1_count=a$counts[[1]],sheared_h1_count=b$counts[[1]],
    base_h2_count=a$counts[[2]],sheared_h2_count=b$counts[[2]],actions=720L)
  details[[completion]] <- data.frame(completion=completion,action=seq_len(720L),
    base_h1=a$null[,1],sheared_h1=b$null[,1],base_h2=a$null[,2],sheared_h2=b$null[,2])
}
affine <- do.call(rbind,results)
stopifnot(max(affine$max_root_difference)<1e-12,max(affine$max_observed_difference)<1e-12,
          max(affine$max_null_h1_difference)<1e-10,
          affine['coefficient-euclidean','max_null_h2_difference']>1e-3,
          affine['score-orthogonal','max_null_h2_difference']<1e-10)
write.table(affine,file.path(out,'affine-completion.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
write.table(do.call(rbind,details),file.path(out,'affine-actions.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
write.table(cbind(x,y),file.path(out,'affine-input.tsv'),sep='\t',quote=FALSE,row.names=FALSE)

# Rootwise interlacing also bounds the entire unscaled Wilks tail, not only
# its largest root. This checks the algebra, not a population rejection rate.
interlacing <- list()
for(mix in c(0,1,2)) {
  transform <- diag(2);transform[1,2] <- mix
  xx <- x %*% transform
  all_roots <- cca(xx,y)$d
  for(s in 0:1) {
    reduced_roots <- cca(xx[,seq_len(2-s),drop=FALSE],y)$d
    tail <- all_roots[(s+1):2]
    lhs <- -sum(log1p(-tail^2));rhs <- -sum(log1p(-reduced_roots^2))
    stopifnot(max(tail-reduced_roots)<1e-12,lhs<=rhs+1e-12)
    interlacing[[length(interlacing)+1L]] <- data.frame(mix=mix,s=s,
      largest_rootwise_difference=max(tail-reduced_roots),tail_wilks=lhs,reference_wilks=rhs)
  }
}
write.table(do.call(rbind,interlacing),file.path(out,'interlacing-checks.tsv'),sep='\t',quote=FALSE,row.names=FALSE)

# Equal second moments after projection do not imply exchangeable residuals.
signs <- as.matrix(expand.grid(rep(list(c(-1,1)),4)))
q <- cbind(c(1,-1,0,0)/sqrt(2),c(1,1,-1,-1)/2,c(0,0,1,-1)/sqrt(2))
stopifnot(max(abs(crossprod(q)-diag(3)))<1e-12,max(abs(colSums(q)))<1e-12)
e <- signs %*% q
moments <- data.frame(coordinate=1:3,second=colMeans(e^2),fourth=colMeans(e^4))
stopifnot(max(abs(moments$second-1))<1e-12,max(abs(moments$fourth-c(2,2.5,2)))<1e-12)
write.table(moments,file.path(out,'non-gaussian-residual-moments.tsv'),sep='\t',quote=FALSE,row.names=FALSE)

# Independent discovery of an imperfect direction does not erase true rank-one association.
rho <- .5;theta <- pi/6
covariance <- diag(c(rho,0))
tail <- c(-sin(theta),cos(theta))
leak <- drop(t(tail) %*% covariance %*% tail)
stopifnot(abs(leak-.125)<1e-12)
write.table(data.frame(population_rank=1,rho=rho,angle=theta,tail_association=leak),
            file.path(out,'frozen-direction-leakage.tsv'),sep='\t',quote=FALSE,row.names=FALSE)
cat('PASS: affine invariant roots/observed statistics; coefficient completion changes transformed tails; score completion preserves them.\n')
print(affine)
cat('PASS: exact Rademacher moments and frozen-direction rank-one leakage counterexamples.\n')
print(moments);print(leak)
print(sessionInfo())
