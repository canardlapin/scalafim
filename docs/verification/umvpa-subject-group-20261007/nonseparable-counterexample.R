# Independent rows and marginal Gaussian I are insufficient for a homogeneous feature law.
args <- commandArgs(trailingOnly=TRUE);out <- if(length(args))args[[1]] else "."
t <- c(-2,-1,1,2); c <- c(.25,-.25,-.25,.25);D <- cbind(1,t)
C <- matrix(0,8,8)
for(i in 1:4)C[(2*i-1):(2*i),(2*i-1):(2*i)] <- matrix(c(1,c[i],c[i],1),2,2)
A <- solve(crossprod(D),t(D)); P <- diag(4)-D%*%A
betaCov <- kronecker(A,diag(2))%*%C%*%t(kronecker(A,diag(2)))
expectedSigma <- Reduce(`+`,lapply(1:4,function(i)P[i,i]*C[(2*i-1):(2*i),(2*i-1):(2*i)]))/2
surrogate <- expectedSigma * solve(crossprod(D))[2,2]
values <- c(minEigen=min(eigen(C,symmetric=TRUE,only.values=TRUE)$values),trueSlopeCross=betaCov[3,4],pooledResidualCross=expectedSigma[1,2],pooledSurrogateCross=surrogate[1,2])
write.table(data.frame(name=names(values),value=unname(values)),file.path(out,"nonseparable-expected.tsv"),sep="\t",row.names=FALSE,quote=FALSE)
stopifnot(abs(values[["minEigen"]]-.75)<1e-12,abs(values[["trueSlopeCross"]]-.015)<1e-12,abs(values[["pooledResidualCross"]]+.075)<1e-12,abs(values[["pooledSurrogateCross"]]+.0075)<1e-12)
print(values,digits=17)
