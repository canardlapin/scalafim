# Independent base-R first-level loading and known-variance small-group fixtures.
args <- commandArgs(trailingOnly=TRUE); out <- if(length(args)) args[[1]] else "."
dir.create(out,recursive=TRUE,showWarnings=FALSE)
record <- function(name, x) {
  x <- as.matrix(x); z <- expand.grid(column=seq_len(ncol(x)),row=seq_len(nrow(x)))
  z$value <- as.vector(t(x)); z$case <- name; z[,c("case","row","column","value")]
}
M <- matrix(c(1,.25,.25,9),2,2,byrow=TRUE)
C <- list(matrix(c(1,.4,.4,4),2,2,byrow=TRUE),matrix(c(4,-.3,-.3,1),2,2,byrow=TRUE),matrix(c(1,.2,.2,1),2,2,byrow=TRUE))
Gamma <- lapply(C,function(x)kronecker(M,x))
B <- rbind(c(1,2,10,20),c(3,4,30,40),c(5,6,50,60))
V <- do.call(rbind,lapply(Gamma,diag)); W <- 1/V
mean <- colSums(W*B)/colSums(W); variance <- 1/colSums(W)
z <- list(record("known.group.mean",matrix(mean,1)),record("known.group.variance",matrix(variance,1)),record("known.group.se",matrix(sqrt(variance),1)))
for(i in seq_along(Gamma))z <- c(z,list(record(paste0("known.subject.",i,".covariance"),Gamma[[i]])))
t1 <- c(1,1,1,1,-1,-1,-1,-1);t2 <- c(1,1,-1,-1,1,1,-1,-1)
drift <- c(1,-1,1,-1,1,-1,1,-1);e1 <- c(1,1,-1,-1,-1,-1,1,1);e2 <- c(1,-1,1,-1,-1,1,-1,1)
Y <- cbind(t1,t2); X <- cbind(7+4*drift+2*t1-t2+.5*e1,-3-2*drift-t1+3*t2+.25*(.5*e1+sqrt(.75)*e2))
Cs <- matrix(c(1,1,0,1),2,2,byrow=TRUE); Cg <- matrix(c(1,.5,0,1),2,2,byrow=TRUE)
D <- cbind(1,drift,Y%*%Cs);beta <- solve(crossprod(D),crossprod(D,X));df <- nrow(D)-ncol(D)
Sigma <- crossprod(X-D%*%beta)/df;G <- solve(crossprod(D))[3:4,3:4]
R <- solve(Cs,Cg);joint <- kronecker(Sigma,G); T <- kronecker(diag(2),solve(R))
common <- t(beta[3:4,])%*%t(solve(R));covariance <- T%*%joint%*%t(T)
z <- c(z,list(record("loading.normalized.component.covariance",G),record("loading.residual.feature.covariance",Sigma),record("loading.source.full.covariance",joint),record("loading.common.mean",common),record("loading.common.full.covariance",covariance),record("loading.residual.df",matrix(df,1))))
write.table(do.call(rbind,z),file.path(out,"expected.tsv"),sep="\t",quote=FALSE,row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"R-session.txt"))
cat("Independent base-R full covariance and known-variance group oracle written\n")
