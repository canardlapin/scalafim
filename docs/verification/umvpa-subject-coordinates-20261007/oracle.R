# Independent base-R change-of-basis oracle, with explicit row-major coefficient order.
args <- commandArgs(trailingOnly=TRUE)
out <- if (length(args)) args[[1]] else "."
dir.create(out, recursive=TRUE, showWarnings=FALSE)
B <- matrix(c(2,3,5,7),2,2,byrow=TRUE)
A <- matrix(c(2,0,0,0,1,2,0,0,0,1,3,0,1,0,1,2),4,4,byrow=TRUE)
Sigma <- A %*% t(A)
R <- matrix(c(1,1,0,1),2,2,byrow=TRUE)
M <- matrix(c(1,2,-1,1),2,2,byrow=TRUE)
rot <- matrix(c(0,-1,1,0),2,2,byrow=TRUE)
unequal <- matrix(c(1,0,0,1,.5,.5),3,2,byrow=TRUE)
record <- function(name, matrix) {
  table <- expand.grid(column=seq_len(ncol(matrix)),row=seq_len(nrow(matrix)))
  table$value <- as.vector(t(matrix))
  table$case <- name
  table[,c("case","row","column","value")]
}
records <- list(record("source.mean", B), record("source.covariance",Sigma),
  record("oblique.mean",M %*% B %*% t(solve(R))),
  record("oblique.covariance",kronecker(M,solve(R)) %*% Sigma %*% t(kronecker(M,solve(R)))),
  record("rotation.mean",M %*% B %*% t(solve(rot))),
  record("rotation.covariance",kronecker(M,solve(rot)) %*% Sigma %*% t(kronecker(M,solve(rot)))),
  record("gauge.mean",B %*% t(solve(R))),
  record("gauge.covariance",kronecker(diag(2),solve(R)) %*% Sigma %*% t(kronecker(diag(2),solve(R)))),
  record("unequal.mean",unequal %*% B %*% t(solve(R))),
  record("unequal.covariance",kronecker(unequal,solve(R)) %*% Sigma %*% t(kronecker(unequal,solve(R)))))
write.table(do.call(rbind, records), file.path(out,"expected.tsv"),sep="\t",row.names=FALSE,quote=FALSE)
writeLines(capture.output(sessionInfo()),file.path(out,"R-session.txt"))
cat("Wrote independent base-R mean/full-covariance fixture\n")
