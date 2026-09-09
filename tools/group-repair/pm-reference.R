suppressPackageStartupMessages(library(metafor))
args=commandArgs(TRUE)
x=as.matrix(read.csv(file.path(args[1],'x.csv'),header=FALSE));y=as.matrix(read.csv(file.path(args[1],'y.csv'),header=FALSE));v=as.matrix(read.csv(file.path(args[1],'v.csv'),header=FALSE))
rows=list()
for(mode in c('DL','PM'))for(test in c('z','adhoc'))for(s in seq_len(ncol(y))){
 f=rma.uni(yi=y[,s],vi=v[,s],mods=x,intercept=FALSE,method=mode,test=test,control=list(threshold=1e-11,tol=1e-12,maxiter=1000));b=coef(f);cov=vcov(f);w=c(0,1,-.5,.2);est=sum(w*b);se=sqrt(drop(t(w)%*%cov%*%w));stat=est/se
 rows[[length(rows)+1]]=data.frame(mode=mode,test=test,sample=s-1,estimate=est,se=se,stat=stat,p=if(test=='adhoc')2*pt(-abs(stat),nrow(x)-ncol(x)) else 2*pnorm(-abs(stat)),tau=f$tau2,q=f$QE,b0=b[1],b1=b[2],b2=b[3],b3=b[4],se0=sqrt(cov[1,1]),se1=sqrt(cov[2,2]),se2=sqrt(cov[3,3]),se3=sqrt(cov[4,4]))
}
write.csv(do.call(rbind,rows),file.path(args[2],'pm-reference.csv'),row.names=FALSE)
writeLines(capture.output(sessionInfo()),file.path(args[2],'reference-session.txt'))
