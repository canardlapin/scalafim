suppressPackageStartupMessages(library(metafor))
x=as.matrix(read.csv('x.csv',header=FALSE));y=as.matrix(read.csv('y.csv',header=FALSE));v=as.matrix(read.csv('v.csv',header=FALSE))
rows=list()
for(mode in c('OLS','FE','DL'))for(s in seq_len(ncol(y))){
 if(mode=='OLS'){
  f=lm(y[,s]~x-1);b=coef(f);cov=vcov(f);df=f$df.residual;tau=NA;q=NA
 }else{
  f=rma.uni(yi=y[,s],vi=v[,s],mods=x,intercept=FALSE,method=mode,test='z');b=coef(f);cov=vcov(f);df=NA;tau=f$tau2;q=f$QE
 }
 w=c(0,1,-.5,.2);est=sum(w*b);se=sqrt(drop(t(w)%*%cov%*%w));stat=est/se
 rows[[length(rows)+1]]=data.frame(mode=mode,sample=s-1,estimate=est,se=se,stat=stat,p=if(mode=='OLS')2*pt(-abs(stat),df) else 2*pnorm(-abs(stat)),tau=tau,q=q,b0=b[1],b1=b[2],b2=b[3],b3=b[4],se0=sqrt(cov[1,1]),se1=sqrt(cov[2,2]),se2=sqrt(cov[3,3]),se3=sqrt(cov[4,4]))
}
write.csv(do.call(rbind,rows),'reference.csv',row.names=FALSE)
writeLines(capture.output(sessionInfo()),'reference-session.txt')
