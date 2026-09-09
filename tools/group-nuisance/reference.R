.libPaths(c(Sys.getenv('GROUP_NUISANCE_RLIB', .libPaths()[1]), .libPaths()))
args=commandArgs(trailingOnly=TRUE)
outDir=if(length(args)) args[1] else '.'
library(clubSandwich);library(jsonlite)
cases=list()
for(n in c(8,20,80)) for(grp in c('balanced','quarter')) {
 i=0:(n-1);g=as.numeric(i<if(grp=='balanced') n%/%2 else max(2,n%/%4));z=cos((i+.5)*2*pi/n)
 y=1.7+.45*g+.8*z+sqrt(seq(.04,1,length.out=n))*(sin((i+.7)*4.3)+cos(i*2.9))
 f=lm(y~g+z);v=vcovCR(f,cluster=seq_len(n),type='CR2',target=rep(1,n),inverse_var=FALSE)
 test=coef_test(f,vcov=v,test='Satterthwaite')
 cases[[length(cases)+1]]=list(n=n,group=grp,X=unname(model.matrix(f)),y=y,coef=unname(coef(f)), leverage=unname(hatvalues(f)), influence=unname(solve(crossprod(model.matrix(f)),t(model.matrix(f)))),covariance=matrix(as.numeric(v),nrow(v)),se=test$SE,df=test$df_Satt,p=test$p_Satt)
}
write_json(list(R=R.version.string,clubSandwich=as.character(packageVersion('clubSandwich')),cases=cases),file.path(outDir,'reference.json'),digits=17,pretty=TRUE,auto_unbox=TRUE)
