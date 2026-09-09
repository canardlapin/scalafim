.libPaths(c(Sys.getenv('GROUP_NUISANCE_RLIB', .libPaths()[1]), .libPaths()))
args=commandArgs(trailingOnly=TRUE)
outDir=if(length(args)) args[1] else '.'
library(clubSandwich);library(jsonlite)
n=8;i=0:(n-1);g=as.numeric(i<2);z=cos((i+.5)*2*pi/n)
y=1.7+.45*g+.8*z+sqrt(seq(.04,1,length.out=n))*(sin((i+.7)*4.3)+cos(i*2.9))
actions=as.matrix(expand.grid(rep(list(c(-1,1)),n)))
stat=function(y) { f=lm(y~g+z); v=vcovCR(f,cluster=seq_len(n),type='CR2',target=rep(1,n),inverse_var=FALSE);unname(coef(f)[2]/sqrt(v[2,2])) }
restricted=lm(y~z)
residual=residuals(restricted)/sqrt(1-hatvalues(restricted));fitted=fitted(restricted)
observed=stat(y)
boot=apply(actions,1,function(s) stat(fitted+s*residual))
write_json(list(R=R.version.string,clubSandwich=as.character(packageVersion('clubSandwich')),X=unname(model.matrix(lm(y~g+z))),y=y,actions=unname(actions),observed=observed,statistics=boot,exceedances=sum(abs(boot)>=abs(observed)-1e-12),monteCarloP=(1+sum(abs(boot)>=abs(observed)-1e-12))/257),file.path(outDir,'bootstrap-reference.json'),digits=17,pretty=TRUE,auto_unbox=TRUE)
