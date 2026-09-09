.libPaths(c(Sys.getenv('GROUP_NUISANCE_RLIB', .libPaths()[1]), .libPaths()))
library(clubSandwich);library(jsonlite)
args=commandArgs(trailingOnly=TRUE);outDir=if(length(args)) args[1] else '.'
inputs=fromJSON(file.path(outDir,'reference-input.json'),simplifyVector=FALSE)
mat=function(x) do.call(rbind,lapply(x,unlist))
results=list()
for(input in inputs) {
 X=mat(input$X);y=unlist(input$y);v=unlist(input$v);n=length(y)
 fit=lm(y~X-1);tests=list()
 for(method in c('HC2_Satt','HC3_Satt','CR2_true_target')) {
  covariance=vcovCR(fit,cluster=seq_len(n),type=if(method=='HC3_Satt') 'CR3' else 'CR2',target=if(method=='CR2_true_target') v else rep(1,n),inverse_var=FALSE)
  test=coef_test(fit,vcov=covariance,test='Satterthwaite')
  tests[[method]]=list(estimate=unname(coef(fit)[2]),variance=unname(covariance[2,2]),df=test$df_Satt[2],p=test$p_Satt[2])
 }
 fw=lm(y~X-1,weights=1/v);tests$oracle_WLS_t=list(p=unname(summary(fw)$coefficients[2,4]))
 vz=solve(crossprod(X,X/v))[2,2];tests$oracle_WLS_z=list(p=2*pnorm(-abs(coef(fw)[2])/sqrt(vz)))
 tests$OLS_t=list(p=unname(summary(fit)$coefficients[2,4]))
 R=X[,-2,drop=FALSE];fr=lm(y~R-1)
 for(hc in c(1,2)) for(family in c('rad','mammen')) {
  actions=mat(input$actions[[family]])
  scale=residuals(fr)/(1-hatvalues(fr))^(hc/2)
  statistic=function(response) {
   f=lm(response~X-1);vc=vcovCR(f,cluster=seq_len(n),type=if(hc==1) 'CR2' else 'CR3',target=rep(1,n),inverse_var=FALSE)
   unname(coef(f)[2]/sqrt(vc[2,2]))
  }
  observed=statistic(y);star=apply(actions,1,function(action) statistic(fitted(fr)+scale*action))
  tests[[paste0('wild_HC',hc+1,'_',family)]]=list(observed=observed,statistics=star,p=(1+sum(abs(star)>=abs(observed)-1e-12))/(1+length(star)))
 }
 results[[length(results)+1]]=list(cell=input$cell,tests=tests)
}
write_json(list(R=R.version.string,clubSandwich=as.character(packageVersion('clubSandwich')),results=results),file.path(outDir,'reference.json'),auto_unbox=TRUE,digits=17,pretty=TRUE)
