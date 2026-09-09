.libPaths(c(Sys.getenv('GROUP_NUISANCE_RLIB', .libPaths()[1]), .libPaths()))
library(clubSandwich);library(metafor);library(jsonlite)
args=commandArgs(trailingOnly=TRUE);outDir=if(length(args)) args[1] else '.'
inputs=fromJSON(file.path(outDir,'estimated-reference-input.json'),simplifyVector=FALSE)
mat=function(x) do.call(rbind,lapply(x,unlist))
results=list()
for(input in inputs) {
 X=mat(input$X);y=unlist(input$y);vi=unlist(input$vhat);total=vi+input$cell$tau2;n=length(y)
 fm=rma.uni(yi=y,vi=vi,mods=X,intercept=FALSE,method='PM',test='adhoc',control=list(threshold=1e-11,tol=1e-12,maxiter=1000))
 xw=X/sqrt(total);yw=y/sqrt(total);fw=lm(yw~xw-1)
 vc=vcovCR(fw,cluster=seq_len(n),type='CR3',target=rep(1,n),inverse_var=FALSE)
 ct=coef_test(fw,vcov=vc,test='Satterthwaite')
 results[[length(results)+1]]=list(index=input$index,study=input$study,estimate=unname(fm$b[2,1]),se=unname(fm$se[2]),t=unname(fm$zval[2]),p=unname(fm$pval[2]),tau2=unname(fm$tau2),plugin_GLS_t=unname(summary(fw)$coefficients[2,4]),plugin_HC3_Satt=ct$p_Satt[2],pluginDf=ct$df_Satt[2])
}
write_json(list(R=R.version.string,clubSandwich=as.character(packageVersion('clubSandwich')),metafor=as.character(packageVersion('metafor')),results=results),file.path(outDir,'estimated-reference.json'),auto_unbox=TRUE,digits=17,pretty=TRUE)
