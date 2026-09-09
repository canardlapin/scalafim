.libPaths(c(Sys.getenv('GROUP_NUISANCE_RLIB', .libPaths()[1]), .libPaths()))
library(clubSandwich);library(jsonlite)
args=commandArgs(trailingOnly=TRUE);outDir=if(length(args)) args[1] else '.'
inputs=fromJSON(file.path(outDir,'feasible-reference-input.json'),simplifyVector=FALSE)
mat=function(x) do.call(rbind,lapply(x,unlist))
results=list()
for(input in inputs) {
 X=mat(input$X);y=unlist(input$y);vi=unlist(input$vhat);n=length(y)
 xw=X/sqrt(vi);yw=y/sqrt(vi);fw=lm(yw~xw-1)
 vc=vcovCR(fw,cluster=seq_len(n),type='CR3',target=rep(1,n),inverse_var=FALSE)
 ct=coef_test(fw,vcov=vc,test='Satterthwaite')
 results[[length(results)+1]]=list(index=input$index,study=input$study,p=ct$p_Satt[2],df=ct$df_Satt[2])
}
write_json(list(R=R.version.string,clubSandwich=as.character(packageVersion('clubSandwich')),results=results),file.path(outDir,'feasible-reference.json'),auto_unbox=TRUE,digits=17,pretty=TRUE)
