# Descriptive paired exploration; no final calibration admission.
suppressMessages(library(jsonlite))
args<-commandArgs(TRUE);stopifnot(length(args)==3L)
manifest<-fromJSON(args[1],simplifyVector=FALSE);packet<-args[2];out<-args[3]
receipt<-fromJSON(file.path(packet,'campaign.json'),simplifyVector=FALSE)
stopifnot(isTRUE(receipt$complete),identical(receipt$scientific_admission,FALSE))
dir.create(out,recursive=TRUE,showWarnings=FALSE)
cp<-function(k,n,confidence=.90) {
 a<-(1-confidence)/2
 c(if(k==0)0 else qbeta(a,k,n-k+1),if(k==n)1 else qbeta(1-a,k+1,n-k))
}
methods<-unlist(manifest$methods);results<-paired<-list();all_rows<-list()
for(cell in manifest$cells) {
 rows<-list()
 for(provider in c('historical','current')) {
  con<-gzfile(file.path(packet,cell$id,paste0(provider,'-records.jsonl.gz')),'rt')
  raw<-readLines(con);close(con)
  rows<-c(rows,lapply(raw,fromJSON,simplifyVector=FALSE))
 }
 stopifnot(length(rows)==600L,all(vapply(rows,function(r)r$status=='evaluated',FALSE)))
 rank<-sum(unlist(cell$rho)>0);first<-rank+1L
 by_method<-list()
 for(method in methods) {
  a<-Filter(function(r)r$rank_method==method,rows)
  a<-a[order(vapply(a,function(r)r$dataset_index,0))]
  stopifnot(length(a)==200L,identical(vapply(a,function(r)as.integer(r$dataset_index),0L),0:199))
  raw<-do.call(rbind,lapply(a,function(r)unlist(r$raw_reject)))
  closed<-do.call(rbind,lapply(a,function(r)unlist(r$closed_reject)))
  first_true<-raw[,first];over<-closed[,first]
  h3<-closed[,3L]
  sizes<-colSums(raw);closed_sizes<-colSums(closed)
  results[[length(results)+1L]]<-list(cell=cell$id,method=method,n=cell$n,p=cell$p,q=cell$q,nuisance=cell$nuisance,
   population=cell$population,rho=unlist(cell$rho),datasets=200L,draws=199L,
   first_true_null_stage=first,first_true_null_rejections=sum(first_true),
   first_true_null_rate=mean(first_true),first_true_null_cp90=cp(sum(first_true),200),
   false_rank_overestimates=sum(over),false_rank_rate=mean(over),false_rank_cp90=cp(sum(over),200),
   closed_H3_detections=sum(h3),closed_H3_rate=mean(h3),closed_H3_cp90=cp(sum(h3),200),
   closed_H3_lower95=if(sum(h3)==0)0 else qbeta(.05,sum(h3),201-sum(h3)),
   closed_H3_standard_power_claim=cell$population=='R3',
   raw_stage_counts=sizes,closed_stage_counts=closed_sizes,
   mean_detected_rank=mean(rowSums(closed)),mean_adapter_seconds=mean(vapply(a,function(r)r$elapsed_seconds,0)),
   scientific_admission=FALSE)
  by_method[[method]]<-list(raw=raw,closed=closed,first_true=first_true,over=over,h3=h3)
 }
 for(pair in combn(methods,2,simplify=FALSE)) {
  for(metric in c('first_true','over','h3')) {
   a<-by_method[[pair[1]]][[metric]];b<-by_method[[pair[2]]][[metric]]
   delta<-as.numeric(b)-as.numeric(a);half<-qnorm(.975)*sd(delta)/sqrt(length(delta))
   paired[[length(paired)+1L]]<-list(cell=cell$id,left=pair[1],right=pair[2],metric=metric,
     both=sum(a&b),left_only=sum(a&!b),right_only=sum(!a&b),neither=sum(!a&!b),
     right_minus_left=mean(delta),approximate_paired_normal95=mean(delta)+c(-half,half),
     scope='Exploratory paired descriptive interval; no multiplicity-adjusted or final admission claim')
  }
 }
}
summary<-list(role='complete paired exploratory comparison; scientific admission unavailable',
 unique_datasets=12800L,method_evaluations=38400L,methods=methods,results=results,paired=paired)
writeLines(toJSON(summary,auto_unbox=TRUE,pretty=TRUE,digits=NA),file.path(out,'summary.json'))
tab<-do.call(rbind,lapply(results,function(r)data.frame(cell=r$cell,method=r$method,
 first_true_rate=r$first_true_null_rate,overestimate_rate=r$false_rank_rate,H3_rate=r$closed_H3_rate,
 H3_lower95=r$closed_H3_lower95,adapter_seconds=r$mean_adapter_seconds)))
write.csv(tab,file.path(out,'table.csv'),row.names=FALSE)
capture.output(sessionInfo(),file=file.path(out,'R-session.txt'))
print(tab[grepl('paired-R3-n',tab$cell),],row.names=FALSE)
