# External simulator qualification. No candidate engine or confirmation entry point.
suppressMessages(library(jsonlite))
suppressMessages(library(parallel))
args <- commandArgs(TRUE)
stopifnot(length(args) %in% c(2L,3L), args[1] %in% c('qa','control'))
phase <- args[1]; out <- args[2]
root <- normalizePath('.')
dir <- file.path(root,'tools/scenarios/fgls-cmrg')
protocol <- fromJSON(file.path(dir,'protocol.json'), simplifyVector=FALSE)
cells <- fromJSON(file.path(dir,'cells.json'), simplifyVector=FALSE)
sha <- function(p) digest::digest(file=p,algo='sha256',serialize=FALSE)
locks <- setNames(lapply(c('qualify.R','protocol.json','cells.json'), function(p) sha(file.path(dir,p))), c('qualify.R','protocol.json','cells.json'))
if (phase=='control') {
  stopifnot(length(args)==3L)
  qa <- fromJSON(args[3],simplifyVector=FALSE)
  stopifnot(isTRUE(qa$passed),identical(qa$source_locks,locks),qa$phase=='qa')
}
stopifnot(!dir.exists(out)); dir.create(out,recursive=TRUE)
writeLines(toJSON(list(phase=phase,source_locks=locks,started=format(Sys.time(),tz='UTC'),role=protocol$role),auto_unbox=TRUE,pretty=TRUE),file.path(out,'started.json'))
setTimeLimit(elapsed=protocol$maximum_wall_seconds_per_phase,transient=FALSE)
RNGkind(protocol$rng[[1]],protocol$rng[[2]],protocol$rng[[3]])
set.seed(protocol$root_seed)
stream <- .Random.seed
for(i in seq_len(protocol$streams[[phase]])) stream <- nextRNGStream(stream)
N <- protocol$replicates; M <- protocol$qa$maximum_scalar_checks
p <- protocol$qa$family_alpha/(2*M)
meanBound <- qnorm(1-p)/sqrt(N)
varianceBounds <- qchisq(c(p,1-p),N-1)/(N-1)
checks <- 0L; failures <- list(); reports <- list(); manifest <- list()
check <- function(a,label) {
  # Rows are draw positions; columns are independent dataset streams.
  stopifnot(ncol(a)==N,all(is.finite(a)))
  means <- rowMeans(a); vars <- (rowSums(a*a)-N*means*means)/(N-1)
  checks <<- checks+2L*nrow(a)
  bad <- which(abs(means)>meanBound | vars<varianceBounds[1] | vars>varianceBounds[2])
  if(length(bad)) failures[[length(failures)+1L]] <<- list(label=label,positions=bad-1L,means=means[bad],variances=vars[bad])
  list(label=label,positions=nrow(a),max_abs_mean=max(abs(means)),min_variance=min(vars),max_variance=max(vars),failed=length(bad))
}
mat <- function(a) do.call(rbind,lapply(a,unlist))
states <- file(file.path(out,'stream-states.tsv'),'wt')
writeLines('phase\tcell\tdataset\tvoxel\trun\theader\ts1\ts2\ts3\ts4\ts5\ts6',states)
for(cell in cells) {
  X <- mat(cell$design); beta <- mat(cell$beta); lengths <- unlist(cell$run_lengths)
  n <- nrow(X); noise <- array(NA_real_,c(n,4L,N))
  innovations <- lapply(1:8,function(k) matrix(NA_real_,lengths[(k-1L)%%2L+1L]+2L,N))
  for(d in seq_len(N)) {
    sub <- stream; offset <- 0L
    for(v in 1:4) for(r in 1:2) {
      writeLines(paste(c(phase,cell$cell,d-1L,v-1L,r-1L,sub),collapse='\t'),states)
      assign('.Random.seed',sub,envir=.GlobalEnv)
      z <- rnorm(lengths[r]+2L)
      innovations[[(v-1L)*2L+r]][,d] <- z
      phi <- unlist(cell$phi_by_voxel_run[[v]][[r]])
      g0 <- 1/(1-phi[1]^2*(1+phi[2])/(1-phi[2])-phi[2]^2)
      g1 <- g0*phi[1]/(1-phi[2])
      initial <- as.numeric(t(chol(matrix(c(g0,g1,g1,g0),2))) %*% z[1:2])
      previous2 <- initial[1]; previous <- initial[2]
      offset <- if(r==1L) 0L else lengths[1]
      for(t in seq_len(lengths[r])) {
        value <- z[t+2L]+phi[1]*previous+phi[2]*previous2
        noise[offset+t,v,d] <- value
        previous2 <- previous; previous <- value
      }
      sub <- nextRNGSubStream(sub)
    }
    stream <- nextRNGStream(stream)
  }
  binary <- file.path(out,paste0(cell$cell,'.f64le'))
  writeBin(as.numeric(noise),binary,size=8L,endian='little')
  manifest[[length(manifest)+1L]] <- list(cell=cell$cell,file=basename(binary),sha256=sha(binary),shape=list(N,4L,n),order='dataset,voxel,time; time fastest',bytes=file.info(binary)$size)
  if(phase=='qa') {
    for(k in seq_along(innovations)) reports[[length(reports)+1L]] <- check(innovations[[k]],paste(cell$cell,'innovation',k))
    standard <- list()
    for(v in 1:4) for(r in 1:2) {
      phi <- unlist(cell$phi_by_voxel_run[[v]][[r]])
      g0 <- 1/(1-phi[1]^2*(1+phi[2])/(1-phi[2])-phi[2]^2)
      ix <- (if(r==1L) 0L else lengths[1])+seq_len(lengths[r])
      a <- noise[ix,v,]/sqrt(g0)
      standard[[length(standard)+1L]] <- a
      label <- paste(cell$cell,'stationary',v,r)
      reports[[length(reports)+1L]] <- check(a,label)
      acf <- as.numeric(ARMAacf(ar=phi,lag.max=4))
      for(lag in 1:4) for(sign in c(-1,1)) {
        b <- (a[(lag+1):nrow(a),,drop=FALSE]+sign*a[1:(nrow(a)-lag),,drop=FALSE])/sqrt(2*(1+sign*acf[lag+1]))
        reports[[length(reports)+1L]] <- check(b,paste(label,'lag',lag,'sign',sign))
      }
    }
    for(i in 1:7) for(j in (i+1):8) for(sign in c(-1,1)) {
      len <- min(nrow(standard[[i]]),nrow(standard[[j]]))
      b <- (standard[[i]][1:len,,drop=FALSE]+sign*standard[[j]][1:len,,drop=FALSE])/sqrt(2)
      reports[[length(reports)+1L]] <- check(b,paste(cell$cell,'independence',i,j,sign))
    }
  } else {
    keep <- setdiff(seq_len(n),unlist(cell$censored_rows_zero_based)+1L)
    oracle <- list()
    for(v in 1:4) {
      V <- matrix(0,n,n);offset <- 0L
      for(r in 1:2) {
        phi <- unlist(cell$phi_by_voxel_run[[v]][[r]])
        g0 <- 1/(1-phi[1]^2*(1+phi[2])/(1-phi[2])-phi[2]^2)
        ix <- offset+seq_len(lengths[r]);V[ix,ix] <- toeplitz(g0*as.numeric(ARMAacf(ar=phi,lag.max=lengths[r]-1L)))
        offset <- offset+lengths[r]
      }
      Xk <- X[keep,,drop=FALSE]; U <- chol(V[keep,keep]); E <- noise[keep,v,]
      ViX <- backsolve(U,forwardsolve(t(U),Xk)); C <- solve(crossprod(Xk,ViX)); A <- C%*%t(ViX)
      estimates <- beta[,v]+A%*%E; residual <- E-Xk%*%(estimates-beta[,v])
      df <- length(keep)-ncol(X);s2 <- colSums(forwardsolve(t(U),residual)^2)/df
      variance <- C[1,1]*s2; prob <- pf((estimates[1,]-beta[1,v])^2/variance,1,df,lower.tail=FALSE)
      B <- estimates[1:2,,drop=FALSE]-beta[1:2,v]
      fp <- pf(colSums(B*(solve(C[1:2,1:2])%*%B))/(2*s2),2,df,lower.tail=FALSE)
      oracle[[v]] <- list(coefficients=unname(t(estimates)),residual_variance=s2,variance=variance,probability=prob,f_probability=fp,df=df)
    }
    writeLines(toJSON(oracle,auto_unbox=TRUE,digits=NA),file.path(out,paste0(cell$cell,'.oracle.json')))
  }
  cat(phase,cell$cell,'complete\n');flush.console()
}
close(states)
stopifnot(checks<=M)
# Historical affine-seed counterexample is deliberately evaluated under the new QA bound.
negative <- NULL
if(phase=='qa') {
  state <- (1499711011+22*10000019+2*1000003+(0:(N-1))*7919) %% 2147483646+1
  for(i in 1:845) {
    state <- (state*48271) %% 2147483647;u1 <- state/2147483647
    state <- (state*48271) %% 2147483647;u2 <- state/2147483647
    z <- sqrt(-2*log(u1))*cos(2*pi*u2)
  }
  negative <- list(mean=mean(z),positive=sum(z>0),bound=meanBound,rejected=abs(mean(z))>meanBound)
  stopifnot(negative$rejected)
}
passed <- length(failures)==0L
result <- list(schema=protocol$schema,role=protocol$role,phase=phase,passed=passed,
  source_locks=locks,replicates=N,scalar_checks=checks,bounds=list(mean=meanBound,variance=varianceBounds),
  failures=failures,qa_reports=reports,legacy_negative_control=negative,inputs=manifest,
  stream_states_sha256=sha(file.path(out,'stream-states.tsv')),control_fit_results_qualified=FALSE,scientific_admission=FALSE)
writeLines(toJSON(result,auto_unbox=TRUE,pretty=TRUE,digits=NA),file.path(out,'receipt.json'))
capture.output(sessionInfo(),file=file.path(out,'R-session.txt'))
if(!passed) quit(status=1)
