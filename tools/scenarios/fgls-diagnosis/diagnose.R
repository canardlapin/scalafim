# Independent, deterministic reconstruction of previously spent FGLS inputs.
# No RNG calls or new datasets: explicit historical integer state arithmetic.
suppressMessages(library(jsonlite))
args <- commandArgs(TRUE)
stopifnot(length(args) == 4L)
input_path <- args[1]; jvm_path <- args[2]; js_path <- args[3]; out <- args[4]
dir.create(out, recursive=TRUE, showWarnings=FALSE)
mat <- function(x) do.call(rbind, lapply(x, unlist))
read_packets <- function(path, prefix) {
  con <- if (grepl("\\.gz$",path)) gzfile(path,"rt") else file(path,"rt")
  on.exit(close(con)); lines <- readLines(con, warn=FALSE)
  lapply(substring(lines[startsWith(lines,prefix)], nchar(prefix)+1L), fromJSON, simplifyVector=FALSE)
}
inputs <- read_packets(input_path,"FGLS_DIAGNOSIS_INPUT ")
jvm <- read_packets(jvm_path,"FGLS_STUDY_TRIAL ")
js <- read_packets(js_path,"FGLS_STUDY_TRIAL ")
stopifnot(length(inputs)==5L, length(jvm)==20480L, length(js)==20480L)
maximum <- function(a,b) max(abs(a-b)/(1+abs(b)))
assert_close <- function(a,b,label,tolerance=1e-8) {
  d <- maximum(a,b)
  if (!is.finite(d) || d > tolerance) stop(label,": ",d)
  d
}
results <- list(); oracle_fixtures <- list()
for (cell in inputs) {
  X <- mat(cell$design); beta <- mat(cell$beta)
  lengths <- unlist(cell$run_lengths); n <- nrow(X); p <- ncol(X)
  keep <- setdiff(seq_len(n),unlist(cell$censored_rows_zero_based)+1L)
  N <- 2048L; reps <- 0:(N-1L)
  seeds <- (1499711011+22*10000019+(cell$group+1)*1000003+reps*7919) %% 2147483646+1
  state <- seeds
  errors <- array(NA_real_,c(n,N,2L)); innovations <- vector("list",2L)
  uniform_states <- vector("list",2L)
  for (voxel in 1:2) {
    start <- 0L; innovations[[voxel]] <- list(); uniform_states[[voxel]] <- list()
    for (run in seq_along(lengths)) {
      phi <- unlist(cell$phi_by_voxel_run[[voxel]][[run]])
      len <- lengths[run]; previous <- previous2 <- rep(0,N)
      inn <- radial <- matrix(NA_real_,len+256L,N)
      for (step in seq_len(len+256L)) {
        state <- (state*48271) %% 2147483647; u1 <- state/2147483647
        radial[step,] <- state
        state <- (state*48271) %% 2147483647; u2 <- state/2147483647
        innovation <- sqrt(-2*log(u1))*cos(2*pi*u2)
        value <- innovation+phi[1]*previous+phi[2]*previous2
        inn[step,] <- innovation
        if (step>256L) errors[start+step-256L,,voxel] <- value
        previous2 <- previous; previous <- value
      }
      innovations[[voxel]][[run]] <- inn
      uniform_states[[voxel]][[run]] <- radial
      start <- start+len
    }
  }
  noise_gap <- 0
  for (fixture in cell$fixtures) {
    i <- fixture$replicate+1L
    stopifnot(fixture$seed==seeds[i])
    noise_gap <- max(noise_gap,assert_close(errors[,i,],mat(fixture$noise),"retained noise"))
  }
  data <- lapply(list(jvm,js),function(records) {
    a <- Filter(function(r) r$cell==cell$cell && r$engine=="known-continuous", records)
    a <- a[order(vapply(a,function(r)r$replicate,0))]
    stopifnot(length(a)==N,identical(vapply(a,function(r)as.integer(r$replicate),0L),reps))
    a
  })
  fields <- function(a,name) vapply(a,function(r)r[[name]],0)
  voxel_results <- list(); retained_fit <- list()
  for (voxel in 1:2) {
    V <- matrix(0,n,n); start <- 0L
    for (run in seq_along(lengths)) {
      phi <- unlist(cell$phi_by_voxel_run[[voxel]][[run]])
      gamma0 <- 1/(1-phi[1]^2*(1+phi[2])/(1-phi[2])-phi[2]^2)
      gamma <- gamma0*as.numeric(stats::ARMAacf(ar=phi,lag.max=lengths[run]-1L))
      ix <- start+seq_len(lengths[run]); V[ix,ix] <- toeplitz(gamma)
      start <- start+lengths[run]
    }
    Vk <- V[keep,keep]; Xk <- X[keep,,drop=FALSE]
    # Independent dense retained-row covariance; no censor indicator columns or
    # Scala recursion-factor implementation in this oracle.
    U <- chol(Vk); ViX <- backsolve(U,forwardsolve(t(U),Xk))
    C <- solve(crossprod(Xk,ViX)); A <- C %*% t(ViX)
    E <- errors[keep,,voxel]; estimates <- beta[,voxel] + A %*% E
    resid <- E-Xk%*%(estimates-beta[,voxel]); white <- forwardsolve(t(U),resid)
    df <- length(keep)-p; s2 <- colSums(white^2)/df
    variance <- C[1,1]*s2
    tp <- pf((estimates[1,]-beta[1,voxel])^2/variance,1,df,lower.tail=FALSE)
    estimate_name <- if(voxel==1L) "nullEstimate" else "signalEstimate"
    variance_name <- if(voxel==1L) "nullVariance" else "signalVariance"
    probability_name <- if(voxel==1L) "tPValue" else "coverPValue"
    gaps <- list()
    for(platform in 1:2) {
      d <- data[[platform]]
      gaps[[platform]] <- c(
        estimate=assert_close(estimates[1,],fields(d,estimate_name),estimate_name),
        variance=assert_close(variance,fields(d,variance_name),variance_name),
        probability=assert_close(tp,fields(d,probability_name),probability_name))
      stopifnot(all(fields(d,"tDf")==df))
      if(voxel==1L) {
        B <- estimates[1:2,,drop=FALSE]
        fp <- pf(colSums(B*(solve(C[1:2,1:2])%*%B))/(2*s2),2,df,lower.tail=FALSE)
        gaps[[platform]] <- c(gaps[[platform]],f_probability=assert_close(fp,fields(d,"fPValue"),"F probability"))
        stopifnot(identical(tp<.05,vapply(d,function(r)r$tRejected,FALSE)),
                  identical(fp<.05,vapply(d,function(r)r$fRejected,FALSE)))
      } else stopifnot(identical(tp>=.05,vapply(d,function(r)r$covered,FALSE)))
    }
    mean_error <- rowMeans(E)
    mean_beta <- mean(estimates[1,]-beta[1,voxel])
    decomposition <- as.numeric(A[1,]%*%mean_error)
    assert_close(mean_beta,decomposition,"mean linear decomposition",1e-12)
    full_weights <- numeric(n); full_weights[keep] <- A[1,]
    contributions <- list(); start <- 0L
    for (run in seq_along(lengths)) {
      len <- lengths[run]; phi <- unlist(cell$phi_by_voxel_run[[voxel]][[run]])
      psi <- numeric(len+256L); psi[1] <- 1; psi[2] <- phi[1]
      for(i in 3:length(psi)) psi[i] <- phi[1]*psi[i-1]+phi[2]*psi[i-2]
      H <- matrix(0,len,len+256L)
      for(i in seq_len(len)) H[i,seq_len(i+256L)] <- rev(psi[seq_len(i+256L)])
      iw <- as.numeric(crossprod(H,full_weights[start+seq_len(len)]))
      means <- rowMeans(innovations[[voxel]][[run]])
      states <- uniform_states[[voxel]][[run]]
      contributions[[run]] <- data.frame(run=run-1L,time_zero_based=seq_len(len+256L)-257L,
        mean_innovation=means,linear_weight=iw,contribution=iw*means,
        radial_u_min=apply(states,1,min)/2147483647,radial_u_max=apply(states,1,max)/2147483647,
        radial_state_step=(states[,2]-states[,1]) %% 2147483647,
        angular_u_first=((states[,1]*48271) %% 2147483647)/2147483647,
        angular_u_last=((states[,N]*48271) %% 2147483647)/2147483647,
        angular_state_step=(((states[,2]-states[,1])*48271) %% 2147483647),
        positive_innovations=rowSums(innovations[[voxel]][[run]]>0))
      start <- start+len
    }
    ct <- do.call(rbind,contributions)
    assert_close(sum(ct$contribution),mean_beta,"innovation decomposition",1e-12)
    ct <- ct[order(-abs(ct$contribution)),]
    write.csv(ct,file.path(out,paste0(cell$cell,"-voxel",voxel-1L,"-contributions.csv")),row.names=FALSE)
    retained_fit[[voxel]] <- lapply(cell$fixtures,function(f) {
      i <- f$replicate+1L
      list(replicate=f$replicate,coefficients=unname(estimates[,i]),residual_variance=s2[i],
        variance=variance[i],probability=tp[i],df=df)
    })
    voxel_results[[voxel]] <- list(voxel=voxel-1L,truth=beta[1,voxel],
      mean_error=mean_beta,standardized_bias=mean_beta/sd(estimates[1,]),
      linear_decomposition=decomposition,innovation_decomposition=sum(ct$contribution),
      mean_sigma2=mean(s2),theoretical_coefficient_variance=C[1,1],
      actual_coefficient_variance=var(estimates[1,]),gaps=gaps,
      largest_contributions=head(ct,10L))
  }
  known <- data[[1]]
  candidate <- Filter(function(r)r$cell==cell$cell&&r$engine=="run-shrink-075-kr",jvm)
  candidate <- candidate[order(vapply(candidate,function(r)r$replicate,0))]
  stopifnot(length(candidate)==N)
  kc <- vapply(known,function(r)r$covered,FALSE); cc <- vapply(candidate,function(r)r$covered,FALSE)
  paired <- list(known_covered=sum(kc),candidate_covered=sum(cc),
    both_covered=sum(kc&cc),known_only=sum(kc&!cc),candidate_only=sum(!kc&cc),neither=sum(!kc&!cc),
    signal_mean_variance_ratio=mean(fields(candidate,"signalVariance"))/mean(fields(known,"signalVariance")),
    signal_empirical_variance_ratio=var(fields(candidate,"signalEstimate"))/var(fields(known,"signalEstimate")))
  results[[length(results)+1L]] <- list(cell=cell$cell,retained_noise_gap=noise_gap,
    voxels=voxel_results,paired_coverage=paired)
  oracle_fixtures[[length(oracle_fixtures)+1L]] <- list(cell=cell$cell,voxels=retained_fit)
  cat(cell$cell,": exact retained reconstruction; standardized null mean",voxel_results[[1]]$standardized_bias,"\n")
}
writeLines(toJSON(list(role="exploratory retained-input diagnosis; no admission",method="base R dense retained-row stationary-covariance GLS",results=results),
  auto_unbox=TRUE,pretty=TRUE,digits=NA),file.path(out,"diagnosis.json"))
writeLines(toJSON(oracle_fixtures,auto_unbox=TRUE,pretty=TRUE,digits=NA),file.path(out,"oracle-fixtures.json"))
capture.output(sessionInfo(),file=file.path(out,"R-session.txt"))
