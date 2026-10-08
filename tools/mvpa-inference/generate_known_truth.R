#!/usr/bin/env Rscript
# Independent base-R seed, moment and small numerical oracle path.
# No ScalaFIM/Gale/Multivar fit, CCA, group or threshold helper is called.
args <- commandArgs(trailingOnly = TRUE)
option <- function(name, default = NULL) {
  at <- match(name, args)
  if (is.na(at)) default else {
    if (at == length(args)) stop(paste("missing value", name))
    args[[at + 1L]]
  }
}
script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script <- normalizePath(sub("^--file=", "", script_arg[[1L]]))
repo <- dirname(dirname(dirname(script)))
source(file.path(repo, "tools/mvpa-inference/rank_population.R"))
seed_fixture <- option("--seed-fixture", file.path(repo, "docs/scenarios/fixtures/mvpa.inference-known-truth.v1.r.json"))
output <- option("--out-dir", file.path(repo, "docs/verification/umvpa-inference-calibration-20261007/fixture"))
namespace <- "scalafim/umvpa/inference-calibration/v1"

sha_raw <- function(bytes) {
  temporary <- tempfile("umvpa-sha-")
  on.exit(unlink(temporary), add = TRUE)
  writeBin(bytes, temporary)
  answer <- system2("openssl", c("dgst", "-sha256", shQuote(temporary)), stdout = TRUE)
  if (!is.null(attr(answer, "status"))) stop("SHA256 provider failed")
  hash <- sub("^.*= ", "", answer[[1L]])
  if (!grepl("^[0-9a-f]{64}$", hash)) stop("invalid SHA256 provider output")
  hash
}

hex_bytes <- function(text) {
  starts <- seq.int(1L, nchar(text), 2L)
  as.integer(vapply(starts, function(i) strtoi(substr(text, i, i + 1L), 16L), integer(1)))
}

seed_decimal <- function(hash) {
  # Independent base-10 expansion; no imprecise R double representation of u64.
  bytes <- hex_bytes(substr(hash, 1L, 16L))
  bytes[[1L]] <- bytes[[1L]] %% 128L
  digits <- 0L
  for (byte in bytes) {
    carry <- byte
    for (i in seq_along(digits)) {
      value <- digits[[i]] * 256L + carry
      digits[[i]] <- value %% 10L
      carry <- value %/% 10L
    }
    while (carry > 0L) {
      digits <- c(digits, carry %% 10L)
      carry <- carry %/% 10L
    }
  }
  answer <- paste(rev(digits), collapse = "")
  if (answer == "0") "1" else answer
}

seed_utf8 <- function(phase, scenario_utf8_bytes, ordinal) {
  bytes <- c(charToRaw(namespace), as.raw(0), charToRaw(phase), as.raw(0),
             scenario_utf8_bytes, as.raw(0), charToRaw(as.character(ordinal)))
  hash <- sha_raw(bytes)
  list(hash = hash, seed = seed_decimal(hash))
}

python_json_table <- function(path) {
  # Python is used only as a JSON decoder; SHA/seed arithmetic above is independent.
  code <- paste0(
    "import json,sys; d=json.load(open(sys.argv[1])); ",
    "[print('\\t'.join([s['phase'],s['scenario_id'].encode('utf-8').hex(),str(s['dataset_index']),",
    "s['sha256_utf8'],s['root_seed64']])) for s in d['seeds']]")
  lines <- system2("python3", c("-c", shQuote(code), shQuote(path)), stdout = TRUE)
  if (!is.null(attr(lines, "status"))) stop("fixture JSON decode failed")
  read.delim(text = lines, header = FALSE, colClasses = "character", check.names = FALSE)
}

check_fixture <- function() {
  table <- python_json_table(seed_fixture)
  for (i in seq_len(nrow(table))) {
    # ASCII hex transports exact UTF-8 bytes even under LC_CTYPE=C.
    actual <- seed_utf8(table[i, 1L], as.raw(hex_bytes(table[i, 2L])), table[i, 3L])
    if (!identical(actual$hash, table[i, 4L]) || !identical(actual$seed, table[i, 5L]))
      stop(paste("seed fixture mismatch", i, table[i,1L], table[i,2L],
                 "hash", actual$hash, table[i,4L], "seed", actual$seed, table[i,5L]))
  }
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  write.table(table, file.path(output, "seed-vectors.tsv"), sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)

  # Exact known-covariance contrast with unequal independent-unit sizes.
  metric <- diag(c(1, 2))
  full <- rbind(c(1, 2), c(-1, 1), c(2, -1))
  reduced <- rbind(c(0, 1), c(1, 0), c(-1, 2))
  response <- rbind(c(2, 1), c(0, -1), c(3, 0))
  weights <- c(1/4, 1/4, 1/2)
  a <- as.vector(t(2 * (full - reduced) %*% metric * weights))
  constant <- sum(weights * rowSums((reduced %*% metric) * reduced - (full %*% metric) * full))
  gamma <- matrix(0, 6, 6)
  gamma[1:4, 1:4] <- matrix(c(8,2,4,1, 2,4,1,2, 4,1,12,3, 1,2,3,6), 4, 4, byrow = TRUE)
  gamma[5:6, 5:6] <- matrix(c(5,2,2,7), 2, 2)
  estimate <- constant + sum(a * as.vector(t(response)))
  variance <- drop(crossprod(a, gamma %*% a))
  stopifnot(abs(estimate - 9.25) < 1e-12, abs(variance - 244) < 1e-12)
  oracle <- data.frame(
    key = c("mean", "variance", "one_sided_05_critical", "two_sided_95_critical", "lower95", "upper95"),
    value = c(estimate, variance, qnorm(.95), qnorm(.975),
              estimate - qnorm(.975) * sqrt(variance), estimate + qnorm(.975) * sqrt(variance)))
  write.table(oracle, file.path(output, "analytic-oracle.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  writeLines(c("fixture phase only; no calibration datasets or study confirmation streams",
               "R SHA256 uses unframed UTF-8 with explicit NUL separators",
               paste("seed vectors checked:", nrow(table))), file.path(output, "fixture-scope.txt"))
  capture.output(sessionInfo(), file = file.path(output, "R-session.txt"))
  cat("PASS: independent R raw-SHA/decimal seed vectors and analytic contrast oracle\n")
}

read_cell <- function(manifest, cell, phase) {
  code <- paste0(
    "import sys,json; sys.path.insert(0,sys.argv[4]+'/tools/mvpa-inference'); ",
    "from calibration_protocol import validate_manifest; from pathlib import Path; ",
    "d=json.load(open(sys.argv[1])); validate_manifest(d,Path(sys.argv[4]),sys.argv[3]); ",
    "c=next(x for x in d['cells'] if x['id']==sys.argv[2]); p=c['parameters']; ",
    "print('\\t'.join([c['procedure'],c['definition_status'],str(p.get('n','')),",
    "str(p.get('p','')),str(p.get('q','')),','.join(map(str,p.get('correlations',[]))),",
    "p.get('nuisance','')]))")
  result <- system2("python3", c("-c", shQuote(code), shQuote(manifest), shQuote(cell),
                               shQuote(phase), shQuote(repo)), stdout = TRUE)
  if (!is.null(attr(result, "status"))) stop("manifest/cell refused")
  strsplit(result[[1L]], "\t", fixed = TRUE)[[1L]]
}

init_noise <- function(state) {
  RNGkind("L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")
  assign(".Random.seed", c(10407L, as.integer(state)), envir = .GlobalEnv)
}

simulator_qa <- function() {
  manifest <- option("--manifest"); cell <- option("--cell"); seeds <- option("--seed-records")
  if (is.null(manifest) || is.null(cell) || is.null(seeds)) stop("QA needs manifest, cell and fixed seed-records TSV")
  fields <- read_cell(manifest, cell, "simulator")
  if (fields[[1L]] != "rank" || fields[[2L]] != "frozen" || !fields[[7L]] %in% c("intercept", "three-column"))
    stop("this generator implements only the declared independent-row Gaussian rank populations")
  records <- read.delim(seeds, colClasses = "character", check.names = FALSE)
  stopifnot(nrow(records) == 10000L, identical(as.integer(records$dataset_index), 0:9999))
  stopifnot(all(records$phase == "simulator"), all(records$scenario_id == cell))
  stopifnot(!anyDuplicated(records$root_seed64))
  n <- as.integer(fields[[3L]]); p <- as.integer(fields[[4L]]); q <- as.integer(fields[[5L]])
  rho <- as.numeric(strsplit(fields[[6L]], ",", fixed = TRUE)[[1L]])
  width <- p + q + if (fields[[7L]] == "three-column") 2L else 0L
  total <- numeric(width); squares <- matrix(0, width, width)
  started <- proc.time()
  for (i in seq_len(10000L)) {
    state <- as.integer(records[i, paste0("r_state", 1:6)])
    init_noise(state)
    generated <- rank_population(n, p, q, rho, fields[[7L]])
    total <- total + colSums(generated$joint)
    squares <- squares + crossprod(generated$joint)
  }
  count <- 10000 * n
  means <- total/count
  covariance <- squares/count - tcrossprod(means)
  mean_error <- max(abs(means))
  covariance_error <- max(abs(covariance - generated$covariance))
  # A separate noiseless linear construction validates the declared coefficients.
  truth <- matrix(0,p,q)
  for (j in seq_along(rho)) truth[j,j] <- rho[[j]]
  signal <- generated$residual_x %*% truth
  recovered <- qr.coef(qr(generated$residual_x), signal)
  noiseless_error <- max(abs(recovered-truth))
  qa <- data.frame(
    key = c("independent_datasets", "rows_per_dataset", "mean_absolute_error", "covariance_absolute_error",
            "noiseless_coefficient_error", "elapsed_seconds", "cpu_seconds"),
    value = c(10000, n, mean_error, covariance_error, noiseless_error,
              unname((proc.time()-started)[["elapsed"]]), sum((proc.time()-started)[1:2])))
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  write.table(qa, file.path(output, "simulator-qa.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  capture.output(sessionInfo(), file = file.path(output, "R-session.txt"))
  writeLines(c("SIMULATOR QA ONLY: no permutation/p-value/coverage/power procedure was run",
               paste("cell:", cell)), file.path(output, "qa-scope.txt"))
  if (mean_error > .02 || covariance_error > .03 || noiseless_error > 1e-12)
    stop("simulator QA failed frozen moment/noiseless gates; retain the written receipt")
  cat("PASS: 10000-dataset simulator moment/noiseless QA; no inferential admission\n")
}

emit_case_batch <- function() {
  manifest <- option("--manifest"); cell <- option("--cell"); seeds <- option("--seed-records")
  phase <- option("--phase"); draws <- as.integer(option("--draws"))
  if (is.null(manifest) || is.null(cell) || is.null(seeds) || !phase %in% c("fixture","pilot"))
    stop("case emission permits explicitly locked fixture/pilot inputs only; never confirmation")
  if (!is.finite(draws) || draws <= 0 || (phase == "pilot" && draws != 199L))
    stop("pilot B is exactly199; fixture cost counts must be explicitly supplied")
  fields <- read_cell(manifest,cell,phase)
  if (fields[[1L]] != "rank" || fields[[2L]] != "frozen" || !fields[[7L]] %in% c("intercept", "three-column"))
    stop("only the declared independent-row Gaussian rank cells are implemented")
  records <- read.delim(seeds,colClasses="character",check.names=FALSE)
  expected <- if (phase == "pilot") 200L else 1L
  stopifnot(nrow(records)==expected,identical(as.integer(records$dataset_index),0:(expected-1L)))
  stopifnot(all(records$phase==phase),all(records$scenario_id==cell),!anyDuplicated(records$root_seed64))
  n <- as.integer(fields[[3L]]);p <- as.integer(fields[[4L]]);q <- as.integer(fields[[5L]])
  rho <- as.numeric(strsplit(fields[[6L]],",",fixed=TRUE)[[1L]])
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  number_text <- function(values) paste(format(values,digits=17,scientific=TRUE,trim=TRUE),collapse=",")
  matrix_line <- function(name,matrix) paste(name,nrow(matrix),ncol(matrix),number_text(as.vector(t(matrix))),sep="\t")
  files <- character(expected)
  for (i in seq_len(expected)) {
    init_noise(as.integer(records[i,paste0("r_state",1:6)]))
    generated <- rank_population(n,p,q,rho,fields[[7L]])
    path <- file.path(output,sprintf("dataset-%05d.tsv",i-1L))
    if (file.exists(path)) stop("case output already exists; retain prior emission")
    header <- paste("case",phase,cell,i-1L,records$root_seed64[[i]],draws,n,sep="\t")
    writeLines(c(header,matrix_line("X",generated$x),matrix_line("Y",generated$y),
                 matrix_line("Z",generated$nuisance),paste("rho",number_text(rho),sep="\t")),path,useBytes=TRUE)
    files[[i]] <- normalizePath(path)
  }
  writeLines(files,file.path(output,"case-files.txt"))
  scope <- c(paste("phase:",phase),paste("datasets:",expected),paste("actual requested per-dataset B:",draws),
             "Generated source inputs only; no statistic, rate, p-value, pilot result or scientific release.",
             "Root seeds are protocol data seeds; production rank uses named resampling child domain103/ordinal0.",
             "Per-rank null truth is all remaining population correlations zero, not all members null.")
  writeLines(scope,file.path(output,"emission-scope.txt"))
  capture.output(sessionInfo(),file=file.path(output,"R-session.txt"))
  cat("PASS: emitted",expected,"independent",phase,"inputs; no statistic procedure run\n")
}

if ("--check" %in% args) {
  check_fixture()
} else if ("--simulator-qa" %in% args) {
  simulator_qa()
} else if ("--emit-case-batch" %in% args) {
  emit_case_batch()
} else {
  stop("choose --check or explicitly budgeted --simulator-qa; confirmation is not implemented by this QA generator")
}
