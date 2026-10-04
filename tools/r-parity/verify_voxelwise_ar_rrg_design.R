# Independent, synthetic design evidence; no ScalaFIM or fmrireg implementation.
# Run from the repository root:
# Rscript tools/r-parity/verify_voxelwise_ar_rrg_design.R [output.json]
# This profiles a two-target rank-one problem over RP(1). It is a numerical
# oracle for this fixture, not a general global-optimality certificate.
options(digits = 17)
t <- seq_len(12)
X <- cbind((t - 6.5) / 4, sin(t * 0.8))
Z <- matrix(1, 12, 1)
truth <- rbind(c(1.2, -0.7, 0.4, 1.1), c(0.3, 1.4, -1.1, 0.8))
Y <- X %*% truth + outer(rep(1, 12), c(0.4, -0.2, 0.8, 0.1)) +
  0.12 * outer(t, seq_len(4), function(a, b) cos(a * (0.3 + b * 0.17)))
rho <- c(0.15, 0.65, -0.3, 0.65)

whitener <- function(rho) {
  W <- diag(12)
  for (segment in list(1:6, 7:12)) {
    W[segment[1], segment[1]] <- sqrt(1 - rho^2)
    for (i in segment[-1]) W[i, i - 1] <- -rho
  }
  W
}
geometry <- function(W, y) {
  WX <- W %*% X
  WZ <- W %*% Z
  Qz <- qr.Q(qr(WZ))
  D <- WX - Qz %*% crossprod(Qz, WX)
  z <- W %*% y
  z <- z - Qz %*% crossprod(Qz, z)
  list(W = W, D = D, z = z, G = crossprod(D), h = crossprod(D, z))
}
g <- lapply(seq_len(4), function(v) geometry(whitener(rho[v]), Y[, v]))
loss <- function(B) sum(vapply(seq_len(4), function(v) {
  sum((g[[v]]$z - g[[v]]$D %*% B[, v])^2)
}, numeric(1)))
profile <- function(angle, geometries = g) {
  A <- matrix(c(cos(angle), sin(angle)), 2, 1)
  B <- sapply(geometries, function(a) {
    drop(A) * drop(crossprod(A, a$h) / crossprod(A, a$G %*% A))
  })
  value <- sum(vapply(seq_along(geometries), function(v) {
    sum((geometries[[v]]$z - geometries[[v]]$D %*% B[, v])^2)
  }, numeric(1)))
  list(A = A, B = B, loss = value)
}
search <- function(n, geometries = g) {
  grid <- seq(0, pi, length.out = n + 1)
  f <- function(angle) profile(angle, geometries)$loss
  values <- vapply(grid, f, numeric(1))
  local <- which(values[2:n] <= values[1:(n - 1)] &
    values[2:n] <= values[3:(n + 1)]) + 1L
  # Always inspect both periodic edges as well as every grid-local minimum.
  intervals <- c(local, 1L, n + 1L)
  candidates <- lapply(intervals, function(i) {
    optimize(f, c(grid[max(1, i - 1)], grid[min(n + 1, i + 1)]),
      tol = 1e-12)
  })
  best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "objective"))]]
  profile(best$minimum, geometries)
}
global <- search(4096)
refined <- search(8192)
stopifnot(abs(global$loss - refined$loss) < 1e-10)
stopifnot(max(abs(global$B - refined$B)) < 1e-7)

# Unconstrained GLS, with a separate direct joint-design least-squares oracle.
full <- sapply(g, function(a) solve(a$G, a$h))
direct <- sapply(seq_len(4), function(v) {
  lm.fit(g[[v]]$W %*% cbind(X, Z), g[[v]]$W %*% Y[, v])$coefficients[1:2]
})
full_error <- max(abs(full - direct))
stopifnot(full_error < 1e-12, loss(full) <= global$loss)

rrr <- function(geometries) {
  D <- geometries[[1]]$D
  response <- do.call(cbind, lapply(geometries, `[[`, "z"))
  q <- qr(D)
  Q <- qr.Q(q)
  scores <- crossprod(Q, response)
  s <- svd(scores)
  fitted <- Q %*% (s$u[, 1, drop = FALSE] * s$d[1]) %*% t(s$v[, 1, drop = FALSE])
  qr.coef(q, fitted)
}
# The common-whitening limiting case agrees with the independent angle search.
common <- lapply(seq_len(4), function(v) geometry(whitener(0.35), Y[, v]))
common_error <- max(abs(rrr(common) - search(8192, common)$B))
stopifnot(common_error < 1e-6)

s <- svd(full)
coefficient_svd <- (s$u[, 1, drop = FALSE] * s$d[1]) %*% t(s$v[, 1, drop = FALSE])
pooled <- lapply(seq_len(4), function(v) geometry(whitener(mean(rho)), Y[, v]))
pooled_B <- rrr(pooled)
grouped <- matrix(0, 2, 4)
for (group in split(seq_len(4), rho)) {
  grouped[, group] <- rrr(g[group])
}
grouped_rank <- qr(grouped)$rank
stopifnot(grouped_rank == 2L)
stopifnot(loss(coefficient_svd) > global$loss + 1e-4)
stopifnot(loss(pooled_B) > global$loss + 1e-4)

# Fixed predictor-subspace covariance: compare FWL formula with the complete
# time-to-coefficient linear operator of the constrained joint design [X A, Z].
# Assumption: W_v e_v has covariance sigma_v^2 I. A and W_v are fixed here.
A <- global$A
covariance_error <- max(vapply(g, function(a) {
  normalized <- A %*% solve(crossprod(A, a$G %*% A)) %*% t(A)
  F <- a$W %*% cbind(X %*% A, Z)
  K <- qr.coef(qr(F), diag(nrow(F)))[1, , drop = FALSE]
  propagated <- A %*% tcrossprod(K) %*% t(A)
  max(abs(normalized - propagated))
}, numeric(1)))
stopifnot(covariance_error < 1e-12)

# Mixing response columns does not commute with heterogeneous W_v.
mixed_white <- (g[[1]]$W %*% Y[, 1] + g[[2]]$W %*% Y[, 2]) / sqrt(2)
white_mixed <- g[[1]]$W %*% ((Y[, 1] + Y[, 2]) / sqrt(2))
mixing_error <- max(abs(mixed_white - white_mixed))
stopifnot(mixing_error > 1e-3)

metrics <- c(
  heterogeneous_global_rank_one_loss = global$loss,
  coefficient_svd_loss = loss(coefficient_svd),
  pooled_whitening_loss_in_original_metric = loss(pooled_B),
  grouped_rank_one_loss = loss(grouped),
  grouped_global_coefficient_rank = grouped_rank,
  full_rank_loss = loss(full),
  full_rank_joint_gls_max_error = full_error,
  shared_whitening_qr_svd_max_error = common_error,
  rank_one_search_loss_refinement_error = abs(global$loss - refined$loss),
  fixed_predictor_subspace_covariance_max_error = covariance_error,
  heterogeneous_response_mixing_max_error = mixing_error
)
entries <- sprintf('    "%s": %.17g', names(metrics), metrics)
numeric_array <- function(values) paste(sprintf("%.17g", values), collapse = ", ")
rows <- apply(global$B, 1, function(row) paste0("[", numeric_array(row), "]"))
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
source_md5 <- unname(tools::md5sum(script))
json <- paste0(
  '{\n  "schema": "scalafim.voxelwise-ar-rrg-design.v1",\n',
  '  "scope": "synthetic mathematical design evidence; no production qualification",\n',
  '  "r_version": "', getRversion(), '",\n',
  '  "oracle_source_md5": "', source_md5, '",\n',
  '  "fixture": {"rows": 12, "targets": 2, "nuisance": 1, "voxels": 4, ',
  '"segment_starts_zero_based": [0, 6], "ar1": [', numeric_array(rho), ']},\n',
  '  "metrics": {\n', paste(entries, collapse = ",\n"), '\n  },\n',
  '  "global_rank_one_coefficients": [', paste(rows, collapse = ", "), ']\n}\n'
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) writeLines(json, args[1], useBytes = TRUE) else cat(json)
