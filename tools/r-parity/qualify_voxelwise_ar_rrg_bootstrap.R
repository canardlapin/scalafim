# Independent frozen-whitening bootstrap oracle. Base R QR/solve and a scalar
# projective-circle search; no ScalaFIM or Gale numerical implementation.
# Rscript tools/r-parity/qualify_voxelwise_ar_rrg_bootstrap.R [output.json]
options(digits = 17)
t <- seq_len(12)
X <- cbind((t - 6.5) / 4, sin(t * 0.8), 1)
truth <- rbind(c(1.2, -0.7, 0.4, 1.1), c(0.3, 1.4, -1.1, 0.8), c(0.4, -0.2, 0.8, 0.1))
Y <- X %*% truth + 0.12 * outer(t, seq_len(4), function(a, b) cos(a * (0.3 + b * 0.17)))
rho <- c(0.15, 0.65, -0.3, 0.65)
segments <- list(1:6, 7:12)
Ws <- lapply(rho, function(r) {
  w <- diag(12)
  for (s in segments) {
    w[s[1], s[1]] <- sqrt(1 - r^2)
    for (i in s[-1]) w[i, i - 1] <- -r
  }
  w
})
fit <- function(y, grid_size = 128) {
  gs <- lapply(seq_len(4), function(v) {
    wx <- Ws[[v]] %*% X
    wy <- Ws[[v]] %*% y[, v]
    qz <- qr(wx[, 3, drop = FALSE])
    d <- qr.resid(qz, wx[, 1:2])
    z <- qr.resid(qz, wy)
    list(wx = wx, wy = wy, d = d, z = z, G = crossprod(d), h = crossprod(d, z))
  })
  profile <- function(angle) {
    a <- c(cos(angle), sin(angle))
    b <- sapply(gs, function(g) a * drop(crossprod(a, g$h) / crossprod(a, g$G %*% a)))
    loss <- sum(vapply(seq_along(gs), function(v) sum((gs[[v]]$z - gs[[v]]$d %*% b[, v])^2), numeric(1)))
    list(b = b, loss = loss)
  }
  f <- function(angle) profile(angle)$loss
  grid <- seq(0, pi, length.out = grid_size + 1)
  values <- vapply(grid, f, numeric(1))
  local <- which(values[2:grid_size] <= values[1:(grid_size - 1)] & values[2:grid_size] <= values[3:(grid_size + 1)]) + 1
  candidates <- lapply(c(1, local, grid_size + 1), function(i) optimize(f, grid[c(max(1, i - 1), min(grid_size + 1, i + 1))], tol = 1e-12))
  best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "objective"))]]
  result <- profile(best$minimum)
  gamma <- vapply(seq_len(4), function(v) qr.coef(qr(gs[[v]]$wx[, 3, drop = FALSE]), gs[[v]]$wy - gs[[v]]$wx[, 1:2] %*% result$b[, v])[1], numeric(1))
  result$coefficients <- rbind(result$b, gamma)
  result
}
fitted <- fit(Y)
# Unrestricted GLS residuals; HC2 and runwise centering. Both reset segments
# belong to the same run, as in a selected time axis with a gap at row six.
innovations <- sapply(seq_len(4), function(v) {
  q <- qr(Ws[[v]] %*% X)
  residual <- qr.resid(q, Ws[[v]] %*% Y[, v])
  corrected <- residual / sqrt(1 - rowSums(qr.Q(q)^2))
  corrected - mean(corrected)
})
state <- 19
next_int <- function(n) {
  state <<- (state * 48271) %% 2147483647
  as.integer((state - 1) %% n) + 1L
}
starts <- c(1:5, 7:11)
replicates <- 32L
samples <- array(0, c(2, 4, replicates))
objectives <- numeric(replicates)
donor_indices <- matrix(0L, replicates, 12)
refinement_error <- 0
for (b in seq_len(replicates)) {
  indices <- integer(12)
  for (segment in segments) for (target in seq(segment[1], tail(segment, 1), by = 2)) {
    donor <- starts[next_int(length(starts))]
    indices[target + 0:1] <- donor + 0:1
  }
  donor_indices[b, ] <- indices - 1L
  raw <- sapply(seq_len(4), function(v) solve(Ws[[v]], innovations[indices, v]))
  y <- X %*% fitted$coefficients + raw
  result <- fit(y)
  refined <- fit(y, 256)
  refinement_error <- max(refinement_error, abs(result$b - refined$b), abs(result$loss - refined$loss))
  samples[, , b] <- result$b
  objectives[b] <- result$loss
}
stopifnot(refinement_error < 1e-6)
covariances <- lapply(seq_len(4), function(v) cov(t(samples[, v, ])))
lower <- apply(samples, 1:2, quantile, probs = 0.025, type = 7)
upper <- apply(samples, 1:2, quantile, probs = 0.975, type = 7)
array_json <- function(x) paste0("[", paste(sprintf("%.17g", x), collapse = ","), "]")
matrix_json <- function(x) paste0("[", paste(apply(x, 1, array_json), collapse = ","), "]")
script <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
json <- paste0('{\n  "schema":"scalafim.voxelwise-rrg-bootstrap-oracle.v1",\n',
  '  "scope":"independent frozen-whitening implementation parity, not coverage calibration",\n',
  '  "source_md5":"', unname(tools::md5sum(script)), '",\n',
  '  "r_version":"', getRversion(), '",\n',
  '  "replicates":32,"block_size":2,"seed":19,\n',
  '  "refinement_max_error":', sprintf("%.17g", refinement_error), ',\n',
  '  "covariance_by_voxel":[', paste(lapply(covariances, matrix_json), collapse = ","), '],\n',
  '  "lower":', matrix_json(lower), ',\n  "upper":', matrix_json(upper), ',\n',
  '  "objectives":', array_json(objectives), ',\n',
  '  "donor_indices":', matrix_json(donor_indices), '\n}\n')
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) writeLines(json, args[1], useBytes = TRUE) else cat(json)
