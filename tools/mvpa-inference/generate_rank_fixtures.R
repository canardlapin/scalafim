#!/usr/bin/env Rscript
# Independent arithmetic fixtures, not a calibration simulator or seeded pilot.
# Base R QR/SVD/triangular solves retain the full canonical coefficient complement.
n <- 16L
i <- 0:(n - 1L)
w <- function(mask) vapply(i, function(j) {
  if (sum(as.integer(intToBits(bitwAnd(j, mask)))) %% 2L == 0L) 1 else -1
}, numeric(1))
x <- cbind(w(1L), w(2L), w(4L))
y <- cbind(.8 * w(1L) + .6 * w(8L), .3 * w(2L) + sqrt(.91) * w(3L))
cca <- function(x, y) {
  qx <- qr(x); qy <- qr(y)
  stopifnot(qx$rank == ncol(x), qy$rank == ncol(y))
  s <- svd(crossprod(qr.Q(qx), qr.Q(qy)), nu = ncol(x), nv = ncol(y))
  k <- min(ncol(x), ncol(y))
  a <- backsolve(qr.R(qx), s$u[, seq_len(k), drop = FALSE])
  b <- backsolve(qr.R(qy), s$v[, seq_len(k), drop = FALSE])
  list(rho = s$d, a = a[order(qx$pivot), , drop = FALSE],
       b = b[order(qy$pivot), , drop = FALSE])
}
complete <- function(coef) {
  if (nrow(coef) == ncol(coef)) coef else {
    cbind(coef, qr.Q(qr(coef), complete = TRUE)[,
      (ncol(coef) + 1L):nrow(coef), drop = FALSE])
  }
}
wilks <- function(rho) -sum(log1p(-rho^2))
full <- cca(x, y)
u <- x %*% complete(full$a)
v <- y %*% complete(full$b)
k <- length(full$rho)
stopifnot(max(abs(full$rho - c(.8, .3))) < 1e-12)
cat("independent base-R QR/cross-basis SVD and triangular solves\n")
cat("observed roots", format(full$rho, digits = 17), "\n")
for (step in seq_len(k)) {
  cat("observed step", step, format(wilks(full$rho[step:k]), digits = 17), "\n")
}
# Explicit fixture transformations, independent of any mutable RNG state.
permutations <- list(
  affine = (i * 5L + 3L) %% n + 1L,
  modular_order = order((i * 7L) %% 19L),
  complement_counterexample = c(1L, 5L, 10L, 13L, 9L, 6L, 7L, 4L,
                                11L, 14L, 2L, 3L, 16L, 15L, 12L, 8L)
)
for (name in names(permutations)) {
  perm <- permutations[[name]]
  stopifnot(identical(sort(perm), seq_len(n)))
  cat(name, "one-based permutation", perm, "\n")
  values <- vapply(seq_len(k), function(step) {
    left <- u[, step:ncol(u), drop = FALSE]
    right <- v[, step:ncol(v), drop = FALSE]
    wilks(cca(left[perm, , drop = FALSE], right)$rho)
  }, numeric(1))
  cat(name, "null steps", format(values, digits = 17), "\n")
  dropped <- wilks(cca(u[perm, 2L, drop = FALSE], v[, 2L, drop = FALSE])$rho)
  cat(name, "invalid dropped-complement H2", format(dropped, digits = 17), "\n")
  if (name == "affine") {
    stopifnot(max(abs(values - c(3.4295968561838563, 2.4079456086518722))) < 1e-11)
  }
  if (name == "complement_counterexample") {
    stopifnot(max(abs(values - c(.4892231550030629, .054936300920663678))) < 1e-11,
              abs(values[2L] - dropped) > .02)
    reverse <- vapply(seq_len(k), function(step) {
      wilks(cca(v[order(perm), step:ncol(v), drop = FALSE],
               u[, step:ncol(u), drop = FALSE])$rho)
    }, numeric(1))
    stopifnot(max(abs(reverse - values)) < 1e-11)
    cat("reverse imbalance inverse-permutation null steps",
        format(reverse, digits = 17), "\n")
  }
}
z <- cbind(1, w(12L))
basis <- qr.Q(qr(z), complete = TRUE)[, 3:n, drop = FALSE]
residual_roots <- cca(crossprod(basis, x), crossprod(basis, y))$rho
stopifnot(max(abs(residual_roots - c(.8, .3))) < 1e-12)
cat("nuisance basis observed roots", format(residual_roots, digits = 17), "\n")
# Independent scalar cofactor oracle for all six unrestricted transformations.
# Norms are invariant: four squared cross-products meet the observed 13^2.
tiny_perms <- rbind(c(1, 2, 3), c(1, 3, 2), c(2, 1, 3),
                   c(2, 3, 1), c(3, 1, 2), c(3, 2, 1))
tiny_x <- c(1, 2, 4); tiny_y <- c(3, 1, 2)
cross_products <- apply(tiny_perms, 1L, function(p) sum(tiny_x[p] * tiny_y))
stopifnot(identical(as.numeric(cross_products), c(13, 11, 15, 12, 17, 16)))
exact_p <- mean(cross_products^2 >= cross_products[1L]^2)
plus_one_p <- (1L + sum(cross_products[-1L]^2 >= cross_products[1L]^2)) / 6
stopifnot(exact_p == 2 / 3, plus_one_p == exact_p)
cat("tiny group cross-products", cross_products, "exact inclusive p", exact_p, "\n")
