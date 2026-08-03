design <- cbind(
  intercept = 1,
  a = c(-1, 0, 1, 0, -1, 0, 1, 0, -1, 1),
  b = c(0, -1, 0, 1, 0, -1, 0, 1, 0, 0),
  drift = seq(-1, 1, length.out = 10)
)

coefficients <- list(
  matrix(c(
    0.2,  0.1, -0.1,  0.0,  0.3,
    1.2, -0.4,  0.7,  0.2, -0.3,
   -0.2,  1.0,  0.5, -0.6,  0.4,
    0.1, -0.1,  0.2,  0.0,  0.1
  ), nrow = 4, byrow = TRUE),
  matrix(c(
    0.1,  0.2,  0.0, -0.1,  0.2,
    1.0, -0.2,  0.8,  0.3, -0.4,
   -0.1,  1.1,  0.4, -0.5,  0.5,
    0.0, -0.2,  0.1,  0.1,  0.0
  ), nrow = 4, byrow = TRUE),
  matrix(c(
    0.3,  0.0, -0.2,  0.1,  0.4,
    1.1, -0.3,  0.6,  0.4, -0.2,
   -0.3,  0.9,  0.6, -0.4,  0.3,
    0.2,  0.0,  0.3, -0.1,  0.2
  ), nrow = 4, byrow = TRUE)
)

noise <- list(
  matrix(c(
     0.10, -0.05,  0.02,  0.04, -0.03,
    -0.08,  0.03, -0.01,  0.02,  0.05,
     0.04,  0.06, -0.03, -0.02,  0.01,
    -0.02, -0.04,  0.05,  0.03, -0.06,
     0.06,  0.01,  0.04, -0.05,  0.02,
    -0.05,  0.02,  0.01,  0.06, -0.04,
     0.03, -0.06,  0.02, -0.01,  0.04,
    -0.01,  0.05, -0.04,  0.02,  0.03,
     0.02, -0.02,  0.03, -0.04,  0.01,
    -0.04,  0.00, -0.02,  0.05, -0.03
  ), nrow = 10, byrow = TRUE),
  matrix(c(
    -0.03,  0.04,  0.01, -0.02,  0.05,
     0.05, -0.02,  0.03,  0.01, -0.04,
    -0.01,  0.06, -0.04,  0.03,  0.02,
     0.04, -0.05,  0.02, -0.01,  0.03,
    -0.02,  0.01,  0.05, -0.04,  0.00,
     0.03, -0.04, -0.01,  0.05,  0.02,
    -0.05,  0.02,  0.04,  0.00, -0.03,
     0.01,  0.03, -0.02,  0.04, -0.05,
     0.02, -0.01,  0.00, -0.03,  0.04,
    -0.04,  0.05, -0.03,  0.02,  0.01
  ), nrow = 10, byrow = TRUE),
  matrix(c(
     0.02,  0.01, -0.04,  0.05, -0.02,
    -0.04,  0.05,  0.02, -0.03,  0.01,
     0.06, -0.03,  0.01,  0.02, -0.05,
    -0.01, -0.02,  0.05, -0.04,  0.03,
     0.03,  0.04, -0.02,  0.01, -0.06,
    -0.05,  0.00,  0.03,  0.04, -0.01,
     0.01, -0.05,  0.04, -0.02,  0.02,
     0.04,  0.02, -0.03,  0.00, -0.04,
    -0.02,  0.03,  0.00, -0.05,  0.05,
     0.00, -0.04, -0.01,  0.03,  0.02
  ), nrow = 10, byrow = TRUE)
)
noise <- lapply(noise, function(values) 10 * values)

responses <- Map(function(beta, eps) design %*% beta + eps, coefficients, noise)
contrast <- rbind(c(0, 1, 0, 0), c(0, 0, 1, 0))
k <- solve(crossprod(design))
v <- contrast %*% k %*% t(contrast)
q <- k %*% t(contrast) %*% solve(chol(v))

moments <- lapply(responses, function(y) {
  g <- crossprod(y, design)
  u <- g %*% q
  list(
    h = tcrossprod(u),
    e = crossprod(y) - g %*% k %*% t(g)
  )
})

generalized <- function(h, e, rank) {
  r <- chol(e)
  whitened <- solve(t(r), h) %*% solve(r)
  solved <- eigen((whitened + t(whitened)) / 2, symmetric = TRUE)
  roots <- pmax(solved$values[seq_len(rank)], 0)
  frame <- solve(r, solved$vectors[, seq_len(rank), drop = FALSE])
  list(roots = roots, frame = frame)
}

statistic <- function(roots) c(
  roy = roots[1],
  wilks = prod(1 / (1 + roots)),
  pillai = sum(roots / (1 + roots)),
  hotelling = sum(roots)
)

folds <- lapply(seq_along(moments), function(held) {
  training <- moments[-held]
  fit <- generalized(
    Reduce(`+`, lapply(training, `[[`, "h")),
    Reduce(`+`, lapply(training, `[[`, "e")),
    2
  )
  held_h <- crossprod(fit$frame, moments[[held]]$h %*% fit$frame)
  held_e <- crossprod(fit$frame, moments[[held]]$e %*% fit$frame)
  held_fit <- generalized(held_h, held_e, 2)
  c(held_fit$roots, statistic(held_fit$roots))
})

options(digits = 17)
for (i in seq_along(responses)) {
  cat(sprintf("run%d <- c(\n  %s\n)\n", i, paste(sprintf("%.17g", as.vector(t(responses[[i]]))), collapse = ", ")))
}
for (i in seq_along(folds)) {
  cat(sprintf("fold%d <- c(%s)\n", i, paste(sprintf("%.17g", folds[[i]]), collapse = ", ")))
}
cat(sprintf("mean <- c(%s)\n", paste(sprintf("%.17g", Reduce(`+`, lapply(folds, function(x) x[3:6])) / length(folds)), collapse = ", ")))
