# fmrihrf 18d418f26dd4547ca21ce11d12ea9e5e91a80b87; R/hrf-functions.R blob 69a6c8476de9b87e01bca6183eae32d639935c2c
# Exact parsed function definition; generated, do not edit.
hrf_bspline <- function(t, span = 24, N = 5, degree = 3, ...) {
    degree <- as.integer(degree)
    N <- as.integer(N)
    if (N < max(1L, degree - 1L)) {
        stop(sprintf("N must be at least %d for degree = %d.", max(1L, degree - 1L), degree), call. = FALSE)
    }
    n_interior <- N + 2L - (degree + 1L)
    knots <- if (n_interior > 0L) {
        seq(0, span, length.out = n_interior + 2L)[-c(1L, n_interior + 2L)]
    }
    else {
        numeric(0)
    }
    dots <- list(...)
    dots[c("intercept", "df", "knots")] <- NULL
    t <- as.numeric(t)
    in_support <- !is.na(t) & t >= 0 & t <= span
    t_eval <- t
    t_eval[!in_support] <- 0
    full <- do.call(splines::bs, c(list(x = t_eval, knots = knots, degree = degree, intercept = TRUE, Boundary.knots = c(0,
        span)), dots))
    basis <- full[, -c(1L, ncol(full)), drop = FALSE]
    if (any(!in_support)) {
        basis[!in_support, ] <- 0
    }
    basis
}
