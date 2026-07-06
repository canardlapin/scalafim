#!/usr/bin/env Rscript

# Regenerates the DctBasisSuite fixture values from fmrilatent's DCT-II
# convention in ~/code/fmrilatent/R/dct_basis.R.

dct_basis <- function(n_time, k, norm = c("ortho", "none")) {
  norm <- match.arg(norm)
  tt <- seq.int(0L, n_time - 1L)
  kk <- seq.int(0L, k - 1L)
  mat <- outer(
    tt, kk,
    function(t, j) cos(pi * (t + 0.5) * j / n_time)
  )

  if (identical(norm, "ortho")) {
    mat[, 1L] <- mat[, 1L] / sqrt(n_time)
    if (k > 1L) {
      mat[, 2L:k] <- mat[, 2L:k, drop = FALSE] * sqrt(2 / n_time)
    }
  }

  mat
}

scala_vector <- function(mat) {
  row_text <- apply(
    mat,
    1L,
    function(row) paste0("Vector(", paste(format(row, digits = 16), collapse = ", "), ")")
  )
  paste0("Vector(\n  ", paste(row_text, collapse = ",\n  "), "\n)")
}

n_time <- 5L
k <- 4L

cat("ortho <-\n")
cat(scala_vector(dct_basis(n_time, k, "ortho")))
cat("\n\nraw <-\n")
cat(scala_vector(dct_basis(n_time, k, "none")))
cat("\n")
