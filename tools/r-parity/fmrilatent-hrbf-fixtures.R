#!/usr/bin/env Rscript

# Regenerates the RadialBasisSuite HRBF fixture values from fmrilatent's
# direct atom formulas in R/hrbf.R and src/hrbf_atoms_rcpp.cpp.
#
# The Scala tests use these constants without requiring R at sbt runtime.

gaussian <- function(distance, sigma) {
  exp(-(distance^2) / (2 * sigma^2))
}

wendland_c4 <- function(distance, sigma) {
  r <- distance / sigma
  ifelse(r >= 1, 0, (1 - r)^6 * (35 * r^2 + 18 * r + 3) / 3)
}

wendland_c6 <- function(distance, sigma) {
  r <- distance / sigma
  ifelse(r >= 1, 0, (1 - r)^8 * (32 * r^3 + 25 * r^2 + 8 * r + 1))
}

scala_matrix <- function(mat) {
  rows <- apply(
    mat,
    1L,
    function(row) paste0("Vector(", paste(format(row, digits = 16), collapse = ", "), ")")
  )
  paste0("Vector(\n  ", paste(rows, collapse = ",\n  "), "\n)")
}

points <- matrix(c(
  0, 0, 0,
  1, 0, 0,
  2, 0, 0
), ncol = 3, byrow = TRUE)

centres <- matrix(c(
  0, 0, 0,
  2, 0, 0
), ncol = 3, byrow = TRUE)
sigmas <- c(1, 2)

gaussian_loadings <- matrix(0, nrow = nrow(points), ncol = nrow(centres))
for (v in seq_len(nrow(points))) {
  for (a in seq_len(nrow(centres))) {
    d <- sqrt(sum((points[v, ] - centres[a, ])^2))
    gaussian_loadings[v, a] <- gaussian(d, sigmas[a])
  }
}

distances <- c(0, 0.5, 1, 1.5)
wendland_loadings <- cbind(
  wendland_c4(distances, 1),
  wendland_c6(distances, 1)
)

cat("gaussian_loadings <-\n")
cat(scala_matrix(gaussian_loadings))
cat("\n\nwendland_loadings <-\n")
cat(scala_matrix(wendland_loadings))
cat("\n")
