#!/usr/bin/env Rscript

# Check the Scala atlas fixture corpus against the local neuroatlas semantics.
#
# From the scalafim repository root:
#   Rscript tools/r-parity/check_neuroatlas_atlas_parity.R
#
# Override the local neuroatlas checkout with:
#   NEUROATLAS_R=/path/to/neuroatlas Rscript tools/r-parity/check_neuroatlas_atlas_parity.R

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("pkgload is required to load the local neuroatlas checkout", call. = FALSE)
}
if (!requireNamespace("neuroim2", quietly = TRUE)) {
  stop("neuroim2 is required to build atlas parity fixtures", call. = FALSE)
}

r_pkg <- Sys.getenv("NEUROATLAS_R", file.path(path.expand("~"), "code", "neuroatlas"))
pkgload::load_all(r_pkg, quiet = TRUE)

dims <- c(5L, 5L, 5L)
sp3 <- neuroim2::NeuroSpace(dim = dims, spacing = c(1, 1, 1), origin = c(0, 0, 0))

atlas_arr <- array(0L, dim = dims)
atlas_arr[1:2, 1:2, 1:2] <- 10L
atlas_arr[4:5, 4:5, 4:5] <- 50L
atlas_arr[3, 3, 1:2] <- 90L

atlas_obj <- list(
  name = "scala_noncontig_fixture",
  atlas = neuroim2::NeuroVol(atlas_arr, space = sp3),
  ids = c(10L, 50L, 90L),
  labels = c("RegionA", "RegionB", "RegionC"),
  orig_labels = c("left_RegionA", "right_RegionB", "midline_RegionC"),
  hemi = c("left", "right", "midline"),
  network = c("NetA", "NetA", "NetB")
)
class(atlas_obj) <- c("noncontig", "atlas")

mask <- neuroim2::LogicalNeuroVol(array(TRUE, dim = dims), space = sp3)
sp4 <- neuroim2::NeuroSpace(dim = c(dims, 3L), spacing = c(1, 1, 1), origin = c(0, 0, 0))
data_arr <- array(0, dim = c(dims, 3L))
base_values <- c("10" = 5, "50" = 9, "90" = 13)
for (tt in seq_len(3L)) {
  slice <- data_arr[, , , tt]
  for (id in names(base_values)) {
    slice[atlas_arr == as.integer(id)] <- base_values[[id]] * tt
  }
  data_arr[, , , tt] <- slice
}
data_vol <- neuroim2::NeuroVec(data_arr, sp4)

cvec <- reduce_atlas_vec(atlas_obj, data_vol, mask)
actual <- as.matrix(neuroim2::values(cvec))
expected <- matrix(
  c(5, 10, 15, 9, 18, 27, 13, 26, 39),
  nrow = 3L,
  ncol = 3L
)
stopifnot(isTRUE(all.equal(actual, expected, tolerance = 1e-12, check.attributes = FALSE)))

comparison_arr <- array(0L, dim = dims)
comparison_arr[1, 1:2, 1:2] <- 101L
comparison_arr[4:5, 4:5, 4:5] <- 202L
comparison_arr[3, 3, 1:2] <- 303L
comparison_obj <- list(
  name = "scala_noncontig_comparison",
  atlas = neuroim2::NeuroVol(comparison_arr, space = sp3),
  ids = c(101L, 202L, 303L),
  labels = c("RegionA-left-half", "RegionB-copy", "RegionC-copy"),
  orig_labels = c("RegionA-left-half", "RegionB-copy", "RegionC-copy"),
  hemi = c("left", "right", "midline")
)
class(comparison_obj) <- c("comparison", "atlas")

ov <- atlas_overlap(atlas_obj, comparison_obj)
expected_pairs <- data.frame(
  atlas1_id = c(50L, 90L, 10L),
  atlas2_id = c(202L, 303L, 101L),
  dice = c(1, 1, 2 / 3),
  jaccard = c(1, 1, 0.5),
  n_overlap = c(8L, 2L, 4L),
  n_atlas1 = c(8L, 2L, 8L),
  n_atlas2 = c(8L, 2L, 4L)
)
stopifnot(identical(as.integer(ov$atlas1_id), expected_pairs$atlas1_id))
stopifnot(identical(as.integer(ov$atlas2_id), expected_pairs$atlas2_id))
stopifnot(isTRUE(all.equal(ov$dice, expected_pairs$dice, tolerance = 1e-12)))
stopifnot(isTRUE(all.equal(ov$jaccard, expected_pairs$jaccard, tolerance = 1e-12)))
stopifnot(identical(as.integer(ov$n_overlap), expected_pairs$n_overlap))
stopifnot(identical(as.integer(ov$n_atlas1), expected_pairs$n_atlas1))
stopifnot(identical(as.integer(ov$n_atlas2), expected_pairs$n_atlas2))

message("neuroatlas atlas parity fixture checks passed")
