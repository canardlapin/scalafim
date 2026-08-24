#!/usr/bin/env Rscript

# Generate an independent neuroim2 oracle over the nibabel-authored 4D fixture.
# Run tools/image/generate_image_library_parity.py first.

if (!requireNamespace("neuroim2", quietly = TRUE)) {
  stop("neuroim2 is required to generate the image parity oracle", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) normalizePath(args[[1L]]) else normalizePath(".")
output <- file.path(root, "modules", "image", "jvm", "src", "test", "resources", "scalafim", "image", "io")
nifti_path <- file.path(output, "nibabel-neuroim2-basic-4d.nii")
table_path <- file.path(output, "neuroim2-basic-4d.tsv")

if (!file.exists(nifti_path)) {
  stop("missing nibabel fixture; run tools/image/generate_image_library_parity.py first", call. = FALSE)
}

series <- neuroim2::read_vec(nifti_path)
data <- as.array(series)
shape <- c(2L, 3L, 4L, 3L)
if (!identical(as.integer(dim(series)), shape)) {
  stop(sprintf("unexpected neuroim2 shape: %s", paste(dim(series), collapse = "x")), call. = FALSE)
}

nifti_ordinal <- function(x, y, z, time) {
  x + shape[[1L]] * (y + shape[[2L]] * (z + shape[[3L]] * time))
}

ravel_ordinal <- function(x, y, z, time) {
  time + shape[[4L]] * (z + shape[[3L]] * (y + shape[[2L]] * x))
}

format_values <- function(values) {
  paste(sprintf("%.17g", as.numeric(values)), collapse = "\t")
}

affine <- neuroim2::trans(neuroim2::space(series))
axis_codes <- c("A", "L", "S")
expected_affine <- matrix(
  c(
    0, -3, 0, 10,
    2, 0, 0, -20,
    0, 0, 4, 30,
    0, 0, 0, 1
  ),
  nrow = 4L,
  byrow = TRUE
)
if (!isTRUE(all.equal(affine, expected_affine, tolerance = 1e-7, check.attributes = FALSE))) {
  stop("neuroim2 affine does not match the persisted nibabel sform", call. = FALSE)
}

connection <- file(table_path, open = "wt", encoding = "UTF-8")
on.exit(close(connection), add = TRUE)
writeLines("# fixture_version\timage-library-parity-v1", connection)
writeLines(sprintf("# generator\tneuroim2-%s", as.character(utils::packageVersion("neuroim2"))), connection)
writeLines("# source\tnibabel-neuroim2-basic-4d.nii", connection)
writeLines(sprintf("# shape\t%s", paste(shape, collapse = "\t")), connection)
writeLines(sprintf("# axis_codes\t%s", paste(axis_codes, collapse = "\t")), connection)
writeLines(sprintf("# affine_row_major\t%s", format_values(t(affine))), connection)
writeLines("# temporal_spacing_seconds\t1.5", connection)
writeLines(
  paste(
    c(
      "x", "y", "z", "time", "nifti_ordinal0", "ravel_ordinal0",
      "value", "world_x", "world_y", "world_z"
    ),
    collapse = "\t"
  ),
  connection
)

for (x in 0:(shape[[1L]] - 1L)) {
  for (y in 0:(shape[[2L]] - 1L)) {
    for (z in 0:(shape[[3L]] - 1L)) {
      for (time in 0:(shape[[4L]] - 1L)) {
        value <- data[x + 1L, y + 1L, z + 1L, time + 1L]
        expected <- 0.25 + 1000 * time + 100 * x + 10 * y + z
        if (!identical(as.numeric(value), as.numeric(expected))) {
          stop("neuroim2 coordinate value disagrees with the fixture formula", call. = FALSE)
        }
        first_axis_ordinal <- nifti_ordinal(x, y, z, time)
        if (!identical(as.numeric(as.vector(data)[first_axis_ordinal + 1L]), as.numeric(value))) {
          stop("neuroim2 array does not retain NIfTI first-axis-fastest order", call. = FALSE)
        }
        world <- neuroim2::grid_to_coord(
          neuroim2::space(series),
          c(x + 1L, y + 1L, z + 1L)
        )
        record <- c(
          x, y, z, time,
          first_axis_ordinal,
          ravel_ordinal(x, y, z, time),
          value,
          as.numeric(world)
        )
        writeLines(paste(sprintf("%.17g", record), collapse = "\t"), connection)
      }
    }
  }
}

message(sprintf("wrote %s", table_path))
