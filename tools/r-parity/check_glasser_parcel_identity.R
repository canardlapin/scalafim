#!/usr/bin/env Rscript

# Check the independent hemisphere edge fixture used in ParcelIdentitySuite.
# This checks key alignment only; it does not acquire atlas assets or run IO.
# Reference: neuroatlas tests/testthat/test-glasser-parcel-identity.R at d65ff97.
r_pkg <- Sys.getenv('NEUROATLAS_R', file.path(path.expand('~'), 'code', 'neuroatlas'))
pkgload::load_all(r_pkg, quiet = TRUE)
ids <- c(1L, 180L, 181L, 360L)
volume_keys <- c('R_V1_ROI', 'R_p24_ROI', 'L_V1_ROI', 'L_p24_ROI')
surface_keys <- c('L_V1_ROI', 'L_p24_ROI', 'R_V1_ROI', 'R_p24_ROI')
make_atlas <- function(keys, representation, convention) {
  x <- list(
    name = 'Glasser identity edge fixture', ids = ids,
    roi_metadata = data.frame(id = ids, label_full = keys),
    atlas_ref = new_atlas_ref('glasser', 'HCP-MMP1.0',
                             representation = representation,
                             template_space = if (representation == 'volume') 'MNI152' else 'fsaverage',
                             coord_space = if (representation == 'volume') 'MNI152' else 'fsaverage',
                             id_convention = convention)
  )
  class(x) <- 'atlas'
  x
}
volume <- make_atlas(volume_keys, 'volume', 'hcp_R_first')
surface <- make_atlas(surface_keys, 'surface', 'surfatlas_L_first')
source <- data.frame(label_full = rev(volume_keys), value = rev(ids))
stopifnot(identical(as.integer(align_parcel_values(surface, source, value, by = 'label_full')),
                    c(181L, 360L, 1L, 180L)))
reverse <- data.frame(label_full = rev(surface_keys), value = rev(ids))
stopifnot(identical(as.integer(align_parcel_values(volume, reverse, value, by = 'label_full')),
                    c(181L, 360L, 1L, 180L)))
wrong_ids <- data.frame(id = ids, value = ids, id_convention = 'hcp_R_first')
error <- tryCatch(align_parcel_values(surface, wrong_ids, value, by = 'id'), error = identity)
stopifnot(inherits(error, 'neuroatlas_error_id_convention'))
message('Glasser canonical-key alignment fixture passed; cross-convention ID alignment rejected')
