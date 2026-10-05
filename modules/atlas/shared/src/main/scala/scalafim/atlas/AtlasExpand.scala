package scalafim.atlas

import locus4s.data.Field
import scalafim.image.{ScalarVolume, VolumeParcellationError}

/** Materialize a parcel field through the atlas's exact assignment and grid. */
object AtlasExpand:
  def volume(atlas: VolumeAtlas)(
      values: Field[atlas.realization.P, Double],
      background: Double
  ): Either[
    VolumeParcellationError,
    ScalarVolume[atlas.realization.parcellation.sampleSpace.type, Double]
  ] =
    atlas.realization.parcellation.renderContinuous(values, background)
