package scalafim.atlas

final case class ParcelRecord[A](region: Region, value: A)

final case class ParcelData[A](
  atlasRef: AtlasRef,
  records: Vector[ParcelRecord[A]],
  schemaVersion: String = "1.0.0"
):
  require(schemaVersion.trim.nonEmpty, "schemaVersion must be non-empty")
  require(records.nonEmpty, "parcel data must contain at least one record")
  private val duplicateIds =
    records.groupBy(_.region.id).collect { case (id, rs) if rs.length > 1 => id }.toVector.sorted
  require(duplicateIds.isEmpty, AtlasError.DuplicateRegionIds(duplicateIds).message)

  def regionIndex: RegionIndex =
    RegionIndex(records.map(_.region))

  def values: Vector[A] =
    records.map(_.value)

object ParcelData:
  def fromValues[A](atlas: VolumeAtlas, values: Vector[A]): ParcelData[A] =
    require(values.length == atlas.regions.size, "values length must match atlas region count")
    ParcelData(atlas.ref, atlas.regions.regions.zip(values).map(ParcelRecord.apply))
