package scalafim.fmri.mvpa

final case class FeatureSet private (
    id: RoiId,
    featureIndices: Vector[FeatureIndex],
    center: Option[FeatureIndex],
    label: Option[String]
):
  require(featureIndices.nonEmpty, "feature set must be non-empty")
  require(label.forall(_.nonEmpty), "feature set label must be non-empty")

  def size: Int = featureIndices.length

object FeatureSet:
  def apply(
      id: RoiId,
      indices: Seq[Int],
      center: Option[Int] = None,
      label: Option[String] = None
  ): Either[MvpaError, FeatureSet] =
    if indices.isEmpty then Left(MvpaError.EmptyFeatureSet(id))
    else if indices.distinct.length != indices.length then Left(MvpaError.DuplicateFeatureIndices(id))
    else if indices.exists(_ < 0) then
      val bad = indices.find(_ < 0).get
      Left(MvpaError.FeatureIndexOutOfBounds(id, FeatureIndex.unsafe(bad)))
    else
      val parsed = indices.map(FeatureIndex.apply).toVector
      val parsedCenter = center.map(FeatureIndex.apply)
      val parsedLabel = label.map(_.trim).filter(_.nonEmpty)
      Right(new FeatureSet(id, parsed, parsedCenter, parsedLabel))

  def unsafe(id: RoiId, indices: Seq[Int], center: Option[Int] = None, label: Option[String] = None): FeatureSet =
    apply(id, indices, center, label).fold(error => throw new IllegalArgumentException(error.message), identity)
