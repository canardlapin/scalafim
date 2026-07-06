package scalafim.fmri.mvpa

opaque type ClassLabel = String
object ClassLabel:
  def apply(value: String): ClassLabel =
    val trimmed = value.trim
    require(trimmed.nonEmpty, "class label must be non-empty")
    trimmed

  def unsafe(value: String): ClassLabel =
    value

  extension (label: ClassLabel)
    inline def value: String = label

opaque type RoiId = Int
object RoiId:
  def apply(value: Int): RoiId =
    require(value >= 0, "ROI id must be non-negative")
    value

  def unsafe(value: Int): RoiId =
    value

  extension (id: RoiId)
    inline def value: Int = id

opaque type FeatureIndex = Int
object FeatureIndex:
  def apply(value: Int): FeatureIndex =
    require(value >= 0, "feature index must be non-negative")
    value

  def unsafe(value: Int): FeatureIndex =
    value

  extension (index: FeatureIndex)
    inline def value: Int = index

opaque type SampleIndex = Int
object SampleIndex:
  def apply(value: Int): SampleIndex =
    require(value >= 0, "sample index must be non-negative")
    value

  def unsafe(value: Int): SampleIndex =
    value

  extension (index: SampleIndex)
    inline def value: Int = index
