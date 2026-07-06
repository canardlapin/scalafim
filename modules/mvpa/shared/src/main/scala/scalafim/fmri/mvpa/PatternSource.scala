package scalafim.fmri.mvpa

trait PatternSource:
  def samples: Int
  def featureIndices: Vector[FeatureIndex]
  def selectFeatures(featureSet: FeatureSet): Either[MvpaError, PatternMatrix]

final case class InMemoryPatternSource(data: PatternMatrix) extends PatternSource:
  override def samples: Int =
    data.samples

  override def featureIndices: Vector[FeatureIndex] =
    data.featureIndices

  override def selectFeatures(featureSet: FeatureSet): Either[MvpaError, PatternMatrix] =
    data.selectFeatures(featureSet)

object PatternSource:
  def fromMatrix(data: PatternMatrix): PatternSource =
    InMemoryPatternSource(data)
