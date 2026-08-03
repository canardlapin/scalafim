package scalafim.fmri.mvpa

/** Feature-selecting MVPA source with its selected pattern representation in
  * the type. Sources are covariant because they only produce that
  * representation.
  */
trait PatternSource[+Patterns]:
  def samples: Int
  def featureIndices: Vector[FeatureIndex]
  def selectFeatures(featureSet: FeatureSet): Either[MvpaError, Patterns]

type DensePatternSource = PatternSource[PatternMatrix]
type OperatorPatternSource = PatternSource[PatternOperator]

final case class InMemoryPatternSource(data: PatternMatrix) extends PatternSource[PatternMatrix]:
  override def samples: Int =
    data.samples

  override def featureIndices: Vector[FeatureIndex] =
    data.featureIndices

  override def selectFeatures(featureSet: FeatureSet): Either[MvpaError, PatternMatrix] =
    data.selectFeatures(featureSet)

object PatternSource:
  def fromMatrix(data: PatternMatrix): PatternSource[PatternMatrix] =
    InMemoryPatternSource(data)

  def fromOperator(data: PatternOperator): OperatorPatternSource =
    InMemoryOperatorPatternSource(data)

private final case class InMemoryOperatorPatternSource(
    data: PatternOperator
) extends OperatorPatternSource:
  override def samples: Int =
    data.samples

  override def featureIndices: Vector[FeatureIndex] =
    data.featureIndices

  override def selectFeatures(featureSet: FeatureSet): Either[MvpaError, PatternOperator] =
    data.selectFeatures(featureSet)
