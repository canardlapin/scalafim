package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

enum GeneralizedLowRankError:
  case InvalidDefinition(detail: String)
  case InvalidObservedValue(feature: GlrmFeatureId, row: Int, value: Double, detail: String)
  case LossDomainMismatch(feature: GlrmFeatureId, domain: FeatureDomain, loss: EntryLoss)
  case NaturalParameterMismatch(feature: GlrmFeatureId, expected: Int, actual: Int)
  case InvalidNaturalParameter(feature: GlrmFeatureId, detail: String)
  case IndexOutOfBounds(kind: String, index: Int, limit: Int)
  case UnsupportedCensoring(row: Int, feature: Int, interval: CensoringInterval)
  case NonFiniteObjective(context: String, value: Double)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case InvalidObservedValue(feature, row, value, detail) =>
        s"feature ${feature.value} row $row has invalid observed value $value: $detail"
      case LossDomainMismatch(feature, domain, loss) =>
        s"feature ${feature.value} domain $domain is incompatible with entry loss $loss"
      case NaturalParameterMismatch(feature, expected, actual) =>
        s"feature ${feature.value} requires $expected natural parameters, got $actual"
      case InvalidNaturalParameter(feature, detail) =>
        s"feature ${feature.value} has invalid natural parameters: $detail"
      case IndexOutOfBounds(kind, index, limit) => s"$kind index $index is outside 0..${limit - 1}"
      case UnsupportedCensoring(row, feature, interval) =>
        s"cell ($row,$feature) is $interval; a censoring likelihood must be declared before objective evaluation"
      case NonFiniteObjective(context, value) => s"$context produced a non-finite objective value $value"

opaque type GlrmFeatureId = String

object GlrmFeatureId:
  def apply(value: String): Either[GeneralizedLowRankError, GlrmFeatureId] =
    Identifier
      .validate("GLRM feature id", value)
      .left
      .map(error => GeneralizedLowRankError.InvalidDefinition(error.message))

  def unsafe(value: String): GlrmFeatureId =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: GlrmFeatureId)
    inline def value: String = id

opaque type OrderedLevels = Vector[String]

object OrderedLevels:
  def from(values: Vector[String]): Either[GeneralizedLowRankError, OrderedLevels] =
    glrmValidatedLevels("ordered levels", values)

  def unsafe(values: Vector[String]): OrderedLevels =
    from(values).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (levels: OrderedLevels)
    inline def values: Vector[String] = levels
    inline def size: Int = levels.length

opaque type CategoryLevels = Vector[String]

object CategoryLevels:
  def from(values: Vector[String]): Either[GeneralizedLowRankError, CategoryLevels] =
    glrmValidatedLevels("category levels", values)

  def unsafe(values: Vector[String]): CategoryLevels =
    from(values).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (levels: CategoryLevels)
    inline def values: Vector[String] = levels
    inline def size: Int = levels.length

enum FeatureDomain:
  case Real
  case Binary
  case Count
  case Ordinal(levels: OrderedLevels)
  case Categorical(levels: CategoryLevels)

  def naturalDimension: Int =
    this match
      case Real | Binary | Count => 1
      case Ordinal(levels) => levels.size - 1
      case Categorical(levels) => levels.size

  def validate(value: Double): Either[String, Unit] =
    if !value.isFinite then Left("value must be finite")
    else
      this match
        case Real => Right(())
        case Binary =>
          if value == 0.0 || value == 1.0 then Right(())
          else Left("binary values must be exactly zero or one")
        case Count =>
          if value >= 0.0 && value <= Int.MaxValue.toDouble && value == Math.rint(value) then Right(())
          else Left("count values must be non-negative integers within Int range")
        case Ordinal(levels) => glrmValidateLevelIndex(value, levels.size, "ordinal")
        case Categorical(levels) => glrmValidateLevelIndex(value, levels.size, "categorical")

enum LossConvexity:
  case StrictlyConvexInNaturalParameter
  case ConvexInNaturalParameter

enum NaturalParameterDomain:
  case Unconstrained(dimension: Int)
  case OrderedNonIncreasing(dimension: Int)

enum NaturalParameterGauge:
  case Identified
  case CommonShiftInvariant

opaque type HuberDelta = Double

object HuberDelta:
  def apply(value: Double): Either[GeneralizedLowRankError, HuberDelta] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(GeneralizedLowRankError.InvalidDefinition(s"Huber delta must be finite and positive, got $value"))

  def unsafe(value: Double): HuberDelta =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (delta: HuberDelta)
    inline def value: Double = delta

enum EntryLoss:
  case Quadratic
  case Huber(delta: HuberDelta)
  case Logistic
  case Poisson
  case OrdinalCumulativeLogit
  case CategoricalCrossEntropy

  def accepts(domain: FeatureDomain): Boolean =
    (this, domain) match
      case (Quadratic | Huber(_), FeatureDomain.Real) => true
      case (Logistic, FeatureDomain.Binary) => true
      case (Poisson, FeatureDomain.Count) => true
      case (OrdinalCumulativeLogit, FeatureDomain.Ordinal(_)) => true
      case (CategoricalCrossEntropy, FeatureDomain.Categorical(_)) => true
      case _ => false

  def convexity: LossConvexity =
    this match
      case Quadratic | Logistic | Poisson => LossConvexity.StrictlyConvexInNaturalParameter
      case Huber(_) | OrdinalCumulativeLogit | CategoricalCrossEntropy =>
        LossConvexity.ConvexInNaturalParameter

  def naturalDomain(feature: FeatureDomain): NaturalParameterDomain =
    feature match
      case FeatureDomain.Ordinal(_) => NaturalParameterDomain.OrderedNonIncreasing(feature.naturalDimension)
      case _ => NaturalParameterDomain.Unconstrained(feature.naturalDimension)

  def gauge: NaturalParameterGauge =
    this match
      case CategoricalCrossEntropy => NaturalParameterGauge.CommonShiftInvariant
      case _ => NaturalParameterGauge.Identified

  def value(
      feature: GlrmFeatureId,
      observed: FeatureEmbedding,
      natural: DVec
  ): Either[GeneralizedLowRankError, Double] =
    for
      _ <- glrmRequireLossDomain(feature, this, observed.domain)
      _ <- glrmRequireNatural(feature, observed.domain, natural)
      result <-
        this match
          case Quadratic =>
            val residual = natural(0) - observed.rawValue
            Right(0.5 * residual * residual)
          case Huber(delta) =>
            val residual = Math.abs(natural(0) - observed.rawValue)
            if residual <= delta.value then Right(0.5 * residual * residual)
            else Right(delta.value * (residual - 0.5 * delta.value))
          case Logistic => Right(glrmSoftplus(natural(0)) - observed.rawValue * natural(0))
          case Poisson =>
            val count = observed.rawValue.toInt
            Right(Math.exp(natural(0)) - observed.rawValue * natural(0) + glrmLogFactorial(count))
          case OrdinalCumulativeLogit =>
            var result = 0.0
            var index = 0
            while index < natural.length do
              result += glrmSoftplus(natural(index)) - observed.values(index) * natural(index)
              index += 1
            Right(result)
          case CategoricalCrossEntropy =>
            val observedIndex = observed.rawValue.toInt
            Right(glrmLogSumExp(natural) - natural(observedIndex))
      _ <- glrmFinite("entry loss", result)
    yield result

  private[multivar] def naturalGradient(
      feature: GlrmFeatureId,
      observed: FeatureEmbedding,
      natural: DVec
  ): Either[GeneralizedLowRankError, DVec] =
    for
      _ <- glrmRequireLossDomain(feature, this, observed.domain)
      _ <- glrmRequireNatural(feature, observed.domain, natural)
      gradient =
        this match
          case Quadratic => Array(natural(0) - observed.rawValue)
          case Huber(delta) =>
            val residual = natural(0) - observed.rawValue
            Array(Math.max(-delta.value, Math.min(delta.value, residual)))
          case Logistic => Array(glrmSigmoid(natural(0)) - observed.rawValue)
          case Poisson => Array(Math.exp(natural(0)) - observed.rawValue)
          case OrdinalCumulativeLogit =>
            Array.tabulate(natural.length)(index => glrmSigmoid(natural(index)) - observed.values(index))
          case CategoricalCrossEntropy =>
            val probabilities = glrmSoftmax(natural)
            val observedIndex = observed.rawValue.toInt
            probabilities(observedIndex) -= 1.0
            probabilities
      _ <- glrmRequireFiniteArray(feature, "entry-loss gradient", gradient)
    yield GaleNumerics.vectorFromArray(gradient)

  private[multivar] def curvatureUpperBound: Option[Double] =
    this match
      case Quadratic | Huber(_) => Some(1.0)
      case Logistic | OrdinalCumulativeLogit => Some(0.25)
      case CategoricalCrossEntropy => Some(0.5)
      case Poisson => None

  def decode(
      feature: GlrmFeatureId,
      domain: FeatureDomain,
      natural: DVec
  ): Either[GeneralizedLowRankError, DecodedPrediction] =
    for
      _ <- glrmRequireLossDomain(feature, this, domain)
      _ <- glrmRequireNatural(feature, domain, natural)
      decoded <-
        this match
          case Quadratic | Huber(_) => Right(DecodedPrediction.Point(natural(0)))
          case Logistic => glrmProbability(glrmSigmoid(natural(0))).map(DecodedPrediction.Binary.apply)
          case Poisson =>
            val mean = Math.exp(natural(0))
            if mean.isFinite then Right(DecodedPrediction.ExpectedCount(mean))
            else Left(GeneralizedLowRankError.InvalidNaturalParameter(feature, "Poisson mean overflowed"))
          case OrdinalCumulativeLogit => glrmDecodeOrdinal(feature, domain, natural)
          case CategoricalCrossEntropy => glrmDecodeCategorical(feature, domain, natural)
    yield decoded

final class FeatureEmbedding private (
    val domain: FeatureDomain,
    val rawValue: Double,
    val values: DVec
)

object FeatureEmbedding:
  def from(
      feature: GlrmFeatureId,
      domain: FeatureDomain,
      row: Int,
      value: Double
  ): Either[GeneralizedLowRankError, FeatureEmbedding] =
    domain
      .validate(value)
      .left
      .map(detail => GeneralizedLowRankError.InvalidObservedValue(feature, row, value, detail))
      .map: _ =>
        val encoded = domain match
          case FeatureDomain.Real | FeatureDomain.Binary | FeatureDomain.Count => Array(value)
          case FeatureDomain.Ordinal(levels) =>
            val level = value.toInt
            Array.tabulate(levels.size - 1)(threshold => if level > threshold then 1.0 else 0.0)
          case FeatureDomain.Categorical(levels) =>
            val level = value.toInt
            Array.tabulate(levels.size)(index => if index == level then 1.0 else 0.0)
        new FeatureEmbedding(domain, value, GaleNumerics.vectorFromArray(encoded))

opaque type Probability = Double

object Probability:
  extension (probability: Probability)
    inline def value: Double = probability

  private[multivar] def from(value: Double): Either[GeneralizedLowRankError, Probability] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value)
    else Left(GeneralizedLowRankError.InvalidDefinition(s"probability must lie in [0,1], got $value"))

enum DecodedPrediction:
  case Point(value: Double)
  case Binary(probabilityOfOne: Probability)
  case ExpectedCount(mean: Double)
  case Ordinal(levels: OrderedLevels, probabilities: Vector[Probability])
  case Categorical(levels: CategoryLevels, probabilities: Vector[Probability])

opaque type ObservationWeight = Double

object ObservationWeight:
  def apply(value: Double): Either[GeneralizedLowRankError, ObservationWeight] =
    if value.isFinite && value > 0.0 then Right(value)
    else
      Left(
        GeneralizedLowRankError.InvalidDefinition(
          s"an observed-entry weight must be finite and strictly positive, got $value"
        )
      )

  def unsafe(value: Double): ObservationWeight =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  val one: ObservationWeight = 1.0

  extension (weight: ObservationWeight)
    inline def value: Double = weight

opaque type ObservationReason = String

object ObservationReason:
  def apply(value: String): Either[GeneralizedLowRankError, ObservationReason] =
    val clean = value.trim
    if clean.nonEmpty then Right(clean)
    else Left(GeneralizedLowRankError.InvalidDefinition("observation-state reason must be non-empty"))

  def unsafe(value: String): ObservationReason =
    apply(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (reason: ObservationReason)
    inline def value: String = reason

enum CensoringKind:
  case AtOrBelow
  case AtOrAbove
  case Interval

final class CensoringInterval private (
    val kind: CensoringKind,
    val lower: Option[Double],
    val upper: Option[Double]
):
  override def toString: String =
    kind match
      case CensoringKind.AtOrBelow => s"censored at or below ${upper.get}"
      case CensoringKind.AtOrAbove => s"censored at or above ${lower.get}"
      case CensoringKind.Interval => s"interval-censored in [${lower.get}, ${upper.get}]"

object CensoringInterval:
  def atOrBelow(bound: Double): Either[GeneralizedLowRankError, CensoringInterval] =
    glrmFiniteBound(bound).map(_ => new CensoringInterval(CensoringKind.AtOrBelow, None, Some(bound)))

  def atOrAbove(bound: Double): Either[GeneralizedLowRankError, CensoringInterval] =
    glrmFiniteBound(bound).map(_ => new CensoringInterval(CensoringKind.AtOrAbove, Some(bound), None))

  def between(lower: Double, upper: Double): Either[GeneralizedLowRankError, CensoringInterval] =
    for
      _ <- glrmFiniteBound(lower)
      _ <- glrmFiniteBound(upper)
      _ <-
        if lower < upper then Right(())
        else Left(GeneralizedLowRankError.InvalidDefinition("censoring interval lower bound must be below upper bound"))
    yield new CensoringInterval(CensoringKind.Interval, Some(lower), Some(upper))

enum ObservationCell:
  case Observed(value: Double, weight: ObservationWeight = ObservationWeight.one)
  case Missing(reason: ObservationReason)
  case StructurallyInapplicable(reason: ObservationReason)
  case Censored(interval: CensoringInterval, weight: ObservationWeight = ObservationWeight.one)

final class ObservationPattern[Rows <: SemanticSpace, Feature <: SemanticSpace] private (
    val rowSpace: SpaceEvidence[Rows],
    val featureSpace: SpaceEvidence[Feature],
    private val cells: Vector[ObservationCell],
    val valueIdentity: ValueIdentity
):
  def cell(row: Int, feature: Int): Either[GeneralizedLowRankError, ObservationCell] =
    if row < 0 || row >= rowSpace.dimension then
      Left(GeneralizedLowRankError.IndexOutOfBounds("observation row", row, rowSpace.dimension))
    else if feature < 0 || feature >= featureSpace.dimension then
      Left(GeneralizedLowRankError.IndexOutOfBounds("observation feature", feature, featureSpace.dimension))
    else Right(cellUnsafe(row, feature))

  private[multivar] def cellUnsafe(row: Int, feature: Int): ObservationCell =
    cells(row * featureSpace.dimension + feature)

  def observedCount: Int = cells.count(_.isInstanceOf[ObservationCell.Observed])
  def missingCount: Int = cells.count(_.isInstanceOf[ObservationCell.Missing])
  def structurallyInapplicableCount: Int = cells.count(_.isInstanceOf[ObservationCell.StructurallyInapplicable])
  def censoredCount: Int = cells.count(_.isInstanceOf[ObservationCell.Censored])

  def isPointComplete: Boolean = observedCount == cells.length

object ObservationPattern:
  def from[Rows <: SemanticSpace, Feature <: SemanticSpace](
      rowSpace: SpaceEvidence[Rows],
      featureSpace: SpaceEvidence[Feature],
      cells: Vector[ObservationCell],
      valueIdentity: ValueIdentity
  ): Either[GeneralizedLowRankError, ObservationPattern[Rows, Feature]] =
    val expected = rowSpace.dimension * featureSpace.dimension
    if cells.length != expected then
      Left(
        GeneralizedLowRankError.InvalidDefinition(
          s"observation pattern requires $expected row-major cells, got ${cells.length}"
        )
      )
    else Right(new ObservationPattern(rowSpace, featureSpace, cells, valueIdentity))

  def singleRowSparse[Rows <: SemanticSpace, Feature <: SemanticSpace](
      rowSpace: SpaceEvidence[Rows],
      featureSpace: SpaceEvidence[Feature],
      observed: Vector[SparsePointObservation],
      unobservedReason: ObservationReason,
      valueIdentity: ValueIdentity
  ): Either[GeneralizedLowRankError, ObservationPattern[Rows, Feature]] =
    val foreignFeature = observed.find(entry => entry.feature < 0 || entry.feature >= featureSpace.dimension)
    if rowSpace.dimension != 1 then
      Left(GeneralizedLowRankError.InvalidDefinition("a sparse partial-row pattern requires a one-row semantic space"))
    else
      foreignFeature match
        case Some(entry) =>
          Left(
            GeneralizedLowRankError.IndexOutOfBounds(
              "sparse observation feature",
              entry.feature,
              featureSpace.dimension
            )
          )
        case None if observed.isEmpty =>
          from(
            rowSpace,
            featureSpace,
            Vector.fill(featureSpace.dimension)(ObservationCell.Missing(unobservedReason)),
            valueIdentity
          )
        case None if observed.map(_.feature).distinct.length != observed.length =>
          Left(GeneralizedLowRankError.InvalidDefinition("sparse point-observation feature indices must be distinct"))
        case None =>
          val cells = Array.fill[ObservationCell](featureSpace.dimension)(ObservationCell.Missing(unobservedReason))
          var index = 0
          while index < observed.length do
            val entry = observed(index)
            cells(entry.feature) = ObservationCell.Observed(entry.value, entry.weight)
            index += 1
          from(rowSpace, featureSpace, cells.toVector, valueIdentity)

final class SparsePointObservation private (
    val feature: Int,
    val value: Double,
    val weight: ObservationWeight
)

object SparsePointObservation:
  def from(
      feature: Int,
      featureCount: Int,
      value: Double,
      weight: ObservationWeight = ObservationWeight.one
  ): Either[GeneralizedLowRankError, SparsePointObservation] =
    if feature < 0 || feature >= featureCount then
      Left(GeneralizedLowRankError.IndexOutOfBounds("sparse observation feature", feature, featureCount))
    else Right(new SparsePointObservation(feature, value, weight))

enum MissingnessMechanism:
  case MCAR
  case MAR
  case MNAR

enum MissingnessStatement:
  case Complete
  case Unspecified
  case StructuralByDesign(reason: ObservationReason)
  case UserDeclared(mechanism: MissingnessMechanism, rationale: ObservationReason)

  /** This is descriptive provenance, not an inferential certificate. */
  def grantsInferentialClaim: Boolean = false

enum GlrmPredictionTarget:
  case ReconstructObserved
  case ImputeDeclaredMissing
  case EncodeNewRows
  case DescribeLatentStructure

final class GlrmFeatureSpec private (
    val id: GlrmFeatureId,
    val domain: FeatureDomain,
    val loss: EntryLoss
)

object GlrmFeatureSpec:
  def from(
      id: GlrmFeatureId,
      domain: FeatureDomain,
      loss: EntryLoss
  ): Either[GeneralizedLowRankError, GlrmFeatureSpec] =
    if loss.accepts(domain) then Right(new GlrmFeatureSpec(id, domain, loss))
    else Left(GeneralizedLowRankError.LossDomainMismatch(id, domain, loss))

final class GlrmFeatureLayout[Feature <: SemanticSpace] private (
    val featureSpace: SpaceEvidence[Feature],
    val features: Vector[GlrmFeatureSpec],
    private val offsets: Vector[Int],
    val naturalDimension: Int,
    val valueIdentity: ValueIdentity
):
  def feature(index: Int): Either[GeneralizedLowRankError, GlrmFeatureSpec] =
    if index < 0 || index >= features.length then
      Left(GeneralizedLowRankError.IndexOutOfBounds("GLRM feature", index, features.length))
    else Right(featureUnsafe(index))

  private[multivar] def featureUnsafe(index: Int): GlrmFeatureSpec = features(index)
  private[multivar] def offset(index: Int): Int = offsets(index)
  private[multivar] def width(index: Int): Int = features(index).domain.naturalDimension

  private[multivar] def sameStructure(other: GlrmFeatureLayout[? <: SemanticSpace]): Boolean =
    valueIdentity == other.valueIdentity &&
      features.map(feature => (feature.id, feature.domain, feature.loss)) ==
        other.features.map(feature => (feature.id, feature.domain, feature.loss))

object GlrmFeatureLayout:
  def from[Feature <: SemanticSpace](
      featureSpace: SpaceEvidence[Feature],
      features: Vector[GlrmFeatureSpec],
      valueIdentity: ValueIdentity
  ): Either[GeneralizedLowRankError, GlrmFeatureLayout[Feature]] =
    if features.length != featureSpace.dimension then
      Left(
        GeneralizedLowRankError.InvalidDefinition(
          s"feature layout for ${featureSpace.id.value} requires ${featureSpace.dimension} features, got ${features.length}"
        )
      )
    else if features.map(_.id).distinct.length != features.length then
      Left(GeneralizedLowRankError.InvalidDefinition("GLRM feature ids must be distinct"))
    else
      val offsets = features.scanLeft(0)((offset, feature) => offset + feature.domain.naturalDimension).dropRight(1)
      val dimension = features.map(_.domain.naturalDimension).sum
      Right(new GlrmFeatureLayout(featureSpace, features, offsets, dimension, valueIdentity))

final class GlrmRowCodes[Rows <: SemanticSpace, Latent <: SemanticSpace] private (
    val rowSpace: SpaceEvidence[Rows],
    val latentSpace: SpaceEvidence[Latent],
    val values: DMat,
    val valueIdentity: ValueIdentity
)

object GlrmRowCodes:
  def from[Rows <: SemanticSpace, Latent <: SemanticSpace](
      rowSpace: SpaceEvidence[Rows],
      latentSpace: SpaceEvidence[Latent],
      values: DMat,
      valueIdentity: ValueIdentity
  ): Either[GeneralizedLowRankError, GlrmRowCodes[Rows, Latent]] =
    if values.rows != rowSpace.dimension || values.cols != latentSpace.dimension then
      Left(
        GeneralizedLowRankError.InvalidDefinition(
          s"row codes must be ${rowSpace.dimension} x ${latentSpace.dimension}, got ${values.rows} x ${values.cols}"
        )
      )
    else glrmRequireFiniteMatrix("row codes", values).map(_ => new GlrmRowCodes(rowSpace, latentSpace, values, valueIdentity))

final class FeatureDecoder[Feature <: SemanticSpace, Latent <: SemanticSpace] private (
    val layout: GlrmFeatureLayout[Feature],
    val latentSpace: SpaceEvidence[Latent],
    val values: DMat,
    val valueIdentity: ValueIdentity
):
  private[multivar] def natural(rowCode: DMat, row: Int, feature: Int): DVec =
    val width = layout.width(feature)
    val offset = layout.offset(feature)
    val result = new Array[Double](width)
    var output = 0
    while output < width do
      var value = 0.0
      var latent = 0
      while latent < latentSpace.dimension do
        value += rowCode(row, latent) * values(latent, offset + output)
        latent += 1
      result(output) = value
      output += 1
    GaleNumerics.vectorFromArray(result)

  private[multivar] def naturalFromCode(code: DMat, feature: Int): DVec =
    val width = layout.width(feature)
    val offset = layout.offset(feature)
    val result = new Array[Double](width)
    var output = 0
    while output < width do
      var value = 0.0
      var latent = 0
      while latent < latentSpace.dimension do
        value += code(latent, 0) * values(latent, offset + output)
        latent += 1
      result(output) = value
      output += 1
    GaleNumerics.vectorFromArray(result)

object FeatureDecoder:
  def from[Feature <: SemanticSpace, Latent <: SemanticSpace](
      layout: GlrmFeatureLayout[Feature],
      latentSpace: SpaceEvidence[Latent],
      values: DMat,
      valueIdentity: ValueIdentity
  ): Either[GeneralizedLowRankError, FeatureDecoder[Feature, Latent]] =
    if values.rows != latentSpace.dimension || values.cols != layout.naturalDimension then
      Left(
        GeneralizedLowRankError.InvalidDefinition(
          s"feature decoder must be ${latentSpace.dimension} x ${layout.naturalDimension}, " +
            s"got ${values.rows} x ${values.cols}"
        )
      )
    else glrmRequireFiniteMatrix("feature decoder", values).map(_ => new FeatureDecoder(layout, latentSpace, values, valueIdentity))

final class GlrmFactors[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace,
    Latent <: SemanticSpace
] private (
    val rowCodes: GlrmRowCodes[Rows, Latent],
    val decoder: FeatureDecoder[Feature, Latent]
):
  def decoded(row: Int, feature: Int): Either[GeneralizedLowRankError, DecodedPrediction] =
    if row < 0 || row >= rowCodes.rowSpace.dimension then
      Left(GeneralizedLowRankError.IndexOutOfBounds("GLRM row", row, rowCodes.rowSpace.dimension))
    else
      decoder.layout.feature(feature).flatMap: specification =>
        specification.loss.decode(
          specification.id,
          specification.domain,
          decoder.natural(rowCodes.values, row, feature)
        )

object GlrmFactors:
  def from[
      Rows <: SemanticSpace,
      Feature <: SemanticSpace,
      Latent <: SemanticSpace
  ](
      rowCodes: GlrmRowCodes[Rows, Latent],
      decoder: FeatureDecoder[Feature, Latent]
  ): Either[GeneralizedLowRankError, GlrmFactors[Rows, Feature, Latent]] =
    if rowCodes.latentSpace.descriptor != decoder.latentSpace.descriptor then
      Left(GeneralizedLowRankError.InvalidDefinition("row codes and feature decoder use different latent spaces"))
    else Right(new GlrmFactors(rowCodes, decoder))

enum GlrmFactorTarget:
  case RowCodes
  case FeatureDecoder

enum GlrmFactorPenalty:
  case ElementwiseL1
  case SquaredFrobenius

  private[multivar] def value(matrix: DMat): Double =
    var result = 0.0
    var row = 0
    while row < matrix.rows do
      var column = 0
      while column < matrix.cols do
        val current = matrix(row, column)
        this match
          case ElementwiseL1 => result += Math.abs(current)
          case SquaredFrobenius => result += 0.5 * current * current
        column += 1
      row += 1
    result

final case class GlrmFactorPenaltyTerm(
    target: GlrmFactorTarget,
    functional: GlrmFactorPenalty,
    weight: PenaltyWeight,
    valueIdentity: ValueIdentity
)

final case class GlrmObjectiveValue(
    observedEntryLoss: Double,
    rowPenalty: Double,
    decoderPenalty: Double
):
  require(observedEntryLoss.isFinite && observedEntryLoss >= 0.0)
  require(rowPenalty.isFinite && rowPenalty >= 0.0)
  require(decoderPenalty.isFinite && decoderPenalty >= 0.0)

  def total: Double = observedEntryLoss + rowPenalty + decoderPenalty

final class GeneralizedLowRankProgram[
    Rows <: SemanticSpace,
    Feature <: SemanticSpace
] private (
    val observations: ObservationPattern[Rows, Feature],
    val layout: GlrmFeatureLayout[Feature],
    val factorPenalties: Vector[GlrmFactorPenaltyTerm],
    val missingness: MissingnessStatement,
    val predictionTarget: GlrmPredictionTarget,
    val programIdentity: ValueIdentity
):
  def evaluate[Latent <: SemanticSpace](
      factors: GlrmFactors[Rows, Feature, Latent]
  ): Either[GeneralizedLowRankError, GlrmObjectiveValue] =
    if factors.rowCodes.rowSpace.descriptor != observations.rowSpace.descriptor then
      Left(GeneralizedLowRankError.InvalidDefinition("GLRM factors use a foreign row space"))
    else if !factors.decoder.layout.sameStructure(layout) then
      Left(GeneralizedLowRankError.InvalidDefinition("GLRM factors use a foreign feature layout"))
    else
      var observedLoss = 0.0
      var row = 0
      var failure = Option.empty[GeneralizedLowRankError]
      while row < observations.rowSpace.dimension && failure.isEmpty do
        var feature = 0
        while feature < observations.featureSpace.dimension && failure.isEmpty do
          observations.cellUnsafe(row, feature) match
            case ObservationCell.Observed(value, weight) =>
              val specification = layout.featureUnsafe(feature)
              val contribution =
                for
                  embedding <- FeatureEmbedding.from(specification.id, specification.domain, row, value)
                  loss <- specification.loss.value(
                    specification.id,
                    embedding,
                    factors.decoder.natural(factors.rowCodes.values, row, feature)
                  )
                  weighted = weight.value * loss
                  _ <- glrmFinite("weighted entry loss", weighted)
                yield weighted
              contribution match
                case Left(error) => failure = Some(error)
                case Right(value) => observedLoss += value
            case ObservationCell.Missing(_) | ObservationCell.StructurallyInapplicable(_) => ()
            case ObservationCell.Censored(interval, _) =>
              failure = Some(GeneralizedLowRankError.UnsupportedCensoring(row, feature, interval))
          feature += 1
        row += 1
      failure match
        case Some(error) => Left(error)
        case None =>
          val rowPenalty = glrmPenaltyValue(factorPenalties, GlrmFactorTarget.RowCodes, factors.rowCodes.values)
          val decoderPenalty = glrmPenaltyValue(
            factorPenalties,
            GlrmFactorTarget.FeatureDecoder,
            factors.decoder.values
          )
          val result = GlrmObjectiveValue(observedLoss, rowPenalty, decoderPenalty)
          glrmFinite("GLRM objective", result.total).map(_ => result)

object GeneralizedLowRankProgram:
  def from[Rows <: SemanticSpace, Feature <: SemanticSpace](
      observations: ObservationPattern[Rows, Feature],
      layout: GlrmFeatureLayout[Feature],
      factorPenalties: Vector[GlrmFactorPenaltyTerm],
      missingness: MissingnessStatement,
      predictionTarget: GlrmPredictionTarget
  ): Either[GeneralizedLowRankError, GeneralizedLowRankProgram[Rows, Feature]] =
    if observations.featureSpace.descriptor != layout.featureSpace.descriptor then
      Left(GeneralizedLowRankError.InvalidDefinition("observation pattern and feature layout use different feature spaces"))
    else if factorPenalties.map(_.valueIdentity).distinct.length != factorPenalties.length then
      Left(GeneralizedLowRankError.InvalidDefinition("GLRM factor penalty identities must be distinct"))
    else if missingness == MissingnessStatement.Complete && !observations.isPointComplete then
      Left(GeneralizedLowRankError.InvalidDefinition("complete missingness metadata requires every cell to be point-observed"))
    else if observations.observedCount == 0 then
      Left(GeneralizedLowRankError.InvalidDefinition("GLRM objective requires at least one point-observed entry"))
    else
      var row = 0
      var failure = Option.empty[GeneralizedLowRankError]
      while row < observations.rowSpace.dimension && failure.isEmpty do
        var feature = 0
        while feature < observations.featureSpace.dimension && failure.isEmpty do
          observations.cellUnsafe(row, feature) match
            case ObservationCell.Observed(value, _) =>
              val specification = layout.featureUnsafe(feature)
              if !specification.loss.accepts(specification.domain) then
                failure = Some(
                  GeneralizedLowRankError.LossDomainMismatch(
                    specification.id,
                    specification.domain,
                    specification.loss
                  )
                )
              else
                specification.domain.validate(value).left.foreach: detail =>
                  failure = Some(
                    GeneralizedLowRankError.InvalidObservedValue(specification.id, row, value, detail)
                  )
            case _ => ()
          feature += 1
        row += 1
      failure match
        case Some(error) => Left(error)
        case None =>
          val inputs = Vector(observations.valueIdentity, layout.valueIdentity) ++ factorPenalties.map(_.valueIdentity)
          Right(
            new GeneralizedLowRankProgram(
              observations,
              layout,
              factorPenalties,
              missingness,
              predictionTarget,
              ValueIdentity.derived("generalized-low-rank-program", inputs*)
            )
          )

private def glrmValidatedLevels(
    label: String,
    values: Vector[String]
): Either[GeneralizedLowRankError, Vector[String]] =
  val clean = values.map(_.trim)
  if clean.length < 2 then Left(GeneralizedLowRankError.InvalidDefinition(s"$label require at least two entries"))
  else if clean.exists(_.isEmpty) then Left(GeneralizedLowRankError.InvalidDefinition(s"$label must be non-empty"))
  else if clean.distinct.length != clean.length then Left(GeneralizedLowRankError.InvalidDefinition(s"$label must be distinct"))
  else Right(clean)

private def glrmValidateLevelIndex(value: Double, levels: Int, label: String): Either[String, Unit] =
  if value == Math.rint(value) && value >= 0.0 && value < levels.toDouble then Right(())
  else Left(s"$label values must be integer level indices in 0..${levels - 1}")

private def glrmRequireLossDomain(
    feature: GlrmFeatureId,
    loss: EntryLoss,
    domain: FeatureDomain
): Either[GeneralizedLowRankError, Unit] =
  if loss.accepts(domain) then Right(())
  else Left(GeneralizedLowRankError.LossDomainMismatch(feature, domain, loss))

private def glrmRequireNatural(
    feature: GlrmFeatureId,
    domain: FeatureDomain,
    natural: DVec
): Either[GeneralizedLowRankError, Unit] =
  if natural.length != domain.naturalDimension then
    Left(GeneralizedLowRankError.NaturalParameterMismatch(feature, domain.naturalDimension, natural.length))
  else
    var index = 0
    var nonFinite = Option.empty[(Int, Double)]
    while index < natural.length && nonFinite.isEmpty do
      if !natural(index).isFinite then nonFinite = Some(index -> natural(index))
      index += 1
    nonFinite match
      case Some((position, value)) =>
        Left(GeneralizedLowRankError.InvalidNaturalParameter(feature, s"coordinate $position is not finite: $value"))
      case None =>
        domain match
          case FeatureDomain.Ordinal(_) =>
            var position = 1
            var violation = Option.empty[Int]
            while position < natural.length && violation.isEmpty do
              if natural(position - 1) < natural(position) then violation = Some(position)
              position += 1
            violation match
              case Some(current) =>
                Left(
                  GeneralizedLowRankError.InvalidNaturalParameter(
                    feature,
                    s"ordinal cumulative logits must be non-increasing at coordinates ${current - 1} and $current"
                  )
                )
              case None => Right(())
          case _ => Right(())

private def glrmDecodeOrdinal(
    feature: GlrmFeatureId,
    domain: FeatureDomain,
    natural: DVec
): Either[GeneralizedLowRankError, DecodedPrediction] =
  domain match
    case FeatureDomain.Ordinal(levels) =>
      val cumulative = Array.tabulate(natural.length)(index => glrmSigmoid(natural(index)))
      val probabilities = new Array[Double](levels.size)
      probabilities(0) = 1.0 - cumulative(0)
      var level = 1
      while level < levels.size - 1 do
        probabilities(level) = cumulative(level - 1) - cumulative(level)
        level += 1
      probabilities(levels.size - 1) = cumulative.last
      glrmProbabilities(probabilities.toVector).map(DecodedPrediction.Ordinal(levels, _))
    case _ => Left(GeneralizedLowRankError.LossDomainMismatch(feature, domain, EntryLoss.OrdinalCumulativeLogit))

private def glrmDecodeCategorical(
    feature: GlrmFeatureId,
    domain: FeatureDomain,
    natural: DVec
): Either[GeneralizedLowRankError, DecodedPrediction] =
  val normalized = glrmSoftmax(natural).toVector
  domain match
    case FeatureDomain.Categorical(levels) =>
      glrmProbabilities(normalized).map(DecodedPrediction.Categorical(levels, _))
    case _ =>
      Left(
        GeneralizedLowRankError.LossDomainMismatch(
          feature,
          domain,
          EntryLoss.CategoricalCrossEntropy
        )
      )

private def glrmProbability(value: Double): Either[GeneralizedLowRankError, Probability] =
  Probability.from(value)

private def glrmProbabilities(values: Vector[Double]): Either[GeneralizedLowRankError, Vector[Probability]] =
  values.foldLeft[Either[GeneralizedLowRankError, Vector[Probability]]](Right(Vector.empty)): (result, value) =>
    for
      current <- result
      probability <- Probability.from(value)
    yield current :+ probability

private def glrmSoftplus(value: Double): Double =
  if value > 0.0 then value + Math.log1p(Math.exp(-value))
  else Math.log1p(Math.exp(value))

private def glrmSigmoid(value: Double): Double =
  if value >= 0.0 then
    val exponential = Math.exp(-value)
    1.0 / (1.0 + exponential)
  else
    val exponential = Math.exp(value)
    exponential / (1.0 + exponential)

private def glrmSoftmax(values: DVec): Array[Double] =
  var maximum = Double.NegativeInfinity
  var index = 0
  while index < values.length do
    maximum = Math.max(maximum, values(index))
    index += 1
  val result = new Array[Double](values.length)
  var total = 0.0
  index = 0
  while index < values.length do
    result(index) = Math.exp(values(index) - maximum)
    total += result(index)
    index += 1
  index = 0
  while index < result.length do
    result(index) /= total
    index += 1
  result

private def glrmLogSumExp(values: DVec): Double =
  var maximum = Double.NegativeInfinity
  var index = 0
  while index < values.length do
    maximum = Math.max(maximum, values(index))
    index += 1
  var sum = 0.0
  index = 0
  while index < values.length do
    sum += Math.exp(values(index) - maximum)
    index += 1
  maximum + Math.log(sum)

private def glrmLogFactorial(value: Int): Double =
  var result = 0.0
  var current = 2
  while current <= value do
    result += Math.log(current.toDouble)
    current += 1
  result

private def glrmPenaltyValue(
    penalties: Vector[GlrmFactorPenaltyTerm],
    target: GlrmFactorTarget,
    matrix: DMat
): Double =
  penalties.filter(_.target == target).map(term => term.weight.value * term.functional.value(matrix)).sum

private def glrmFinite(context: String, value: Double): Either[GeneralizedLowRankError, Unit] =
  if value.isFinite && value >= 0.0 then Right(())
  else Left(GeneralizedLowRankError.NonFiniteObjective(context, value))

private def glrmFiniteBound(value: Double): Either[GeneralizedLowRankError, Unit] =
  if value.isFinite then Right(())
  else Left(GeneralizedLowRankError.InvalidDefinition(s"censoring bound must be finite, got $value"))

private def glrmRequireFiniteMatrix(context: String, matrix: DMat): Either[GeneralizedLowRankError, Unit] =
  var row = 0
  var failure = Option.empty[(Int, Int, Double)]
  while row < matrix.rows && failure.isEmpty do
    var column = 0
    while column < matrix.cols && failure.isEmpty do
      if !matrix(row, column).isFinite then failure = Some((row, column, matrix(row, column)))
      column += 1
    row += 1
  failure match
    case Some((actualRow, column, value)) =>
      Left(
        GeneralizedLowRankError.InvalidDefinition(
          s"$context value ($actualRow,$column) is not finite: $value"
        )
      )
    case None => Right(())

private def glrmRequireFiniteArray(
    feature: GlrmFeatureId,
    context: String,
    values: Array[Double]
): Either[GeneralizedLowRankError, Unit] =
  var index = 0
  var failure = Option.empty[(Int, Double)]
  while index < values.length && failure.isEmpty do
    if !values(index).isFinite then failure = Some(index -> values(index))
    index += 1
  failure match
    case Some((position, value)) =>
      Left(
        GeneralizedLowRankError.InvalidNaturalParameter(
          feature,
          s"$context coordinate $position is not finite: $value"
        )
      )
    case None => Right(())
