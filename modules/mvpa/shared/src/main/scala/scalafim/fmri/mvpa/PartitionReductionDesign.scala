package scalafim.fmri.mvpa

import multivar.core.SemanticSpace

final case class PartitionWeight private (
    partition: PartitionId,
    value: Double
)

object PartitionWeight:
  def apply(
      partition: PartitionId,
      value: Double
  ): Either[PartitionReductionDesignError, PartitionWeight] =
    if !value.isFinite then Left(PartitionReductionDesignError.NonFiniteWeight(partition, value))
    else Right(new PartitionWeight(partition, canonicalZero(value)))

  private def canonicalZero(value: Double): Double =
    if value == 0.0 then 0.0 else value

enum PartitionReducer:
  case WeightedMean
  case WeightedSum

  def label: String =
    this match
      case WeightedMean => "weighted-mean"
      case WeightedSum  => "weighted-sum"

/** Whether the requested partition reduction is conditional on the observed partitions or treats them as a sample from
  * a declared population axis.
  */
enum PartitionScope:
  case FixedObserved
  case SampledPopulation(generalizesOver: GeneralizationAxis)

  def label: String =
    this match
      case FixedObserved        => "fixed-observed"
      case SampledPopulation(_) => "sampled-population"

/** A first-order partition reduction. It is deliberately distinct from a validation design and from a second-order
  * pairing design: no observation is trained, assessed, or paired with another observation here.
  */
final class PartitionReductionDesign[P <: SemanticSpace] private (
    val partitions: PartitionAxis[P],
    val weights: Vector[PartitionWeight],
    val reducer: PartitionReducer,
    val scope: PartitionScope,
    val identity: DesignIdentity,
    val referencedAxes: Vector[DesignAxisReference]
) extends EvidenceDesign:
  def coefficient(position: Int): Double =
    reducer match
      case PartitionReducer.WeightedMean =>
        weights(position).value / weights.foldLeft(0.0)(_ + _.value)
      case PartitionReducer.WeightedSum =>
        weights(position).value

object PartitionReductionDesign:
  private val Protocol = "scalafim-mvpa-partition-reduction/v1"

  def equalMean[P <: SemanticSpace](
      partitions: PartitionAxis[P],
      scope: PartitionScope
  ): Either[PartitionReductionDesignError, PartitionReductionDesign[P]] =
    val weights = Vector.newBuilder[PartitionWeight]
    var position = 0
    while position < partitions.axis.size do
      PartitionWeight(partitions.axis.keys(position), 1.0) match
        case Left(error)  => return Left(error)
        case Right(value) => weights += value
      position += 1
    weighted(partitions, weights.result(), PartitionReducer.WeightedMean, scope)

  def weighted[P <: SemanticSpace](
      partitions: PartitionAxis[P],
      input: Seq[PartitionWeight],
      reducer: PartitionReducer,
      scope: PartitionScope
  ): Either[PartitionReductionDesignError, PartitionReductionDesign[P]] =
    for
      weights <- canonicalWeights(partitions, input, reducer)
      references <- referencesOf(partitions, scope)
      identity <- DesignIdentity(
        DesignKind.unsafe("partition-reduction"),
        Vector(
          "assignment" -> weightDigest(weights),
          "partition-axis" -> partitions.name.value,
          "partition-space" -> partitions.axis.identity.fingerprint.value,
          "protocol" -> Protocol,
          "reducer" -> reducer.label,
          "scope" -> scope.label,
          "sampling-axis" -> samplingAxis(scope),
          "sampling-space" -> samplingSpace(scope)
        )
      ).left.map(PartitionReductionDesignError.Identity.apply)
    yield new PartitionReductionDesign(
      partitions,
      weights,
      reducer,
      scope,
      identity,
      references
    )

  private def canonicalWeights[P <: SemanticSpace](
      partitions: PartitionAxis[P],
      input: Seq[PartitionWeight],
      reducer: PartitionReducer
  ): Either[PartitionReductionDesignError, Vector[PartitionWeight]] =
    if input.isEmpty then Left(PartitionReductionDesignError.EmptyWeights)
    else
      val indexed = scala.collection.mutable.HashMap.empty[PartitionId, PartitionWeight]
      val iterator = input.iterator
      while iterator.hasNext do
        val weight = iterator.next()
        if partitions.axis.positionOf(weight.partition).isEmpty then
          return Left(PartitionReductionDesignError.UnknownPartition(weight.partition))
        if indexed.contains(weight.partition) then
          return Left(PartitionReductionDesignError.DuplicatePartition(weight.partition))
        if reducer == PartitionReducer.WeightedMean && weight.value <= 0.0 then
          return Left(
            PartitionReductionDesignError.NonPositiveMeanWeight(
              weight.partition,
              weight.value
            )
          )
        indexed += weight.partition -> weight

      val output = Vector.newBuilder[PartitionWeight]
      var position = 0
      var anyNonZero = false
      while position < partitions.axis.size do
        val partition = partitions.axis.keys(position)
        indexed.get(partition) match
          case None         => return Left(PartitionReductionDesignError.MissingPartition(partition))
          case Some(weight) =>
            output += weight
            anyNonZero ||= weight.value != 0.0
        position += 1
      if !anyNonZero then Left(PartitionReductionDesignError.AllWeightsZero)
      else Right(output.result())

  private def referencesOf[P <: SemanticSpace](
      partitions: PartitionAxis[P],
      scope: PartitionScope
  ): Either[PartitionReductionDesignError, Vector[DesignAxisReference]] =
    scope match
      case PartitionScope.FixedObserved                      => Right(Vector(partitions.reference))
      case PartitionScope.SampledPopulation(generalizesOver) =>
        if partitions.name != generalizesOver.name then
          Right(Vector(partitions.reference, generalizesOver.reference).sortBy(_.name.value))
        else if partitions.axis.identity == generalizesOver.identity then Right(Vector(partitions.reference))
        else
          Left(
            PartitionReductionDesignError.ConflictingAxisReference(
              partitions.name,
              partitions.axis.identity.fingerprint,
              generalizesOver.identity.fingerprint
            )
          )

  private def samplingAxis(scope: PartitionScope): String =
    scope match
      case PartitionScope.FixedObserved                      => "none"
      case PartitionScope.SampledPopulation(generalizesOver) => generalizesOver.name.value

  private def samplingSpace(scope: PartitionScope): String =
    scope match
      case PartitionScope.FixedObserved                      => "none"
      case PartitionScope.SampledPopulation(generalizesOver) =>
        generalizesOver.identity.fingerprint.value

  private def weightDigest(weights: Vector[PartitionWeight]): String =
    val writer = CanonicalWriter()
    writer.string(Protocol)
    writer.int(weights.length)
    weights.foreach: weight =>
      writer.string(weight.partition.value)
      writer.double(weight.value)
    AxisDigest.sha256Hex(writer.result())

enum PartitionReductionDesignError:
  case Identity(error: ScientificIdentityError)
  case EmptyWeights
  case NonFiniteWeight(partition: PartitionId, value: Double)
  case UnknownPartition(partition: PartitionId)
  case DuplicatePartition(partition: PartitionId)
  case MissingPartition(partition: PartitionId)
  case NonPositiveMeanWeight(partition: PartitionId, value: Double)
  case AllWeightsZero
  case ConflictingAxisReference(
      name: ScientificAxisName,
      first: AxisFingerprint,
      second: AxisFingerprint
  )

  def message: String =
    this match
      case Identity(error)                   => error.message
      case EmptyWeights                      => "partition reduction requires at least one weight"
      case NonFiniteWeight(partition, value) =>
        s"partition '${partition.value}' has non-finite weight $value"
      case UnknownPartition(partition) =>
        s"partition reduction references unknown partition '${partition.value}'"
      case DuplicatePartition(partition) =>
        s"partition reduction repeats partition '${partition.value}'"
      case MissingPartition(partition) =>
        s"partition reduction omits partition '${partition.value}'"
      case NonPositiveMeanWeight(partition, value) =>
        s"weighted mean requires a positive weight for '${partition.value}', obtained $value"
      case AllWeightsZero => "weighted sum requires at least one non-zero partition weight"
      case ConflictingAxisReference(name, first, second) =>
        s"design axis '${name.value}' refers to both ${first.value} and ${second.value}"
