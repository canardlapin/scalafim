package scalafim.fmri.mvpa.spatial

import locus4s.{Index, Region}
import scalafim.fmri.mvpa.measurement.MeasurementId
import scalafim.locus.IndexedField

/** A completed local analysis or a visible local failure. Failure never turns
  * into a numeric fill value during scattering. */
final case class SpatialLocalOutcome[S, A](
    measurement: MeasurementId,
    support: Region[S],
    weight: Double,
    value: Either[String, A]
)

final case class SpatialScatterContributor(
    measurement: MeasurementId,
    weight: Double
)

enum SpatialScatterCell[+A]:
  case Unvisited
  case LocalFailure(failures: Vector[(MeasurementId, String)])
  case Aggregated(
      value: A,
      contributors: Vector[SpatialScatterContributor],
      denominator: Double,
      failures: Vector[(MeasurementId, String)]
  )

enum SpatialScatterError:
  case WrongRuntimeOwner(expected: String, actual: String)
  case InvalidWeight(measurement: MeasurementId, value: Double)
  case DenominatorOverflow(pointOrdinal: Int)
  case NonFiniteAggregate(pointOrdinal: Int)
  case FieldConstruction(detail: String)
  case DuplicateMeasurement(measurement: MeasurementId)

  def message: String =
    this match
      case WrongRuntimeOwner(expected, actual) => s"scatter support belongs to $actual, expected live domain owner $expected"
      case InvalidWeight(measurement, value) => s"scatter weight for '${measurement.value}' must be finite and positive, got $value"
      case DenominatorOverflow(pointOrdinal) => s"scatter denominator at point $pointOrdinal is not finite"
      case NonFiniteAggregate(pointOrdinal) => s"scatter aggregate at point $pointOrdinal is not finite"
      case FieldConstruction(detail) => detail
      case DuplicateMeasurement(measurement) => s"scatter repeats local outcome '${measurement.value}'"

/** The aggregation law is caller-declared. It is intentionally separate from
  * support geometry, so overlapping searchlights cannot silently acquire an
  * arbitrary average or zero-fill convention. */
trait SpatialScatterAlgebra[A]:
  def zero: A
  def add(total: A, value: A, weight: Double): A
  def finish(total: A, denominator: Double): A

  /** Generic algebras choose their own notion of a valid accumulator. */
  def acceptsAccumulator(value: A): Boolean = true

object SpatialScatterAlgebra:
  val weightedMeanDouble: SpatialScatterAlgebra[Double] =
    new SpatialScatterAlgebra[Double]:
      def zero: Double = 0.0
      def add(total: Double, value: Double, weight: Double): Double = total + value * weight
      def finish(total: Double, denominator: Double): Double = total / denominator
      override def acceptsAccumulator(value: Double): Boolean = value.isFinite

object SpatialMeasurementScatter:
  /** Scatter local outcomes to their locus domain. Cells with neither a
    * successful contributor nor a local failure stay `Unvisited`; the algebra
    * is never called with a zero denominator. The returned field uses locus's
    * typed vector-field builder.
    */
  def scatter[S, A](
      domain: locus4s.FiniteDomain[S],
      outcomes: Seq[SpatialLocalOutcome[S, A]],
      algebra: SpatialScatterAlgebra[A]
  ): Either[SpatialScatterError, IndexedField[S, SpatialScatterCell[A]]] =
    // Results can arrive from parallel/local execution in arbitrary order.
    // Fix the reduction order by declared measurement identity before any
    // floating-point accumulation or contributor receipt is produced.
    val items = outcomes.toVector.sortBy(_.measurement.value)
    val seen = scala.collection.mutable.HashSet.empty[MeasurementId]
    var item = 0
    while item < items.length do
      val outcome = items(item)
      if !domain.sameRuntimeOwnerAs(outcome.support.space) then
        return Left(SpatialScatterError.WrongRuntimeOwner(domain.toString, outcome.support.space.toString))
      if !outcome.weight.isFinite || outcome.weight <= 0.0 then
        return Left(SpatialScatterError.InvalidWeight(outcome.measurement, outcome.weight))
      if seen.contains(outcome.measurement) then return Left(SpatialScatterError.DuplicateMeasurement(outcome.measurement))
      seen += outcome.measurement
      item += 1

    val totals = Array.fill[Option[A]](domain.size)(None)
    val denominators = Array.fill(domain.size)(0.0)
    val contributors = Array.fill(domain.size)(Vector.empty[SpatialScatterContributor])
    val failures = Array.fill(domain.size)(Vector.empty[(MeasurementId, String)])
    item = 0
    while item < items.length do
      val outcome = items(item)
      val ordinals = outcome.support.ordinalsInDomainOrder
      outcome.value match
        case Left(error) =>
          var position = 0
          while position < ordinals.length do
            val ordinal = ordinals(position)
            failures(ordinal) = failures(ordinal) :+ (outcome.measurement -> error)
            position += 1
        case Right(value) =>
          var position = 0
          while position < ordinals.length do
            val ordinal = ordinals(position)
            val total = if denominators(ordinal) == 0.0 then algebra.zero else totals(ordinal).get
            val aggregate = algebra.add(total, value, outcome.weight)
            if !algebra.acceptsAccumulator(aggregate) then
              return Left(SpatialScatterError.NonFiniteAggregate(ordinal))
            totals(ordinal) = Some(aggregate)
            val denominator = denominators(ordinal) + outcome.weight
            if !denominator.isFinite then
              return Left(SpatialScatterError.DenominatorOverflow(ordinal))
            denominators(ordinal) = denominator
            contributors(ordinal) = contributors(ordinal) :+ SpatialScatterContributor(outcome.measurement, outcome.weight)
            position += 1
      item += 1

    IndexedField
      .fromValues(domain, domain.indices.map: point =>
        val ordinal = point.ordinal
        if denominators(ordinal) > 0.0 then
          SpatialScatterCell.Aggregated(
            algebra.finish(totals(ordinal).get, denominators(ordinal)),
            contributors(ordinal),
            denominators(ordinal),
            failures(ordinal)
          )
        else if failures(ordinal).nonEmpty then SpatialScatterCell.LocalFailure(failures(ordinal))
        else SpatialScatterCell.Unvisited
      )
      .left
      .map(error => SpatialScatterError.FieldConstruction(error.message))
