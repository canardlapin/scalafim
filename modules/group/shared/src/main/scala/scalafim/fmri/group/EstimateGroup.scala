package scalafim.fmri.group

import gale.linalg.Matrix
import scalafim.dataset.SubjectId
import scalafim.estimates.*
import scala.util.control.NonFatal

enum GroupMarginalUncertainty:
  case Variance(id: ProductId)
  case StandardError(id: ProductId)

  def product: ProductId = this match
    case Variance(id) => id
    case StandardError(id) => id

final case class GroupEstimateInput(
    reference: PinnedUnit,
    observation: ObservationId,
    effect: ProductId,
    uncertainty: Option[GroupMarginalUncertainty]
)

/** Group-ready values retain the exact pinned inputs and ordered axes used to
  * assemble them. The numerical group model consumes data; its aligned
  * uncertainty and geometry receipts remain attached to that data.
  */
final class GroupEstimateBlock private[group] (
    val data: GroupData[VarianceCapability],
    val inputs: Vector[GroupEstimateInput],
    val estimands: Vector[EstimandId],
    val samples: Vector[Int]
)

/** Scientific admission stays with the consumer. Implementations must verify
  * that the units are appropriate independent participant rows and that each
  * grid's linear sample identity names the same anatomical location. A matching
  * shape, affine or label alone is not registration evidence. The first unit's
  * grid becomes the admitted output grid; this bridge performs no resampling.
  */
trait GroupEstimateAdmission:
  def verify(units: Vector[EstimateUnit]): Either[EstimateError, GroupGeometryEvidence.Verified]

/** Pinned inputs and identified targets, compiled without opening numerical
  * payloads. One source is opened, fully verified and closed at a time per block.
  * This bounds file handles but repeats integrity scans for subsequent blocks.
  */
final class EstimateGroup private (
    reader: EstimateSetReader,
    val inputs: Vector[GroupEstimateInput],
    val estimands: Vector[EstimandId],
    val units: Vector[EstimateUnit],
    val geometry: GroupGeometryEvidence.Verified,
    val maximumBlockCells: Int
):
  val participants: Vector[ParticipantId] = inputs.zip(units).map((input, unit) =>
    unit.observations.find(_.id == input.observation).get.participant)
  private val weighted = inputs.head.uncertainty.nonEmpty

  /** A strict complete-case block. Any unavailable requested cell rejects the
    * block; no participant or sample is silently dropped, and invalid fills
    * never become zero effects. Effects, variances and sample-dependent df all
    * count toward the same block budget.
    */
  def readBlock(samples: Vector[Int], cancelled: () => Boolean = () => false): Either[EstimateError, GroupEstimateBlock] =
    val sampleDfRows = inputs.zip(units).count: (input, unit) =>
      input.uncertainty.exists: requested =>
        unit.marginalUncertainty.find(_.product == requested.product).exists(_.origin match
          case MarginalVarianceOrigin.Estimated(DegreesOfFreedom(_, DfValue.Product(_), _, _)) => true
          case _ => false)
    val cells = estimands.size.toLong * samples.size *
      (inputs.size.toLong * (if weighted then 2L else 1L) + sampleDfRows.toLong)
    if samples.isEmpty || samples.distinct.size != samples.size || samples.exists(i => units.exists(u => i < 0 || i >= u.domain.sampleCount)) then
      Left(EstimateError.Invalid("group samples must be nonempty, unique and in every admitted grid"))
    else if cells > maximumBlockCells then Left(EstimateError.Invalid("group block exceeds the total effect/variance cell budget"))
    else protect:
      val effects = estimands.map(_ => Matrix.newBuilder(inputs.size, samples.size))
      val variances = if weighted then estimands.map(_ => Matrix.newBuilder(inputs.size, samples.size)) else Vector.empty
      val values = new Array[Double](samples.size)
      val validity = new Array[Byte](samples.size)
      val uncertaintySources = Vector.newBuilder[GroupUncertaintySource]
      var failure: Option[EstimateError] = None
      var row = 0
      while row < inputs.size && failure.isEmpty do
        if cancelled() then failure = Some(EstimateError.Cancelled)
        else
          val input = inputs(row)
          reader.open(input.reference, ReadLimits(samples.size)) match
            case Left(error) => failure = Some(error)
            case Right(source) =>
              val result = protect:
                if !samePinnedIdentity(source.unit, units(row)) then
                  failure = Some(EstimateError.Invalid("opened group source differs from its inspected pinned unit"))
                var target = 0
                while target < estimands.size && failure.isEmpty do
                  val selection = EstimateSelection(Vector(input.observation), Vector(estimands(target)), samples)
                  def read(product: ProductId, square: Boolean, write: (Int, Double) => Unit): Unit =
                    source.read(product, selection, values, validity, cancelled) match
                      case Left(error) => failure = Some(error)
                      case Right(_) =>
                        var sample = 0
                        while sample < samples.size && failure.isEmpty do
                          val value = if square then values(sample) * values(sample) else values(sample)
                          if validity(sample) != Validity.Valid.code || !value.isFinite then
                            failure = Some(EstimateError.Invalid(s"unavailable group input at participant ${participants(row).label}, estimand ${estimands(target).value}, sample ${samples(sample)}"))
                          else write(sample, value)
                          sample += 1
                  read(input.effect, false, (sample, value) => effects(target)(row, sample) = value)
                  if failure.isEmpty then input.uncertainty.foreach: uncertainty =>
                    read(uncertainty.product, uncertainty.isInstanceOf[GroupMarginalUncertainty.StandardError],
                      (sample, value) => variances(target)(row, sample) = value)
                    if failure.isEmpty then
                      units(row).marginalUncertainty.find(_.product == uncertainty.product) match
                        case None =>
                          failure = Some(EstimateError.Invalid("opened group input lacks its inspected uncertainty semantics"))
                        case Some(descriptor) =>
                          val unit = units(row)
                          val fit = GroupFitProvenance(unit.provenance.estimator, unit.provenance.noise,
                            unit.provenance.nuisance, unit.provenance.runCombination)
                          val resolved = descriptor.origin match
                            case MarginalVarianceOrigin.Known(method) => GroupVarianceOrigin.Known(method)
                            case MarginalVarianceOrigin.Unknown(reason) => GroupVarianceOrigin.Unknown(reason)
                            case MarginalVarianceOrigin.Estimated(df) =>
                              df.value match
                                case DfValue.Scalar(value) =>
                                  GroupVarianceOrigin.Estimated(GroupDegreesOfFreedom(df.role,
                                    GroupDfValues.Scalar(value), df.method, df.approximate))
                                case DfValue.Product(product) =>
                                  val dfValues = new Array[Double](samples.size)
                                  read(product, false, (sample, value) => dfValues(sample) = value)
                                  if failure.isEmpty && dfValues.exists(value => !value.isFinite || value <= 0.0) then
                                    failure = Some(EstimateError.Invalid(s"invalid first-level degrees of freedom at participant ${participants(row).label}, estimand ${estimands(target).value}"))
                                  if failure.isEmpty then
                                    GroupVarianceOrigin.Estimated(GroupDegreesOfFreedom(df.role,
                                      GroupDfValues.BySample(product, samples, dfValues.toVector), df.method, df.approximate))
                                  else GroupVarianceOrigin.Unknown("sample-dependent degrees of freedom were unavailable")
                                case _ =>
                                  failure = Some(EstimateError.Invalid("estimated uncertainty lacks supported scalar or product degrees of freedom"))
                                  GroupVarianceOrigin.Unknown("estimated uncertainty degrees of freedom were invalid")
                          if failure.isEmpty then
                            val pooling = unit.products.find(_.id == input.effect).map(_.pooling)
                            uncertaintySources += GroupUncertaintySource(
                              SubjectId(s"${participants(row).dataset.value}/${participants(row).label}"),
                              estimands(target).value, samples, Some(uncertainty.product), resolved, fit, pooling)
                  target += 1
                failure.toLeft(())
              val closing = protect(source.close())
              result.flatMap(_ => closing) match
                case Left(error) => failure = Some(error)
                case Right(_) => ()
        row += 1
      failure.toLeft(()).flatMap: _ =>
        val responses = estimands.indices.toVector.foldLeft[Either[EstimateError, Vector[(String, GroupResponse[VarianceCapability])]]](Right(Vector.empty)):
          (previous, index) => previous.flatMap: accumulated =>
            val response = if weighted then GroupResponse.weighted(effects(index).result(), variances(index).result())
              else GroupResponse.fromEffects(effects(index).result())
            response.left.map(error => EstimateError.Invalid(error.message)).map(value => accumulated :+ (estimands(index).value -> value))
        responses.flatMap: values =>
          val subjects = participants.map(p => SubjectId(s"${p.dataset.value}/${p.label}"))
          val uncertainty = if weighted then
            GroupUncertaintyReceipt.make(subjects, estimands.map(_.value), samples, uncertaintySources.result(), geometry)
              .left.map(error => EstimateError.Invalid(error.message)).map(Some(_))
          else Right(None)
          uncertainty.flatMap(receipt => GroupData.build(subjects,
            GroupSpace.VoxelAxis(units.head.domain.space, samples), values, receipt)
            .left.map(error => EstimateError.Invalid(error.message)))
              .map(data => new GroupEstimateBlock(data, inputs, estimands, samples))

  private def protect[A](body: => Either[EstimateError, A]): Either[EstimateError, A] =
    try body
    catch case NonFatal(error) => Left(EstimateError.Io(Option(error.getMessage).getOrElse(error.getClass.getName)))

  private def samePinnedIdentity(actual: EstimateUnit, expected: EstimateUnit): Boolean =
    actual.dataset == expected.dataset && actual.unit == expected.unit && actual.revision == expected.revision

object EstimateGroup:
  def prepare(reader: EstimateSetReader, inputs: Vector[GroupEstimateInput], estimands: Vector[EstimandId],
      admission: GroupEstimateAdmission, maximumBlockCells: Int): Either[EstimateError, EstimateGroup] =
    if inputs.isEmpty || estimands.isEmpty || estimands.distinct.size != estimands.size || maximumBlockCells <= 0 then
      Left(EstimateError.Invalid("group inputs and unique estimands must be nonempty, with a positive block budget"))
    else if inputs.map(_.uncertainty.nonEmpty).distinct.size != 1 then
      Left(EstimateError.Invalid("group inputs must all supply marginal uncertainty or all be effects-only"))
    else
      inputs.foldLeft[Either[EstimateError, Vector[EstimateUnit]]](Right(Vector.empty)): (previous, input) =>
        previous.flatMap: units =>
          reader.inspect(input.reference).flatMap: unit =>
            val effect = unit.products.find(_.id == input.effect)
            val validEffect = effect.exists(p => p.kind == ProductKind.Effect && p.observations.contains(input.observation) &&
              estimands.forall(p.targets.estimands.contains))
            val validUncertainty = input.uncertainty.forall: uncertainty =>
              val kind = uncertainty match
                case GroupMarginalUncertainty.Variance(_) => ProductKind.Variance
                case GroupMarginalUncertainty.StandardError(_) => ProductKind.StandardError
              unit.products.exists(p => p.id == uncertainty.product && p.kind == kind &&
                p.observations == effect.map(_.observations).getOrElse(Vector.empty) &&
                p.targets == effect.map(_.targets).getOrElse(ProductTargets.Scalar(Vector.empty)) &&
                effect.exists(_.pooling == p.pooling)) &&
                unit.marginalUncertainty.exists(descriptor => descriptor.product == uncertainty.product && descriptor.effects == input.effect)
            if !validEffect || !validUncertainty then Left(EstimateError.Invalid("group row lacks the requested effect or matching marginal uncertainty axes"))
            else if estimands.exists(id => unit.catalog.entry(id).exists(_.unitScope.nonEmpty)) then
              Left(EstimateError.Invalid("unit-local targets cannot form an automatic cohort join"))
            else if units.headOption.exists(first => first.dataset != unit.dataset || first.catalog != unit.catalog ||
                first.products.find(_.id == inputs.head.effect).exists(p => p.units != effect.get.units || p.pooling != effect.get.pooling)) then
              Left(EstimateError.Invalid("group units require one dataset, shared immutable catalog, effect units and pooling scope"))
            else Right(units :+ unit)
      .flatMap: units =>
        val participants = inputs.zip(units).map((input, unit) => unit.observations.find(_.id == input.observation).get.participant)
        if participants.distinct.size != participants.size then
          Left(EstimateError.Invalid("runs, trials and pooled rows from one participant cannot become independent group subjects"))
        else admission.verify(units).flatMap: geometry =>
          if units.exists(_.domain.worldFrame != geometry.worldFrame) then
            Left(EstimateError.Invalid("geometry admission world frame must match every group input domain"))
          else Right(new EstimateGroup(reader, inputs, estimands, units, geometry, maximumBlockCells))
