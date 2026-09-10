package scalafim.fmri.fit.estimates

import gale.backend.Backend
import gale.linalg.{DMat, DVec}
import scalafim.dataset.DatasetSeriesReader
import scalafim.estimates.*
import scalafim.fmri.fit.*
import scala.util.control.NonFatal

/** Persistent identity is allocated by the application before execution, and
  * is reused only for a retry of the same scientific computation.
  */
final case class EstimatePublicationIdentity(
    dataset: scalafim.estimates.DatasetId,
    unit: UnitId,
    revision: UnitRevisionId,
    participantLabel: String,
    observation: ObservationId,
    acquisitions: Vector[AcquisitionId],
    executionId: String,
    producerVersion: String,
    responseUnits: String,
    inputs: Vector[ExternalInput]
)

final class FitEstimateProducer private (
    val unit: EstimateUnit,
    val prepared: FirstLevelEstimatePlan,
    outputs: Vector[EstimandId]
):
  /** Own the sink through completion, cancellation and callback failure. The
    * caller continues to own the dataset reader and numerical backend.
    */
  def write(reader: DatasetSeriesReader, sink: EstimateSink, cancelled: () => Boolean = () => false)(using Backend): Either[EstimateError, PinnedUnit] =
    if sink.unit != unit then Left(EstimateError.Invalid("sink declaration differs from the compiled scientific output"))
    else if prepared.request.uncertainty == EstimateUncertaintyRequest.Joint && !sink.supportsCovariance then
      sink.abort()
      Left(EstimateError.Unsupported("requested joint uncertainty requires a pair-axis sink"))
    else if prepared.maxBlockVoxels > sink.maximumBlockCells then Left(EstimateError.Invalid("sink block budget is smaller than the prepared voxel block"))
    else
      var sinkError: Option[EstimateError] = None
      def deliver(kind: ProductKind, matrix: DMat, samples: Vector[Int]): Either[FitError, Unit] =
        val descriptor = unit.products.find(_.kind == kind).get
        var failure: Option[FitError] = None
        var row = 0
        while row < outputs.size && failure.isEmpty do
          val selection = EstimateSelection(Vector(unit.observations.head.id), Vector(outputs(row)), samples)
          val values = Array.tabulate(samples.size)(col => matrix(row, col))
          val validity = Array.fill[Byte](samples.size)(Validity.Valid.code)
          sink.write(descriptor.id, selection, values, validity) match
            case Left(error) => sinkError = Some(error); failure = Some(FitError.IncompatibleFitBlocks(error.message))
            case Right(_) => ()
          row += 1
        failure.toLeft(())
      def residual(variance: DVec, samples: Vector[Int]): Either[FitError, Unit] =
        val product = unit.products.find(_.kind == ProductKind.ResidualVariance).get
        outputs.foldLeft[Either[FitError, Unit]](Right(())): (previous, output) =>
          previous.flatMap: _ =>
            sink.write(product.id, EstimateSelection(Vector(unit.observations.head.id), Vector(output), samples),
              variance.toSeq.toArray, Array.fill[Byte](samples.size)(Validity.Valid.code)) match
              case Left(error) => sinkError = Some(error); Left(FitError.IncompatibleFitBlocks(error.message))
              case Right(_) => Right(())
      def joint(matrix: DMat, samples: Vector[Int]): Either[FitError, Unit] =
        val product = unit.products.find(_.kind == ProductKind.Covariance).get
        var failure: Option[FitError] = None
        var i = 0
        while i < outputs.size && failure.isEmpty do
          var j = i
          while j < outputs.size && failure.isEmpty do
            val selection = CovarianceSelection(Vector(unit.observations.head.id), Vector(EstimandPair(outputs(i), outputs(j))), samples)
            sink.writeCovariance(product.id, selection, Array.fill(samples.size)(matrix(i, j)), Array.fill[Byte](samples.size)(Validity.Valid.code)) match
              case Left(error) => sinkError = Some(error); failure = Some(FitError.IncompatibleFitBlocks(error.message))
              case Right(_) => ()
            j += 1
          i += 1
        failure.toLeft(())
      try
        prepared.foreachBlock(reader, block =>
          deliver(ProductKind.Effect, block.result.estimates, block.voxelIndices).flatMap: _ =>
            block.result.uncertainty match
              case OlsEstimateUncertainty.NotRequested => Right(())
              case OlsEstimateUncertainty.Marginal(se, variance, _) =>
                deliver(ProductKind.StandardError, se.value, block.voxelIndices).flatMap: _ =>
                  residual(variance, block.voxelIndices)
              case OlsEstimateUncertainty.Joint(covariance, se, variance, _) =>
                deliver(ProductKind.StandardError, se.value, block.voxelIndices)
                  .flatMap(_ => residual(variance, block.voxelIndices))
                  .flatMap(_ => joint(covariance, block.voxelIndices)),
          cancelled) match
          case Left(error) => sink.abort(); Left(sinkError.getOrElse(EstimateError.Invalid(error.message)))
          case Right(EstimateExecutionOutcome.Cancelled(_, _)) => sink.abort(); Left(EstimateError.Cancelled)
          case Right(EstimateExecutionOutcome.Completed(_, _)) =>
            sink.seal() match
              case Left(error) => sink.abort(); Left(error)
              case success => success
      catch case NonFatal(error) =>
        sink.abort()
        Left(EstimateError.Io(Option(error.getMessage).getOrElse(error.getClass.getName)))

object FitEstimateProducer:
  /** Shared full-rank OLS effects with optional marginal or joint
    * uncertainty. Unsupported requested products are refused before any reads;
    * they are never silently omitted from publication.
    */
  def shared(
      plan: FirstLevelEstimatePlan,
      identity: EstimatePublicationIdentity,
      catalog: EstimandCatalog,
      outputs: Vector[EstimandId],
      worldFrame: String
  ): Either[EstimateError, FitEstimateProducer] =
    if !plan.diagnostics.fullRank then Left(EstimateError.Unsupported("this producer requires full-rank native QR evidence"))
    else if identity.acquisitions.size != plan.fitPlan.model.dataset.samplingFrame.blockLens.size then
      Left(EstimateError.Invalid("acquisition identities must follow the dataset run axis"))
    else if outputs.size != plan.outputs.size || outputs.distinct.size != outputs.size || outputs.exists(catalog.entry(_).isEmpty) then
      Left(EstimateError.Invalid("catalog output mapping must uniquely cover the native selected output axis"))
    else
      val samples = plan.chunks.iterator.flatMap(_.voxelIndices).toVector
      EstimateDomain.make(plan.fitPlan.model.dataset.shape.space, samples, worldFrame).flatMap: domain =>
        try
          val lengths = plan.fitPlan.model.dataset.samplingFrame.blockLens
          val starts = lengths.scanLeft(0)(_ + _).dropRight(1)
          val scans = identity.acquisitions.indices.toVector.flatMap: run =>
            val selected = plan.timepoints.filter(i => i >= starts(run) && i < starts(run) + lengths(run)).map(_ - starts(run))
            if selected.isEmpty then Vector.empty else Vector(AcquisitionScans(identity.acquisitions(run), selected))
          val observations = Vector(Observation(identity.observation, ParticipantId(identity.dataset, identity.participantLabel), scans.map(_.acquisition)))
          val units = outputs.map(id => catalog.entry(id).get.units).distinct
          if units.size != 1 then Left(EstimateError.Unsupported("this producer requires one common physical unit per scalar product"))
          else
            val hasUncertainty = plan.request.uncertainty != EstimateUncertaintyRequest.None
            val kinds = Vector(ProductKind.Effect) ++ (if hasUncertainty then Vector(ProductKind.StandardError, ProductKind.ResidualVariance) else Vector.empty) ++
              (if plan.request.uncertainty == EstimateUncertaintyRequest.Joint then Vector(ProductKind.Covariance) else Vector.empty)
            val products = kinds.map: kind =>
              val suffix = kind match
                case ProductKind.Effect => "effect"
                case ProductKind.StandardError => "standard-error"
                case ProductKind.Covariance => "normalized-covariance"
                case _ => "residual-variance"
              ProductDescriptor(ProductId(s"${identity.revision.value}/$suffix"), kind, NumericPrecision.Float64,
                Vector(identity.observation), (if kind == ProductKind.Covariance then ProductTargets.UpperTriangle(outputs) else ProductTargets.Scalar(outputs)),
                (if scans.size == 1 then PoolingScope.Run else PoolingScope.JointRuns),
                if kind == ProductKind.ResidualVariance then s"(${identity.responseUnits})^2"
                else if kind == ProductKind.Covariance then s"(${units.head})^2 / (${identity.responseUnits})^2"
                else units.head)
            val columns = plan.coefficientAxis.columnIds.map(id => scalafim.estimates.ColumnId(id.value))
            val bindings = outputs.zipWithIndex.map: (id, row) =>
              EstimandBinding(id, columns, 1, Vector.tabulate(columns.size)(col => plan.readout.weights(row, col)))
            val provenance = EstimateProvenance("ScalaFIM", identity.producerVersion, identity.executionId,
              ScientificFact.Known("shared ordinary least squares; compiled rank-revealing QR selected readout"),
              ScientificFact.Known("independent homoscedastic errors under the fitted model"),
              ScientificFact.Known("nuisance columns retained in the native coefficient binding"),
              ScientificFact.Known("one shared fit over the declared selected rows"), scans, identity.inputs)
            val unit = EstimateUnit(identity.dataset, identity.unit, identity.revision, catalog, domain, observations, bindings,
              products, products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
              EstimabilityEvidence.FullRank(columns, plan.timepoints.size, plan.diagnostics.rankReport.tolerance,
                s"${plan.diagnostics.solveMethod}; ${plan.diagnostics.rankReport.toleranceConvention}"), provenance,
              covariance = products.find(_.kind == ProductKind.Covariance).toVector.map(c =>
                CovarianceDescriptor(c.id, products.find(_.kind == ProductKind.Effect).get.id,
                  CovarianceEquation.Normalized(products.find(_.kind == ProductKind.ResidualVariance).get.id), true, true)),
              degreesOfFreedom = Vector(DegreesOfFreedom(DfRole.Residual,
                DfValue.Scalar((plan.timepoints.size - plan.diagnostics.rank).toDouble), "OLS n - numerical rank", false)))
            Right(new FitEstimateProducer(unit, plan, outputs))
        catch case NonFatal(error) => Left(EstimateError.Invalid(error.getMessage))
