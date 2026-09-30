package scalafim.fmri.fit.estimates

import gale.backend.Backend
import gale.linalg.DMat
import scalafim.dataset.DatasetSeriesReader
import scalafim.estimates.*
import scalafim.fmri.fit.*
import scalafim.fmri.hrf.ResponseUnits
import scala.util.control.NonFatal

/** Primary outputs from one native multivariate precision-pooling execution.
  * The sink belongs to this operation; the reader and backend belong to callers.
  */
final class PooledFitEstimateProducer private (
    val unit: EstimateUnit,
    val prepared: FirstLevelFixedEffectsEstimatePlan,
    outputs: Vector[EstimandId]
):
  def write(reader: DatasetSeriesReader, sink: EstimateSink, cancelled: () => Boolean = () => false)(using Backend): Either[EstimateError, PinnedUnit] =
    var sinkError: Option[EstimateError] = None
    def fail(error: EstimateError): Either[EstimateError, PinnedUnit] =
      sink.abort()
      Left(error)
    def delivery(action: => Either[EstimateError, Unit]): Either[FitError, Unit] =
      val result = if cancelled() then Left(EstimateError.Cancelled) else action
      result.left.map: error =>
        sinkError = Some(error)
        FitError.IncompatibleFitBlocks(error.message)
    def scalar(kind: ProductKind, matrix: Option[DMat], positions: Map[Int, Int],
        samples: Vector[Int], codes: Array[Byte]): Either[FitError, Unit] =
      val product = unit.products.find(_.kind == kind).get
      var failure: Option[FitError] = None
      var row = 0
      while row < outputs.size && failure.isEmpty do
        val values = Array.tabulate(samples.size)(i => positions.get(samples(i)).fold(0.0)(p => matrix.get(row, p)))
        delivery(sink.write(product.id, EstimateSelection(Vector(unit.observations.head.id), Vector(outputs(row)), samples), values, codes)) match
          case Left(error) => failure = Some(error)
          case Right(_) => ()
        row += 1
      failure.toLeft(())
    def consume(block: FirstLevelFixedEffectsEstimateBlock): Either[FitError, Unit] =
      val (selected, exclusions) = block.result match
        case FixedEffectsEstimateBlockResult.Selected(result, excluded) => (Some(result), excluded)
        case FixedEffectsEstimateBlockResult.Excluded(excluded) => (None, excluded)
      val kept = selected.toVector.flatMap(_.voxelIndices)
      val covered = kept ++ exclusions.map(_.voxelIndex)
      if covered.distinct.size != covered.size || covered.toSet != block.inputVoxelIndices.toSet then
        return Left(FitError.IncompatibleFitBlocks("pooled block does not cover its exact input sample axis"))
      val positions = kept.zipWithIndex.toMap
      val excluded = exclusions.map(e => e.voxelIndex -> PooledFitEstimateProducer.validity(e.status).code).toMap
      val codes = block.inputVoxelIndices.map(i => excluded.getOrElse(i, Validity.Valid.code)).toArray
      val effects = selected.map(_.estimates)
      val se = selected.flatMap: result =>
        result.uncertainty match
          case FixedEffectsEstimateUncertainty.NotRequested => None
          case FixedEffectsEstimateUncertainty.Marginal(errors) => Some(errors.value)
          case FixedEffectsEstimateUncertainty.Joint(errors, _) => Some(errors.value)
      scalar(ProductKind.Effect, effects, positions, block.inputVoxelIndices, codes).flatMap: _ =>
        val marginal = if prepared.selection.request.uncertainty == EstimateUncertaintyRequest.None then Right(())
          else scalar(ProductKind.StandardError, se, positions, block.inputVoxelIndices, codes)
        marginal.flatMap: _ =>
          if prepared.selection.request.uncertainty != EstimateUncertaintyRequest.Joint then Right(())
          else
            // Access only this block's matrices, aligned to retained voxel positions.
            val matrices = selected match
              case None => Right(Vector.empty[DMat])
              case Some(result) => result.uncertainty match
                case FixedEffectsEstimateUncertainty.Joint(_, covariance) =>
                  result.voxelIndices.indices.foldLeft[Either[FitError, Vector[DMat]]](Right(Vector.empty))((prior, p) =>
                    prior.flatMap(values => covariance.matrixForVoxelPosition(p).map(values :+ _)))
                case _ => Left(FitError.IncompatibleFitBlocks("native pooled joint covariance was not returned"))
            matrices.flatMap: values =>
              val product = unit.products.find(_.kind == ProductKind.Covariance).get
              var failure: Option[FitError] = None
              var i = 0
              while i < outputs.size && failure.isEmpty do
                var j = i
                while j < outputs.size && failure.isEmpty do
                  val cells = Array.tabulate(block.inputVoxelIndices.size)(k => positions.get(block.inputVoxelIndices(k)).fold(0.0)(p => values(p)(i, j)))
                  delivery(sink.writeCovariance(product.id, CovarianceSelection(Vector(unit.observations.head.id),
                    Vector(EstimandPair(outputs(i), outputs(j))), block.inputVoxelIndices), cells, codes)) match
                    case Left(error) => failure = Some(error)
                    case Right(_) => ()
                  j += 1
                i += 1
              failure.toLeft(())
    try
      val compact = sink match
        case capability: SharedCovarianceSink => unit.covariance.exists(c => capability.sharedCovarianceProducts.contains(c.product))
        case _ => false
      if sink.unit != unit then fail(EstimateError.Invalid("sink declaration differs from the compiled pooled scientific output"))
      else if prepared.selection.request.uncertainty == EstimateUncertaintyRequest.Joint && (!sink.supportsCovariance || compact) then
        fail(EstimateError.Unsupported("pooled absolute samplewise joint uncertainty requires a full pair-axis sink"))
      else if prepared.maxBlockVoxels > sink.maximumBlockCells then
        fail(EstimateError.Invalid("sink block budget is smaller than the prepared voxel block"))
      else prepared.foreachBlock(reader, consume, cancelled) match
        case Left(error) => fail(sinkError.getOrElse(EstimateError.Invalid(error.message)))
        case Right(EstimateExecutionOutcome.Cancelled(_, _)) => fail(EstimateError.Cancelled)
        case Right(EstimateExecutionOutcome.Completed(_, _)) =>
          if cancelled() then fail(EstimateError.Cancelled)
          else sink.seal() match
            case Left(error) => fail(error)
            case success => success
    catch case NonFatal(error) =>
      // Even an abort callback failure must remain a typed failure.
      try sink.abort() catch case NonFatal(_) => ()
      Left(EstimateError.Io(Option(error.getMessage).getOrElse(error.getClass.getName)))

object PooledFitEstimateProducer:
  val UnknownPooledDfReason = "variance estimated from run residual variances; effective degrees of freedom for estimated inverse-covariance pooling are unavailable"

  /** Catalog normalization is an exact native-coordinate receipt, independent
    * of labels. Operators remain numeric bindings; no coefficients are rescaled.
    */
  def normalization(prepared: FirstLevelFixedEffectsEstimatePlan, row: Int): String =
    val coordinates = prepared.pooledCoefficientAxis.columns.map(c => s"${c.id.value}=${c.hrfScale.canonical}").mkString(";")
    prepared.selection.outputs(row) match
      case EstimateOutputMetadata.Coefficient(column) => s"native coefficient; ${column.origin.canonical}; ${column.hrfScale.canonical}"
      case EstimateOutputMetadata.Contrast(hypothesis) =>
        s"native linear readout; hypothesis=${hypothesis.id.value}; $coordinates; response-functionals=${hypothesis.responseFunctionals.mkString(";")}"

  private[estimates] def validity(status: VoxelFitStatus): Validity = status match
    case VoxelFitStatus.NoObservedResponses => Validity.MissingInput
    case VoxelFitStatus.NonFinite => Validity.NumericalFailure
    case VoxelFitStatus.Estimable => Validity.Valid
    case VoxelFitStatus.AllZero | VoxelFitStatus.Constant | VoxelFitStatus.InsufficientResidualDegreesOfFreedom |
        VoxelFitStatus.RankDeficientObservedDesign | VoxelFitStatus.ZeroResidualVariance => Validity.NonEstimable

  def make(
      prepared: FirstLevelFixedEffectsEstimatePlan,
      identity: EstimatePublicationIdentity,
      catalog: EstimandCatalog,
      outputs: Vector[EstimandId],
      worldFrame: String
  ): Either[EstimateError, PooledFitEstimateProducer] =
    val runs = prepared.runPreparations
    if prepared.selection.request.retainRunCoefficients.nonEmpty then
      Left(EstimateError.Unsupported("primary pooled persistence cannot retain run coefficients; a separate one-pass run publisher is required"))
    else if identity.responseUnits.trim.isEmpty then
      Left(EstimateError.Invalid("physical response units are required"))
    else if runs.isEmpty || runs.exists(r => !r.diagnostics.fullRank) then
      Left(EstimateError.Unsupported("pooled persistence requires every selected run's native full-rank QR admission"))
    else if identity.acquisitions.size != prepared.fitPlan.model.dataset.samplingFrame.blockLens.size ||
        identity.acquisitions.distinct.size != identity.acquisitions.size then
      Left(EstimateError.Invalid("distinct acquisition identities must follow the complete dataset run axis"))
    else if outputs.size != prepared.selection.outputs.size || outputs.distinct.size != outputs.size || outputs.exists(catalog.entry(_).isEmpty) then
      Left(EstimateError.Invalid("catalog output mapping must uniquely cover the native selected output axis"))
    else
      try
        val entries = outputs.map(id => catalog.entry(id).get)
        val kindsMatch = entries.indices.forall: row =>
          val expected = prepared.selection.outputs(row) match
            case EstimateOutputMetadata.Coefficient(_) => EstimandKind.Coefficient
            case EstimateOutputMetadata.Contrast(hypothesis) =>
              if hypothesis.responseFunctionals.isEmpty then EstimandKind.LinearContrast else EstimandKind.BasisReadout
          entries(row).kind == expected && entries(row).normalization == normalization(prepared, row) &&
            entries(row).unitScope.forall(_ == identity.unit)
        val unitsMatch = prepared.selection.outputs.zip(entries).forall:
          case (EstimateOutputMetadata.Contrast(hypothesis), entry) =>
            hypothesis.responseFunctionals.forall(receipt => entry.units == (receipt.units match
              case ResponseUnits.ResponseValue => identity.responseUnits
              case ResponseUnits.ResponseIntegral => s"(${identity.responseUnits}) * seconds"))
          case _ => true
        if !kindsMatch then Left(EstimateError.Invalid("catalog kind, normalization or unit scope disagrees with the native output interpretation"))
        else if entries.map(_.units).distinct.size != 1 || !unitsMatch then
          Left(EstimateError.Unsupported("pooled scalar products require one common physical unit consistent with native response functionals"))
        else prepared.selection.alignTo(prepared.pooledCoefficientAxis).left.map(e => EstimateError.Invalid(e.message)).flatMap: readout =>
          val samples = prepared.chunks.iterator.flatMap(_.voxelIndices).toVector
          EstimateDomain.make(prepared.fitPlan.model.dataset.shape.space, samples, worldFrame).map: domain =>
            val lengths = prepared.fitPlan.model.dataset.samplingFrame.blockLens
            val starts = lengths.scanLeft(0)(_ + _).dropRight(1)
            val scans = runs.map(r => AcquisitionScans(identity.acquisitions(r.partition.runIndex),
              r.partition.timepoints.map(_ - starts(r.partition.runIndex))))
            val observation = Observation(identity.observation, ParticipantId(identity.dataset, identity.participantLabel), scans.map(_.acquisition))
            val uncertainty = prepared.selection.request.uncertainty
            val kinds = Vector(ProductKind.Effect) ++ (if uncertainty == EstimateUncertaintyRequest.None then Vector.empty else Vector(ProductKind.StandardError)) ++
              (if uncertainty == EstimateUncertaintyRequest.Joint then Vector(ProductKind.Covariance) else Vector.empty)
            val products = kinds.map: kind =>
              val suffix = kind match
                case ProductKind.Effect => "pooled-effect"
                case ProductKind.StandardError => "pooled-standard-error"
                case _ => "pooled-absolute-covariance"
              ProductDescriptor(ProductId(s"${identity.revision.value}/$suffix"), kind, NumericPrecision.Float64,
                Vector(identity.observation), if kind == ProductKind.Covariance then ProductTargets.UpperTriangle(outputs) else ProductTargets.Scalar(outputs),
                PoolingScope.PooledRuns, if kind == ProductKind.Covariance then s"(${entries.head.units})^2" else entries.head.units)
            val columns = prepared.pooledCoefficientAxis.columnIds.map(c => scalafim.estimates.ColumnId(c.value))
            val bindings = outputs.indices.toVector.map(row => EstimandBinding(outputs(row), columns, 1,
              Vector.tabulate(columns.size)(col => readout.weights(row, col))))
            val rankMethod = runs.map: run =>
              val d = run.diagnostics
              s"run=${run.partition.runIndex};n=${run.partition.timepoints.size};rank=${d.rank}/${d.predictors};df=${run.residualDegreesOfFreedom.value};method=${d.solveMethod};tolerance=${d.rankReport.tolerance};convention=${d.rankReport.toleranceConvention}"
            val method = s"all selected run designs admitted full rank; common pooled axis has positive summed precision; reporting tolerance is maximum of native run thresholds; ${rankMethod.mkString(" | ")}"
            val provenance = EstimateProvenance("ScalaFIM", identity.producerVersion, identity.executionId,
              ScientificFact.Known("native multivariate inverse-covariance pooling followed by the exact selected readout"),
              ScientificFact.Known("independent runs; within-run homoscedastic OLS; precision estimated from positive run residual variances"),
              ScientificFact.Known("run-local nuisance columns enter each native fit before common-axis precision pooling"),
              ScientificFact.Known(s"selected zero-based run indices=${runs.map(_.partition.runIndex).mkString(",")}; policy=${prepared.policy}; $method"), scans, identity.inputs)
            val df = DegreesOfFreedom(DfRole.Residual, DfValue.Scalar(runs.map(_.residualDegreesOfFreedom.value.toLong).sum.toDouble),
              "nominal residual accounting: sum of selected run residual df; descriptive only, not calibrated pooled effective or reference df", false)
            val effect = products.head
            val unit = EstimateUnit(identity.dataset, identity.unit, identity.revision, catalog, domain, Vector(observation), bindings, products,
              products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
              EstimabilityEvidence.FullRank(columns, prepared.timepoints.size, runs.map(_.diagnostics.rankReport.tolerance).max, method), provenance,
              covariance = products.filter(_.kind == ProductKind.Covariance).map(p => CovarianceDescriptor(p.id, effect.id, CovarianceEquation.Absolute, true, false)),
              degreesOfFreedom = Vector(df),
              marginalUncertainty = products.filter(_.kind == ProductKind.StandardError).map(p =>
                MarginalUncertaintyDescriptor(p.id, effect.id, MarginalVarianceOrigin.Unknown(UnknownPooledDfReason))))
            new PooledFitEstimateProducer(unit, prepared, outputs)
      catch case NonFatal(error) => Left(EstimateError.Invalid(Option(error.getMessage).getOrElse(error.getClass.getName)))
