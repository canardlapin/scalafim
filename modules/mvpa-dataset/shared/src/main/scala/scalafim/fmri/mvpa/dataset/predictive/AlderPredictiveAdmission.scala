package scalafim.fmri.mvpa.dataset.predictive

import alder.data.{CompleteResampler, FixedCoverage, FixedHoldout, FixedTrainValidationTest, FixedValidation, IdentifiedRows, Resample4sResampler}
import alder.kernel.{DataFingerprint, Example, FingerprintPolicy}
import gale.linalg.DMat
import multivar.core.SemanticSpace
import resample4s.core.{DigestAlgorithm, Labels, PlanReceipt}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, CrossFitDesign, EvidenceError, MultiResponse, Observations}

final case class NativeAxisEntry private[predictive] (stableKey: String, nativeId: Long)

/** Immutable declared identity for a provider root. Construction checks every
  * ordinal; callers cannot forge a revised map with `copy`.
  */
final class NativeAxisMapping private (
    val axis: AxisDescriptor,
    val entriesByOrdinal: Vector[NativeAxisEntry],
    val declaredSource: DataFingerprint
):
  def nativeIds: Vector[Long] = entriesByOrdinal.map(_.nativeId)

  private[predictive] val declaredMappingIdentity: DataFingerprint =
    new DataFingerprint(FingerprintPolicy.Summary("scalafim.native-declared-map.v1"), AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.native-declared-map.v1")
      writer.string(axis.coordinateSignature.value)
      writer.string(declaredSource.policy.toString)
      writer.string(declaredSource.digest)
      writer.intLE(entriesByOrdinal.length)
      entriesByOrdinal.foreach: entry =>
        writer.string(entry.stableKey)
        writer.string(entry.nativeId.toString)
    )

object NativeAxisMapping:
  def fromAxis[K](axis: AxisRef[K], nativeIdsByOrdinal: Vector[Long], declaredSource: DataFingerprint): Either[AlderPredictiveAdmissionError, NativeAxisMapping] =
    if nativeIdsByOrdinal.length != axis.size then Left(AlderPredictiveAdmissionError.NativeIdCountMismatch(axis.size, nativeIdsByOrdinal.length))
    else
      val seen = scala.collection.mutable.HashSet.empty[Long]
      val entries = Vector.newBuilder[NativeAxisEntry]
      var ordinal = 0
      var failure: Option[AlderPredictiveAdmissionError] = None
      while ordinal < axis.size && failure.isEmpty do
        val id = nativeIdsByOrdinal(ordinal)
        if !seen.add(id) then failure = Some(AlderPredictiveAdmissionError.DuplicateNativeId(id))
        else axis.index.stableKeyAt(ordinal) match
          case Left(error) => failure = Some(AlderPredictiveAdmissionError.Axis(error))
          case Right(key)  => entries += NativeAxisEntry(key, id)
        ordinal += 1
      failure.fold[Either[AlderPredictiveAdmissionError, NativeAxisMapping]](Right(new NativeAxisMapping(axis.descriptor, entries.result(), declaredSource)))(Left.apply)

  def verify(axis: AxisDescriptor, mapping: NativeAxisMapping, source: DataFingerprint): Either[AlderPredictiveAdmissionError, Unit] =
    if axis != mapping.axis then Left(AlderPredictiveAdmissionError.AxisFingerprintMismatch(axis.stableKey, mapping.axis.stableKey))
    else if source.policy != mapping.declaredSource.policy || source.digest != mapping.declaredSource.digest then Left(AlderPredictiveAdmissionError.DeclaredSourceMismatch)
    else if mapping.entriesByOrdinal.length != axis.size then Left(AlderPredictiveAdmissionError.NativeIdCountMismatch(axis.size, mapping.entriesByOrdinal.length))
    else Right(())

final case class MaterializationReceipt(rows: Int, inputs: Int, targets: Int, copiedCells: Long, workspaceCells: Long)
final class MaterializationBudget private (val maximumCells: Long)
object MaterializationBudget:
  def apply(maximumCells: Long): Either[AlderPredictiveAdmissionError, MaterializationBudget] =
    if maximumCells <= 0L then Left(AlderPredictiveAdmissionError.InvalidBudget) else Right(new MaterializationBudget(maximumCells))

  /** All adapter allocations: one input and one target array for every row.
    * Inputs are already materialized by the caller; this boundary never asks a
    * semantic table to build a feature-by-feature identity workspace.
    */
  def authorize(budget: MaterializationBudget, rows: Int, inputs: Int, targets: Int): Either[AlderPredictiveAdmissionError, MaterializationReceipt] =
    if rows <= 0 || inputs <= 0 || targets <= 0 then Left(AlderPredictiveAdmissionError.InvalidShape(rows, inputs, targets))
    else
      val sourceCells = rows.toLong * (inputs.toLong + targets.toLong)
      val copied = sourceCells
      if sourceCells < 0L || copied < 0L || copied > budget.maximumCells then Left(AlderPredictiveAdmissionError.MaterializationOverBudget(sourceCells, copied, budget.maximumCells))
      else Right(MaterializationReceipt(rows, inputs, targets, copied, 0L))

enum AlderPredictiveAdmissionError:
  case Axis(error: EvidenceError)
  case AxisFingerprintMismatch(expected: String, actual: String)
  case NativeIdCountMismatch(expected: Int, actual: Int)
  case DuplicateNativeId(id: Long)
  case DeclaredSourceMismatch
  case InvalidBudget
  case InvalidShape(rows: Int, inputs: Int, targets: Int)
  case MaterializationOverBudget(requiredCells: Long, copiedCells: Long, maximumCells: Long)
  case MatrixShapeMismatch(name: String, rows: Int, columns: Int, expectedRows: Int, expectedColumns: Int)
  case OperatorTableMaterializationUnavailable
  case ProviderFixedSelection(detail: String)
  case CrossFitPopulationMismatch
  case CrossFitOrderMismatch(fold: Int, role: String)
  case CrossFitAssignment(detail: String)
  case MatrixNativeRidgeUnavailable

final class AlderMaterializedRows[M] private[predictive] (
    val root: IdentifiedRows[Example[Array[Double], Array[Double], M]],
    val mapping: NativeAxisMapping,
    val receipt: MaterializationReceipt
):
  def fixedValidation(train: Vector[Long], validation: Vector[Long], coverage: FixedCoverage): Either[AlderPredictiveAdmissionError, FixedValidation[Example[Array[Double], Array[Double], M]]] = root.fixedValidation(train, validation, coverage).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))
  def fixedHoldout(train: Vector[Long], test: Vector[Long], coverage: FixedCoverage): Either[AlderPredictiveAdmissionError, FixedHoldout[Example[Array[Double], Array[Double], M]]] = root.fixedHoldout(train, test, coverage).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))
  def fixedTrainValidationTest(train: Vector[Long], validation: Vector[Long], test: Vector[Long], coverage: FixedCoverage): Either[AlderPredictiveAdmissionError, FixedTrainValidationTest[Example[Array[Double], Array[Double], M]]] = root.fixedTrainValidationTest(train, validation, test, coverage).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))

final case class NativeCrossFitBridge[A](resampler: CompleteResampler[A], nativeReceipt: PlanReceipt)

object AlderPredictiveAdmission:
  /** Binds caller-owned materialized matrices. A direct Observations adapter is
    * deliberately unavailable: its only public conversion currently requires
    * DMat.eye(nFeatures), which is an unbudgeted quadratic workspace.
    */
  def materialized[M](axis: AxisDescriptor, inputs: DMat, targets: DMat, metadata: Vector[M], mapping: NativeAxisMapping, budget: MaterializationBudget): Either[AlderPredictiveAdmissionError, AlderMaterializedRows[M]] =
    for
      _ <- NativeAxisMapping.verify(axis, mapping, mapping.declaredSource)
      _ <- if inputs.rows == axis.size && inputs.cols > 0 then Right(()) else Left(AlderPredictiveAdmissionError.MatrixShapeMismatch("inputs", inputs.rows, inputs.cols, axis.size, -1))
      _ <- if targets.rows == axis.size && targets.cols > 0 then Right(()) else Left(AlderPredictiveAdmissionError.MatrixShapeMismatch("targets", targets.rows, targets.cols, axis.size, -1))
      _ <- if metadata.length == axis.size then Right(()) else Left(AlderPredictiveAdmissionError.MatrixShapeMismatch("metadata", metadata.length, 1, axis.size, 1))
      receipt <- MaterializationBudget.authorize(budget, axis.size, inputs.cols, targets.cols)
      rows = Vector.tabulate(axis.size): row =>
        mapping.entriesByOrdinal(row).nativeId -> Example(
          Array.tabulate(inputs.cols)(column => inputs(row, column)),
          Array.tabulate(targets.cols)(column => targets(row, column)), metadata(row)
        )
      root <- IdentifiedRows.fromRows(rows, mapping.declaredMappingIdentity).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))
    yield new AlderMaterializedRows(root, mapping, receipt)

  def directTablesUnavailable[S <: SemanticSpace, N <: SemanticSpace, F <: SemanticSpace](observations: Observations[S, N], targets: MultiResponse[S, F]): Either[AlderPredictiveAdmissionError, Nothing] =
    val _ = observations
    val _ = targets
    Left(AlderPredictiveAdmissionError.OperatorTableMaterializationUnavailable)

  def crossFit[S <: SemanticSpace, K, M](rows: AlderMaterializedRows[M], design: CrossFitDesign[S, K])(using DigestAlgorithm): Either[AlderPredictiveAdmissionError, NativeCrossFitBridge[Example[Array[Double], Array[Double], M]]] =
    if rows.mapping.axis != design.samples.descriptor || rows.root.ids != rows.mapping.nativeIds then Left(AlderPredictiveAdmissionError.CrossFitPopulationMismatch)
    else
      val assignments = Array.fill(design.samples.size)(-1)
      var fold = 0
      var failure: Option[AlderPredictiveAdmissionError] = None
      while fold < design.keys.length && failure.isEmpty do
        design.at(design.keys(fold)) match
          case Left(error) => failure = Some(AlderPredictiveAdmissionError.Axis(error))
          case Right(unit) =>
            val assessed = unit.assessment.ordinals.toVector
            val expectedAssessment = assessed.sorted
            val expectedAnalysis = (0 until assignments.length).filterNot(assessed.toSet).toVector
            if assessed != expectedAssessment then failure = Some(AlderPredictiveAdmissionError.CrossFitOrderMismatch(fold, "assessment"))
            else if unit.analysis.ordinals.toVector != expectedAnalysis then failure = Some(AlderPredictiveAdmissionError.CrossFitOrderMismatch(fold, "analysis"))
            else
              assessed.foreach: ordinal =>
                if ordinal < 0 || ordinal >= assignments.length || assignments(ordinal) >= 0 then failure = Some(AlderPredictiveAdmissionError.CrossFitAssignment("assessment ordinals are not an exact partition"))
                else assignments(ordinal) = fold
        fold += 1
      failure match
        case Some(error) => Left(error)
        case None if assignments.contains(-1) => Left(AlderPredictiveAdmissionError.CrossFitAssignment("assessment ordinals omit a native sample"))
        case None =>
          for
            labels <- Labels.retained(IArray.unsafeFromArray(assignments)).left.map(error => AlderPredictiveAdmissionError.CrossFitAssignment(error.message))
            fixed <- FixedPartitions.once(labels).left.map(error => AlderPredictiveAdmissionError.CrossFitAssignment(error.message))
            training <- rows.root.training(rows.mapping.nativeIds).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))
            resampler <- Resample4sResampler.fromDesignForPopulation[Example[Array[Double], Array[Double], M]](fixed, training.fingerprint, rows.mapping.nativeIds).left.map(error => AlderPredictiveAdmissionError.CrossFitAssignment(error.toString))
          yield NativeCrossFitBridge(resampler, design.receipt)

  def rejectMatrixNativeRidge: Either[AlderPredictiveAdmissionError, Nothing] = Left(AlderPredictiveAdmissionError.MatrixNativeRidgeUnavailable)
