package scalafim.fmri.mvpa.dataset.predictive

import alder.data.{CompleteResampler, FixedCoverage, FixedHoldout, FixedTrainValidationTest, FixedValidation, IdentifiedRows, Resample4sResampler}
import alder.kernel.{DataFingerprint, Example, FingerprintPolicy}
import gale.linalg.DMat
import multivar.core.{SemanticSpace, ValueIdentity}
import resample4s.core.{DigestAlgorithm, Labels, PlanReceipt}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef, CrossFitDesign, EvidenceError, MultiResponse, Observations}
import scalafim.response.ProvenanceOperation
import scala.util.control.NonFatal

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

  private[predictive] def authorizeNative(budget: MaterializationBudget, rows: Int, inputs: Int, targets: Int, batchWidth: Int): Either[AlderPredictiveAdmissionError, MaterializationReceipt] =
    if rows <= 0 || inputs <= 0 || targets <= 0 || batchWidth <= 0 then Left(AlderPredictiveAdmissionError.InvalidShape(rows, inputs, targets))
    else
      val retained = rows.toLong * (inputs.toLong + targets.toLong)
      val widestColumns = math.max(inputs.toLong, targets.toLong)
      val widestBlock = math.min(batchWidth.toLong, widestColumns)
      // This is an adapter-visible authorization bound: basis, input/output
      // vector allowances, Gale builder, and result snapshot allowance.
      val workspaceFactor = widestColumns * 2L + rows.toLong * 3L
      val workspaceOverflow = widestBlock > 0L && workspaceFactor > Long.MaxValue / widestBlock
      val workspace =
        if workspaceOverflow then Long.MaxValue
        else workspaceFactor * widestBlock
      val totalOverflow = retained < 0L || workspaceOverflow || workspace < 0L || retained > Long.MaxValue - workspace
      val required =
        if totalOverflow then Long.MaxValue
        else retained + workspace
      if totalOverflow || required > budget.maximumCells || widestBlock > 0L && (widestColumns > Int.MaxValue.toLong / widestBlock || rows.toLong > Int.MaxValue.toLong / widestBlock) then
        Left(AlderPredictiveAdmissionError.MaterializationOverBudget(required, retained, budget.maximumCells))
      else Right(MaterializationReceipt(rows, inputs, targets, retained, workspace))

sealed trait NativeReadAccess
object NativeReadAccess:
  case object SingleApplication extends NativeReadAccess

  final class OwnedReplay private (val owner: String) extends NativeReadAccess
  object OwnedReplay:
    def apply(owner: String): Either[AlderPredictiveAdmissionError, OwnedReplay] =
      if owner.trim.isEmpty then Left(AlderPredictiveAdmissionError.InvalidReplayOwner)
      else Right(new OwnedReplay(owner))

/** Explicit policy for reading native semantic tables. The budget accounts for
  * retained input and target values plus an adapter-visible read workspace.
  */
final class NativeReadPolicy private (val maximumColumnsPerRead: Int, val budget: MaterializationBudget, val access: NativeReadAccess)
object NativeReadPolicy:
  def apply(maximumColumnsPerRead: Int, budget: MaterializationBudget, access: NativeReadAccess = NativeReadAccess.SingleApplication): Either[AlderPredictiveAdmissionError, NativeReadPolicy] =
    if maximumColumnsPerRead <= 0 then Left(AlderPredictiveAdmissionError.InvalidReadWidth(maximumColumnsPerRead))
    else Right(new NativeReadPolicy(maximumColumnsPerRead, budget, access))

/** Receipt for a concrete table-read attempt. Returned cells reached the
  * adapter; copied cells reached retained arrays. `maximumWorkspaceCells` is
  * the authorized adapter-visible bound, not an observed process peak.
  */
final case class NativeReadReceipt(
    materialization: MaterializationReceipt,
    inputBlockCalls: Int,
    targetBlockCalls: Int,
    inputReturnedCells: Long,
    targetReturnedCells: Long,
    inputCopiedCells: Long,
    targetCopiedCells: Long,
    maximumWorkspaceCells: Long,
    metadataIdentity: DataFingerprint,
    observationsIdentity: scalafim.fmri.mvpa.EvidenceIdentity,
    targetsIdentity: scalafim.fmri.mvpa.EvidenceIdentity
)

enum AlderPredictiveAdmissionError:
  case Axis(error: EvidenceError)
  case AxisFingerprintMismatch(expected: String, actual: String)
  case NativeIdCountMismatch(expected: Int, actual: Int)
  case DuplicateNativeId(id: Long)
  case DeclaredSourceMismatch
  case InvalidBudget
  case InvalidReadWidth(width: Int)
  case InvalidReplayOwner
  case InvalidShape(rows: Int, inputs: Int, targets: Int)
  case MaterializationOverBudget(requiredCells: Long, copiedCells: Long, maximumCells: Long)
  case MatrixShapeMismatch(name: String, rows: Int, columns: Int, expectedRows: Int, expectedColumns: Int)
  case ReplayRequired(inputBlocks: Int, targetBlocks: Int)
  case NativeReadFailure(stage: String, detail: String, receipt: NativeReadReceipt)
  case ProviderFixedSelection(detail: String)
  case CrossFitPopulationMismatch
  case CrossFitOrderMismatch(fold: Int, role: String)
  case CrossFitAssignment(detail: String)
  case MatrixNativeRidgeUnavailable

final class AlderMaterializedRows[M] private[predictive] (
    val root: IdentifiedRows[Example[Array[Double], Array[Double], M]],
    val mapping: NativeAxisMapping,
    val receipt: MaterializationReceipt,
    val nativeReadReceipt: Option[NativeReadReceipt] = None
):
  def fixedValidation(train: Vector[Long], validation: Vector[Long], coverage: FixedCoverage): Either[AlderPredictiveAdmissionError, FixedValidation[Example[Array[Double], Array[Double], M]]] = root.fixedValidation(train, validation, coverage).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))
  def fixedHoldout(train: Vector[Long], test: Vector[Long], coverage: FixedCoverage): Either[AlderPredictiveAdmissionError, FixedHoldout[Example[Array[Double], Array[Double], M]]] = root.fixedHoldout(train, test, coverage).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))
  def fixedTrainValidationTest(train: Vector[Long], validation: Vector[Long], test: Vector[Long], coverage: FixedCoverage): Either[AlderPredictiveAdmissionError, FixedTrainValidationTest[Example[Array[Double], Array[Double], M]]] = root.fixedTrainValidationTest(train, validation, test, coverage).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))

final case class NativeCrossFitBridge[A](resampler: CompleteResampler[A], nativeReceipt: PlanReceipt)

object AlderPredictiveAdmission:
  /** Binds caller-owned materialized matrices. */
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

  /** Materializes native tables only through an explicitly bounded complete
    * read. Single-application tables require one block; replay is explicit.
    */
  def nativeTables[S <: SemanticSpace, N <: SemanticSpace, F <: SemanticSpace, M](
      observations: Observations[S, N],
      targets: MultiResponse[S, F],
      metadata: Vector[M],
      metadataIdentity: DataFingerprint,
      mapping: NativeAxisMapping,
      policy: NativeReadPolicy
  ): Either[AlderPredictiveAdmissionError, AlderMaterializedRows[M]] =
    for
      _ <- NativeAxisMapping.verify(observations.sampleAxis, mapping, mapping.declaredSource)
      _ <- if observations.sampleAxis == targets.sampleAxis then Right(()) else Left(AlderPredictiveAdmissionError.AxisFingerprintMismatch(observations.sampleAxis.stableKey, targets.sampleAxis.stableKey))
      _ <- if observations.rows == observations.sampleAxis.size && observations.columns > 0 then Right(()) else Left(AlderPredictiveAdmissionError.MatrixShapeMismatch("observations", observations.rows, observations.columns, observations.sampleAxis.size, -1))
      _ <- if targets.rows == observations.sampleAxis.size && targets.columns > 0 then Right(()) else Left(AlderPredictiveAdmissionError.MatrixShapeMismatch("targets", targets.rows, targets.columns, observations.sampleAxis.size, -1))
      _ <- if metadata.length == observations.sampleAxis.size then Right(()) else Left(AlderPredictiveAdmissionError.MatrixShapeMismatch("metadata", metadata.length, 1, observations.sampleAxis.size, 1))
      receipt <- MaterializationBudget.authorizeNative(policy.budget, observations.sampleAxis.size, observations.columns, targets.columns, policy.maximumColumnsPerRead)
      inputBlocks = blocksFor(observations.columns, policy.maximumColumnsPerRead)
      targetBlocks = blocksFor(targets.columns, policy.maximumColumnsPerRead)
      _ <- policy.access match
        case NativeReadAccess.SingleApplication if inputBlocks > 1 || targetBlocks > 1 => Left(AlderPredictiveAdmissionError.ReplayRequired(inputBlocks, targetBlocks))
        case _ => Right(())
      result <- materializeNativeTables(observations, targets, metadata, metadataIdentity, mapping, policy, receipt)
    yield result

  private def materializeNativeTables[S <: SemanticSpace, N <: SemanticSpace, F <: SemanticSpace, M](
      observations: Observations[S, N],
      targets: MultiResponse[S, F],
      metadata: Vector[M],
      metadataIdentity: DataFingerprint,
      mapping: NativeAxisMapping,
      policy: NativeReadPolicy,
      receipt: MaterializationReceipt
  ): Either[AlderPredictiveAdmissionError, AlderMaterializedRows[M]] =
    var inputCalls = 0
    var targetCalls = 0
    var inputReturnedCells = 0L
    var targetReturnedCells = 0L
    var inputCopiedCells = 0L
    var targetCopiedCells = 0L

    def attemptReceipt: NativeReadReceipt =
      NativeReadReceipt(receipt, inputCalls, targetCalls, inputReturnedCells, targetReturnedCells, inputCopiedCells, targetCopiedCells, receipt.workspaceCells, metadataIdentity, observations.identity, targets.identity)

    val retained =
      try
        val inputValues = Array.ofDim[Array[Double]](observations.rows)
        val targetValues = Array.ofDim[Array[Double]](targets.rows)
        var row = 0
        while row < observations.rows do
          inputValues(row) = Array.ofDim[Double](observations.columns)
          targetValues(row) = Array.ofDim[Double](targets.columns)
          row += 1
        Right((inputValues, targetValues))
      catch case NonFatal(error) => Left(AlderPredictiveAdmissionError.NativeReadFailure("retained-allocation", error.toString, attemptReceipt))

    def copyBlocks[R <: SemanticSpace, C <: SemanticSpace](table: multivar.core.Table[R, C], columns: Int, destination: Array[Array[Double]], stage: String): Either[AlderPredictiveAdmissionError, Unit] =
      var start = 0
      while start < columns do
        val width = math.min(policy.maximumColumnsPerRead, columns - start)
        val block =
          try
            val basis = DMat.tabulate(columns, width): (basisRow, basisColumn) =>
              if basisRow == start + basisColumn then 1.0 else 0.0
            if stage == "inputs" then inputCalls += 1 else targetCalls += 1
            table.apply(basis).left.map(error => error.toString)
          catch case NonFatal(error) => Left(error.toString)
        block match
          case Left(detail) => return Left(AlderPredictiveAdmissionError.NativeReadFailure(stage, detail, attemptReceipt))
          case Right(values) =>
            try
              if stage == "inputs" then inputReturnedCells += values.rows.toLong * values.cols.toLong
              else targetReturnedCells += values.rows.toLong * values.cols.toLong
              if values.rows != destination.length || values.cols != width then
                return Left(AlderPredictiveAdmissionError.NativeReadFailure(stage, s"returned ${values.rows}x${values.cols}, expected ${destination.length}x$width", attemptReceipt))
              var outputRow = 0
              while outputRow < values.rows do
                var outputColumn = 0
                while outputColumn < width do
                  destination(outputRow)(start + outputColumn) = values(outputRow, outputColumn)
                  outputColumn += 1
                outputRow += 1
              if stage == "inputs" then inputCopiedCells += values.rows.toLong * values.cols.toLong
              else targetCopiedCells += values.rows.toLong * values.cols.toLong
            catch case NonFatal(error) => return Left(AlderPredictiveAdmissionError.NativeReadFailure(stage, error.toString, attemptReceipt))
        start += width
      Right(())

    retained.flatMap: (inputValues, targetValues) =>
      for
        _ <- copyBlocks(observations.patterns, observations.columns, inputValues, "inputs")
        _ <- copyBlocks(targets.targets, targets.columns, targetValues, "targets")
        rootIdentity = nativeRootIdentity(mapping, metadataIdentity, observations, targets)
        root <- IdentifiedRows.fromRows(Vector.tabulate(observations.rows): index =>
          mapping.entriesByOrdinal(index).nativeId -> Example(inputValues(index), targetValues(index), metadata(index)), rootIdentity).left.map(error => AlderPredictiveAdmissionError.ProviderFixedSelection(error.toString))
      yield new AlderMaterializedRows(root, mapping, receipt, Some(attemptReceipt))

  private def blocksFor(columns: Int, width: Int): Int =
    1 + (columns - 1) / width

  private def nativeRootIdentity[S <: SemanticSpace, N <: SemanticSpace, F <: SemanticSpace](mapping: NativeAxisMapping, metadata: DataFingerprint, observations: Observations[S, N], targets: MultiResponse[S, F]): DataFingerprint =
    new DataFingerprint(FingerprintPolicy.Summary("scalafim.native-table-root.v2"), AxisDigest.sha256Hex: writer =>
      writer.string("scalafim.native-table-root.v2")
      writer.string(mapping.declaredMappingIdentity.policy.toString)
      writer.string(mapping.declaredMappingIdentity.digest)
      writer.string(metadata.policy.toString)
      writer.string(metadata.digest)
      writeEvidenceIdentity(writer, observations.identity)
      writeEvidenceIdentity(writer, targets.identity)
    )

  private def writeEvidenceIdentity(writer: AxisDigest.Writer, identity: scalafim.fmri.mvpa.EvidenceIdentity): Unit =
    writer.string(identity.rows.coordinateSignature.value)
    writer.string(identity.columns.coordinateSignature.value)
    writer.string(identity.source.value)
    writer.intLE(identity.provenanceNodes.length)
    identity.provenanceNodes.foreach: node =>
      writer.string(node.id.value)
      node.operation match
        case ProvenanceOperation.SourceRead(source) =>
          writer.string("source-read")
          writer.string(source.value)
        case ProvenanceOperation.Selection => writer.string("selection")
        case ProvenanceOperation.Assembly => writer.string("assembly")
        case ProvenanceOperation.Adapter(adapter) =>
          writer.string("adapter")
          writer.string(adapter.value)
        case ProvenanceOperation.Derived(operation) =>
          writer.string("derived")
          writer.string(operation.value)
      writer.intLE(node.parents.length)
      node.parents.foreach(parent => writer.string(parent.value))
    writer.intLE(identity.provenanceRoots.length)
    identity.provenanceRoots.foreach(root => writer.string(root.value))
    writeValueIdentity(writer, identity.values)
    identity.origins.writeFramed(writer)

  private def writeValueIdentity(writer: AxisDigest.Writer, identity: ValueIdentity): Unit =
    identity match
      case ValueIdentity.Source(id) =>
        writer.string("source")
        writer.string(id.value)
      case ValueIdentity.Adjoint(of) =>
        writer.string("adjoint")
        writeValueIdentity(writer, of)
      case ValueIdentity.Composition(first, second) =>
        writer.string("composition")
        writeValueIdentity(writer, first)
        writeValueIdentity(writer, second)
      case ValueIdentity.Derived(operation, inputs) =>
        writer.string("derived")
        writer.string(operation)
        writer.intLE(inputs.length)
        inputs.foreach(writeValueIdentity(writer, _))

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
