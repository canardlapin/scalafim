package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetSeriesReader, IndexSelection}
import scalafim.fmri.model.{FitEngine, FitPlan}
import scala.util.control.NonFatal

/** Immutable registry keys. The resolver must bind planRevision to the complete
  * scientific recipe (design, sampling geometry, configuration and provenance),
  * and sourceRevision to an immutable response snapshot, not a mutable path.
  */
final case class FitWorkReference(unitId: String, planRevision: String, sourceRevision: String):
  require(unitId.trim.nonEmpty, "unit identity must be non-empty")
  require(planRevision.trim.nonEmpty, "scientific plan revision must be non-empty")
  require(sourceRevision.trim.nonEmpty, "response source revision must be non-empty")

/** A portable recipe, not a snapshot of a solver or finalized preparation state.
  * Binding in a new process replays any declared response-dependent preparation.
  * No reader, function, numerical factorization or scheduler is part of this value.
  */
final case class FitWorkDescriptor private (
    reference: FitWorkReference,
    datasetId: String,
    engine: FitEngine,
    preparation: FitPreparationRequirements,
    timepoints: Vector[Int],
    voxelIndices: Vector[Int],
    blockSize: ChunkSize
):
  require(datasetId.nonEmpty, "dataset identity must be non-empty")
  require(timepoints.nonEmpty && timepoints.forall(_ >= 0) && timepoints.distinct == timepoints,
    "timepoints must be non-empty, unique and non-negative")
  require(voxelIndices.nonEmpty && voxelIndices.forall(_ >= 0) && voxelIndices.distinct == voxelIndices,
    "voxel indices must be non-empty, unique and non-negative")

  def selection: DataSelection =
    DataSelection(IndexSelection.Indices(timepoints), IndexSelection.Indices(voxelIndices))

  def chunks: Vector[FitChunkSpec] =
    voxelIndices.grouped(blockSize.value).zipWithIndex.map { (voxels, ordinal) =>
      FitChunkSpec.unsafe(ChunkOrdinal.unsafe(ordinal), timepoints, voxels)
    }.toVector

  /** Collision-free canonical identities; callers may hash these for compact storage.
    * The unit identity excludes partitioning, while block identities include exact axes.
    */
  def workId: String =
    FitWorkDescriptor.frame(Vector("fit-unit-v1", reference.unitId, reference.planRevision,
      reference.sourceRevision, datasetId, engine.toString, preparation.topology.toString,
      preparation.reductions.mkString(","), timepoints.mkString(","), voxelIndices.mkString(",")))

  def blockWorkIds: Vector[String] =
    chunks.map(chunk => FitWorkDescriptor.frame(Vector("fit-block-v1", workId,
      chunk.ordinal.value.toString, chunk.voxelIndices.mkString(","))))

  def encode: String =
    FitWorkDescriptor.frame(Vector("fit-work-v1", reference.unitId, reference.planRevision,
      reference.sourceRevision, datasetId, engine.toString, preparation.topology.toString,
      preparation.reductions.mkString(","), timepoints.mkString(","), voxelIndices.mkString(","),
      blockSize.value.toString))

object FitWorkDescriptor:
  def compile(
      reference: FitWorkReference,
      plan: FitPlan,
      blockSize: ChunkSize,
      selection: DataSelection = DataSelection.All
  ): Either[FitError, FitWorkDescriptor] =
    FitChunkPlan.fromSelection(plan, selection, FitChunkingStrategy.ByVoxelCount(blockSize)).map { chunks =>
      FitWorkDescriptor(reference, plan.model.dataset.id.value, plan.engine, FitPreparation.describe(plan),
        chunks.timepoints, chunks.voxelIndices, blockSize)
    }

  /** Length-prefixed UTF-16 text fields; no escaping, JVM serialization or JSON
    * dependency. Schema v1 uses exactly eleven fields and stable enum case names.
    */
  def decode(text: String): Either[FitError, FitWorkDescriptor] =
    try
      val fields = Vector.newBuilder[String]
      var offset = 0
      var count = 0
      while offset < text.length && count < 11 do
        val colon = text.indexOf(':', offset)
        require(colon > offset, "missing field length")
        val lengthText = text.substring(offset, colon)
        val length = lengthText.toInt
        require(length >= 0 && length.toString == lengthText, "noncanonical field length")
        val start = colon + 1
        require(length <= text.length - start, "truncated field")
        fields += text.substring(start, start + length)
        offset = start + length
        count += 1
      require(offset == text.length && count == 11, "expected eleven complete fields")
      val values = fields.result()
      require(values(0) == "fit-work-v1", "unsupported fit work schema")
      def indices(value: String): Vector[Int] = value.split(",", -1).toVector.map(_.toInt)
      val reductions = if values(7).isEmpty then Vector.empty
        else values(7).split(",", -1).toVector.map(FitPreparationReduction.valueOf)
      val descriptor = FitWorkDescriptor(
        FitWorkReference(values(1), values(2), values(3)), values(4), FitEngine.valueOf(values(5)),
        FitPreparationRequirements(FitPreparationTopology.valueOf(values(6)), reductions),
        indices(values(8)), indices(values(9)), ChunkSize.unsafe(values(10).toInt)
      )
      require(descriptor.encode == text, "noncanonical fit work encoding")
      Right(descriptor)
    catch
      case NonFatal(error) => Left(FitError.InvalidFitAxis("fit work descriptor", error.getMessage))

  private[fit] def frame(fields: Vector[String]): String =
    fields.map(value => s"${value.length}:$value").mkString

/** Explicit runtime capability. Implementations must verify the immutable keys,
  * including source content/version and all scientific plan settings, on resolution.
  */
trait FitWorkResolver:
  def resolve(reference: FitWorkReference): Either[FitError, ResolvedFitWork]

final case class ResolvedFitWork(reference: FitWorkReference, plan: FitPlan, reader: DatasetSeriesReader)

object FitWorkExecutor:
  /** Execute only preparations with an implemented bounded interpreter. Legacy
    * dense global fallbacks remain accessible through ChunkedFitExecutor directly.
    */
  def fit(descriptor: FitWorkDescriptor, resolver: FitWorkResolver): Either[FitError, FmriFitResult] =
    for
      bound <- resolver.resolve(descriptor.reference)
      _ <- validate(descriptor, bound)
      result <- ChunkedFitExecutor.fit(bound.reader, bound.plan, descriptor.selection,
        FitChunkingStrategy.ByVoxelCount(descriptor.blockSize))
    yield result

  private def validate(descriptor: FitWorkDescriptor, bound: ResolvedFitWork): Either[FitError, Unit] =
    if bound.reference != descriptor.reference then
      Left(FitError.InvalidFitAxis("fit work binding", "resolved immutable reference differs from the descriptor"))
    else if bound.plan.model.dataset.id.value != descriptor.datasetId ||
        bound.reader.dataset.id != bound.plan.model.dataset.id ||
        bound.reader.dataset.shape != bound.plan.model.dataset.shape then
      Left(FitError.InvalidFitAxis("fit work binding", "resolved dataset identity or shape differs"))
    else if bound.plan.engine != descriptor.engine || FitPreparation.describe(bound.plan) != descriptor.preparation then
      Left(FitError.InvalidFitAxis("fit work binding", "resolved engine or preparation topology differs"))
    else if !descriptor.preparation.supportsBoundedExecution then
      Left(FitError.UnsupportedEngine(
        s"bounded preparation is not implemented for ${descriptor.preparation.reductions.mkString(", ")}"
      ))
    else Right(())
