package scalafim.fmri.fit

import gale.linalg.DMat
import scalafim.dataset.FmriSeries
import scalafim.fmri.ar.{ArmaCoefficients, InitialConditionPolicy, NoisePooling, TimeSegment, WhiteningMethod, WhiteningPlan}
import scalafim.fmri.model.{FitEngine, FitPlan, FitStrategy, MissingDataPolicy, NuisanceProjection, VolumeWeighting}
import scala.util.control.NonFatal

enum PreparedGlsScope:
  case Shared, Runwise

/** A source-run identity is absent only for the shared-coefficient fit. */
final case class PreparedGlsUnit private[fit] (sourceRun: Option[Int], whitening: WhiteningPlan):
  require(sourceRun.forall(_ >= 0), "source run must be non-negative")
  require(whitening.method == WhiteningMethod.Estimated && whitening.maOrder == 0,
    "completed GLS artifacts contain estimated pure-AR whitening")
  require(whitening.initialCondition == InitialConditionPolicy.Identity ||
    whitening.initialCondition == InitialConditionPolicy.ExactAr1, "unsupported GLS initial condition")

private[fit] final case class CompletedGlsNoise(
    designIdentity: String,
    retainedVoxelIndices: Vector[Int],
    units: Vector[PreparedGlsUnit]
)

/** Completed pooled noise preparation, without readers, factors or response data.
  * Immutable references are verified by the resolver. The codec validates structure
  * and compatibility; artifact authenticity belongs to the caller's storage boundary.
  */
final class PreparedGlsArtifact private[fit] (
    val descriptor: FitWorkDescriptor,
    val scope: PreparedGlsScope,
    private[fit] val noise: CompletedGlsNoise
):
  require(descriptor.engine == FitEngine.GeneralizedLeastSquares &&
    descriptor.preparation.reductions == Vector(FitPreparationReduction.PooledAutocorrelation),
    "artifact requires pooled GLS without other global preparation phases")
  require(noise.designIdentity.nonEmpty, "design identity must be non-empty")
  require(noise.retainedVoxelIndices.nonEmpty && noise.retainedVoxelIndices.distinct == noise.retainedVoxelIndices,
    "retained voxels must be non-empty and unique")
  require(descriptor.voxelIndices.filter(noise.retainedVoxelIndices.toSet) == noise.retainedVoxelIndices,
    "retained voxels must be an ordered subset of the descriptor")
  require(noise.units.nonEmpty, "completed preparation must contain whitening units")
  require(scope match
    case PreparedGlsScope.Shared => noise.units.length == 1 && noise.units.head.sourceRun.isEmpty
    case PreparedGlsScope.Runwise =>
      val runs = noise.units.flatMap(_.sourceRun)
      runs.length == noise.units.length && runs.distinct == runs && runs.sorted == runs
  , "whitening units must match the declared coefficient scope")

  def retainedVoxelIndices: Vector[Int] = noise.retainedVoxelIndices
  def units: Vector[PreparedGlsUnit] = noise.units

  def encode: String = PreparedGlsArtifact.encode(this)

  /** Rebuilds only design-dependent state. Does not read any response block. */
  def restore(resolver: FitWorkResolver): Either[FitError, RestoredGlsWork] =
    PreparedGlsArtifact.restore(this, resolver)

object PreparedGlsArtifact:
  private val Version = "prepared-gls-v1"

  /** Performs the configured bounded AR estimation passes, but no final fit pass. */
  def prepare(descriptor: FitWorkDescriptor, resolver: FitWorkResolver): Either[FitError, PreparedGlsArtifact] =
    for
      bound <- resolver.resolve(descriptor.reference)
      _ <- FitWorkExecutor.validate(descriptor, bound)
      scope <- admit(bound.plan)
      chunks <- chunkPlan(descriptor, bound.plan)
      completed <- PooledGlsPreparation.complete(bound.reader, bound.plan, chunks)
    yield new PreparedGlsArtifact(descriptor, scope, completed._2)

  private def admit(plan: FitPlan): Either[FitError, PreparedGlsScope] =
    if FitPreparation.describe(plan).reductions != Vector(FitPreparationReduction.PooledAutocorrelation) ||
        plan.config.volumeWeighting != VolumeWeighting.Disabled ||
        plan.config.nuisanceProjection != NuisanceProjection.Disabled then
      Left(FitError.UnsupportedEngine("completed GLS artifacts require pooled estimated AR, disabled temporal weighting/projection, and no observation-pattern discovery"))
    else plan.strategy match
      case FitStrategy.GeneralizedLeastSquares(_, _) => Right(PreparedGlsScope.Shared)
      case FitStrategy.RunwiseGeneralizedLeastSquares(_, _) => Right(PreparedGlsScope.Runwise)
      case _ => Left(FitError.UnsupportedEngine("completed GLS artifacts require shared or runwise GLS"))

  private def chunkPlan(descriptor: FitWorkDescriptor, plan: FitPlan): Either[FitError, FitChunkPlan] =
    FitChunkPlan.fromSelection(plan, descriptor.selection, FitChunkingStrategy.ByVoxelCount(descriptor.blockSize))

  private def restore(artifact: PreparedGlsArtifact, resolver: FitWorkResolver): Either[FitError, RestoredGlsWork] =
    for
      bound <- resolver.resolve(artifact.descriptor.reference)
      _ <- FitWorkExecutor.validate(artifact.descriptor, bound)
      scope <- admit(bound.plan)
      _ <- if scope == artifact.scope then Right(())
        else Left(invalid("coefficient scope differs"))
      _ <- if bound.plan.config.missingData != MissingDataPolicy.Error ||
          artifact.retainedVoxelIndices == artifact.descriptor.voxelIndices then Right(())
        else Left(invalid("error-on-missing policy requires every selected voxel"))
      chunks <- chunkPlan(artifact.descriptor, bound.plan)
      context <- PooledGlsPreparation.restore(bound.plan, chunks, artifact.noise)
    yield new RestoredGlsWork(artifact, bound, chunks, context)

  private[fit] def invalid(detail: String): FitError = FitError.InvalidFitAxis("prepared GLS artifact", detail)

  private[fit] def bits(value: Double): String =
    require(value.isFinite, "artifact numbers must be finite")
    java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(value))

  private def number(value: String): Double =
    val decoded = java.lang.Double.longBitsToDouble(java.lang.Long.parseUnsignedLong(value, 16))
    require(bits(decoded) == value, "noncanonical number")
    decoded

  private[fit] def matrixIdentity(matrix: DMat): String =
    val values = Vector.newBuilder[String]
    values += matrix.rows.toString
    values += matrix.cols.toString
    var row = 0
    while row < matrix.rows do
      var column = 0
      while column < matrix.cols do
        values += bits(matrix(row, column))
        column += 1
      row += 1
    FitWorkDescriptor.frame(values.result())

  private[fit] def policyIdentity(policy: OlsSolvePolicy): String =
    val rank = policy.rankPolicy match
      case OlsRankPolicy.StrictFullRank => "strict-full-rank"
      case OlsRankPolicy.MinimumNorm => "minimum-norm"
      case OlsRankPolicy.RidgeRegularized(value) => s"ridge:${bits(value)}"
    val tolerance = policy.rankTolerance match
      case OlsRankTolerance.ScaleAware => "scale-aware"
      case OlsRankTolerance.Absolute(value) => s"absolute:${bits(value)}"
    FitWorkDescriptor.frame(Vector(policy.method.toString, rank, tolerance, bits(policy.choleskyTolerance)))

  private def encodeUnit(unit: PreparedGlsUnit): String =
    val w = unit.whitening
    FitWorkDescriptor.frame(Vector(
      unit.sourceRun.fold("shared")(_.toString), w.pooling.toString,
      if w.exactFirstAr1 then "exact-ar1" else "identity",
      FitWorkDescriptor.frame(w.segments.map(s => s"${s.start},${s.endExclusive},${s.runIndex}")),
      FitWorkDescriptor.frame(w.coefficients.map(c => c.phi.map(bits).mkString(",")))
    ))

  private def encode(artifact: PreparedGlsArtifact): String =
    FitWorkDescriptor.frame(Vector(Version, artifact.descriptor.encode, artifact.scope.toString,
      artifact.noise.designIdentity, artifact.retainedVoxelIndices.mkString(","),
      FitWorkDescriptor.frame(artifact.units.map(encodeUnit))))

  /** Canonical UTF-16 length framing, with exact IEEE-754 bit strings for numbers.
    * Unknown versions, noncanonical text, invalid axes and invalid AR state fail closed.
    */
  def decode(text: String): Either[FitError, PreparedGlsArtifact] =
    try
      val fields = unframe(text)
      require(fields.length == 6 && fields.head == Version, "unsupported or malformed prepared GLS schema")
      val descriptor = FitWorkDescriptor.decode(fields(1)).fold(e => throw new IllegalArgumentException(e.message), identity)
      val units = unframe(fields(5)).map { encoded =>
        val parts = unframe(encoded)
        require(parts.length == 5, "invalid whitening unit")
        val run = if parts(0) == "shared" then None else Some(parts(0).toInt)
        val pooling = NoisePooling.valueOf(parts(1))
        val exact = parts(2) match
          case "exact-ar1" => true
          case "identity" => false
          case _ => throw new IllegalArgumentException("invalid initial condition")
        val segments = unframe(parts(3)).map { value =>
          val axis = value.split(",", -1).toVector.map(_.toInt)
          require(axis.length == 3, "invalid whitening segment")
          TimeSegment(axis(0), axis(1), axis(2))
        }
        val coefficients = unframe(parts(4)).map { value =>
          ArmaCoefficients.ar((if value.isEmpty then Vector.empty else value.split(",", -1).toVector.map(number))*)
        }
        PreparedGlsUnit(run, WhiteningPlan(coefficients, segments, pooling, exact, WhiteningMethod.Estimated))
      }
      val artifact = new PreparedGlsArtifact(descriptor, PreparedGlsScope.valueOf(fields(2)),
        CompletedGlsNoise(fields(3), fields(4).split(",", -1).toVector.map(_.toInt), units))
      require(artifact.encode == text, "noncanonical prepared GLS encoding")
      Right(artifact)
    catch
      case NonFatal(error) => Left(invalid(Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))

  private def unframe(text: String): Vector[String] =
    val result = Vector.newBuilder[String]
    var offset = 0
    while offset < text.length do
      val colon = text.indexOf(':', offset)
      require(colon > offset, "missing field length")
      val encodedLength = text.substring(offset, colon)
      val length = encodedLength.toInt
      require(length >= 0 && length.toString == encodedLength, "noncanonical field length")
      val start = colon + 1
      require(length <= text.length - start, "truncated field")
      result += text.substring(start, start + length)
      offset = start + length
    result.result()

/** Runtime capabilities bound to a validated artifact. Each fit reads one spatial
  * block at a time and checks membership against the completed noise population.
  */
final class RestoredGlsWork private[fit] (
    val artifact: PreparedGlsArtifact,
    bound: ResolvedFitWork,
    chunks: FitChunkPlan,
    context: PreparedFitContext
):
  def fit(): Either[FitError, FmriFitResult] =
    val retained = artifact.retainedVoxelIndices.toSet
    val results = Vector.newBuilder[FitBlockResult]
    val iterator = chunks.iterator
    while iterator.hasNext do
      val chunk = iterator.next()
      val fitted = for
        series <- ChunkedFitExecutor.readChunk(bound.reader, chunk)
        membership <- retainedMembership(series)
        _ <- if membership == chunk.voxelIndices.filter(retained) then Right(())
          else Left(PreparedGlsArtifact.invalid(s"retained voxel membership changed in chunk ${chunk.ordinal.value}"))
        result <- context.fitChunk(series)
      yield result
      fitted match
        case Left(error) => return Left(FitError.ChunkFailed(chunk.ordinal.value, error))
        case Right(value) => results += value
    context.merge(results.result())

  private def retainedMembership(series: FmriSeries): Either[FitError, Vector[Int]] =
    MatrixAdapters.responseBlock(series, bound.plan.config.missingData) match
      case Left(FitError.AllVoxelsExcluded(_)) => Right(Vector.empty)
      case Left(error) => Left(error)
      case Right(value) => Right(value.voxelIndices)
