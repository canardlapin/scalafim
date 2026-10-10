package scalafim.fmri.fit

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, CorrectionFallback, CorrectionSkip, InitialConditionPolicy, NoisePooling, RunCorrection, TimeSegment, WhiteningMethod, WhiteningPlan}
import scalafim.fmri.model.{FitEngine, FitPlan, FitStrategy, MissingDataPolicy, NuisanceProjection, VolumeWeighting}
import scala.util.control.NonFatal

enum PreparedGlsScope:
  case Shared, Runwise

/** A source-run identity is absent only for the shared-coefficient fit. */
final case class PreparedGlsUnit private[fit] (
    sourceRun: Option[Int],
    whitening: WhiteningPlan,
    corrections: Vector[RunCorrection] = Vector.empty
):
  require(sourceRun.forall(_ >= 0), "source run must be non-negative")
  require(corrections.isEmpty || corrections.length == whitening.segments.map(_.runIndex).max + 1,
    "correction outcomes must cover the whitening runs")
  require(whitening.method == WhiteningMethod.Estimated && whitening.maOrder == 0,
    "completed GLS artifacts contain estimated pure-AR whitening")
  require(whitening.initialCondition == InitialConditionPolicy.Identity ||
    whitening.initialCondition == InitialConditionPolicy.ExactAr1 || whitening.initialCondition == InitialConditionPolicy.Stationary, "unsupported GLS initial condition")

private[fit] final case class CompletedGlsNoise(
    designIdentity: String,
    retainedVoxelIndices: Vector[Int],
    units: Vector[PreparedGlsUnit]
)

/** Completed pooled noise preparation, without readers, factors or response data.
  * Immutable references are verified by the resolver. The codec validates structure
  * and compatibility; artifact authenticity belongs to the caller's storage boundary.
  *
  * Contents are platform-specific even though the descriptor and its work IDs are
  * not: JVM and Scala.js producers may estimate coefficients a few ulp apart. Either
  * artifact restores on either platform and fits with exactly its stored coefficients,
  * so a cache that must be bit-reproducible across platforms keys on the producer too.
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
  private val CorrectedVersion = "prepared-gls-v2"
  private val StationaryVersion = "prepared-gls-v3"

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

  private[fit] def invalid(detail: String): FitError = FitError.PreparedArtifactInvalid(s"prepared GLS: $detail")

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

  private def encodeUnit(unit: PreparedGlsUnit, includeCorrections: Boolean): String =
    val w = unit.whitening
    FitWorkDescriptor.frame(Vector(
      unit.sourceRun.fold("shared")(_.toString), w.pooling.toString,
      (w.initialCondition match
        case InitialConditionPolicy.Identity => "identity"
        case InitialConditionPolicy.ExactAr1 => "exact-ar1"
        case InitialConditionPolicy.Stationary => "stationary"
        case _ => throw new IllegalArgumentException("unsupported GLS initial condition")),
      FitWorkDescriptor.frame(w.segments.map(s => s"${s.start},${s.endExclusive},${s.runIndex}")),
      FitWorkDescriptor.frame(w.coefficients.map(c => c.phi.map(bits).mkString(",")))
    ) ++ (if !includeCorrections then Vector.empty else Vector(FitWorkDescriptor.frame(unit.corrections.map(encodeCorrection)))))

  private def encode(artifact: PreparedGlsArtifact): String =
    val version = if artifact.units.exists(unit => unit.whitening.initialCondition == InitialConditionPolicy.Stationary || unit.corrections.exists(_.isInstanceOf[RunCorrection.AppliedWithTailAnchor])) then StationaryVersion
      else if artifact.units.exists(_.corrections.nonEmpty) then CorrectedVersion else Version
    FitWorkDescriptor.frame(Vector(version, artifact.descriptor.encode, artifact.scope.toString,
      artifact.noise.designIdentity, artifact.retainedVoxelIndices.mkString(","),
      FitWorkDescriptor.frame(artifact.units.map(unit => encodeUnit(unit, version != Version)))))

  /** Canonical UTF-16 length framing, with exact IEEE-754 bit strings for numbers.
    * Unknown versions, noncanonical text, invalid axes and invalid AR state fail closed.
    */
  def decode(text: String): Either[FitError, PreparedGlsArtifact] =
    try
      val fields = unframe(text)
      require(fields.length == 6 && Set(Version, CorrectedVersion, StationaryVersion).contains(fields.head), "unsupported or malformed prepared GLS schema")
      val descriptor = FitWorkDescriptor.decode(fields(1)).fold(e => throw new IllegalArgumentException(e.message), identity)
      val units = unframe(fields(5)).map { encoded =>
        val parts = unframe(encoded)
        require(parts.length == (if fields.head == Version then 5 else 6), "invalid whitening unit")
        val run = if parts(0) == "shared" then None else Some(parts(0).toInt)
        val pooling = NoisePooling.valueOf(parts(1))
        val initial = parts(2) match
          case "exact-ar1" => InitialConditionPolicy.ExactAr1
          case "identity" => InitialConditionPolicy.Identity
          case "stationary" if fields.head == StationaryVersion => InitialConditionPolicy.Stationary
          case _ => throw new IllegalArgumentException("invalid initial condition")
        val segments = unframe(parts(3)).map { value =>
          val axis = value.split(",", -1).toVector.map(_.toInt)
          require(axis.length == 3, "invalid whitening segment")
          TimeSegment(axis(0), axis(1), axis(2))
        }
        val coefficients = unframe(parts(4)).map { value =>
          ArmaCoefficients.ar((if value.isEmpty then Vector.empty else value.split(",", -1).toVector.map(number))*)
        }
        val corrections = if fields.head == Version then Vector.empty else unframe(parts(5)).map(decodeCorrection)
        val scope = if pooling == NoisePooling.Global then scalafim.fmri.ar.CoefficientScope.Global(coefficients.head) else scalafim.fmri.ar.CoefficientScope.ByRun(coefficients)
        val whitening = WhiteningPlan.withScope(scope, segments, initial, WhiteningMethod.Estimated).fold(e => throw new IllegalArgumentException(e.message), identity)
        PreparedGlsUnit(run, whitening, corrections)
      }
      val artifact = new PreparedGlsArtifact(descriptor, PreparedGlsScope.valueOf(fields(2)),
        CompletedGlsNoise(fields(3), fields(4).split(",", -1).toVector.map(_.toInt), units))
      require(artifact.encode == text, "noncanonical prepared GLS encoding")
      Right(artifact)
    catch
      case NonFatal(error) => Left(invalid(Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))

  private def encodeCorrection(correction: RunCorrection): String =
    val fields = correction match
      case RunCorrection.Uncorrected => Vector("raw")
      case RunCorrection.Applied(rcond) => Vector("applied", bits(rcond))
      case RunCorrection.AppliedWithTailAnchor(rcond, directions) => Vector("tail-applied", bits(rcond), directions.toString)
      case RunCorrection.IllConditioned(rcond) => Vector("ill-conditioned", bits(rcond))
      case RunCorrection.NotAttempted(reason) => Vector("skipped", reason.toString)
      case RunCorrection.SolveFallback(reason) => reason match
        case CorrectionFallback.IllConditionedBlock(rcond) => Vector("ill-conditioned-block", bits(rcond))
        case CorrectionFallback.SingularSystem => Vector("singular")
        case CorrectionFallback.NonFiniteSolution => Vector("nonfinite")
        case CorrectionFallback.NonPositiveVariance(value) => Vector("nonpositive", bits(value))
        case CorrectionFallback.NonPositiveRawVariance(value) => Vector("nonpositive-raw", bits(value))
    FitWorkDescriptor.frame(fields)

  private def decodeCorrection(text: String): RunCorrection =
    val fields = unframe(text)
    fields match
      case Vector("raw") => RunCorrection.Uncorrected
      case Vector("applied", value) =>
        val rcond = number(value)
        require(rcond >= scalafim.fmri.ar.AcvfBias.ReciprocalConditionFloor, "invalid applied conditioning")
        RunCorrection.Applied(rcond)
      case Vector("tail-applied", value, count) =>
        val rcond = number(value)
        val directions = count.toInt
        require(rcond >= scalafim.fmri.ar.AcvfBias.ReciprocalConditionFloor && directions > 0, "invalid tail correction diagnostic")
        RunCorrection.AppliedWithTailAnchor(rcond, directions)
      case Vector("ill-conditioned", value) =>
        val rcond = number(value)
        require(rcond >= 0.0 && rcond < scalafim.fmri.ar.AcvfBias.ReciprocalConditionFloor, "invalid rejected conditioning")
        RunCorrection.IllConditioned(rcond)
      case Vector("skipped", reason) => RunCorrection.NotAttempted(CorrectionSkip.valueOf(reason))
      case Vector("ill-conditioned-block", value) =>
        val rcond = number(value)
        require(rcond >= 0.0 && rcond < scalafim.fmri.ar.AcvfBias.ReciprocalConditionFloor, "invalid block conditioning")
        RunCorrection.SolveFallback(CorrectionFallback.IllConditionedBlock(rcond))
      case Vector("singular") => RunCorrection.SolveFallback(CorrectionFallback.SingularSystem)
      case Vector("nonfinite") => RunCorrection.SolveFallback(CorrectionFallback.NonFiniteSolution)
      case Vector("nonpositive", value) =>
        val variance = number(value)
        require(variance <= 0.0, "invalid nonpositive variance")
        RunCorrection.SolveFallback(CorrectionFallback.NonPositiveVariance(variance))
      case Vector("nonpositive-raw", value) =>
        val variance = number(value)
        require(variance <= 0.0, "invalid nonpositive raw variance")
        RunCorrection.SolveFallback(CorrectionFallback.NonPositiveRawVariance(variance))
      case _ => throw new IllegalArgumentException("invalid correction outcome")

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
  * block at a time; the restored context checks every block's finite membership
  * against the completed noise population (see [[RetainedMembershipFitContext]]).
  */
final class RestoredGlsWork private[fit] (
    val artifact: PreparedGlsArtifact,
    bound: ResolvedFitWork,
    chunks: FitChunkPlan,
    context: PreparedFitContext
):
  def fit(): Either[FitError, FmriFitResult] =
    val results = Vector.newBuilder[FitBlockResult]
    val iterator = chunks.iterator
    while iterator.hasNext do
      val chunk = iterator.next()
      ChunkedFitExecutor.readChunk(bound.reader, chunk).flatMap(context.fitChunk) match
        case Left(error) => return Left(FitError.ChunkFailed(chunk.ordinal.value, error))
        case Right(value) => results += value
    context.merge(results.result())
