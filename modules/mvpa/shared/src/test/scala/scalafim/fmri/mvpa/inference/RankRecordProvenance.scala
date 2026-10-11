package scalafim.fmri.mvpa.inference

import multivar.inference.{Alternative, CanonicalRankMethod, ComponentIx, GaussianCanonicalRank, MonteCarloReceipt, MonteCarloStopReason, ReplicateProvenance}
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.pattern.RankConfirmationResult
import scala.util.control.NonFatal

/** Engineering provenance for the current schema-2 records. This does not admit
  * a rank procedure or replace frozen scientific qualification.
  */
object RankRecordProvenance:
  final class SourceSnapshot private[RankRecordProvenance] (
      val commit: String, val lockSha256: String, private val locks: Vector[(String, String)]
  ):
    def verify(readSource: String => String): Either[String, Unit] =
      try
        if locks.forall((path, digest) => CalibrationPlatform.sha256Utf8(readSource(path)) == digest) then Right(())
        else Left("rank source lock differs from executed source files")
      catch case NonFatal(error) => Left("cannot verify rank source lock: " + error.getMessage)

    def jsonFields(input: CalibrationProtocolSupport.Case): String =
      ",\"source_commit\":" + quoted(commit) + ",\"source_lock_sha256\":" + quoted(lockSha256) +
        ",\"case_input_sha256\":" + quoted(input.inputSha256)

  object SourceSnapshot:
    def from(commit: String, lockText: String): Either[String, SourceSnapshot] =
      if !commit.matches("[0-9a-f]{40}") then Left("rank source commit must be a full lowercase git revision")
      else
        try
          val locks = ujson.read(lockText).obj.toVector.map((path, hash) => path -> hash.str)
          if locks.isEmpty || locks.exists((path, hash) => path.isEmpty || path.startsWith("/") || path.contains('\\') ||
              path.split('/').exists(p => p == ".." || p == "." || p.isEmpty) || !hash.matches("[0-9a-f]{64}")) then
            Left("rank source lock requires relative paths and SHA-256 digests")
          else Right(new SourceSnapshot(commit, CalibrationPlatform.sha256Utf8(lockText), locks))
        catch case NonFatal(error) => Left("invalid rank source lock: " + error.getMessage)

    def environment(): Either[String, SourceSnapshot] =
      for
        commit <- CalibrationPlatform.environment("SCALAFIM_CALIBRATION_SOURCE_COMMIT").toRight("rank source commit is required")
        path <- CalibrationPlatform.environment("SCALAFIM_CALIBRATION_SOURCE_LOCK").toRight("rank source lock file is required")
        snapshot <- from(commit, CalibrationPlatform.readText(path))
        _ <- snapshot.verify(readSource)
      yield snapshot

  def readSource(path: String): String =
    val root = CalibrationPlatform.environment("SCALAFIM_CALIBRATION_SOURCE_ROOT").getOrElse(".")
    CalibrationPlatform.readText(root + "/" + path)

  private def quoted(value: String): String = CalibrationProtocolSupport.jsonString(value)

  final class Verified private[RankRecordProvenance] (val seeds: Vector[Long], val algorithm: String, val observed: Vector[Double]):
    def jsonFields: String =
      ",\"provenance_version\":1,\"seed_receipts_verified\":true,\"receipt_seed64\":[" +
        seeds.map(s => quoted(s.toString)).mkString(",") + "],\"receipt_algorithm\":" + quoted(algorithm) +
        ",\"observed_statistics\":[" + observed.mkString(",") + "]"

  /** Validate receipts from the actual estimator, including method-specific
    * hypothesis seeds. Declared environment values alone cannot satisfy this.
    */
  def verifyReceipts(rootSeed: Long, draws: Int, method: CanonicalRankMethod, receipts: Vector[MonteCarloReceipt]): Either[String, Verified] =
    CalibrationProtocolSupport.child(rootSeed, 103, method.ordinal).flatMap: resampling =>
      val expected = receipts.indices.map: step =>
        method match
          case CanonicalRankMethod.ScoreOrthogonalPermutationV2 => resampling
          case CanonicalRankMethod.GaussianInterlacingWilksV1 =>
            GaussianCanonicalRank.hypothesisSeed(Seed.fromLong(resampling), ComponentIx(step).toOption.get).value
      val algorithm = method match
        case CanonicalRankMethod.ScoreOrthogonalPermutationV2 => "canonical-rank-distinct-nonidentity/v1"
        case CanonicalRankMethod.GaussianInterlacingWilksV1 => GaussianCanonicalRank.referenceAlgorithm.value
      val valid = receipts.zip(expected).forall: (receipt, expectedSeed) =>
        receipt.observed.isFinite && receipt.allocated.value == draws && receipt.consumed.value == draws &&
          receipt.stopReason == MonteCarloStopReason.Exhausted && receipt.alternative == Alternative.Greater &&
          (receipt.provenance match
            case ReplicateProvenance.Deterministic(seed, actualAlgorithm, derivation) =>
              seed.value == expectedSeed && actualAlgorithm.value == algorithm && derivation == Seed.derivationAlgorithm
            case ReplicateProvenance.External => false)
      if receipts.isEmpty || !valid then Left("inconsistent actual rank seed, algorithm or completed-draw receipt")
      else Right(new Verified(expected.toVector, algorithm, receipts.map(_.observed)))

  def verify(input: CalibrationProtocolSupport.Case, value: RankConfirmationResult): Either[String, Verified] =
    val arithmetic = value.candidateArithmetic
    val k = input.populationCorrelations.length
    val bound = CalibrationProtocolSupport.bindingIdentity(input.scenario, input.ordinal, input.rootSeed).exists: identity =>
      value.brainEvidenceIdentity.source.value == identity + "-brain-source" &&
        value.targetEvidenceIdentity.source.value == identity + "-target-source" &&
        value.confirmationRows.size == input.x.rows && value.brainEvidenceIdentity.columns.size == input.x.cols &&
        value.targetEvidenceIdentity.columns.size == input.y.cols
    if !bound || arithmetic.receipts.size != k || arithmetic.adjustedPValues.size != k ||
        arithmetic.correlations.size != k || arithmetic.completedCompactFits != input.draws.toLong * k then
      Left("rank arithmetic is incomplete or bound to another input")
    else verifyReceipts(input.rootSeed, input.draws, arithmetic.method, arithmetic.receipts)
