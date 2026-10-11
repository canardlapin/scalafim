package scalafim.phrfcmp.exec

import scala.util.control.NonFatal
import scalafim.phrfcmp.run.*
import scalafim.phrfcmp.score.*

/** Truth-free, immutable scoring data. Only the codec constructs decoded contributions. */
private[phrfcmp] enum ContributionData:
  case Condition(voxels: Vector[VoxelOutcome[ConditionEstimate]])
  case Trial(voxels: Vector[VoxelOutcome[TrialEstimate]])
  case Unavailable
  override def toString: String = "ContributionData(<redacted>)"

private[phrfcmp] final class ConditionEstimate(
    val lags: Vector[Double], val curves: Vector[Vector[Double]], val gaussian: Option[GaussianEstimate]):
  def response: ConditionResponse = ConditionResponse(ResponseGrid.of(lags).toOption.get, curves.map(_.toArray))
  override def toString: String = "ConditionEstimate(<redacted>)"

/** Conditional shape SDs are in the decoder's (tau, log sigma) coordinates. */
private[phrfcmp] final class GaussianEstimate(val tau: Double, val logSigma: Double, val sd: Vector[Double], val peaks: Vector[Double]):
  override def toString: String = "GaussianEstimate(<redacted>)"

private[phrfcmp] final class DecodedContribution(val unit: WorkUnit, val inputFingerprint: Option[String], val data: ContributionData, val sourceFileSha256: Option[String]):
  override def toString: String = "DecodedContribution(<redacted>)"

/** Canonical UTF-8 JSON with doubles represented by their 16-digit IEEE-754 bit patterns. No truth, timing,
  * attempt number, diagnostics or run id. Decoding accepts exactly the canonical encoding, bounds the payload,
  * and checks all dimensions and finite numbers before returning an immutable value. Error tokens are redacted.
  */
object PilotContribution:
  val Schema = "phrf-pilot-contribution-v1"
  private val MaxBytes = 64 * 1024 * 1024
  private def num(x: Double): ujson.Value = ujson.Str(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(x)).reverse.padTo(16, '0').reverse)
  private def nums(xs: Iterable[Double]): ujson.Value = ujson.Arr.from(xs.map(num))
  private def envelope(unit: WorkUnit, input: Option[String], kind: String, values: ujson.Value, sourceFile: Option[String] = None): ujson.Value =
    ujson.Arr(Schema, ujson.Arr(unit.cell.value, unit.dataset, unit.arm.value), input.fold[ujson.Value](ujson.Null)(ujson.Str(_)), kind, values, sourceFile.fold[ujson.Value](ujson.Null)(ujson.Str(_)))
  private def bytes(v: ujson.Value): Array[Byte] = ujson.write(v).getBytes("UTF-8")
  private def checked(v: ujson.Value): Either[String, Array[Byte]] =
    val b = bytes(v)
    decode(b).map(_ => b)
  private def outcome[A](v: VoxelOutcome[A])(f: A => ujson.Value): ujson.Value = v match
    case VoxelOutcome.Estimated(x) => ujson.Arr("e", f(x))
    case VoxelOutcome.Refused => ujson.Arr("r")
    case VoxelOutcome.Failed => ujson.Arr("f")

  def unavailable(unit: WorkUnit): Array[Byte] = bytes(envelope(unit, None, "unavailable", ujson.Null))

  def condition(unit: WorkUnit, fingerprint: String, result: ConditionArmResult): Either[String, Array[Byte]] =
    if unit.arm.value != result.arm.id || result.voxels.zipWithIndex.exists((v, i) => v.voxel != i) then Left("contribution_arm_or_voxel")
    else
      val vs = result.voxels.map { v =>
        val estimated = ConditionAdapter.outcome(v.status, v.response.map { r =>
          val g = v.phrf.filter(_.decode == scalafim.fmri.fit.profile.DecodeStatus.Accepted).flatMap { p =>
            // A real condition runner supplies exact peak readouts and both conditional shape SDs.
            if p.coordinates.length != 2 || p.conditionalSd.length != 2 || p.responsePeaks.length != r.conditions then None
            else Some(ujson.Arr(num(p.coordinates(0)), num(p.coordinates(1)), nums(p.conditionalSd), nums(p.responsePeaks)))
          }
          ujson.Arr(nums(r.grid.lags.toVector), ujson.Arr.from(r.curves.map(c => nums(c.toVector))), g.getOrElse(ujson.Null))
        })
        outcome(estimated)(identity)
      }
      checked(envelope(unit, Some(fingerprint), "condition", ujson.Arr.from(vs)))

  def trial(unit: WorkUnit, fingerprint: String, voxels: Vector[VoxelOutcome[TrialEstimate]], sourceFileSha256: Option[String] = None): Either[String, Array[Byte]] =
    checked(envelope(unit, Some(fingerprint), "trial", ujson.Arr.from(voxels.map(v => outcome(v)(e => nums(e.values)))), sourceFileSha256))

  private[phrfcmp] def decode(raw: Array[Byte]): Either[String, DecodedContribution] =
    try
      require(raw.nonEmpty && raw.length <= MaxBytes)
      val json = ujson.read(new String(raw, "UTF-8"))
      require(java.util.Arrays.equals(bytes(json), raw)) // duplicate/extra fields and alternate spellings cannot alias
      def arr(v: ujson.Value, n: Int): Vector[ujson.Value] =
        val a = v.arr.toVector
        require(a.length == n)
        a
      def finite(v: ujson.Value): Double =
        val s = v.str
        require(s.matches("[0-9a-f]{16}"))
        val d = java.lang.Double.longBitsToDouble(java.lang.Long.parseUnsignedLong(s, 16))
        require(d.isFinite)
        d
      def vector(v: ujson.Value): Vector[Double] =
        val a = v.arr.toVector
        require(a.nonEmpty && a.length <= 1000000)
        a.map(finite)
      def values[A](v: ujson.Value)(f: ujson.Value => A): Vector[VoxelOutcome[A]] =
        val a = v.arr.toVector
        require(a.nonEmpty && a.length <= 100000)
        a.map { x =>
          val z = x.arr.toVector
          z.head.str match
            case "e" => require(z.length == 2); VoxelOutcome.Estimated(f(z(1)))
            case "r" => require(z.length == 1); VoxelOutcome.Refused
            case "f" => require(z.length == 1); VoxelOutcome.Failed
            case _ => throw new IllegalArgumentException()
        }
      val a = arr(json, 6)
      require(a(0).str == Schema)
      val u = arr(a(1), 3)
      val index = u(1).num
      require(index >= 0 && index <= Int.MaxValue && index == index.toInt.toDouble)
      val unit = WorkUnit(CellId.parse(u(0).str).toOption.get, index.toInt, ArmId.parse(u(2).str).toOption.get)
      val input = if a(2) == ujson.Null then None else Some(a(2).str)
      require(input.forall(_.matches("[0-9a-f]{64}")))
      val sourceFile = if a(5) == ujson.Null then None else Some(a(5).str)
      require(sourceFile.forall(_.matches("[0-9a-f]{64}")))
      val data = a(3).str match
        case "unavailable" =>
          require(input.isEmpty && sourceFile.isEmpty && a(4) == ujson.Null)
          ContributionData.Unavailable
        case "condition" =>
          require(input.isDefined && sourceFile.isEmpty)
          ContributionData.Condition(values(a(4)) { x =>
            val e = arr(x, 3)
            val lags = vector(e(0))
            require(lags.length > 1 && lags.head == 0.0 && ResponseGrid.of(lags).isRight)
            val curves = e(1).arr.toVector.map(vector)
            require(curves.nonEmpty && curves.forall(_.length == lags.length))
            val g = if e(2) == ujson.Null then None else
              val p = arr(e(2), 4)
              val sd = vector(p(2)); val peaks = vector(p(3))
              require(sd.length == 2 && sd.forall(_ > 0.0) && peaks.length == curves.length)
              Some(new GaussianEstimate(finite(p(0)), finite(p(1)), sd, peaks))
            new ConditionEstimate(lags, curves, g)
          })
        case "trial" =>
          require(input.isDefined)
          ContributionData.Trial(values(a(4))(x => TrialEstimate.of(vector(x)).toOption.get))
        case _ => throw new IllegalArgumentException()
      Right(new DecodedContribution(unit, input, data, sourceFile))
    catch case NonFatal(_) => Left("malformed_contribution")
