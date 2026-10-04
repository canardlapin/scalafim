package scalafim.phrfcmp.prep

import scala.util.Try

import scalafim.fmri.ar.{ArmaCoefficients, InitialConditionPolicy, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig}
import scalafim.phrfcmp.ingest.{CellKind, Digests, Matrix}

/** Native arms that consume the common whitened arrays (design 2.0.5). */
enum NativeArm:
  case Can, Inf3, Fir, Lsa, Lss, Rlss, PhrfInput

object NativeArm:
  def forKind(kind: CellKind): Vector[NativeArm] = kind match
    case CellKind.Condition => Vector(Can, Inf3, Fir, PhrfInput)
    case CellKind.Trial     => Vector(Lsa, Lss, Rlss, PhrfInput)

/**
  * The one whitened array set of a dataset.
  *
  * @param y              whitened responses, V x T (same orientation as `FitInputs.y`)
  * @param nuisance       whitened generator nuisance with the intercept columns dropped, T x (P - R)
  * @param runIntercepts  whitened per-run intercept indicators (the columns `BaselineBasis.Constant` supplies), T x R
  * @param prefitDesign   whitened pre-fit design (also the design for sigma2), T x p
  */
final case class WhitenedArrays(y: Matrix, nuisance: Matrix, runIntercepts: Matrix, prefitDesign: Matrix):

  /** SHA-256 over a canonical byte form: per array its name, dimensions (int32 BE) and IEEE-754 bits (int64 BE). */
  lazy val sha256: String =
    val parts = Vector("y" -> y, "nuisance" -> nuisance, "run_intercepts" -> runIntercepts, "prefit_design" -> prefitDesign)
    val size = parts.map((n, m) => n.length + 8 + m.data.length * 8).sum
    val out = new Array[Byte](size)
    var o = 0
    def put(b: Int): Unit =
      out(o) = b.toByte
      o += 1
    def put32(v: Int): Unit =
      put(v >>> 24); put(v >>> 16); put(v >>> 8); put(v)
    def put64(v: Long): Unit =
      put32((v >>> 32).toInt); put32(v.toInt)
    parts.foreach { (n, m) =>
      n.foreach(ch => put(ch.toInt))
      put32(m.rows); put32(m.cols)
      m.data.foreach(d => put64(java.lang.Double.doubleToLongBits(d)))
    }
    Digests.sha256Hex(out)

/** What one native arm is handed: a reference to the shared arrays, never a private copy. */
final case class ArmInput(arm: NativeArm, arrays: WhitenedArrays):
  def sha256: String = arrays.sha256

/** The whitening transform applied to arbitrary matrices with a given plan. */
object Whiten:

  /** Whiten a T x k matrix (columns are series): the design, nuisance and baseline path. */
  def columns(plan: WhiteningPlan, m: Matrix): Either[PrepRefusal, Matrix] =
    if m.rows != plan.nTimepoints then
      Left(PrepRefusal.Whitening(s"matrix has ${m.rows} rows but the plan covers ${plan.nTimepoints}"))
    else
      WhiteningTransform.matrix(plan, Linear.toDMat(m)).left.map(e => PrepRefusal.Whitening(e.message)).flatMap(Linear.fromDMat)

  /** Whiten a V x T matrix (rows are series): the response path. */
  def series(plan: WhiteningPlan, y: Matrix): Either[PrepRefusal, Matrix] =
    if y.cols != plan.nTimepoints then
      Left(PrepRefusal.Whitening(s"matrix has ${y.cols} columns but the plan covers ${plan.nTimepoints}"))
    else
      WhiteningTransform.matrix(plan, Linear.transposeToDMat(y)).left.map(e => PrepRefusal.Whitening(e.message)).flatMap(Linear.fromDMatTransposed)

/** The single global whitening plan and the PHRF `FitConfig` that must agree with it bit for bit. */
final case class WhiteningSpec(plan: WhiteningPlan, phrfConfig: FitConfig, rho: Double)

object WhiteningSpec:

  /** `WhiteningPlan.global(ArmaCoefficients.ar(rho), segments, exactFirstAr1 = true)` and the matching `FitConfig`. */
  def build(rho: Double, segments: Vector[TimeSegment]): Either[PrepRefusal, WhiteningSpec] =
    Try {
      val plan = WhiteningPlan.global(ArmaCoefficients.ar(rho), segments, exactFirstAr1 = true)
      val config = FitConfig(autocorrelation = ArOptions(ArStructure.Ar(1), global = true, rho = Some(rho)))
      WhiteningSpec(plan, config, rho)
    }.toEither.left.map(e => PrepRefusal.Whitening(String.valueOf(e.getMessage))).flatMap(s => agreement(s).map(_ => s))

  /**
    * The executor's identity check (ProfileHrfFit fixed shared-AR validation): one coefficient set, no MA part, AR order,
    * the initial-condition policy and the run segments all agree, and the plan phi equals the config rho EXACTLY
    * (`doubleToLongBits`, not a tolerance).
    */
  def agreement(spec: WhiteningSpec): Either[PrepRefusal, Unit] =
    val plan = spec.plan
    val cfg = spec.phrfConfig.autocorrelation
    val requested = cfg.phi.getOrElse(cfg.rho.toVector)
    def bits(d: Double) = java.lang.Double.doubleToLongBits(d)
    def mismatch(d: String) = Left(PrepRefusal.PlanConfigMismatch(d))
    if plan.coefficients.length != 1 then mismatch(s"plan has ${plan.coefficients.length} coefficient sets")
    else if plan.coefficients.head.theta.nonEmpty then mismatch("plan has an MA part")
    else if plan.coefficients.head.phi.map(bits) != requested.map(bits) then
      mismatch(s"plan phi ${plan.coefficients.head.phi} versus config ${requested}")
    else if cfg.structure != ArStructure.Ar(plan.arOrder) then mismatch("AR order differs")
    else if !cfg.global then mismatch("config is not global")
    else if plan.initialCondition != InitialConditionPolicy.fromExactFirstAr1(cfg.exactFirst) then
      mismatch("initial-condition policy differs")
    else Right(())
