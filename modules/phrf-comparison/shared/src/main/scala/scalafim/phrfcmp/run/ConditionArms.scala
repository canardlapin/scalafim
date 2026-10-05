package scalafim.phrfcmp.run

import gale.linalg.DMat

import scalafim.fmri.ar.{TimeSegment, WhiteningPlan}
import scalafim.fmri.fit.{DesignMatrix, FitError, Ols, ResponseBlock}
import scalafim.fmri.fit.profile.DecodeStatus
import scalafim.fmri.hrf.{Hrf, Hrfs, Seconds}
import scalafim.fmri.hrf.regressor.Regressor
import scalafim.phrfcmp.ingest.{FitInputs, Matrix}
import scalafim.phrfcmp.prep.{NativeArm, PrepRefusal, Whiten, WhitenedArrays}
import scalafim.phrfcmp.score.{ConditionResponse, ERespError, EResp, KernelBasis, ResponseGrid}

/** The condition arms of the pilot (design 2.1). */
enum ConditionArm(val id: String):
  case Can extends ConditionArm("can")
  case Inf3 extends ConditionArm("inf3")
  case Fir extends ConditionArm("fir")
  case Phrf extends ConditionArm("phrf-gauss")

  /** The shared-input identity this arm is bound to (design 2.0.5). */
  def nativeArm: NativeArm = this match
    case Can  => NativeArm.Can
    case Inf3 => NativeArm.Inf3
    case Fir  => NativeArm.Fir
    case Phrf => NativeArm.PhrfInput

object ConditionArm:
  val All: Vector[ConditionArm] = Vector(Can, Inf3, Fir, Phrf)

/** Why a refusal (a method declining, by design, to give an answer). */
enum RefusalKind:
  case Decode(status: DecodeStatus)
  /** PHRF refused the whole dataset at `stage`; `error` is the constructor name only. */
  case DatasetLevel(stage: String, error: String)

  def code: String = this match
    case Decode(s)            => s"decode.${s.productPrefix}"
    case DatasetLevel(st, e)  => s"dataset.$st.$e"

/** Why an arm failed (a harness or numerical failure, not a refusal). Codes are constructor names, never values. */
enum FailureKind:
  case Prep(refusal: PrepRefusal)
  case KindMismatch
  /** The inputs are not the ones the common preparation was built from (whitened `y` differs bit for bit). */
  case InputsNotPrepared
  case Inconsistent(detail: String)
  case Design(detail: String)
  case Fit(error: FitError)
  case NonFiniteCoefficient
  case Response(error: ERespError)
  case Setup(stage: String, error: String)

  def code: String = this match
    case Prep(r)       => s"prep.${r.productPrefix}"
    case KindMismatch  => "kind-mismatch"
    case InputsNotPrepared => "inputs-not-prepared"
    case Inconsistent(_) => "inconsistent"
    case Design(_)     => "design"
    case Fit(e)        => s"fit.${e.productPrefix}"
    case NonFiniteCoefficient => "non-finite-coefficient"
    case Response(e)   => s"response.${e.productPrefix}"
    case Setup(st, e)  => s"setup.$st.$e"

/** Per voxel per arm (design 2.3). */
enum ConditionArmStatus:
  case Estimated
  case Refused(kind: RefusalKind)
  case Failed(kind: FailureKind)
  case NotRun

  def code: String = this match
    case Estimated  => "estimated"
    case Refused(k) => s"refused:${k.code}"
    case Failed(k)  => s"failed:${k.code}"
    case NotRun     => "not-run"

  def isEstimated: Boolean = this == Estimated

/** What PHRF decoded for a delivered voxel (also for refused ones, as a diagnostic: a scorer must not use those). */
final case class PhrfVoxelDetail(
    coordinates: Vector[Double],
    conditionMeans: Vector[Double],
    decode: DecodeStatus,
    penalizedEnergy: Double
)

/**
  * What reached the PHRF executor as observed-family evidence: the fingerprint the prepared run reports
  * (`setup.observedAdmissionFingerprint`, present only if the policy carried the admission) and the number of held-out
  * shape points the certificate was built from.
  */
final case class PhrfAdmissionSummary(fingerprint: Option[String], heldOutPoints: Int, maxProjectorError: Double)

/** One voxel's outcome. `response` is `Some` exactly when `status` is `Estimated`. */
final case class ConditionVoxel(voxel: Int, status: ConditionArmStatus, response: Option[ConditionResponse], phrf: Option[PhrfVoxelDetail])

/**
  * One arm on one dataset.
  *
  * @param inputSha256        hash of the shared whitened arrays the arm was handed (identical across arms)
  * @param eventCoefficients  native arms: event coefficients, `(conditions * basis) x voxels`, condition-major then basis
  * @param route              PHRF: the executor route (`direct-condition-compact`)
  */
final case class ConditionArmResult(
    arm: ConditionArm,
    inputSha256: String,
    voxels: Vector[ConditionVoxel],
    eventCoefficients: Option[Matrix],
    route: Option[String],
    admission: Option[PhrfAdmissionSummary] = None
):
  def statuses: Vector[ConditionArmStatus] = voxels.map(_.status)

/** The generator events of a condition cell. */
final case class ConditionEvents(onset: Array[Double], cond: Array[Int], run: Array[Int], duration: Array[Double], conditions: Int)

object ConditionEvents:
  def fromInputs(in: FitInputs): Either[FailureKind, ConditionEvents] =
    val n = in.evOnset.length
    if in.evCond.length != n || in.evRun.length != n || in.evDuration.length != n then
      Left(FailureKind.Inconsistent("event arrays differ in length"))
    else if n == 0 then Left(FailureKind.Inconsistent("no events"))
    else if in.evCond.exists(_ < 0) then Left(FailureKind.Inconsistent("negative condition index"))
    else Right(ConditionEvents(in.evOnset, in.evCond, in.evRun, in.evDuration, in.evCond.max + 1))

/** Design of the native basis arms (design 2.1): truncated design kernels, untruncated E-resp kernels. */
object ConditionDesigns:

  /** The kernel convolved with events: SPM kernels at the library span (24 s), FIR 1 s bins over `[0, H)`. */
  def designHrf(arm: ConditionArm, horizonSeconds: Int): Option[Hrf] = arm match
    case ConditionArm.Can  => Some(Hrfs.SPMG1)
    case ConditionArm.Inf3 => Some(Hrfs.SPMG3)
    case ConditionArm.Fir  => Some(Hrfs.fir(nBasis = horizonSeconds, span = Seconds(horizonSeconds.toDouble)))
    case ConditionArm.Phrf => None

  def responseBasis(arm: ConditionArm, horizonSeconds: Int): Option[KernelBasis] = arm match
    case ConditionArm.Can  => Some(KernelBasis.Canonical)
    case ConditionArm.Inf3 => Some(KernelBasis.InformedThree)
    case ConditionArm.Fir  => Some(KernelBasis.Fir(horizonSeconds, 1.0))
    case ConditionArm.Phrf => None

  /**
    * The unwhitened event design, `T x (conditions * nbasis)`, condition-major then basis (fmrireg column order). Each
    * run is evaluated at its own scan times with exact-lag convolution (`EvalMethod.Loop`), so a 0.1 s onset grid
    * carries no quadrature error.
    */
  def build(hrf: Hrf, events: ConditionEvents, sampleTime: Array[Double], segments: Vector[TimeSegment]): Either[FailureKind, Matrix] =
    val nb = hrf.nbasis
    val cols = events.conditions * nb
    val t = segments.last.endExclusive
    if sampleTime.length != t then Left(FailureKind.Inconsistent("sample_time length differs from the run layout"))
    else
      val out = new Array[Double](t * cols)
      var failure: Option[FailureKind] = None
      segments.foreach { seg =>
        if failure.isEmpty then
          val grid = (seg.start until seg.endExclusive).map(i => sampleTime(i))
          var c = 0
          while c < events.conditions && failure.isEmpty do
            val idx = events.onset.indices.filter(e => events.run(e) == seg.runIndex && events.cond(e) == c)
            if idx.nonEmpty then
              Regressor.validated(idx.map(events.onset), hrf, idx.map(events.duration)) match
                case Left(err) => failure = Some(FailureKind.Design(err.message))
                case Right(reg) =>
                  val m = Regressor.evaluate(reg, grid, method = Regressor.EvalMethod.Loop)
                  var i = 0
                  while i < grid.length do
                    var b = 0
                    while b < nb do
                      out((seg.start + i) * cols + c * nb + b) = m(i, b)
                      b += 1
                    i += 1
            c += 1
      }
      failure match
        case Some(f) => Left(f)
        case None    => Matrix.of(t, cols, out).left.map(e => FailureKind.Inconsistent(e.message))

/** Native comparators: shared whitened arrays, arm design whitened with the shared plan, strict-full-rank OLS (= GLS). */
object NativeConditionFit:

  final case class Fit(coefficients: Matrix, voxels: Int)

  /** `design` is the unwhitened event design; the plan is the shared global AR(1) plan. */
  def fit(design: Matrix, plan: WhiteningPlan, arrays: WhitenedArrays): Either[FailureKind, Fit] =
    for
      xw <- Whiten.columns(plan, design).left.map(FailureKind.Prep(_))
      ne = xw.cols
      t = xw.rows
      nuis = arrays.nuisance
      ints = arrays.runIntercepts
      _ <- Either.cond(nuis.rows == t && ints.rows == t && arrays.y.cols == t, (), FailureKind.Inconsistent("whitened arrays and design disagree on rows"))
      full = DMat.tabulate(t, ne + nuis.cols + ints.cols) { (r, c) =>
        if c < ne then xw(r, c) else if c < ne + nuis.cols then nuis(r, c - ne) else ints(r, c - ne - nuis.cols)
      }
      x <- DesignMatrix.fromMatrix(full).left.map(FailureKind.Fit(_))
      y <- ResponseBlock.fromMatrix(DMat.tabulate(t, arrays.y.rows)((r, v) => arrays.y(v, r))).left.map(FailureKind.Fit(_))
      ols <- Ols.fit(x, y).left.map(FailureKind.Fit(_))
    yield
      val v = y.voxels
      val data = Array.tabulate(ne * v)(k => ols.coefficients(k / v, k % v))
      Fit(Matrix.of(ne, v, data).fold(e => throw new IllegalStateException(e.message), identity), v)

  /** Per-voxel outcomes from event coefficients; non-finite voxels fail individually. */
  def outcomes(fit: Fit, basis: KernelBasis, conditions: Int, grid: ResponseGrid): Vector[ConditionVoxel] =
    Vector.tabulate(fit.voxels) { v =>
      val beta = Array.tabulate(fit.coefficients.rows)(r => fit.coefficients(r, v))
      if beta.exists(b => !b.isFinite) then ConditionVoxel(v, ConditionArmStatus.Failed(FailureKind.NonFiniteCoefficient), None, None)
      else
        EResp.fromBasis(grid, basis, beta, conditions) match
          case Right(resp) => ConditionVoxel(v, ConditionArmStatus.Estimated, Some(resp), None)
          case Left(e)     => ConditionVoxel(v, ConditionArmStatus.Failed(FailureKind.Response(e)), None, None)
    }
