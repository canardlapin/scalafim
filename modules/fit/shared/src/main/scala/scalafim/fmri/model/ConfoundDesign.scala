package scalafim.fmri.model

import scalafim.fmri.design.{ColumnId, RunIndex, ScanIndex}
import scalafim.fmri.hrf.linalg.Mat

/** Typed, scan-aligned confound preparation.  This layer prepares columns and
  * censor receipts only; callers decide how an excluded run is handled before
  * constructing a model or estimator.
  *
  * Column ids are run-qualified (`run1_motion_x`, `run2_censor_scan_0003`) so
  * that nuisance names stay unique when runs are concatenated into one model.
  * Scan positions in receipts and spike names are one-based within their run. */
enum MotionExpansion:
  case Raw6, RawAndDerivative12, Friston24

enum ConfoundError:
  case InvalidSpec(detail: String)
  case InvalidInput(run: RunIndex, detail: String)
  case MissingInput(run: RunIndex, name: String)
  case ExcludedRuns(runs: Vector[ExcludedConfoundRun])
  case NoRetainedScans(run: RunIndex, scans: Int)
  case InsufficientRetainedScans(run: RunIndex, retained: Int, nuisanceColumns: Int)

  def message: String = this match
    case InvalidSpec(detail) => s"invalid confound specification: $detail"
    case InvalidInput(run, detail) => s"invalid confound input for run ${run.oneBased}: $detail"
    case MissingInput(run, name) => s"run ${run.oneBased} requires $name input"
    case ExcludedRuns(runs) => s"confound preparation excluded runs: ${runs.map(_.run.oneBased).mkString(", ")}; caller must handle exclusion explicitly"
    case NoRetainedScans(run, scans) => s"run ${run.oneBased} retains none of its $scans scans after censoring; exclude it explicitly"
    case InsufficientRetainedScans(run, retained, columns) =>
      s"run ${run.oneBased} retains $retained scans but needs at least $columns for its non-spike nuisance columns; the nuisance design would be rank deficient"

/** Censor values strictly greater than `threshold`, include neighbouring scans,
  * then censor retained segments shorter than `minimumRetainedSegment`.
  *
  * A run whose censored fraction exceeds `maximumCensoredFraction` becomes an
  * explicit [[ExcludedConfoundRun]]. The default, 1.0, never excludes a run on
  * fraction alone: an exclusion threshold is a study-level decision, not a
  * library default. Independently of this setting, a run that retains no scans,
  * or fewer scans than its non-spike nuisance columns, always fails with
  * [[ConfoundError.NoRetainedScans]] or [[ConfoundError.InsufficientRetainedScans]]. */
final case class FdCensorPolicy(
    threshold: Double,
    before: Int = 0,
    after: Int = 0,
    minimumRetainedSegment: Int = 1,
    maximumCensoredFraction: Double = 1.0
):
  require(threshold.isFinite && threshold >= 0.0, "FD threshold must be finite and non-negative")
  require(before >= 0 && after >= 0, "FD neighbours must be non-negative")
  require(minimumRetainedSegment >= 1, "minimum retained segment must be positive")
  require(maximumCensoredFraction.isFinite && maximumCensoredFraction >= 0.0 && maximumCensoredFraction <= 1.0, "maximum censored fraction must be in [0, 1]")

object FdCensorPolicy:
  def make(threshold: Double, before: Int = 0, after: Int = 0, minimumRetainedSegment: Int = 1, maximumCensoredFraction: Double = 1.0): Either[ConfoundError, FdCensorPolicy] =
    if !threshold.isFinite || threshold < 0.0 then Left(ConfoundError.InvalidSpec("FD threshold must be finite and non-negative"))
    else if before < 0 || after < 0 then Left(ConfoundError.InvalidSpec("FD neighbours must be non-negative"))
    else if minimumRetainedSegment < 1 then Left(ConfoundError.InvalidSpec("minimum retained segment must be positive"))
    else if !maximumCensoredFraction.isFinite || maximumCensoredFraction < 0.0 || maximumCensoredFraction > 1.0 then Left(ConfoundError.InvalidSpec("maximum censored fraction must be in [0, 1]"))
    else Right(FdCensorPolicy(threshold, before, after, minimumRetainedSegment, maximumCensoredFraction))

final case class ConfoundSpec(
    motion: MotionExpansion = MotionExpansion.Raw6,
    acompcorComponents: Int = 0,
    includeWhiteMatter: Boolean = false,
    includeCsf: Boolean = false,
    includeGlobalSignal: Boolean = false,
    censor: Option[FdCensorPolicy] = None
):
  require(acompcorComponents >= 0, "aCompCor component count must be non-negative")

object ConfoundSpec:
  def make(
      motion: MotionExpansion = MotionExpansion.Raw6,
      acompcorComponents: Int = 0,
      includeWhiteMatter: Boolean = false,
      includeCsf: Boolean = false,
      includeGlobalSignal: Boolean = false,
      censor: Option[FdCensorPolicy] = None
  ): Either[ConfoundError, ConfoundSpec] =
    if acompcorComponents < 0 then Left(ConfoundError.InvalidSpec("aCompCor component count must be non-negative"))
    else Right(ConfoundSpec(motion, acompcorComponents, includeWhiteMatter, includeCsf, includeGlobalSignal, censor))

/** Inputs are per run and deliberately contain no implicit imputation or PCA.
  * `acompcor` may be `None` only when the spec requests zero components. */
final case class ConfoundRunInput(
    motion6: Mat,
    acompcor: Option[Mat] = None,
    whiteMatter: Option[Vector[Double]] = None,
    csf: Option[Vector[Double]] = None,
    globalSignal: Option[Vector[Double]] = None,
    framewiseDisplacement: Option[Vector[Double]] = None
)

/** Censoring evidence for one run; every scan index is one-based within the run. */
final case class CensorReceipt(
    censoredScans: Vector[ScanIndex],
    retainedScans: Vector[ScanIndex],
    initialFdScans: Vector[ScanIndex],
    shortSegmentScans: Vector[ScanIndex]
):
  def censoredCount: Int = censoredScans.length

final case class PreparedConfoundRun(run: RunIndex, matrix: Mat, columnNames: Vector[ColumnId], censor: Option[CensorReceipt]):
  require(matrix.cols == columnNames.length, "prepared confound names must match columns")
  def toSampled: Either[ConfoundError, SampledRegressorRun] =
    SampledRegressorRun.fromColumns(columnNames.zipWithIndex.map { (name, column) =>
      name.value -> Vector.tabulate(matrix.rows)(row => matrix(row, column))
    }*).left.map(error => ConfoundError.InvalidInput(run, error.message))

final case class ExcludedConfoundRun(run: RunIndex, censor: CensorReceipt, censoredFraction: Double, maximumCensoredFraction: Double)

final case class PreparedConfounds(included: Vector[PreparedConfoundRun], excluded: Vector[ExcludedConfoundRun]):
  def toNuisanceRegressors: Either[ConfoundError, NuisanceRegressors] =
    if excluded.nonEmpty then Left(ConfoundError.ExcludedRuns(excluded))
    else
      included.foldLeft[Either[ConfoundError, Vector[SampledRegressorRun]]](Right(Vector.empty)) { (acc, run) =>
        for values <- acc; sampled <- run.toSampled yield values :+ sampled
      }.flatMap { runs =>
        NuisanceRegressors.fromRuns(runs).left.map(error => ConfoundError.InvalidSpec(error.message))
      }

object ConfoundDesign:
  private val MotionNames = Vector("motion_x", "motion_y", "motion_z", "motion_rot_x", "motion_rot_y", "motion_rot_z")

  def prepare(spec: ConfoundSpec, runs: Vector[ConfoundRunInput]): Either[ConfoundError, PreparedConfounds] =
    if runs.isEmpty then Left(ConfoundError.InvalidSpec("at least one run is required"))
    else
      runs.zipWithIndex.foldLeft[Either[ConfoundError, PreparedConfounds]](Right(PreparedConfounds(Vector.empty, Vector.empty))) { case (acc, (input, index)) =>
        for
          prepared <- acc
          result <- prepareRun(spec, RunIndex.unsafeOneBased(index + 1), input)
        yield result match
          case Left(excluded) => prepared.copy(excluded = prepared.excluded :+ excluded)
          case Right(included) => prepared.copy(included = prepared.included :+ included)
      }

  private def prepareRun(spec: ConfoundSpec, run: RunIndex, input: ConfoundRunInput): Either[ConfoundError, Either[ExcludedConfoundRun, PreparedConfoundRun]] =
    val rows = input.motion6.rows
    for
      _ <- requireMatrix(run, "motion", input.motion6, rows, 6)
      acompcor <- input.acompcor match
        case None if spec.acompcorComponents > 0 => Left(ConfoundError.MissingInput(run, "aCompCor"))
        case None => Right(None)
        case Some(matrix) =>
          requireMatrix(run, "aCompCor", matrix, rows, -1).flatMap { _ =>
            if matrix.cols >= spec.acompcorComponents then Right(Some(matrix))
            else Left(ConfoundError.InvalidInput(run, s"aCompCor has ${matrix.cols} components; ${spec.acompcorComponents} requested"))
          }
      _ <- requireOptional(run, "white matter", input.whiteMatter, rows, spec.includeWhiteMatter)
      _ <- requireOptional(run, "CSF", input.csf, rows, spec.includeCsf)
      _ <- requireOptional(run, "global signal", input.globalSignal, rows, spec.includeGlobalSignal)
      receipt <- censor(spec.censor, run, input.framewiseDisplacement, rows)
      result <-
        val censored = receipt.fold(0)(_.censoredScans.length)
        val fraction = censored.toDouble / rows.toDouble
        spec.censor.zip(receipt) match
          case Some((policy, evidence)) if fraction > policy.maximumCensoredFraction =>
            Right(Left(ExcludedConfoundRun(run, evidence, fraction, policy.maximumCensoredFraction)))
          case _ =>
            val preparedColumns = columns(spec, run, input, acompcor, rows, receipt)
            val retained = rows - censored
            val nonSpike = preparedColumns.length - censored
            preparedColumns.collectFirst { case (name, values) if values.exists(value => !value.isFinite) => name } match
              case Some(name) =>
                Left(ConfoundError.InvalidInput(run, s"derived confound column '$name' contains a non-finite value"))
              case None if retained == 0 => Left(ConfoundError.NoRetainedScans(run, rows))
              case None if retained < nonSpike => Left(ConfoundError.InsufficientRetainedScans(run, retained, nonSpike))
              case None =>
                columnIds(run, preparedColumns.map(_._1)).map { ids =>
                  val data = Array.tabulate(rows * preparedColumns.length)(index => preparedColumns(index % preparedColumns.length)._2(index / preparedColumns.length))
                  Right(PreparedConfoundRun(run, Mat.unsafe(rows, preparedColumns.length, data), ids, receipt))
                }
    yield result

  private def columnIds(run: RunIndex, names: Vector[String]): Either[ConfoundError, Vector[ColumnId]] =
    names.foldLeft[Either[ConfoundError, Vector[ColumnId]]](Right(Vector.empty)) { (acc, name) =>
      for
        ids <- acc
        id <- ColumnId(name).left.map(error => ConfoundError.InvalidInput(run, error.message))
      yield ids :+ id
    }

  private def requireMatrix(run: RunIndex, name: String, matrix: Mat, rows: Int, columns: Int): Either[ConfoundError, Unit] =
    if matrix.rows == 0 then Left(ConfoundError.InvalidInput(run, s"$name must contain at least one scan"))
    else if matrix.rows != rows || (columns >= 0 && matrix.cols != columns) then Left(ConfoundError.InvalidInput(run, s"$name must have $rows rows${if columns >= 0 then s" and $columns columns" else ""}"))
    else if matrix.data.exists(value => !value.isFinite) then Left(ConfoundError.InvalidInput(run, s"$name contains a non-finite value"))
    else Right(())
  private def requireOptional(run: RunIndex, name: String, value: Option[Vector[Double]], rows: Int, required: Boolean): Either[ConfoundError, Unit] =
    if required && value.isEmpty then Left(ConfoundError.MissingInput(run, name))
    else value match
      case Some(values) if values.length != rows => Left(ConfoundError.InvalidInput(run, s"$name has ${values.length} rows; expected $rows"))
      case Some(values) if values.exists(number => !number.isFinite) => Left(ConfoundError.InvalidInput(run, s"$name contains a non-finite value"))
      case _ => Right(())

  private def columns(spec: ConfoundSpec, run: RunIndex, input: ConfoundRunInput, acompcor: Option[Mat], rows: Int, receipt: Option[CensorReceipt]): Vector[(String, Vector[Double])] =
    val prefix = s"run${run.oneBased}_"
    val raw = MotionNames.zipWithIndex.map((name, column) => name -> Vector.tabulate(rows)(row => input.motion6(row, column)))
    val derivative = raw.map((name, values) => s"${name}_derivative1" -> values.indices.map(index => if index == 0 then 0.0 else values(index) - values(index - 1)).toVector)
    val motion = spec.motion match
      case MotionExpansion.Raw6 => raw
      case MotionExpansion.RawAndDerivative12 => raw ++ derivative
      case MotionExpansion.Friston24 => raw ++ derivative ++ (raw ++ derivative).map((name, values) => s"${name}_squared" -> values.map(value => value * value))
    val components = acompcor.toVector.flatMap(matrix => (0 until spec.acompcorComponents).toVector.map(index => f"acompcor_${index + 1}%02d" -> Vector.tabulate(rows)(row => matrix(row, index))))
    val tissues = Vector(
      if spec.includeWhiteMatter then input.whiteMatter.map("white_matter" -> _) else None,
      if spec.includeCsf then input.csf.map("csf" -> _) else None,
      if spec.includeGlobalSignal then input.globalSignal.map("global_signal" -> _) else None
    ).flatten
    val spikes = receipt.toVector.flatMap(_.censoredScans.map(scan => f"censor_scan_${scan.oneBased}%04d" -> Vector.tabulate(rows)(row => if row == scan.zeroBased then 1.0 else 0.0)))
    (motion ++ components ++ tissues ++ spikes).map((name, values) => (prefix + name) -> values)

  private def censor(policy: Option[FdCensorPolicy], run: RunIndex, fd: Option[Vector[Double]], rows: Int): Either[ConfoundError, Option[CensorReceipt]] = policy match
    case None => Right(None)
    case Some(value) =>
      fd match
        case None => Left(ConfoundError.MissingInput(run, "framewise displacement"))
        case Some(values) if values.length != rows => Left(ConfoundError.InvalidInput(run, s"framewise displacement has ${values.length} rows; expected $rows"))
        case Some(values) if values.exists(number => !number.isFinite) => Left(ConfoundError.InvalidInput(run, "framewise displacement contains a non-finite value"))
        case Some(values) =>
          val censored = Array.fill(rows)(false)
          val initial = values.indices.filter(index => values(index) > value.threshold).toVector
          initial.foreach { index =>
            val start = math.max(0L, index.toLong - value.before.toLong).toInt
            val end = math.min(rows.toLong - 1L, index.toLong + value.after.toLong).toInt
            var scan = start
            while scan <= end do
              censored(scan) = true
              scan += 1
          }
          val short = Vector.newBuilder[Int]
          var start = 0
          while start < rows do
            while start < rows && censored(start) do start += 1
            val end = start
            while start < rows && !censored(start) do start += 1
            if start - end > 0 && start - end < value.minimumRetainedSegment then
              var scan = end
              while scan < start do
                censored(scan) = true
                short += scan
                scan += 1
          def scans(indices: Vector[Int]): Vector[ScanIndex] = indices.map(index => ScanIndex.unsafeOneBased(index + 1))
          val selected = censored.indices.filter(censored).toVector
          Right(Some(CensorReceipt(scans(selected), scans(censored.indices.filterNot(censored).toVector), scans(initial), scans(short.result()))))
