package scalafim.fmri.model

import scalafim.fmri.hrf.linalg.Mat

/** Typed, scan-aligned confound preparation.  This layer prepares columns and
  * censor receipts only; callers decide how an excluded run is handled before
  * constructing a model or estimator. */
enum MotionExpansion:
  case Raw6, RawAndDerivative12, Friston24

enum ConfoundError:
  case InvalidSpec(detail: String)
  case InvalidInput(run: Int, detail: String)
  case MissingInput(run: Int, name: String)
  case ExcludedRuns(runs: Vector[ExcludedConfoundRun])

  def message: String = this match
    case InvalidSpec(detail) => s"invalid confound specification: $detail"
    case InvalidInput(run, detail) => s"invalid confound input for run $run: $detail"
    case MissingInput(run, name) => s"run $run requires $name input"
    case ExcludedRuns(runs) => s"confound preparation excluded runs: ${runs.map(_.run).mkString(", ")}; caller must handle exclusion explicitly"

/** Censor values strictly greater than `threshold`, include neighbouring scans,
  * then censor retained segments shorter than `minimumRetainedSegment`. */
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

/** Inputs are per run and deliberately contain no implicit imputation or PCA. */
final case class ConfoundRunInput(
    motion6: Mat,
    acompcor: Mat,
    whiteMatter: Option[Vector[Double]] = None,
    csf: Option[Vector[Double]] = None,
    globalSignal: Option[Vector[Double]] = None,
    framewiseDisplacement: Option[Vector[Double]] = None
)

final case class CensorReceipt(
    censoredScans: Vector[Int],
    retainedScans: Vector[Int],
    initialFdScans: Vector[Int],
    shortSegmentScans: Vector[Int]
):
  def censoredCount: Int = censoredScans.length

final case class PreparedConfoundRun(run: Int, matrix: Mat, columnNames: Vector[String], censor: Option[CensorReceipt]):
  require(matrix.cols == columnNames.length, "prepared confound names must match columns")
  def toSampled: Either[ConfoundError, SampledRegressorRun] =
    SampledRegressorRun.fromColumns(columnNames.zipWithIndex.map { (name, column) =>
      name -> Vector.tabulate(matrix.rows)(row => matrix(row, column))
    }*).left.map(error => ConfoundError.InvalidInput(run, error.message))

final case class ExcludedConfoundRun(run: Int, censor: CensorReceipt, censoredFraction: Double, maximumCensoredFraction: Double)

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
          result <- prepareRun(spec, index + 1, input)
        yield result match
          case Left(excluded) => prepared.copy(excluded = prepared.excluded :+ excluded)
          case Right(included) => prepared.copy(included = prepared.included :+ included)
      }

  private def prepareRun(spec: ConfoundSpec, run: Int, input: ConfoundRunInput): Either[ConfoundError, Either[ExcludedConfoundRun, PreparedConfoundRun]] =
    val rows = input.motion6.rows
    for
      _ <- requireMatrix(run, "motion", input.motion6, rows, 6)
      _ <- requireMatrix(run, "aCompCor", input.acompcor, rows, -1)
      _ <- if input.acompcor.cols >= spec.acompcorComponents then Right(()) else Left(ConfoundError.InvalidInput(run, s"aCompCor has ${input.acompcor.cols} components; ${spec.acompcorComponents} requested"))
      _ <- requireOptional(run, "white matter", input.whiteMatter, rows, spec.includeWhiteMatter)
      _ <- requireOptional(run, "CSF", input.csf, rows, spec.includeCsf)
      _ <- requireOptional(run, "global signal", input.globalSignal, rows, spec.includeGlobalSignal)
      receipt <- censor(spec.censor, run, input.framewiseDisplacement, rows)
      result <-
        val fraction = receipt.fold(0.0)(value => value.censoredScans.length.toDouble / rows.toDouble)
        spec.censor match
          case Some(policy) if fraction > policy.maximumCensoredFraction =>
            Right(Left(ExcludedConfoundRun(run, receipt.get, fraction, policy.maximumCensoredFraction)))
          case _ =>
            val preparedColumns = columns(spec, input, rows, receipt)
            preparedColumns.collectFirst { case (name, values) if values.exists(value => !value.isFinite) => name } match
              case Some(name) =>
                Left(ConfoundError.InvalidInput(run, s"derived confound column '$name' contains a non-finite value"))
              case None =>
                val data = Array.tabulate(rows * preparedColumns.length)(index => preparedColumns(index % preparedColumns.length)._2(index / preparedColumns.length))
                Right(Right(PreparedConfoundRun(run, Mat.unsafe(rows, preparedColumns.length, data), preparedColumns.map(_._1), receipt)))
    yield result

  private def requireMatrix(run: Int, name: String, matrix: Mat, rows: Int, columns: Int): Either[ConfoundError, Unit] =
    if matrix.rows == 0 then Left(ConfoundError.InvalidInput(run, s"$name must contain at least one scan"))
    else if matrix.rows != rows || (columns >= 0 && matrix.cols != columns) then Left(ConfoundError.InvalidInput(run, s"$name must have $rows rows${if columns >= 0 then s" and $columns columns" else ""}"))
    else if matrix.data.exists(value => !value.isFinite) then Left(ConfoundError.InvalidInput(run, s"$name contains a non-finite value"))
    else Right(())
  private def requireOptional(run: Int, name: String, value: Option[Vector[Double]], rows: Int, required: Boolean): Either[ConfoundError, Unit] =
    if required && value.isEmpty then Left(ConfoundError.MissingInput(run, name))
    else value match
      case Some(values) if values.length != rows => Left(ConfoundError.InvalidInput(run, s"$name has ${values.length} rows; expected $rows"))
      case Some(values) if values.exists(number => !number.isFinite) => Left(ConfoundError.InvalidInput(run, s"$name contains a non-finite value"))
      case _ => Right(())

  private def columns(spec: ConfoundSpec, input: ConfoundRunInput, rows: Int, receipt: Option[CensorReceipt]): Vector[(String, Vector[Double])] =
    val raw = MotionNames.zipWithIndex.map((name, column) => name -> Vector.tabulate(rows)(row => input.motion6(row, column)))
    val derivative = raw.map((name, values) => s"${name}_derivative1" -> values.indices.map(index => if index == 0 then 0.0 else values(index) - values(index - 1)).toVector)
    val motion = spec.motion match
      case MotionExpansion.Raw6 => raw
      case MotionExpansion.RawAndDerivative12 => raw ++ derivative
      case MotionExpansion.Friston24 => raw ++ derivative ++ (raw ++ derivative).map((name, values) => s"${name}_squared" -> values.map(value => value * value))
    val components = (0 until spec.acompcorComponents).toVector.map(index => f"acompcor_${index + 1}%02d" -> Vector.tabulate(rows)(row => input.acompcor(row, index)))
    val tissues = Vector(
      if spec.includeWhiteMatter then Some("white_matter" -> input.whiteMatter.get) else None,
      if spec.includeCsf then Some("csf" -> input.csf.get) else None,
      if spec.includeGlobalSignal then Some("global_signal" -> input.globalSignal.get) else None
    ).flatten
    val spikes = receipt.toVector.flatMap(_.censoredScans.map(scan => f"censor_scan_${scan + 1}%04d" -> Vector.tabulate(rows)(row => if row == scan then 1.0 else 0.0)))
    motion ++ components ++ tissues ++ spikes

  private def censor(policy: Option[FdCensorPolicy], run: Int, fd: Option[Vector[Double]], rows: Int): Either[ConfoundError, Option[CensorReceipt]] = policy match
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
          val selected = censored.indices.filter(censored).toVector
          Right(Some(CensorReceipt(selected, censored.indices.filterNot(censored).toVector, initial, short.result())))
