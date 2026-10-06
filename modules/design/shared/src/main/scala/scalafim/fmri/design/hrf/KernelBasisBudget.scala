package scalafim.fmri.design.hrf

import scalafim.fmri.hrf.family.ShapeChartError

enum KernelBasisAdmissionError:
  case LimitExceeded(quantity: String, requested: Double, maximum: Long)
  case InvalidChartWidth(axis: String, width: Double)
  case InvalidChartPoint(error: ShapeChartError)
  case NonFiniteLag(value: Double)

  def message: String = this match
    case LimitExceeded(quantity, requested, maximum) =>
      s"kernel compiler requests $requested $quantity, maximum is $maximum"
    case InvalidChartWidth(axis, width) => s"kernel compiler chart '$axis' has invalid width $width"
    case InvalidChartPoint(error) => error.message
    case NonFiniteLag(value) => s"kernel compiler has non-finite lag $value"

/** Shapes actually retained after certification selects a rank. Certificates
  * still contain three error vectors for every tested rank, not only the chosen
  * rank. Counts describe primitive cells; Scala object headers are excluded.
  */
final case class KernelBasisAllocation(
    rank: Int,
    phiCells: Long,
    singularValues: Int,
    certificateRanks: Int,
    certificateErrorValues: Long)

/** Inspectable admission estimate for the emitted training matrix, full economy
  * SVD and certification. The retained rank is chosen from `1..availableRank`.
  */
final case class KernelBasisEstimate private[hrf] (
    fineCount: Int,
    gridPoints: Int,
    trainingComponents: Int,
    jetComponents: Int,
    trainingColumns: Int,
    trainingCells: Long,
    thinRank: Int,
    thinLeftCells: Long,
    thinRightCells: Long,
    svdScratchCells: Long,
    svdCanonicalCells: Long,
    svdGramCells: Long,
    availableRank: Int,
    jetScratchCells: Long,
    phiCellsAtMaxRank: Long,
    certificateErrorValues: Long,
    tailSamples: Int,
    storageCells: Long,
    trainingWork: Long,
    spectralWork: Long,
    certificationWork: Long):
  def retained(rank: Int): KernelBasisAllocation =
    require(rank >= 1 && rank <= availableRank, s"retained rank $rank outside 1..$availableRank")
    KernelBasisAllocation(rank, fineCount.toLong * rank.toLong, rank,
      availableRank, certificateErrorValues)

/** Fixed portable compiler policy: requests are refused, never coarsened.
  *
  * Gale 54e73f8e's lexical `SpectralBackend.none` uses the pure full/economy SVD:
  * k=min(rows,cols), raw tall input clone rows*cols and right factor k*k,
  * canonical factors rows*k+k*cols, plus k*k orthogonality Gram matrices and
  * residual vectors. Requested maxRank does not reduce these full-SVD shapes.
  * Storage conservatively retains all these shapes together, both jet buffers,
  * full and selected phi, certificate vectors, and one tail diagnostic. It is
  * a primitive-cell envelope, not a heap-byte or garbage-collection guarantee.
  * The spectral work envelope counts paired-rotation or scalar-accumulation
  * iterations through reduction, bounded QR sweeps and full-factor diagnostics.
  * One unit can read/write multiple cells; these are declared loop-work units,
  * not primitive memory accesses, CPU instructions or elapsed time.
  * Custom family callback internals/overridden tail diagnostics are not inferred.
  */
object KernelBasisBudget:
  val MaxFineSamples: Int = 100_000
  val MaxGridPoints: Int = 100_000
  val MaxTrainingColumns: Int = 1_000_000
  val MaxArrayCells: Int = 2_000_000
  val MaxStorageCells: Long = 16_000_000L
  val MaxTrainingWork: Long = 20_000_000L
  val MaxSpectralWork: Long = 100_000_000_000L
  val MaxCertificationWork: Long = 100_000_000L

  private[hrf] def checked(quantity: String, count: Double, maximum: Long): Either[KernelBasisError, Double] =
    if !count.isFinite || count < 0.0 || count > maximum.toDouble then
      Left(KernelBasisError.Admission(KernelBasisAdmissionError.LimitExceeded(quantity, count, maximum)))
    else Right(count)

  private[hrf] def coordinate(spec: KernelBasisSpec, axis: Int, index: Int): Double =
    // Preserve the original operation order and its admitted IEEE values.
    spec.family.chart.lower(axis) + spec.family.chart.width(axis) * index / (spec.nodesPerAxis(axis) - 1)

  /** These coordinates come only from the compiler's validated node indices or
    * uniform random fractions. Mathematically they lie in the chart, but the
    * original operation order can overshoot an endpoint by a few ULPs. Preserve
    * those finite IEEE values rather than clamp or reorder scientific inputs.
    */
  private[hrf] def generatedPoint(spec: KernelBasisSpec, coordinates: Vector[Double]): Either[KernelBasisError, scalafim.fmri.hrf.family.ShapePoint] =
    var axis = 0
    while axis < coordinates.length do
      if !coordinates(axis).isFinite then
        return Left(KernelBasisError.Admission(KernelBasisAdmissionError.InvalidChartPoint(
          ShapeChartError.NonFinite(spec.family.chart.names(axis), coordinates(axis)))))
      axis += 1
    Right(scalafim.fmri.hrf.family.ShapePoint.unsafe(coordinates))

  private def validateChart(spec: KernelBasisSpec): Either[KernelBasisError, Unit] =
    val chart = spec.family.chart
    var axis = 0
    while axis < chart.dimension do
      val width = chart.width(axis)
      if !width.isFinite || width <= 0.0 then
        return Left(KernelBasisError.Admission(KernelBasisAdmissionError.InvalidChartWidth(chart.names(axis), width)))
      var index = 0
      while index < spec.nodesPerAxis(axis) do
        val value = coordinate(spec, axis, index)
        val error =
          if !value.isFinite then Some(ShapeChartError.NonFinite(chart.names(axis), value))
          else None
        error match
          case Some(e) => return Left(KernelBasisError.Admission(KernelBasisAdmissionError.InvalidChartPoint(e)))
          case None => ()
        index += 1
      axis += 1
    Right(())

  def estimate(spec: KernelBasisSpec): Either[KernelBasisError, KernelBasisEstimate] =
    val family = spec.family
    val d = family.dimension
    if spec.nodesPerAxis.length != d then Left(KernelBasisError.InvalidSpec(s"nodesPerAxis has ${spec.nodesPerAxis.length} entries for a $d-dimensional chart"))
    else if spec.nodesPerAxis.exists(_ < 2) then Left(KernelBasisError.InvalidSpec("every axis needs at least two nodes"))
    else if !(spec.tolerance > 0.0 && spec.tolerance < 1.0) then Left(KernelBasisError.InvalidSpec(s"tolerance must lie in (0, 1), got ${spec.tolerance}"))
    else if spec.maxRank < 1 then Left(KernelBasisError.InvalidSpec(s"maxRank must be >= 1, got ${spec.maxRank}"))
    else if spec.fineStep.value >= family.horizon.value then Left(KernelBasisError.InvalidSpec("fineStep must be smaller than the family horizon"))
    else if spec.heldOutPoints < 1 then Left(KernelBasisError.InvalidSpec("heldOutPoints must be >= 1"))
    else family.validateTailRelativeEnergyGrid(spec.fineStep).left.map(KernelBasisError.Summary.apply).flatMap: _ =>
      val n = math.floor(family.horizon.value / spec.fineStep.value) + 1.0
      val jets = family.jetComponents.toDouble
      val components = if spec.includeDerivatives then jets else 1.0
      val points = spec.nodesPerAxis.iterator.map(_.toDouble).product
      val columns = points * components
      val matrix = n * columns
      val k = math.min(n, columns)
      val available = math.min(spec.maxRank.toDouble, k)
      val tail = math.max(2.0, math.ceil(4.0 * family.horizon.value / spec.fineStep.value) + 1.0)
      val left = n * k
      val right = k * columns
      val raw = matrix + k * k + 3.0 * k
      val canonical = left + right
      // Residual products and subtraction vectors, sort/scalar buffers and two
      // Gram matrices are conservatively charged even when lifetimes overlap.
      val residual = 8.0 * (n + columns) + 12.0 * k
      val gram = 2.0 * k * k
      val jet = n * jets
      val phi = n * available
      val cert = 3.0 * available
      val storage = matrix + raw + canonical + residual + gram + n +
        2.0 * jet + 2.0 * phi + 2.0 * cert + 2.0 * tail + 2.0 * available
      val training = points * (2.0 * d.toDouble + 3.0 * components * n) +
        spec.nodesPerAxis.iterator.map(_.toDouble).sum + d.toDouble
      // At 60 QR sweeps/value, paired rotations are bounded by
      // 30*(q+k)*k*(k-1), cancellation by 30.5*q*k*(k+1), q=max(n,columns).
      // 128*matrix*k also leaves room for reduction/accumulation loops; the
      // additional term charges both residual matvecs and Gram diagnostics.
      val spectral = 128.0 * matrix * k + 4.0 * (n + columns) * k * k
      // Certification always requests a full jet, also for value-only training.
      // Tail work includes lag generation, declared values, finite scans and
      // trapezoid energy; point generation/validation includes the tail API.
      val certification = spec.heldOutPoints.toDouble *
        (3.0 * d.toDouble + 4.0 * tail + 2.0 * jet + jets * available * n + jets * available)
      for
        _ <- checked("fine samples", n, MaxFineSamples)
        _ <- checked("shape grid points", points, MaxGridPoints)
        _ <- checked("training columns", columns, MaxTrainingColumns)
        _ <- checked("training matrix cells", matrix, MaxArrayCells)
        _ <- checked("jet scratch cells", jet, MaxArrayCells)
        _ <- checked("thin left factor cells", left, MaxArrayCells)
        _ <- checked("thin right factor cells", right, MaxArrayCells)
        _ <- checked("right factor/Gram cells", k * k, MaxArrayCells)
        _ <- checked("phi cells", phi, MaxArrayCells)
        _ <- checked("storage cells", storage, MaxStorageCells)
        _ <- checked("training work", training, MaxTrainingWork)
        _ <- checked("spectral work", spectral, MaxSpectralWork)
        _ <- checked("certification work", certification, MaxCertificationWork)
        _ <-
          val last = (n - 1.0) * spec.fineStep.value
          if last.isFinite then Right(()) else Left(KernelBasisError.Admission(KernelBasisAdmissionError.NonFiniteLag(last)))
        _ <- validateChart(spec)
      yield KernelBasisEstimate(n.toInt, points.toInt, components.toInt, jets.toInt,
        columns.toInt, matrix.toLong, k.toInt, left.toLong, right.toLong,
        raw.toLong, canonical.toLong, gram.toLong, available.toInt, jet.toLong,
        phi.toLong, cert.toLong, tail.toInt, storage.toLong,
        training.toLong, spectral.toLong, certification.toLong)
