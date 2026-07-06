package scalafim.fmri.motion

import scalafim.image.{Affine, DMat, NeuroVec, NeuroVol}

final case class DisplacementSummary(
    median: Double,
    p95: Double,
    max: Double
)

object MotionMetrics:
  def framewiseDisplacement(
      trace: MotionTrace,
      radius: HeadRadius = HeadRadius.default
  ): Vector[Double] =
    val out = Array.fill(trace.length)(0.0)
    var t = 1
    while t < trace.length do
      val a = trace.poses(t)
      val b = trace.poses(t - 1)
      out(t) =
        math.abs(a.tx - b.tx) +
          math.abs(a.ty - b.ty) +
          math.abs(a.tz - b.tz) +
          radius.value * (
            math.abs(a.rx - b.rx) +
              math.abs(a.ry - b.ry) +
              math.abs(a.rz - b.rz)
          )
      t += 1
    out.toVector

  def dvars(
      run: NeuroVec[Double],
      mask: Option[NeuroVol[Boolean]] = None,
      robust: Boolean = false
  ): Either[MotionError, Vector[Double]] =
    validateMask(run, mask).map { maskVol =>
      val dims = run.space.spatialDims
      val nSpatial = dims.product
      val nt = run.nVolumes
      val out = Array.fill(nt)(Double.NaN)
      var t = 1
      while t < nt do
        var ss = 0.0
        var n = 0
        var lin = 0
        while lin < nSpatial do
          val include = maskVol.forall(_.values.data(lin))
          if include then
            val a = run.values.data(lin + (t - 1) * nSpatial)
            val b = run.values.data(lin + t * nSpatial)
            val d = b - a
            ss += d * d
            n += 1
          lin += 1
        if n > 0 then out(t) = math.sqrt(ss / n.toDouble)
        t += 1

      if robust then robustClip3xMedian(out).toVector else out.toVector
    }

  def transformDisplacement(
      motion: RigidPose,
      radius: HeadRadius = HeadRadius.default,
      reference: Option[RigidPose] = None
  ): Double =
    val points = radiusCube(radius.value)
    val mat = motion.toMatrix
    val ref = reference.map(_.toMatrix)
    var i = 0
    var sum = 0.0
    while i < points.length do
      val mapped = apply3(mat, points(i))
      val target = ref.map(apply3(_, points(i))).getOrElse(points(i))
      sum += distance(mapped, target)
      i += 1
    sum / points.length.toDouble

  def maskedDisplacements(
      mask: NeuroVol[Boolean],
      motion: RigidPose,
      reference: Option[RigidPose] = None
  ): Vector[Double] =
    val mat = motion.toMatrix
    val ref = reference.map(_.toMatrix)
    val n = mask.space.spatialDims.product
    val out = Vector.newBuilder[Double]
    var lin = 0
    while lin < n do
      if mask.values.data(lin) then
        val grid = mask.space.indexToGrid3D(lin).map(_.toDouble)
        val point = mask.space.indexToCoord(grid)
        val mapped = apply3(mat, point)
        val target = ref.map(apply3(_, point)).getOrElse(point)
        out += distance(mapped, target)
      lin += 1
    out.result()

  def maskedDisplacementSummary(
      mask: NeuroVol[Boolean],
      motion: RigidPose,
      reference: Option[RigidPose] = None
  ): Either[MotionError, DisplacementSummary] =
    val values = maskedDisplacements(mask, motion, reference).filter(_.isFinite).sorted
    if values.isEmpty then Left(MotionError.ShapeMismatch("mask", Vector(1), Vector(0)))
    else
      Right(
        DisplacementSummary(
          median = quantileSorted(values, 0.5),
          p95 = quantileSorted(values, 0.95),
          max = values.last
        )
      )

  private[motion] def validateMask(
      run: NeuroVec[Double],
      mask: Option[NeuroVol[Boolean]]
  ): Either[MotionError, Option[NeuroVol[Boolean]]] =
    mask match
      case None => Right(None)
      case Some(m) =>
        if m.space.spatialDims != run.space.spatialDims then
          Left(MotionError.ShapeMismatch("mask", run.space.spatialDims, m.space.spatialDims))
        else Right(mask)

  private[motion] def robustClip3xMedian(values: Array[Double]): Array[Double] =
    val finite = values.filter(_.isFinite).sorted
    if finite.isEmpty then values.clone()
    else
      val med = quantileSorted(finite.toVector, 0.5)
      val cap = 3.0 * med
      val out = values.clone()
      var i = 0
      while i < out.length do
        if out(i).isFinite && out(i) > cap then out(i) = cap
        i += 1
      out

  private[motion] def quantileSorted(values: Vector[Double], p: Double): Double =
    require(values.nonEmpty, "values must be non-empty")
    if values.length == 1 then values.head
    else
      val h = (values.length - 1).toDouble * p
      val lo = math.floor(h).toInt
      val hi = math.ceil(h).toInt
      if lo == hi then values(lo)
      else
        val w = h - lo.toDouble
        values(lo) * (1.0 - w) + values(hi) * w

  private def radiusCube(radius: Double): Array[Vector[Double]] =
    Array(
      Vector(radius, radius, radius),
      Vector(radius, radius, -radius),
      Vector(radius, -radius, radius),
      Vector(radius, -radius, -radius),
      Vector(-radius, radius, radius),
      Vector(-radius, radius, -radius),
      Vector(-radius, -radius, radius),
      Vector(-radius, -radius, -radius)
    )

  private def apply3(matrix: DMat, point: Vector[Double]): Vector[Double] =
    Affine.applyAffine(matrix, point)

  private def distance(a: Vector[Double], b: Vector[Double]): Double =
    val dx = a(0) - b(0)
    val dy = a(1) - b(1)
    val dz = a(2) - b(2)
    math.sqrt(dx * dx + dy * dy + dz * dz)
