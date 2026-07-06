package scalafim.image

enum DenseFieldOutside:
  case Zero, QueryPoint

  def value(point: Vector[Double], component: Int): Double =
    this match
      case Zero => 0.0
      case QueryPoint => point(component)

private[image] final class DenseFieldStencil(
    val indices: Array[Int],
    val weights: Array[Double],
    val outsideWeight: Double
):
  require(indices.length == weights.length, "dense field stencil indices/weights mismatch")
  require(outsideWeight >= 0.0 && outsideWeight <= 1.0 + 1e-12, "outside weight must be in [0, 1]")

  def size: Int = indices.length

final case class DenseFieldInterpolationPlan private (
    grid: GridSpec,
    points: Vector[Vector[Double]],
    method: Resample.Method,
    private[image] val stencils: Vector[DenseFieldStencil]
):
  require(points.length == stencils.length, "points/stencils length mismatch")

  def queryCount: Int =
    points.length

  def sample(
      field: NDArray[Double],
      outside: DenseFieldOutside
  ): Either[MorphismError, Vector[Vector[Double]]] =
    DenseFieldInterpolationPlan.validateField(grid, field).map { _ =>
      sampleUnsafe(field, outside)
    }

  private[image] def sampleUnsafe(
      field: NDArray[Double],
      outside: DenseFieldOutside
  ): Vector[Vector[Double]] =
    val out = Vector.newBuilder[Vector[Double]]
    out.sizeHint(points.length)
    var i = 0
    while i < points.length do
      val point = points(i)
      val stencil = stencils(i)
      val sampled = Array.ofDim[Double](3)
      var component = 0
      while component < 3 do
        var sum =
          if stencil.outsideWeight == 0.0 then 0.0
          else stencil.outsideWeight * outside.value(point, component)
        val componentOffset = component * grid.nVoxels
        var j = 0
        while j < stencil.size do
          sum += stencil.weights(j) * field.data(stencil.indices(j) + componentOffset)
          j += 1
        sampled(component) = sum
        component += 1
      out += Vector(sampled(0), sampled(1), sampled(2))
      i += 1
    out.result()

object DenseFieldInterpolationPlan:
  def make(
      grid: GridSpec,
      points: Vector[Vector[Double]],
      method: Resample.Method
  ): Either[MorphismError, DenseFieldInterpolationPlan] =
    SpatialMorphism.validateCoords(points)
    for
      _ <- validateMethod(method)
      inverse <- DMat.invert(grid.affine).left.map(MorphismError.SingularMatrix.apply)
    yield
      val stencils = Vector.newBuilder[DenseFieldStencil]
      stencils.sizeHint(points.length)
      var i = 0
      while i < points.length do
        val voxel = Affine.applyAffine(inverse, points(i))
        val stencil =
          method match
            case Resample.Method.Nearest => nearestStencil(grid, voxel)
            case Resample.Method.Linear => linearStencil(grid, voxel)
            case Resample.Method.Cubic => cubicStencil(grid, voxel)
        stencils += stencil
        i += 1
      DenseFieldInterpolationPlan(grid, points, method, stencils.result())

  private[image] def validateField(
      grid: GridSpec,
      field: NDArray[Double]
  ): Either[MorphismError, Unit] =
    val expectedShape = grid.dims :+ 3
    if field.shape != expectedShape then Left(MorphismError.DenseFieldShapeMismatch(expectedShape, field.shape))
    else
      var error = Option.empty[MorphismError]
      var i = 0
      while i < field.data.length && error.isEmpty do
        if !field.data(i).isFinite then error = Some(MorphismError.NonFiniteFieldValue(i))
        i += 1

      error match
        case Some(err) => Left(err)
        case None => Right(())

  private[image] def validateMethod(method: Resample.Method): Either[MorphismError, Unit] =
    method match
      case Resample.Method.Nearest | Resample.Method.Linear | Resample.Method.Cubic => Right(())

  private def nearestStencil(grid: GridSpec, voxel: Vector[Double]): DenseFieldStencil =
    val x = math.round(voxel(0)).toInt
    val y = math.round(voxel(1)).toInt
    val z = math.round(voxel(2)).toInt
    if inBounds(grid, x, y, z) then
      new DenseFieldStencil(Array(Indexing.gridToIndex3D(grid.shape, x, y, z)), Array(1.0), 0.0)
    else new DenseFieldStencil(Array.empty[Int], Array.empty[Double], 1.0)

  private def linearStencil(grid: GridSpec, voxel: Vector[Double]): DenseFieldStencil =
    val x0 = math.floor(voxel(0)).toInt
    val y0 = math.floor(voxel(1)).toInt
    val z0 = math.floor(voxel(2)).toInt
    val xd = voxel(0) - x0
    val yd = voxel(1) - y0
    val zd = voxel(2) - z0
    val indices = Array.ofDim[Int](8)
    val weights = Array.ofDim[Double](8)
    var n = 0
    var outside = 0.0

    var dz = 0
    while dz <= 1 do
      val wz = if dz == 0 then 1.0 - zd else zd
      val z = z0 + dz
      var dy = 0
      while dy <= 1 do
        val wy = if dy == 0 then 1.0 - yd else yd
        val y = y0 + dy
        var dx = 0
        while dx <= 1 do
          val wx = if dx == 0 then 1.0 - xd else xd
          val x = x0 + dx
          val w = wx * wy * wz
          if w != 0.0 then
            if inBounds(grid, x, y, z) then
              indices(n) = Indexing.gridToIndex3D(grid.shape, x, y, z)
              weights(n) = w
              n += 1
            else outside += w
          dx += 1
        dy += 1
      dz += 1

    new DenseFieldStencil(indices.take(n), weights.take(n), outside)

  private def cubicStencil(grid: GridSpec, voxel: Vector[Double]): DenseFieldStencil =
    val x = voxel(0)
    val y = voxel(1)
    val z = voxel(2)
    if x < -0.5 || y < -0.5 || z < -0.5 ||
        x > grid.shape.x.toDouble - 0.5 ||
        y > grid.shape.y.toDouble - 0.5 ||
        z > grid.shape.z.toDouble - 0.5 then
      new DenseFieldStencil(Array.empty[Int], Array.empty[Double], 1.0)
    else if x < 0.5 || y < 0.5 || z < 0.5 ||
        x > grid.shape.x.toDouble - 1.5 ||
        y > grid.shape.y.toDouble - 1.5 ||
        z > grid.shape.z.toDouble - 1.5 then
      linearStencil(grid, voxel)
    else
      val x0 = math.floor(x).toInt
      val y0 = math.floor(y).toInt
      val z0 = math.floor(z).toInt
      val fx = x - x0.toDouble
      val fy = y - y0.toDouble
      val fz = z - z0.toDouble
      val wx = Array.tabulate(4)(j => CubicWeights.catmullRom(fx - (j - 1).toDouble))
      val wy = Array.tabulate(4)(j => CubicWeights.catmullRom(fy - (j - 1).toDouble))
      val wz = Array.tabulate(4)(j => CubicWeights.catmullRom(fz - (j - 1).toDouble))
      val indices = Array.ofDim[Int](64)
      val weights = Array.ofDim[Double](64)
      var n = 0

      var dz = -1
      while dz <= 2 do
        val zz = clamp(z0 + dz, 0, grid.shape.z - 1)
        var dy = -1
        while dy <= 2 do
          val yy = clamp(y0 + dy, 0, grid.shape.y - 1)
          var dx = -1
          while dx <= 2 do
            val xx = clamp(x0 + dx, 0, grid.shape.x - 1)
            val w = wx(dx + 1) * wy(dy + 1) * wz(dz + 1)
            if w != 0.0 then
              indices(n) = Indexing.gridToIndex3D(grid.shape, xx, yy, zz)
              weights(n) = w
              n += 1
            dx += 1
          dy += 1
        dz += 1

      new DenseFieldStencil(indices.take(n), weights.take(n), 0.0)

  private inline def inBounds(grid: GridSpec, x: Int, y: Int, z: Int): Boolean =
    x >= 0 && x < grid.shape.x &&
      y >= 0 && y < grid.shape.y &&
      z >= 0 && z < grid.shape.z

  private inline def clamp(value: Int, low: Int, high: Int): Int =
    math.max(low, math.min(high, value))

private[image] object CubicWeights:
  def catmullRom(t: Double): Double =
    val at = math.abs(t)
    if at <= 1.0 then
      (1.5 * at - 2.5) * at * at + 1.0
    else if at < 2.0 then
      ((-0.5 * at + 2.5) * at - 4.0) * at + 2.0
    else 0.0
