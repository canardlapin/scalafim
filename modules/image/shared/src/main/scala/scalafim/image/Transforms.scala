package scalafim.image

import ravel.DType
import ravel.NDArray as RavelArray
import scala.annotation.targetName

private def copyVolumeIntoLegacy[A](
    source: NeuroVol[A],
    destination: Array[A],
    offset: Int
): Unit =
  var index = 0
  while index < source.values.size do
    destination(offset + index) = source.linear(index)
    index += 1

object Downsample:

  def byFactor(vec: NeuroVec[Double], factor: Double): NeuroVec[Double] =
    byFactor(vec, Vector.fill(3)(factor))

  def byFactor(vec: NeuroVec[Double], factors: Vector[Double]): NeuroVec[Double] =
    require(factors.length == 3 && factors.forall(f => f > 0 && f <= 1.0), "factors must be length-3 in (0,1]")
    val oldDims = vec.space.dims.take(4)
    val newSpatial = Vector.tabulate(3)(d => math.max(1, math.round(oldDims(d) * factors(d)).toInt))
    toDims(vec, newSpatial)

  def toDims(vec: NeuroVec[Double], newSpatialDims: Vector[Int]): NeuroVec[Double] =
    require(newSpatialDims.length == 3 && newSpatialDims.forall(_ > 0), "newSpatialDims must be length-3 positive")
    val old = vec.space
    val oldSpatial = old.spatialDims
    val tLen = vec.nVolumes

    val newDims4 = newSpatialDims :+ tLen
    val scale = Vector.tabulate(3)(d => oldSpatial(d).toDouble / newSpatialDims(d).toDouble)

    val spatialNelsNew = newSpatialDims.product
    val out = Array.ofDim[Double](spatialNelsNew * tLen)

    def blockRange(d: Int, o: Int): (Int, Int) =
      val start = math.floor(o * scale(d)).toInt
      val end = math.min(oldSpatial(d) - 1, math.floor((o + 1) * scale(d) - 1e-9).toInt)
      (start, math.max(start, end))

    var t = 0
    while t < tLen do
      var oz = 0
      while oz < newSpatialDims(2) do
        val (z0, z1) = blockRange(2, oz)
        var oy = 0
        while oy < newSpatialDims(1) do
          val (y0, y1) = blockRange(1, oy)
          var ox = 0
          while ox < newSpatialDims(0) do
            val (x0, x1) = blockRange(0, ox)
            var sum = 0.0
            var count = 0
            var z = z0
            while z <= z1 do
              var y = y0
              while y <= y1 do
                var x = x0
                while x <= x1 do
                  sum += vec(x, y, z, t)
                  count += 1
                  x += 1
                y += 1
              z += 1
            val avg = if count == 0 then 0.0 else sum / count.toDouble
            val lin = Indexing.gridToIndex(Vector(newSpatialDims(0), newSpatialDims(1), newSpatialDims(2), tLen), Vector(ox, oy, oz, t))
            out(lin) = avg
            ox += 1
          oy += 1
        oz += 1
      t += 1

    val scaleFactors = Vector.tabulate(3)(d => oldSpatial(d).toDouble / newSpatialDims(d).toDouble)
    val newSpacing = Vector.tabulate(3)(d => old.spacing(d) * scaleFactors(d))
    val newTrans =
      Affine.rescaleAffine(
        old.trans,
        shape = oldSpatial,
        zooms = newSpacing,
        newShape = Some(newSpatialDims)
      )
    val newOrigin = Vector.tabulate(3)(i => newTrans(i, newTrans.cols - 1))
    val newSpace =
      NeuroSpace(
        dims = newDims4,
        spacing = Some(newSpacing),
        origin = Some(newOrigin),
        axes = Some(old.axes),
        trans = Some(newTrans)
      )
    NeuroVec.fromLinear(out, newSpace, vec.label)

  @scala.annotation.targetName("byFactorNeuroVolScalar")
  def byFactor(vol: NeuroVol[Double], factor: Double): NeuroVol[Double] =
    byFactor(vol, Vector.fill(3)(factor))

  @scala.annotation.targetName("byFactorNeuroVolVector")
  def byFactor(vol: NeuroVol[Double], factors: Vector[Double]): NeuroVol[Double] =
    require(factors.length == 3 && factors.forall(f => f > 0 && f <= 1.0), "factors must be length-3 in (0,1]")
    val old = vol.space
    val oldSpatial = old.spatialDims
    val newSpatial = Vector.tabulate(3)(d => math.max(1, math.round(oldSpatial(d) * factors(d)).toInt))
    toDims(vol, newSpatial)

  @scala.annotation.targetName("toDimsNeuroVol")
  def toDims(vol: NeuroVol[Double], newSpatialDims: Vector[Int]): NeuroVol[Double] =
    require(newSpatialDims.length == 3 && newSpatialDims.forall(_ > 0), "newSpatialDims must be length-3 positive")
    val old = vol.space
    val oldSpatial = old.spatialDims
    val scale = Vector.tabulate(3)(d => oldSpatial(d).toDouble / newSpatialDims(d).toDouble)

    val spatialNelsNew = newSpatialDims.product
    val out = Array.ofDim[Double](spatialNelsNew)

    def blockRange(d: Int, o: Int): (Int, Int) =
      val start = math.floor(o * scale(d)).toInt
      val end = math.min(oldSpatial(d) - 1, math.floor((o + 1) * scale(d) - 1e-9).toInt)
      (start, math.max(start, end))

    var oz = 0
    while oz < newSpatialDims(2) do
      val (z0, z1) = blockRange(2, oz)
      var oy = 0
      while oy < newSpatialDims(1) do
        val (y0, y1) = blockRange(1, oy)
        var ox = 0
        while ox < newSpatialDims(0) do
          val (x0, x1) = blockRange(0, ox)
          var sum = 0.0
          var count = 0
          var z = z0
          while z <= z1 do
            var y = y0
            while y <= y1 do
              var x = x0
              while x <= x1 do
                sum += vol(x, y, z)
                count += 1
                x += 1
              y += 1
            z += 1
          val avg = if count == 0 then 0.0 else sum / count.toDouble
          val lin = Indexing.gridToIndex(newSpatialDims, Vector(ox, oy, oz))
          out(lin) = avg
          ox += 1
        oy += 1
      oz += 1

    val scaleFactors = Vector.tabulate(3)(d => oldSpatial(d).toDouble / newSpatialDims(d).toDouble)
    val newSpacing = Vector.tabulate(3)(d => old.spacing(d) * scaleFactors(d))
    val newTrans =
      Affine.rescaleAffine(
        old.trans,
        shape = oldSpatial,
        zooms = newSpacing,
        newShape = Some(newSpatialDims)
      )
    val newOrigin = Vector.tabulate(3)(i => newTrans(i, newTrans.cols - 1))
    val newSpace =
      NeuroSpace(
        dims = newSpatialDims,
        spacing = Some(newSpacing),
        origin = Some(newOrigin),
        axes = Some(old.axes),
        trans = Some(newTrans)
      )
    NeuroVol.fromLinear(out, newSpace, vol.label)

object Resample:

  enum ResampleError:
    case UnknownMethod(value: String)
    case UnsupportedEngine(value: String)

    def message: String =
      this match
        case UnknownMethod(value) => s"unknown resample method: '$value'"
        case UnsupportedEngine(_) => "Only engine = 'internal'"

  enum Engine:
    case Internal

  object Engine:
    def fromString(value: String): Either[ResampleError, Engine] =
      value.trim.toLowerCase match
        case "internal" => Right(Engine.Internal)
        case other => Left(ResampleError.UnsupportedEngine(other))

  enum Method:
    case Nearest, Linear, Cubic

  object Method:
    def fromString(s: String): Either[ResampleError, Method] =
      s.trim.toLowerCase match
        case "nearest" => Right(Method.Nearest)
        case "linear" | "trilinear" => Right(Method.Linear)
        case "cubic" | "spline" => Right(Method.Cubic)
        case other => Left(ResampleError.UnknownMethod(other))

  trait Resampleable[A]:
    type Out
    def apply(source: A, target: NeuroSpace, method: Method): Out

  object Resampleable:
    given Resampleable[NeuroVol[Double]] with
      type Out = NeuroVol[Double]
      def apply(source: NeuroVol[Double], target: NeuroSpace, method: Method): NeuroVol[Double] =
        method match
          case Method.Nearest => nearest(source, target)
          case Method.Linear => trilinear(source, target)
          case Method.Cubic => tricubic(source, target)

    given Resampleable[NeuroVec[Double]] with
      type Out = NeuroVec[Double]
      def apply(source: NeuroVec[Double], target: NeuroSpace, method: Method): NeuroVec[Double] =
        method match
          case Method.Nearest => nearest(source, target)
          case Method.Linear => trilinear(source, target)
          case Method.Cubic => tricubic(source, target)

    given Resampleable[ClusteredNeuroVol] with
      type Out = ClusteredNeuroVol
      def apply(source: ClusteredNeuroVol, target: NeuroSpace, method: Method): ClusteredNeuroVol =
        val labelVol: NeuroVol[Int] = source.toDense
        val resLabels = nearest(labelVol, target, fill = 0)
        val resMask = nearest(source.mask, target, fill = false)

        val targ = target.spatialSpace
        val spatialNels = targ.spatialDims.product
        val keepFlags = PrimitiveBuffers.fillConst[Boolean](spatialNels, false)

        var lin = 0
        while lin < spatialNels do
          if resMask.linear(lin) && resLabels.linear(lin) != 0 then keepFlags(lin) = true
          lin += 1

        val outMask = NeuroVol.fromLinear[Boolean](keepFlags, targ, source.label)
        val activeIdx = Mask.indices(outMask)
        val outClusters =
          RavelArray.tabulate[Int](activeIdx.size): i =>
            resLabels.linear(activeIdx(i))

        val idsPresent =
          Vector.tabulate(outClusters.size)(i => outClusters(i)).distinct.toSet
        val outLabelMap =
          if source.labelMap.isEmpty then Map.empty[Int, String]
          else source.labelMap.filter { case (k, _) => idsPresent.contains(k) }

        ClusteredNeuroVol(outMask, outClusters, outLabelMap, source.label)

  trait HasSpace[T]:
    def spaceOf(target: T): NeuroSpace

  object HasSpace:
    given HasSpace[NeuroSpace] with
      def spaceOf(target: NeuroSpace): NeuroSpace = target

    given [A]: HasSpace[NeuroVol[A]] with
      def spaceOf(target: NeuroVol[A]): NeuroSpace = target.space

    given [A]: HasSpace[NeuroVec[A]] with
      def spaceOf(target: NeuroVec[A]): NeuroSpace = target.space

    given HasSpace[ClusteredNeuroVol] with
      def spaceOf(target: ClusteredNeuroVol): NeuroSpace = target.space

  def resampleTo[A, T](
    source: A,
    target: T,
    method: Method = Method.Cubic
  )(using r: Resampleable[A], hs: HasSpace[T]): r.Out =
    r.apply(source, hs.spaceOf(target), method)

  def resampleTo[A, T](
    source: A,
    target: T,
    method: Method,
    engine: Engine
  )(using r: Resampleable[A], hs: HasSpace[T]): r.Out =
    engine match
      case Engine.Internal => resampleTo(source, target, method)

  def resampleToEither[A, T](
    source: A,
    target: T,
    method: String,
    engine: String = "internal"
  )(using r: Resampleable[A], hs: HasSpace[T]): Either[ResampleError, r.Out] =
    for
      e <- Engine.fromString(engine)
      m <- Method.fromString(method)
    yield resampleTo(source, target, m, e)

  def resampleTo[A, T](source: A, target: T, method: String)(using r: Resampleable[A], hs: HasSpace[T]): r.Out =
    resampleToEither(source, target, method).fold(err => throw new IllegalArgumentException(err.message), identity)

  def resampleTo[A, T](source: A, target: T, method: String, engine: String)(using r: Resampleable[A], hs: HasSpace[T]): r.Out =
    resampleToEither(source, target, method, engine).fold(err => throw new IllegalArgumentException(err.message), identity)

  def plan(
      source: GridSpec,
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    ResamplingPlan.make(source, target, morphism, method)

  @targetName("planFromNeuroSpaces")
  def plan(
      source: NeuroSpace,
      target: NeuroSpace,
      morphism: SpatialMorphism,
      method: Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    ResamplingPlan.fromSpaces(source, target, morphism, method)

  def resampleTo(
      source: NeuroVol[Double],
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Method,
      outside: Double
  ): Either[ResamplingPlanError, NeuroVol[Double]] =
    plan(GridSpec.fromSpace(source.space), target, morphism, method).flatMap(_.apply(source, outside))

  @scala.annotation.targetName("resampleToNeuroVec")
  def resampleTo(
      source: NeuroVec[Double],
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Method,
      outside: Double
  ): Either[ResamplingPlanError, NeuroVec[Double]] =
    plan(GridSpec.fromSpace(source.space), target, morphism, method).flatMap(_.apply(source, outside))

  def nearest(vol: NeuroVol[Double], target: NeuroSpace): NeuroVol[Double] =
    val src = vol.space
    val targ = target.spatialSpace
    val targDims = targ.spatialDims
    val out = Array.ofDim[Double](targDims.product)

    var z = 0
    while z < targDims(2) do
      var y = 0
      while y < targDims(1) do
        var x = 0
        while x < targDims(0) do
          val world = targ.indexToCoord(Vector(x.toDouble, y.toDouble, z.toDouble))
          val sVox = src.coordToIndex(world)
          val sx = math.round(sVox(0)).toInt
          val sy = math.round(sVox(1)).toInt
          val sz = math.round(sVox(2)).toInt
          val v =
            if sx >= 0 && sx < src.spatialDims(0) &&
               sy >= 0 && sy < src.spatialDims(1) &&
               sz >= 0 && sz < src.spatialDims(2) then
              vol(sx, sy, sz)
            else 0.0
          val lin = Indexing.gridToIndex(targDims, Vector(x, y, z))
          out(lin) = v
          x += 1
        y += 1
      z += 1

    NeuroVol.fromLinear(out, targ, vol.label)

  def nearest[A](
      vol: NeuroVol[A],
      target: NeuroSpace,
      fill: A
  )(using scala.reflect.ClassTag[A], DType[A]): NeuroVol[A] =
    val src = vol.space
    val targ = target.spatialSpace
    val targDims = targ.spatialDims
    val srcDims = src.spatialDims
    val out = PrimitiveBuffers.fillConst[A](targDims.product, fill)

    var z = 0
    while z < targDims(2) do
      var y = 0
      while y < targDims(1) do
        var x = 0
        while x < targDims(0) do
          val world = targ.indexToCoord(Vector(x.toDouble, y.toDouble, z.toDouble))
          val sVox = src.coordToIndex(world)
          val sx = math.round(sVox(0)).toInt
          val sy = math.round(sVox(1)).toInt
          val sz = math.round(sVox(2)).toInt
          val v =
            if sx >= 0 && sx < srcDims(0) &&
               sy >= 0 && sy < srcDims(1) &&
               sz >= 0 && sz < srcDims(2) then
              vol(sx, sy, sz)
            else fill
          val lin = Indexing.gridToIndex(targDims, Vector(x, y, z))
          out(lin) = v
          x += 1
        y += 1
      z += 1

    NeuroVol.fromLinear(out, targ, vol.label)

  @scala.annotation.targetName("nearestNeuroVec")
  def nearest(vec: NeuroVec[Double], target: NeuroSpace): NeuroVec[Double] =
    val tLen = vec.nVolumes
    val targSpatial = target.spatialSpace
    val targDims = targSpatial.spatialDims
    val out = Array.ofDim[Double](targDims.product * tLen)

    var t = 0
    while t < tLen do
      val volT = vec.volume(t)
      val resT = nearest(volT, targSpatial)
      val spatialNels = targDims.product
      copyVolumeIntoLegacy(resT, out, t * spatialNels)
      t += 1

    val newSpace = targSpatial.addDim(tLen, Some(Axis.Time))
    NeuroVec.fromLinear(out, newSpace, vec.label)

  @scala.annotation.targetName("nearestGenericNeuroVec")
  def nearest[A](
    vec: NeuroVec[A],
    target: NeuroSpace,
    fill: A
  )(using scala.reflect.ClassTag[A], DType[A]): NeuroVec[A] =
    val tLen = vec.nVolumes
    val targSpatial = target.spatialSpace
    val targDims = targSpatial.spatialDims
    val out = PrimitiveBuffers.fillConst[A](targDims.product * tLen, fill)

    var t = 0
    while t < tLen do
      val volT = vec.volume(t)
      val resT = nearest(volT, targSpatial, fill)
      val spatialNels = targDims.product
      copyVolumeIntoLegacy(resT, out, t * spatialNels)
      t += 1

    val newSpace = targSpatial.addDim(tLen, Some(Axis.Time))
    NeuroVec.fromLinear(out, newSpace, vec.label)

  def trilinear(vol: NeuroVol[Double], target: NeuroSpace): NeuroVol[Double] =
    val src = vol.space
    val targ = target.spatialSpace
    val targDims = targ.spatialDims
    val srcDims = src.spatialDims
    val out = Array.ofDim[Double](targDims.product)

    inline def sample(x: Int, y: Int, z: Int): Double =
      if x >= 0 && x < srcDims(0) &&
         y >= 0 && y < srcDims(1) &&
         z >= 0 && z < srcDims(2) then
        vol(x, y, z)
      else 0.0

    var zt = 0
    while zt < targDims(2) do
      var yt = 0
      while yt < targDims(1) do
        var xt = 0
        while xt < targDims(0) do
          val world = targ.indexToCoord(Vector(xt.toDouble, yt.toDouble, zt.toDouble))
          val sVox = src.coordToIndex(world)
          val sx = sVox(0)
          val sy = sVox(1)
          val sz = sVox(2)

          val x0 = math.floor(sx).toInt
          val y0 = math.floor(sy).toInt
          val z0 = math.floor(sz).toInt
          val x1 = x0 + 1
          val y1 = y0 + 1
          val z1 = z0 + 1

          val xd = sx - x0
          val yd = sy - y0
          val zd = sz - z0

          val c000 = sample(x0, y0, z0)
          val c100 = sample(x1, y0, z0)
          val c010 = sample(x0, y1, z0)
          val c110 = sample(x1, y1, z0)
          val c001 = sample(x0, y0, z1)
          val c101 = sample(x1, y0, z1)
          val c011 = sample(x0, y1, z1)
          val c111 = sample(x1, y1, z1)

          val c00 = c000 * (1 - xd) + c100 * xd
          val c10 = c010 * (1 - xd) + c110 * xd
          val c01 = c001 * (1 - xd) + c101 * xd
          val c11 = c011 * (1 - xd) + c111 * xd

          val c0 = c00 * (1 - yd) + c10 * yd
          val c1 = c01 * (1 - yd) + c11 * yd

          val c = c0 * (1 - zd) + c1 * zd

          val lin = Indexing.gridToIndex(targDims, Vector(xt, yt, zt))
          out(lin) = c
          xt += 1
        yt += 1
      zt += 1

    NeuroVol.fromLinear(out, targ, vol.label)

  @scala.annotation.targetName("trilinearNeuroVec")
  def trilinear(vec: NeuroVec[Double], target: NeuroSpace): NeuroVec[Double] =
    val tLen = vec.nVolumes
    val targSpatial = target.spatialSpace
    val targDims = targSpatial.spatialDims
    val out = Array.ofDim[Double](targDims.product * tLen)

    var t = 0
    while t < tLen do
      val volT = vec.volume(t)
      val resT = trilinear(volT, targSpatial)
      val spatialNels = targDims.product
      copyVolumeIntoLegacy(resT, out, t * spatialNels)
      t += 1

    val newSpace = targSpatial.addDim(tLen, Some(Axis.Time))
    NeuroVec.fromLinear(out, newSpace, vec.label)

  def tricubic(vol: NeuroVol[Double], target: NeuroSpace): NeuroVol[Double] =
    val src = vol.space
    val targ = target.spatialSpace
    val targDims = targ.spatialDims
    val srcDims = src.spatialDims
    val out = PrimitiveBuffers.ofSize[Double](targDims.product)

    inline def sample(x: Int, y: Int, z: Int): Double =
      if x >= 0 && x < srcDims(0) &&
         y >= 0 && y < srcDims(1) &&
         z >= 0 && z < srcDims(2) then
        vol(x, y, z)
      else 0.0

    inline def cubic(p0: Double, p1: Double, p2: Double, p3: Double, t: Double): Double =
      val t2 = t * t
      val t3 = t2 * t
      0.5 * ((2.0 * p1) +
        (-p0 + p2) * t +
        (2.0 * p0 - 5.0 * p1 + 4.0 * p2 - p3) * t2 +
        (-p0 + 3.0 * p1 - 3.0 * p2 + p3) * t3)

    val tmpY = Array.ofDim[Double](4)
    val tmpZ = Array.ofDim[Double](4)

    var zt = 0
    while zt < targDims(2) do
      var yt = 0
      while yt < targDims(1) do
        var xt = 0
        while xt < targDims(0) do
          val world = targ.indexToCoord(Vector(xt.toDouble, yt.toDouble, zt.toDouble))
          val sVox = src.coordToIndex(world)
          val sx = sVox(0)
          val sy = sVox(1)
          val sz = sVox(2)

          val x1 = math.floor(sx).toInt
          val y1 = math.floor(sy).toInt
          val z1 = math.floor(sz).toInt
          val tx = sx - x1
          val ty = sy - y1
          val tz = sz - z1

          var kk = 0
          while kk < 4 do
            val z = z1 + (kk - 1)
            var jj = 0
            while jj < 4 do
              val y = y1 + (jj - 1)
              val p0 = sample(x1 - 1, y, z)
              val p1 = sample(x1, y, z)
              val p2 = sample(x1 + 1, y, z)
              val p3 = sample(x1 + 2, y, z)
              tmpY(jj) = cubic(p0, p1, p2, p3, tx)
              jj += 1
            tmpZ(kk) = cubic(tmpY(0), tmpY(1), tmpY(2), tmpY(3), ty)
            kk += 1

          val v = cubic(tmpZ(0), tmpZ(1), tmpZ(2), tmpZ(3), tz)
          val lin = Indexing.gridToIndex(targDims, Vector(xt, yt, zt))
          out(lin) = v
          xt += 1
        yt += 1
      zt += 1

    NeuroVol.fromLinear(out, targ, vol.label)

  @scala.annotation.targetName("tricubicNeuroVec")
  def tricubic(vec: NeuroVec[Double], target: NeuroSpace): NeuroVec[Double] =
    val tLen = vec.nVolumes
    val targSpatial = target.spatialSpace
    val targDims = targSpatial.spatialDims
    val out = PrimitiveBuffers.ofSize[Double](targDims.product * tLen)

    var t = 0
    while t < tLen do
      val volT = vec.volume(t)
      val resT = tricubic(volT, targSpatial)
      val spatialNels = targDims.product
      copyVolumeIntoLegacy(resT, out, t * spatialNels)
      t += 1

    val newSpace = targSpatial.addDim(tLen, Some(Axis.Time))
    NeuroVec.fromLinear(out, newSpace, vec.label)

object SpatialFilters:

  def mapf(
    vol: NeuroVol[Double],
    kernel: Kernel3D,
    mask: Option[NeuroVol[Boolean]] = None
  ): NeuroVol[Double] =
    val sp = vol.space
    val dims = sp.spatialDims
    require(dims.length == 3, "volume must be 3D")

    mask.foreach { m =>
      GridCompatibility.requireSpatial(sp, m.space)
    }

    val nx = dims(0); val ny = dims(1); val nz = dims(2)
    val spatialNels = dims.product
    val out = PrimitiveBuffers.fillConst[Double](spatialNels, 0.0)

    var lin = 0
    while lin < spatialNels do
      val keep = mask.forall(_.linear(lin))
      if keep then
        val x = lin % nx
        val yz = lin / nx
        val y = yz % ny
        val z = yz / ny

        var sum = 0.0
        var q = 0
        while q < kernel.size do
          val xx = x + kernel.dx(q)
          val yy = y + kernel.dy(q)
          val zz = z + kernel.dz(q)
          if xx >= 0 && xx < nx && yy >= 0 && yy < ny && zz >= 0 && zz < nz then
            sum += kernel.w(q) * vol(xx, yy, zz)
          q += 1
        out(lin) = sum
      lin += 1

    NeuroVol.fromLinear(out, sp, vol.label)

  def gaussianBlur(
    vol: NeuroVol[Double],
    sigma: Double = 2.0,
    window: Int = 1,
    mask: Option[NeuroVol[Boolean]] = None
  ): NeuroVol[Double] =
    require(window >= 1, "window must be >= 1")
    require(sigma > 0, "sigma must be positive")

    val sp = vol.space
    val dims = sp.spatialDims
    val nx = dims(0); val ny = dims(1); val nz = dims(2)
    val spacing = sp.spacing
    val spatialNels = dims.product

    mask.foreach { m =>
      GridCompatibility.requireSpatial(sp, m.space)
    }

    val idx: Array[Int] =
      mask match
        case None =>
          Array.tabulate(spatialNels)(identity)
        case Some(m) =>
          val active = Mask.indices(m)
          Array.tabulate(active.size)(i => active(i))

    val sz = 2 * window + 1
    val total = sz * sz * sz
    val dxArr = Array.ofDim[Int](total)
    val dyArr = Array.ofDim[Int](total)
    val dzArr = Array.ofDim[Int](total)
    val kernel = Array.ofDim[Double](total)

    val denom = 2.0 * sigma * sigma
    var sumKernel = 0.0
    var q = 0
    var dz = -window
    while dz <= window do
      var dy = -window
      while dy <= window do
        var dx = -window
        while dx <= window do
          dxArr(q) = dx
          dyArr(q) = dy
          dzArr(q) = dz
          val rx = dx.toDouble * spacing(0)
          val ry = dy.toDouble * spacing(1)
          val rz = dz.toDouble * spacing(2)
          val dist2 = rx * rx + ry * ry + rz * rz
          val w = math.exp(-dist2 / denom)
          kernel(q) = w
          sumKernel += w
          q += 1
          dx += 1
        dy += 1
      dz += 1

    if sumKernel != 0.0 then
      q = 0
      while q < total do
        kernel(q) = kernel(q) / sumKernel
        q += 1

    val out = PrimitiveBuffers.fillConst[Double](spatialNels, 0.0)
    var p = 0
    while p < idx.length do
      val lin = idx(p)
      val x = lin % nx
      val yz = lin / nx
      val y = yz % ny
      val z = yz / ny

      var sum = 0.0
      q = 0
      while q < total do
        val xx = x + dxArr(q)
        val yy = y + dyArr(q)
        val zz = z + dzArr(q)
        val v =
          if xx >= 0 && xx < nx && yy >= 0 && yy < ny && zz >= 0 && zz < nz then
            vol(xx, yy, zz)
          else 0.0
        sum += kernel(q) * v
        q += 1
      out(lin) = sum
      p += 1

    NeuroVol.fromLinear(out, sp, vol.label)

  def gaussianBlur(vec: NeuroVec[Double], sigma: Double, window: Int): NeuroVec[Double] =
    val tLen = vec.nVolumes
    val spatialNels = vec.space.spatialDims.product
    val out = Array.ofDim[Double](spatialNels * tLen)
    var t = 0
    while t < tLen do
      val blurred = gaussianBlur(vec.volume(t), sigma = sigma, window = window)
      copyVolumeIntoLegacy(blurred, out, t * spatialNels)
      t += 1
    NeuroVec.fromLinear(out, vec.space, vec.label)

  def gaussianBlur(vec: NeuroVec[Double]): NeuroVec[Double] =
    gaussianBlur(vec, sigma = 2.0, window = 1)

  def gaussianBlur(vec: NeuroVec[Double], sigma: Double): NeuroVec[Double] =
    gaussianBlur(vec, sigma = sigma, window = 1)

  def bilateralFilter(
    vol: NeuroVol[Double],
    mask: Option[NeuroVol[Boolean]] = None,
    window: Int = 1,
    spatialSigma: Double = 2.0,
    intensitySigma: Double = 1.0
  ): NeuroVol[Double] =
    require(window >= 0, "window must be >= 0")
    require(spatialSigma > 0, "spatialSigma must be positive")
    require(intensitySigma > 0, "intensitySigma must be positive")

    val sp = vol.space
    val dims = sp.spatialDims
    val nx = dims(0); val ny = dims(1); val nz = dims(2)
    val spatialNels = dims.product
    val spacing = sp.spacing

    mask.foreach { m =>
      GridCompatibility.requireSpatial(sp, m.space)
    }

    val idx: Array[Int] =
      mask match
        case None =>
          Array.tabulate(spatialNels)(identity)
        case Some(m) =>
          val active = Mask.indices(m)
          Array.tabulate(active.size)(i => active(i))

    var sum = 0.0
    var sumsq = 0.0
    var count = 0
    var p = 0
    while p < idx.length do
      val v = vol.linear(idx(p))
      if v.isFinite then
        sum += v
        sumsq += v * v
        count += 1
      p += 1

    val intensitySd =
      if count <= 1 then 0.0
      else
        val mean = sum / count.toDouble
        val variance = (sumsq - count.toDouble * mean * mean) / (count.toDouble - 1.0)
        if variance > 0.0 then math.sqrt(variance) else 0.0

    val minIntensityVar = 1e-12
    var intensityVar = 2.0 * intensitySigma * intensitySigma * intensitySd * intensitySd
    if !intensityVar.isFinite || intensityVar < minIntensityVar then intensityVar = minIntensityVar

    val spatialVar = 2.0 * spatialSigma * spatialSigma

    val sz = 2 * window + 1
    val total = sz * sz * sz
    val dxArr = Array.ofDim[Int](total)
    val dyArr = Array.ofDim[Int](total)
    val dzArr = Array.ofDim[Int](total)
    val spatialKernel = Array.ofDim[Double](total)

    var q = 0
    var dz = -window
    while dz <= window do
      var dy = -window
      while dy <= window do
        var dx = -window
        while dx <= window do
          dxArr(q) = dx
          dyArr(q) = dy
          dzArr(q) = dz
          val rx = dx.toDouble * spacing(0)
          val ry = dy.toDouble * spacing(1)
          val rz = dz.toDouble * spacing(2)
          val dist2 = rx * rx + ry * ry + rz * rz
          spatialKernel(q) = math.exp(-dist2 / spatialVar)
          q += 1
          dx += 1
        dy += 1
      dz += 1

    val out = PrimitiveBuffers.fillConst[Double](spatialNels, 0.0)

    p = 0
    while p < idx.length do
      val lin = idx(p)
      val x = lin % nx
      val yz = lin / nx
      val y = yz % ny
      val z = yz / ny
      val centerVal = vol.linear(lin)
      if !centerVal.isFinite then
        out(lin) = centerVal
      else
        var valSum = 0.0
        var wSum = 0.0
        q = 0
        while q < total do
          val xx = x + dxArr(q)
          val yy = y + dyArr(q)
          val zz = z + dzArr(q)

          val neighVal =
            if xx >= 0 && xx < nx && yy >= 0 && yy < ny && zz >= 0 && zz < nz then
              vol(xx, yy, zz)
            else 0.0

          if neighVal.isFinite then
            val diff = neighVal - centerVal
            val w = spatialKernel(q) * math.exp(-(diff * diff) / intensityVar)
            valSum += w * neighVal
            wSum += w
          q += 1

        out(lin) = if wSum == 0.0 then centerVal else valSum / wSum
      p += 1

    NeuroVol.fromLinear(out, sp, vol.label)

  private def bilateralFilterVec(
    vec: NeuroVec[Double],
    mask: Option[NeuroVol[Boolean]],
    window: Int,
    spatialSigma: Double,
    intensitySigma: Double
  ): NeuroVec[Double] =
    val tLen = vec.nVolumes
    val spatialNels = vec.space.spatialDims.product
    val out = PrimitiveBuffers.fillConst[Double](spatialNels * tLen, 0.0)
    var t = 0
    while t < tLen do
      val volT = vec.volume(t)
      val filtered = bilateralFilter(volT, mask, window, spatialSigma, intensitySigma)
      var i = 0
      while i < spatialNels do
        out(i + t * spatialNels) = filtered.linear(i)
        i += 1
      t += 1
    NeuroVec.fromLinear(out, vec.space, vec.label)

  def bilateralFilter(vec: NeuroVec[Double]): NeuroVec[Double] =
    bilateralFilterVec(vec, mask = None, window = 1, spatialSigma = 2.0, intensitySigma = 1.0)

  def bilateralFilter(vec: NeuroVec[Double], mask: Option[NeuroVol[Boolean]]): NeuroVec[Double] =
    bilateralFilterVec(vec, mask, window = 1, spatialSigma = 2.0, intensitySigma = 1.0)

  @scala.annotation.targetName("bilateralFilterNeuroVec")
  def bilateralFilter(
    vec: NeuroVec[Double],
    mask: Option[NeuroVol[Boolean]],
    window: Int,
    spatialSigma: Double,
    intensitySigma: Double
  ): NeuroVec[Double] =
    bilateralFilterVec(vec, mask, window, spatialSigma, intensitySigma)

  def bilateralFilter4D(
    vec: NeuroVec[Double],
    mask: Option[NeuroVol[Boolean]] = None,
    spatialWindow: Int = 1,
    temporalWindow: Int = 1,
    spatialSigma: Double = 2.0,
    intensitySigma: Double = 1.0,
    temporalSigma: Double = 1.0,
    temporalSpacing: Double = 1.0
  ): NeuroVec[Double] =
    require(spatialWindow >= 0, "spatialWindow must be >= 0")
    require(temporalWindow >= 0, "temporalWindow must be >= 0")
    require(spatialSigma > 0, "spatialSigma must be positive")
    require(intensitySigma > 0, "intensitySigma must be positive")
    require(temporalSigma > 0, "temporalSigma must be positive")
    require(temporalSpacing > 0, "temporalSpacing must be positive")

    val sp = vec.space
    val dims = sp.spatialDims
    val nx = dims(0); val ny = dims(1); val nz = dims(2)
    val tLen = vec.nVolumes
    val spatialNels = dims.product
    val spacing = sp.spacing

    mask.foreach { m =>
      GridCompatibility.requireSpatial(sp, m.space)
    }

    val spatialIdx: Array[Int] =
      mask match
        case None =>
          Array.tabulate(spatialNels)(identity)
        case Some(m) =>
          val active = Mask.indices(m)
          Array.tabulate(active.size)(i => active(i))

    val out = PrimitiveBuffers.ofSize[Double](vec.values.size)
    var i0 = 0
    while i0 < out.length do
      out(i0) = vec.linear(i0)
      i0 += 1

    var sum = 0.0
    var sumsq = 0.0
    var count = 0
    var p = 0
    while p < spatialIdx.length do
      val lin = spatialIdx(p)
      var t = 0
      while t < tLen do
        val v = vec.linear(lin + t * spatialNels)
        if v.isFinite then
          sum += v
          sumsq += v * v
          count += 1
        t += 1
      p += 1

    val intensitySd =
      if count <= 1 then 0.0
      else
        val mean = sum / count.toDouble
        val variance = (sumsq / count.toDouble) - mean * mean
        if variance > 0.0 then math.sqrt(variance) else 0.0

    val minIntensityVar = 1e-12
    var intensityVar = 2.0 * intensitySigma * intensitySigma * intensitySd * intensitySd
    if !intensityVar.isFinite || intensityVar < minIntensityVar then intensityVar = minIntensityVar

    val spatialVar = 2.0 * spatialSigma * spatialSigma
    val temporalVar = 2.0 * temporalSigma * temporalSigma

    val sw = 2 * spatialWindow + 1
    val tw = 2 * temporalWindow + 1
    val total = tw * sw * sw * sw

    val dxArr = Array.ofDim[Int](total)
    val dyArr = Array.ofDim[Int](total)
    val dzArr = Array.ofDim[Int](total)
    val dtArr = Array.ofDim[Int](total)
    val kernel = Array.ofDim[Double](total)

    var q = 0
    var dt = -temporalWindow
    while dt <= temporalWindow do
      val rt = dt.toDouble * temporalSpacing
      val wTemporal = math.exp(-(rt * rt) / temporalVar)
      var dz = -spatialWindow
      while dz <= spatialWindow do
        val rz = dz.toDouble * spacing(2)
        val rz2 = rz * rz
        var dy = -spatialWindow
        while dy <= spatialWindow do
          val ry = dy.toDouble * spacing(1)
          val ry2 = ry * ry
          var dx = -spatialWindow
          while dx <= spatialWindow do
            val rx = dx.toDouble * spacing(0)
            val dist2 = rx * rx + ry2 + rz2
            val wSpatial = math.exp(-dist2 / spatialVar)
            dxArr(q) = dx
            dyArr(q) = dy
            dzArr(q) = dz
            dtArr(q) = dt
            kernel(q) = wSpatial * wTemporal
            q += 1
            dx += 1
          dy += 1
        dz += 1
      dt += 1

    p = 0
    while p < spatialIdx.length do
      val lin = spatialIdx(p)
      val x0 = lin % nx
      val yz = lin / nx
      val y0 = yz % ny
      val z0 = yz / ny

      var t0 = 0
      while t0 < tLen do
        val centerIdx = lin + t0 * spatialNels
        val centerVal = vec.linear(centerIdx)
        if centerVal.isFinite then
          var valSum = 0.0
          var wSum = 0.0
          q = 0
          while q < total do
            val tt = t0 + dtArr(q)
            if tt >= 0 && tt < tLen then
              val xx = x0 + dxArr(q)
              val yy = y0 + dyArr(q)
              val zz = z0 + dzArr(q)
              if xx >= 0 && xx < nx && yy >= 0 && yy < ny && zz >= 0 && zz < nz then
                val neighLin = xx + yy * nx + zz * nx * ny
                val neighVal = vec.linear(neighLin + tt * spatialNels)
                if neighVal.isFinite then
                  val diff = centerVal - neighVal
                  val w = kernel(q) * math.exp(-(diff * diff) / intensityVar)
                  valSum += w * neighVal
                  wSum += w
            q += 1
          if wSum > 0.0 then out(centerIdx) = valSum / wSum else out(centerIdx) = centerVal
        t0 += 1
      p += 1

    NeuroVec.fromLinear(out, sp, vec.label)
