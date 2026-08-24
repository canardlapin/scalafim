package scalafim.image

import image4s.filter.LinearFilter
import image4s.geometry.D3
import image4s.ops.Border
import image4s.ops.Correlation
import image4s.ops.FilterExtent
import image4s.ops.Kernel as ImageKernel
import image4s.ops.Offset
import image4s.ops.OpError
import image4s.ops.Support
import ravel.DType
import ravel.DType.given
import ravel.NDArray as RavelArray
import ravel.Rank
import ravel.Shape
import scala.annotation.targetName

object Downsample:

  def byFactor(vec: SomeScalarSeries[Double], factor: Double): SomeScalarSeries[Double] =
    byFactor(vec, Vector.fill(3)(factor))

  def byFactor(vec: SomeScalarSeries[Double], factors: Vector[Double]): SomeScalarSeries[Double] =
    require(factors.length == 3 && factors.forall(f => f > 0 && f <= 1.0), "factors must be length-3 in (0,1]")
    val oldDims = vec.space.dims.take(4)
    val newSpatial = Vector.tabulate(3)(d => math.max(1, math.round(oldDims(d) * factors(d)).toInt))
    toDims(vec, newSpatial)

  def toDims(vec: SomeScalarSeries[Double], newSpatialDims: Vector[Int]): SomeScalarSeries[Double] =
    require(newSpatialDims.length == 3 && newSpatialDims.forall(_ > 0), "newSpatialDims must be length-3 positive")
    val old = vec.space
    val oldSpatial = old.spatialDims
    val tLen = vec.nVolumes

    val newDims4 = newSpatialDims :+ tLen
    val scale = Vector.tabulate(3)(d => oldSpatial(d).toDouble / newSpatialDims(d).toDouble)

    def blockRange(d: Int, o: Int): (Int, Int) =
      val start = math.floor(o * scale(d)).toInt
      val end = math.min(oldSpatial(d) - 1, math.floor((o + 1) * scale(d) - 1e-9).toInt)
      (start, math.max(start, end))

    val out =
      RavelArray.tabulate[Double](
        newSpatialDims(0),
        newSpatialDims(1),
        newSpatialDims(2),
        tLen
      ): (ox, oy, oz, time) =>
        val (x0, x1) = blockRange(0, ox)
        val (y0, y1) = blockRange(1, oy)
        val (z0, z1) = blockRange(2, oz)
        var sum = 0.0
        var count = 0
        var z = z0
        while z <= z1 do
          var y = y0
          while y <= y1 do
            var x = x0
            while x <= x1 do
              sum += vec(x, y, z, time)
              count += 1
              x += 1
            y += 1
          z += 1
        if count == 0 then 0.0 else sum / count.toDouble

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
      SampleSpaces(
        dims = newDims4,
        spacing = Some(newSpacing),
        origin = Some(newOrigin),
        axes = Some(old.axes),
        trans = Some(newTrans)
      )
    SomeNeuroSeries.unsafeFromRavel(out, newSpace, vec.label)

  @scala.annotation.targetName("byFactorNeuroVolumeScalar")
  def byFactor(vol: SomeScalarVolume[Double], factor: Double): SomeScalarVolume[Double] =
    byFactor(vol, Vector.fill(3)(factor))

  @scala.annotation.targetName("byFactorNeuroVolumeVector")
  def byFactor(vol: SomeScalarVolume[Double], factors: Vector[Double]): SomeScalarVolume[Double] =
    require(factors.length == 3 && factors.forall(f => f > 0 && f <= 1.0), "factors must be length-3 in (0,1]")
    val old = vol.space
    val oldSpatial = old.spatialDims
    val newSpatial = Vector.tabulate(3)(d => math.max(1, math.round(oldSpatial(d) * factors(d)).toInt))
    toDims(vol, newSpatial)

  @scala.annotation.targetName("toDimsNeuroVolume")
  def toDims(vol: SomeScalarVolume[Double], newSpatialDims: Vector[Int]): SomeScalarVolume[Double] =
    require(newSpatialDims.length == 3 && newSpatialDims.forall(_ > 0), "newSpatialDims must be length-3 positive")
    val old = vol.space
    val oldSpatial = old.spatialDims
    val scale = Vector.tabulate(3)(d => oldSpatial(d).toDouble / newSpatialDims(d).toDouble)

    def blockRange(d: Int, o: Int): (Int, Int) =
      val start = math.floor(o * scale(d)).toInt
      val end = math.min(oldSpatial(d) - 1, math.floor((o + 1) * scale(d) - 1e-9).toInt)
      (start, math.max(start, end))

    val out =
      RavelArray.tabulate[Double](
        newSpatialDims(0),
        newSpatialDims(1),
        newSpatialDims(2)
      ): (ox, oy, oz) =>
        val (x0, x1) = blockRange(0, ox)
        val (y0, y1) = blockRange(1, oy)
        val (z0, z1) = blockRange(2, oz)
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
        if count == 0 then 0.0 else sum / count.toDouble

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
      SampleSpaces(
        dims = newSpatialDims,
        spacing = Some(newSpacing),
        origin = Some(newOrigin),
        axes = Some(old.axes),
        trans = Some(newTrans)
      )
    SomeNeuroVolume.unsafeFromRavel(out, newSpace, vol.label)

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
    def apply(source: A, target: SomeSampleSpace, method: Method): Out

  object Resampleable:
    given Resampleable[SomeScalarVolume[Double]] with
      type Out = SomeScalarVolume[Double]
      def apply(source: SomeScalarVolume[Double], target: SomeSampleSpace, method: Method): SomeScalarVolume[Double] =
        method match
          case Method.Nearest => nearest(source, target)
          case Method.Linear => trilinear(source, target)
          case Method.Cubic => tricubic(source, target)

    given Resampleable[SomeScalarSeries[Double]] with
      type Out = SomeScalarSeries[Double]
      def apply(source: SomeScalarSeries[Double], target: SomeSampleSpace, method: Method): SomeScalarSeries[Double] =
        method match
          case Method.Nearest => nearest(source, target)
          case Method.Linear => trilinear(source, target)
          case Method.Cubic => tricubic(source, target)

  trait HasSpace[T]:
    def spaceOf(target: T): SomeSampleSpace

  object HasSpace:
    given HasSpace[SomeSampleSpace] with
      def spaceOf(target: SomeSampleSpace): SomeSampleSpace = target

    given [A, Sem]: HasSpace[SomeNeuroVolume[A, Sem]] with
      def spaceOf(target: SomeNeuroVolume[A, Sem]): SomeSampleSpace = target.space

    given [A, Sem]: HasSpace[SomeNeuroSeries[A, Sem]] with
      def spaceOf(target: SomeNeuroSeries[A, Sem]): SomeSampleSpace = target.space

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

  @targetName("planFromSampleSpaces")
  def plan(
      source: SomeSampleSpace,
      target: SomeSampleSpace,
      morphism: SpatialMorphism,
      method: Method
  ): Either[ResamplingPlanError, ResamplingPlan] =
    ResamplingPlan.fromSpaces(source, target, morphism, method)

  def resampleTo(
      source: SomeScalarVolume[Double],
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Method,
      outside: Double
  ): Either[ResamplingPlanError, SomeScalarVolume[Double]] =
    plan(GridSpec.fromSpace(source.space), target, morphism, method).flatMap(_.apply(source, outside))

  @scala.annotation.targetName("resampleToNeuroSeries")
  def resampleTo(
      source: SomeScalarSeries[Double],
      target: GridSpec,
      morphism: SpatialMorphism,
      method: Method,
      outside: Double
  ): Either[ResamplingPlanError, SomeScalarSeries[Double]] =
    plan(GridSpec.fromSpace(source.space), target, morphism, method).flatMap(_.apply(source, outside))

  def nearest(vol: SomeScalarVolume[Double], target: SomeSampleSpace): SomeScalarVolume[Double] =
    executeContinuous(vol, target, Method.Nearest)

  def nearest[A, Sem](
      vol: SomeNeuroVolume[A, Sem],
      target: SomeSampleSpace,
      fill: A
  )(using
      scala.reflect.ClassTag[A],
      DType[A],
      image4s.ValueSemantics[A, Sem]
  ): SomeNeuroVolume[A, Sem] =
    val src = vol.space
    val targ = target.spatialSpace
    val targDims = targ.spatialDims
    val srcDims = src.spatialDims
    val out =
      RavelArray.tabulate[A](targDims(0), targDims(1), targDims(2)):
        (x, y, z) =>
          val world =
            targ.indexToCoord(
              Vector(x.toDouble, y.toDouble, z.toDouble)
            )
          val sourceVoxel = src.coordToIndex(world)
          val sx = math.round(sourceVoxel(0)).toInt
          val sy = math.round(sourceVoxel(1)).toInt
          val sz = math.round(sourceVoxel(2)).toInt
          if sx >= 0 && sx < srcDims(0) &&
              sy >= 0 && sy < srcDims(1) &&
              sz >= 0 && sz < srcDims(2)
          then vol(sx, sy, sz)
          else fill

    SomeNeuroVolume.unsafeFromRavel(out, targ, vol.label)

  @scala.annotation.targetName("nearestNeuroSeries")
  def nearest(vec: SomeScalarSeries[Double], target: SomeSampleSpace): SomeScalarSeries[Double] =
    executeContinuousSeries(vec, target, Method.Nearest)

  @scala.annotation.targetName("nearestGenericNeuroSeries")
  def nearest[A, Sem](
    vec: SomeNeuroSeries[A, Sem],
    target: SomeSampleSpace,
    fill: A
  )(using
      scala.reflect.ClassTag[A],
      DType[A],
      image4s.ValueSemantics[A, Sem]
  ): SomeNeuroSeries[A, Sem] =
    val src = vec.space
    val tLen = vec.nVolumes
    val targSpatial = target.spatialSpace
    val targDims = targSpatial.spatialDims
    val srcDims = src.spatialDims
    val out =
      RavelArray.tabulate[A](
        targDims(0),
        targDims(1),
        targDims(2),
        tLen
      ): (x, y, z, time) =>
        val world =
          targSpatial.indexToCoord(
            Vector(x.toDouble, y.toDouble, z.toDouble)
          )
        val sourceVoxel = src.coordToIndex(world)
        val sx = math.round(sourceVoxel(0)).toInt
        val sy = math.round(sourceVoxel(1)).toInt
        val sz = math.round(sourceVoxel(2)).toInt
        if sx >= 0 && sx < srcDims(0) &&
            sy >= 0 && sy < srcDims(1) &&
            sz >= 0 && sz < srcDims(2)
        then vec(sx, sy, sz, time)
        else fill
    val newSpace = targSpatial.addDim(tLen, Some(Axis.Time))
    SomeNeuroSeries.unsafeFromRavel(out, newSpace, vec.label)

  def trilinear(vol: SomeScalarVolume[Double], target: SomeSampleSpace): SomeScalarVolume[Double] =
    executeContinuous(vol, target, Method.Linear)

  @scala.annotation.targetName("trilinearNeuroSeries")
  def trilinear(vec: SomeScalarSeries[Double], target: SomeSampleSpace): SomeScalarSeries[Double] =
    executeContinuousSeries(vec, target, Method.Linear)

  def tricubic(vol: SomeScalarVolume[Double], target: SomeSampleSpace): SomeScalarVolume[Double] =
    executeContinuous(vol, target, Method.Cubic)

  @scala.annotation.targetName("tricubicNeuroSeries")
  def tricubic(vec: SomeScalarSeries[Double], target: SomeSampleSpace): SomeScalarSeries[Double] =
    executeContinuousSeries(vec, target, Method.Cubic)

  private def executeContinuous(
      volume: SomeScalarVolume[Double],
      target: SomeSampleSpace,
      method: Method
  ): SomeScalarVolume[Double] =
    val sourceGrid = GridSpec.fromSpace(volume.space)
    val targetGrid = GridSpec.fromSpace(target.spatialSpace)
    ResamplingPlan
      .make(
        sourceGrid,
        targetGrid,
        IdentityMorphism(SpatialDomainId("world-coordinate-resampling")),
        method
      )
      .flatMap(_.apply(volume, outside = 0.0))
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )

  private def executeContinuousSeries(
      series: SomeScalarSeries[Double],
      target: SomeSampleSpace,
      method: Method
  ): SomeScalarSeries[Double] =
    val sourceGrid = GridSpec.fromSpace(series.space)
    val targetGrid = GridSpec.fromSpace(target.spatialSpace)
    ResamplingPlan
      .make(
        sourceGrid,
        targetGrid,
        IdentityMorphism(SpatialDomainId("world-coordinate-resampling")),
        method
      )
      .flatMap(_.apply(series, outside = 0.0))
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )

object SpatialFilters:

  def mapf(
    vol: SomeScalarVolume[Double],
    kernel: Kernel3D,
    mask: Option[SomeMaskVolume] = None
  ): SomeScalarVolume[Double] =
    mask match
      case None =>
        filterVolumeWithProvider(vol, kernel)
      case Some(activeMask) =>
        val sp = vol.space
        val dims = sp.spatialDims
        GridCompatibility.requireSpatial(sp, activeMask.space)
        val nx = dims(0); val ny = dims(1); val nz = dims(2)
        val out =
          RavelArray.tabulate[Double](nx, ny, nz): (x, y, z) =>
            if activeMask(x, y, z) then
              var sum = 0.0
              var q = 0
              while q < kernel.size do
                val xx = x + kernel.dx(q)
                val yy = y + kernel.dy(q)
                val zz = z + kernel.dz(q)
                if xx >= 0 && xx < nx &&
                    yy >= 0 && yy < ny &&
                    zz >= 0 && zz < nz
                then sum += kernel.w(q) * vol(xx, yy, zz)
                q += 1
              sum
            else 0.0
        SomeNeuroVolume.unsafeFromRavel(out, sp, vol.label)

  private def filterVolumeWithProvider(
      volume: SomeScalarVolume[Double],
      kernel: Kernel3D
  ): SomeScalarVolume[Double] =
    val filtered =
      providerCorrelation(kernel)
        .flatMap(operation =>
          LinearFilter.correlate(
            volume.sampled,
            operation
          )
        )
        .fold(
          error => throw new IllegalArgumentException(error.message),
          identity
        )
    SomeNeuroVolume.unsafeFromSampled(filtered)

  private def filterSeriesWithProvider(
      series: SomeScalarSeries[Double],
      kernel: Kernel3D
  ): SomeScalarSeries[Double] =
    val filtered =
      providerCorrelation(kernel)
        .flatMap(operation =>
          LinearFilter.correlate(
            series.sampled,
            operation
          )
        )
        .fold(
          error => throw new IllegalArgumentException(error.message),
          identity
        )
    val native =
      SomeNeuroSeries
        .fromSampled(filtered)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    native

  private def providerCorrelation(
      kernel: Kernel3D
  ): Either[OpError, Correlation[D3, Double]] =
    val entries =
      Vector
        .tabulate(kernel.size): index =>
          (
            Vector(kernel.dx(index), kernel.dy(index), kernel.dz(index)),
            kernel.w(index)
          )
        .sortBy(entry => (entry._1(0), entry._1(1), entry._1(2)))
    for
      support <- Support.create[D3](
        entries.map(entry => Offset.unsafe[D3](entry._1))
      )
      dense <- ImageKernel.dense[D3, Double](
        support,
        entries.map(_._2)
      )
    yield Correlation(
      dense,
      FilterExtent.same(Border.Constant(0.0))
    )

  def gaussianBlur(
    vol: SomeScalarVolume[Double],
    sigma: Double = 2.0,
    window: Int = 1,
    mask: Option[SomeMaskVolume] = None
  ): SomeScalarVolume[Double] =
    require(window >= 1, "window must be >= 1")
    require(sigma > 0, "sigma must be positive")

    val sp = vol.space
    val spacing = sp.spacing

    mask.foreach { m =>
      GridCompatibility.requireSpatial(sp, m.space)
    }

    mapf(vol, gaussianKernel(spacing, sigma, window), mask)

  def gaussianBlur(vec: SomeScalarSeries[Double], sigma: Double, window: Int): SomeScalarSeries[Double] =
    require(window >= 1, "window must be >= 1")
    require(sigma > 0, "sigma must be positive")
    val kernel = gaussianKernel(vec.space.spacing, sigma, window)
    filterSeriesWithProvider(vec, kernel)

  def gaussianBlur(vec: SomeScalarSeries[Double]): SomeScalarSeries[Double] =
    gaussianBlur(vec, sigma = 2.0, window = 1)

  def gaussianBlur(vec: SomeScalarSeries[Double], sigma: Double): SomeScalarSeries[Double] =
    gaussianBlur(vec, sigma = sigma, window = 1)

  private def gaussianKernel(
      spacing: Vector[Double],
      sigma: Double,
      window: Int
  ): Kernel3D =
    val size = 2 * window + 1
    val denominator = 2.0 * sigma * sigma
    val kernel =
      Kernel3D(Vector(size, size, size), spacing): distance =>
        math.exp(-(distance * distance) / denominator)
    var total = 0.0
    var index = 0
    while index < kernel.size do
      total += kernel.w(index)
      index += 1
    if total != 0.0 then
      index = 0
      while index < kernel.size do
        kernel.w(index) = kernel.w(index) / total
        index += 1
    kernel

  def bilateralFilter(
    vol: SomeScalarVolume[Double],
    mask: Option[SomeMaskVolume] = None,
    window: Int = 1,
    spatialSigma: Double = 2.0,
    intensitySigma: Double = 1.0
  ): SomeScalarVolume[Double] =
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

    var sum = 0.0
    var sumsq = 0.0
    var count = 0
    var ordinal = 0
    while ordinal < spatialNels do
      if mask.forall(_.valueAtCanonicalOrdinal(ordinal)) then
        val value = vol.valueAtCanonicalOrdinal(ordinal)
        if value.isFinite then
          sum += value
          sumsq += value * value
          count += 1
      ordinal += 1

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

    val out =
      RavelArray.tabulate[Double](nx, ny, nz): (x, y, z) =>
        if mask.forall(_(x, y, z)) then
          val centerValue = vol(x, y, z)
          if !centerValue.isFinite then centerValue
          else
            var valueSum = 0.0
            var weightSum = 0.0
            var offset = 0
            while offset < total do
              val xx = x + dxArr(offset)
              val yy = y + dyArr(offset)
              val zz = z + dzArr(offset)
              val neighborValue =
                if xx >= 0 && xx < nx &&
                    yy >= 0 && yy < ny &&
                    zz >= 0 && zz < nz
                then vol(xx, yy, zz)
                else 0.0
              if neighborValue.isFinite then
                val difference = neighborValue - centerValue
                val weight =
                  spatialKernel(offset) *
                    math.exp(
                      -(difference * difference) / intensityVar
                    )
                valueSum += weight * neighborValue
                weightSum += weight
              offset += 1
            if weightSum == 0.0 then centerValue
            else valueSum / weightSum
        else 0.0

    SomeNeuroVolume.unsafeFromRavel(out, sp, vol.label)

  private def bilateralFilterVec(
    vec: SomeScalarSeries[Double],
    mask: Option[SomeMaskVolume],
    window: Int,
    spatialSigma: Double,
    intensitySigma: Double
  ): SomeScalarSeries[Double] =
    require(window >= 0, "window must be >= 0")
    require(spatialSigma > 0, "spatialSigma must be positive")
    require(intensitySigma > 0, "intensitySigma must be positive")
    val space = vec.space
    val dims = space.spatialDims
    val nx = dims(0); val ny = dims(1); val nz = dims(2)
    val tLen = vec.nVolumes
    val spatialNels = dims.product
    mask.foreach(value => GridCompatibility.requireSpatial(space, value.space))

    val width = 2 * window + 1
    val total = width * width * width
    val dx = Array.ofDim[Int](total)
    val dy = Array.ofDim[Int](total)
    val dz = Array.ofDim[Int](total)
    val spatialWeights = Array.ofDim[Double](total)
    val spatialVariance = 2.0 * spatialSigma * spatialSigma
    val spacing = space.spacing
    var offset = 0
    var oz = -window
    while oz <= window do
      var oy = -window
      while oy <= window do
        var ox = -window
        while ox <= window do
          dx(offset) = ox
          dy(offset) = oy
          dz(offset) = oz
          val rx = ox.toDouble * spacing(0)
          val ry = oy.toDouble * spacing(1)
          val rz = oz.toDouble * spacing(2)
          spatialWeights(offset) =
            math.exp(-(rx * rx + ry * ry + rz * rz) / spatialVariance)
          offset += 1
          ox += 1
        oy += 1
      oz += 1

    val out =
      RavelArray.build[Double, Rank[4]](
        Shape(nx, ny, nz, tLen)
      ): output =>
        var time = 0
        while time < tLen do
          var sum = 0.0
          var sumSquares = 0.0
          var count = 0
          var ordinal = 0
          while ordinal < spatialNels do
            if mask.forall(_.valueAtCanonicalOrdinal(ordinal)) then
              val value = vec.valueAtVoxelOrdinal(ordinal, time)
              if value.isFinite then
                sum += value
                sumSquares += value * value
                count += 1
            ordinal += 1
          val standardDeviation =
            if count <= 1 then 0.0
            else
              val mean = sum / count.toDouble
              val variance =
                (sumSquares - count.toDouble * mean * mean) /
                  (count.toDouble - 1.0)
              if variance > 0.0 then math.sqrt(variance) else 0.0
          val rawIntensityVariance =
            2.0 * intensitySigma * intensitySigma *
              standardDeviation * standardDeviation
          val intensityVariance =
            if !rawIntensityVariance.isFinite ||
                rawIntensityVariance < 1e-12
            then 1e-12
            else rawIntensityVariance

          ordinal = 0
          while ordinal < spatialNels do
            val voxel = space.indexToVoxel3D(ordinal)
            val x = voxel.x
            val y = voxel.y
            val z = voxel.z
            val value =
              if mask.forall(_(x, y, z)) then
                val center = vec(x, y, z, time)
                if !center.isFinite then center
                else
                  var valueSum = 0.0
                  var weightSum = 0.0
                  offset = 0
                  while offset < total do
                    val xx = x + dx(offset)
                    val yy = y + dy(offset)
                    val zz = z + dz(offset)
                    val neighbor =
                      if xx >= 0 && xx < nx &&
                          yy >= 0 && yy < ny &&
                          zz >= 0 && zz < nz
                      then vec(xx, yy, zz, time)
                      else 0.0
                    if neighbor.isFinite then
                      val difference = neighbor - center
                      val weight =
                        spatialWeights(offset) *
                          math.exp(
                            -(difference * difference) /
                              intensityVariance
                          )
                      valueSum += weight * neighbor
                      weightSum += weight
                    offset += 1
                  if weightSum == 0.0 then center
                  else valueSum / weightSum
              else 0.0
            output.writeLinear(ordinal * tLen + time, value)
            ordinal += 1
          time += 1
    SomeNeuroSeries.unsafeFromRavel(out, space, vec.label)

  def bilateralFilter(vec: SomeScalarSeries[Double]): SomeScalarSeries[Double] =
    bilateralFilterVec(vec, mask = None, window = 1, spatialSigma = 2.0, intensitySigma = 1.0)

  def bilateralFilter(vec: SomeScalarSeries[Double], mask: Option[SomeMaskVolume]): SomeScalarSeries[Double] =
    bilateralFilterVec(vec, mask, window = 1, spatialSigma = 2.0, intensitySigma = 1.0)

  @scala.annotation.targetName("bilateralFilterNeuroSeries")
  def bilateralFilter(
    vec: SomeScalarSeries[Double],
    mask: Option[SomeMaskVolume],
    window: Int,
    spatialSigma: Double,
    intensitySigma: Double
  ): SomeScalarSeries[Double] =
    bilateralFilterVec(vec, mask, window, spatialSigma, intensitySigma)

  def bilateralFilter4D(
    vec: SomeScalarSeries[Double],
    mask: Option[SomeMaskVolume] = None,
    spatialWindow: Int = 1,
    temporalWindow: Int = 1,
    spatialSigma: Double = 2.0,
    intensitySigma: Double = 1.0,
    temporalSigma: Double = 1.0,
    temporalSpacing: Double = 1.0
  ): SomeScalarSeries[Double] =
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

    var sum = 0.0
    var sumsq = 0.0
    var count = 0
    var ordinal = 0
    while ordinal < spatialNels do
      if mask.forall(_.valueAtCanonicalOrdinal(ordinal)) then
        var time = 0
        while time < tLen do
          val value = vec.valueAtVoxelOrdinal(ordinal, time)
          if value.isFinite then
            sum += value
            sumsq += value * value
            count += 1
          time += 1
      ordinal += 1

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

    val out =
      RavelArray.tabulate[Double](nx, ny, nz, tLen):
        (x, y, z, time) =>
          val center = vec(x, y, z, time)
          if !mask.forall(_(x, y, z)) || !center.isFinite then center
          else
            var valueSum = 0.0
            var weightSum = 0.0
            var offset = 0
            while offset < total do
              val neighborTime = time + dtArr(offset)
              if neighborTime >= 0 && neighborTime < tLen then
                val xx = x + dxArr(offset)
                val yy = y + dyArr(offset)
                val zz = z + dzArr(offset)
                if xx >= 0 && xx < nx &&
                    yy >= 0 && yy < ny &&
                    zz >= 0 && zz < nz
                then
                  val neighbor = vec(xx, yy, zz, neighborTime)
                  if neighbor.isFinite then
                    val difference = center - neighbor
                    val weight =
                      kernel(offset) *
                        math.exp(
                          -(difference * difference) / intensityVar
                        )
                    valueSum += weight * neighbor
                    weightSum += weight
              offset += 1
            if weightSum > 0.0 then valueSum / weightSum else center

    SomeNeuroSeries.unsafeFromRavel(out, sp, vec.label)
