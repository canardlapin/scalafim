package scalafim.image

import SampleSpaces.*

import image4s.Continuous
import image4s.Axis
import image4s.AxisKind
import image4s.Mask as MaskSemantics
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.SamplingAlignment
import image4s.ValueSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import ravel.Array1
import ravel.DType
import ravel.NDArray as RavelArray
import spire.algebra.Order

object NeuroStats:
  private def timeAxis(extent: Int): Axis =
    Axis
      .ordinal("time", AxisKind.Time, extent)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  final case class ScalarSummary(
    count: Int,
    missing: Int,
    sum: Double,
    product: Double,
    min: Double,
    max: Double,
    mean: Double,
    sd: Double,
    zeros: Int,
    nonZeros: Int
  ):
    def range: (Double, Double) = (min, max)

  final case class NeuroVolumeSummary(
    kind: String,
    dims: Vector[Int],
    spacing: Vector[Double],
    origin: Vector[Double],
    orientation: String,
    stats: ScalarSummary
  )

  final case class NeuroSeriesSummary(
    kind: String,
    dims: Vector[Int],
    spacing: Vector[Double],
    origin: Vector[Double],
    orientation: String,
    timePoints: Int,
    global: ScalarSummary,
    temporalMeanRange: (Double, Double),
    temporalSdRange: (Double, Double),
    nonZeroVoxels: Int,
    totalVoxels: Int
  )

  def summarize(values: Array[Double], naRm: Boolean = true): ScalarSummary =
    summarizeIndexed(values.length, values.apply, naRm)

  def summarize(values: Array1[Double], naRm: Boolean): ScalarSummary =
    summarizeIndexed(values.size, index => values(index), naRm)

  private def summarizeIndexed(
      length: Int,
      valueAt: Int => Double,
      naRm: Boolean
  ): ScalarSummary =
    var i = 0
    var n = 0
    var missing = 0
    var zeros = 0
    var nonZeros = 0
    var sum = 0.0
    var sumSq = 0.0
    var product = 1.0
    var min = Double.PositiveInfinity
    var max = Double.NegativeInfinity

    while i < length do
      val v = valueAt(i)
      if v.isNaN then
        missing += 1
        if !naRm then
          sum = Double.NaN
          sumSq = Double.NaN
          product = Double.NaN
      else
        n += 1
        sum += v
        sumSq += v * v
        product *= v
        if v < min then min = v
        if v > max then max = v
        if v == 0.0 then zeros += 1 else nonZeros += 1
      i += 1

    if !naRm && missing > 0 then
      ScalarSummary(n, missing, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, Double.NaN, zeros, nonZeros)
    else
      val mean = if n == 0 then Double.NaN else sum / n.toDouble
      val sd =
        if n <= 1 then Double.NaN
        else
          val variance = (sumSq - (sum * sum) / n.toDouble) / (n - 1).toDouble
          math.sqrt(math.max(0.0, variance))
      ScalarSummary(n, missing, sum, product, min, max, mean, sd, zeros, nonZeros)

  def summarize(vol: SomeScalarVolume[Double]): NeuroVolumeSummary =
    summarize(vol, naRm = true)

  def summarize(vol: SomeScalarVolume[Double], naRm: Boolean): NeuroVolumeSummary =
    NeuroVolumeSummary(
      kind = "SomeNeuroVolume",
      dims = vol.space.spatialDims,
      spacing = vol.space.spacing,
      origin = vol.space.origin,
      orientation = orientation(vol.space),
      stats = summarizeIndexed(
        vol.values.size,
        vol.valueAtCanonicalOrdinal,
        naRm
      )
    )

  def summarize[F <: Frame[D3], S](
      volume: SelectedVolume[F, S, Double, Continuous]
  ): NeuroVolumeSummary =
    summarize(volume, naRm = true)

  def summarize[F <: Frame[D3], S](
      volume: SelectedVolume[F, S, Double, Continuous],
      naRm: Boolean
  ): NeuroVolumeSummary =
    val space = SampleSpace.create(volume.domain.grid, NonSpatialAxes.empty)
    NeuroVolumeSummary(
      kind = "SelectedVolume",
      dims = space.spatialDims,
      spacing = space.spacing,
      origin = space.origin,
      orientation = orientation(space),
      stats = summarizeIndexed(
        volume.data.size,
        volume.data.apply,
        naRm
      )
    )

  def summarize(vec: SomeScalarSeries[Double]): NeuroSeriesSummary =
    summarize(vec, naRm = true)

  def summarize(vec: SomeScalarSeries[Double], naRm: Boolean): NeuroSeriesSummary =
    summarizeVec(
      "SomeNeuroSeries",
      vec.space,
      vec.values.size,
      vec.valueAtCanonicalOrdinal,
      vec.nVolumes,
      vec.space.spatialDims.product,
      naRm
    )

  def summarize[F <: Frame[D3], S](
      series: SelectedSeries[F, S, Double, Continuous]
  ): NeuroSeriesSummary =
    summarize(series, naRm = true)

  def summarize[F <: Frame[D3], S](
      series: SelectedSeries[F, S, Double, Continuous],
      naRm: Boolean
  ): NeuroSeriesSummary =
    val space = SampleSpace.create(series.domain.grid, series.nonSpatialAxes)
    summarizeSparseVec(
      "SelectedSeries",
      space,
      series.nTime,
      series.selection.size,
      (time, position) => series.data(position, time),
      naRm
    )

  def temporalMean(vec: SomeScalarSeries[Double]): SomeScalarVolume[Double] =
    val shape = vec.space.spatialDims
    val tLen = vec.nVolumes
    val out =
      RavelArray.build[Double, ravel.Rank[3]](
        ravel.Shape(shape(0), shape(1), shape(2))
      ): output =>
        var ordinal = 0
        var x = 0
        while x < shape(0) do
          var y = 0
          while y < shape(1) do
            var z = 0
            while z < shape(2) do
              var time = 0
              var sum = 0.0
              while time < tLen do
                sum += vec(x, y, z, time)
                time += 1
              output.writeLinear(ordinal, sum / tLen.toDouble)
              ordinal += 1
              z += 1
            y += 1
          x += 1

    SomeNeuroVolume.unsafeFromRavel(out, vec.space.spatialSpace, vec.label)

  def temporalMean[F <: Frame[D3], S](
      series: SelectedSeries[F, S, Double, Continuous]
  ): Either[
    SelectedImageError,
    SelectedVolume[F, S, Double, Continuous]
  ] =
    given DType[Double] = series.dtype
    val tLen = series.nTime
    val out =
      RavelArray.build[Double, ravel.Rank[1]](
        ravel.Shape(series.selection.size)
      ): output =>
        var position = 0
        while position < series.selection.size do
          var time = 0
          var sum = 0.0
          while time < tLen do
            sum += series.data(position, time)
            time += 1
          output.writeLinear(position, sum / tLen.toDouble)
          position += 1
    SelectedVolume.continuous(
      series.domain,
      series.selection,
      out,
      series.metadata
    )

  private def summarizeVec(
    kind: String,
    space: SomeSampleSpace,
    dataLength: Int,
    valueAt: Int => Double,
    tLen: Int,
    spatialNels: Int,
    naRm: Boolean
  ): NeuroSeriesSummary =
    val global = summarizeIndexed(dataLength, valueAt, naRm)
    var nonZero = 0
    var minimumMean = Double.PositiveInfinity
    var maximumMean = Double.NegativeInfinity
    var minimumSd = Double.PositiveInfinity
    var maximumSd = Double.NegativeInfinity

    var lin = 0
    while lin < spatialNels do
      var t = 0
      var sum = 0.0
      var sumSq = 0.0
      while t < tLen do
        val v = valueAt(lin * tLen + t)
        sum += v
        sumSq += v * v
        t += 1
      val mean = sum / tLen.toDouble
      if mean != 0.0 then nonZero += 1
      if !mean.isNaN then
        if mean < minimumMean then minimumMean = mean
        if mean > maximumMean then maximumMean = mean
      val sd =
        if tLen <= 1 then 0.0
        else math.sqrt(math.max(0.0, (sumSq - (sum * sum) / tLen.toDouble) / (tLen - 1).toDouble))
      if !sd.isNaN then
        if sd < minimumSd then minimumSd = sd
        if sd > maximumSd then maximumSd = sd
      lin += 1

    NeuroSeriesSummary(
      kind = kind,
      dims = space.dims.take(4),
      spacing = space.spacing,
      origin = space.origin,
      orientation = orientation(space),
      timePoints = tLen,
      global = global,
      temporalMeanRange = minimumMean -> maximumMean,
      temporalSdRange = minimumSd -> maximumSd,
      nonZeroVoxels = nonZero,
      totalVoxels = spatialNels
    )

  private def summarizeSparseVec(
    kind: String,
    space: SomeSampleSpace,
    tLen: Int,
    nColumns: Int,
    valueAt: (Int, Int) => Double,
    naRm: Boolean
  ): NeuroSeriesSummary =
    val global =
      summarizeIndexed(
        tLen * nColumns,
        index => valueAt(index / nColumns, index % nColumns),
        naRm
      )
    var nonZero = 0
    var minimumMean = Double.PositiveInfinity
    var maximumMean = Double.NegativeInfinity
    var minimumSd = Double.PositiveInfinity
    var maximumSd = Double.NegativeInfinity

    var col = 0
    while col < nColumns do
      var t = 0
      var sum = 0.0
      var sumSq = 0.0
      while t < tLen do
        val v = valueAt(t, col)
        sum += v
        sumSq += v * v
        t += 1
      val mean = sum / tLen.toDouble
      if mean != 0.0 then nonZero += 1
      if !mean.isNaN then
        if mean < minimumMean then minimumMean = mean
        if mean > maximumMean then maximumMean = mean
      val sd =
        if tLen <= 1 then 0.0
        else math.sqrt(math.max(0.0, (sumSq - (sum * sum) / tLen.toDouble) / (tLen - 1).toDouble))
      if !sd.isNaN then
        if sd < minimumSd then minimumSd = sd
        if sd > maximumSd then maximumSd = sd
      col += 1

    NeuroSeriesSummary(
      kind = kind,
      dims = space.dims.take(4),
      spacing = space.spacing,
      origin = space.origin,
      orientation = orientation(space),
      timePoints = tLen,
      global = global,
      temporalMeanRange = minimumMean -> maximumMean,
      temporalSdRange = minimumSd -> maximumSd,
      nonZeroVoxels = nonZero,
      totalVoxels = space.spatialDims.product
    )

  private def orientation(space: SomeSampleSpace): String =
    space.orientation.axes.map(_.abbrev).mkString(" / ")

object NeuroCompare:

  enum Predicate:
    case LT, LTE, GT, GTE, EQV, NEQ

  def compare[A: Order, Sem](x: SomeNeuroVolume[A, Sem], y: SomeNeuroVolume[A, Sem], predicate: Predicate): SomeMaskVolume =
    requireSameSpace(x.space, y.space)
    val shape = x.space.spatialDims
    val out =
      RavelArray.tabulate[Boolean](shape(0), shape(1), shape(2)):
        (i, j, k) => test(x(i, j, k), y(i, j, k), predicate)
    SomeNeuroVolume.unsafeFromRavel(out, x.space, x.label)

  def compare[A: Order, Sem](x: SomeNeuroVolume[A, Sem], scalar: A, predicate: Predicate): SomeMaskVolume =
    val shape = x.space.spatialDims
    val out =
      RavelArray.tabulate[Boolean](shape(0), shape(1), shape(2)):
        (i, j, k) => test(x(i, j, k), scalar, predicate)
    SomeNeuroVolume.unsafeFromRavel(out, x.space, x.label)

  def compare[F <: Frame[D3], S, A: Order, Sem](
      volume: SelectedVolume[F, S, A, Sem],
      scalar: A,
      predicate: Predicate
  )(using
      DType[Boolean],
      ValueSemantics[Boolean, MaskSemantics]
  ): SelectedVolume[F, S, Boolean, MaskSemantics] =
    SelectedVolume.fromSelected(
      volume.selected.mapValues[Boolean, MaskSemantics]: value =>
        test(value, scalar, predicate)
    )

  def compare[F <: Frame[D3], S, A: Order, Sem](
      scalar: A,
      volume: SelectedVolume[F, S, A, Sem],
      predicate: Predicate
  )(using
      DType[Boolean],
      ValueSemantics[Boolean, MaskSemantics]
  ): SelectedVolume[F, S, Boolean, MaskSemantics] =
    SelectedVolume.fromSelected(
      volume.selected.mapValues[Boolean, MaskSemantics]: value =>
        test(scalar, value, predicate)
    )

  def compare[A: Order, Sem](scalar: A, x: SomeNeuroVolume[A, Sem], predicate: Predicate): SomeMaskVolume =
    val shape = x.space.spatialDims
    val out =
      RavelArray.tabulate[Boolean](shape(0), shape(1), shape(2)):
        (i, j, k) => test(scalar, x(i, j, k), predicate)
    SomeNeuroVolume.unsafeFromRavel(out, x.space, x.label)

  @scala.annotation.targetName("compareNeuroSeriesPair")
  def compare[A: Order, Sem](x: SomeNeuroSeries[A, Sem], y: SomeNeuroSeries[A, Sem], predicate: Predicate): SomeMaskSeries =
    requireSameSpace(x.space, y.space)
    val shape = x.space.spatialDims
    val out =
      RavelArray.tabulate[Boolean](
        shape(0),
        shape(1),
        shape(2),
        x.nVolumes
      )((i, j, k, t) => test(x(i, j, k, t), y(i, j, k, t), predicate))
    SomeNeuroSeries.unsafeFromRavel(out, x.space, x.label)

  @scala.annotation.targetName("compareNeuroSeriesScalar")
  def compare[A: Order, Sem](x: SomeNeuroSeries[A, Sem], scalar: A, predicate: Predicate): SomeMaskSeries =
    val shape = x.space.spatialDims
    val out =
      RavelArray.tabulate[Boolean](
        shape(0),
        shape(1),
        shape(2),
        x.nVolumes
      )((i, j, k, t) => test(x(i, j, k, t), scalar, predicate))
    SomeNeuroSeries.unsafeFromRavel(out, x.space, x.label)

  def gt[A: Order, Sem](x: SomeNeuroVolume[A, Sem], scalar: A): SomeMaskVolume =
    compare(x, scalar, Predicate.GT)

  def lt[A: Order, Sem](x: SomeNeuroVolume[A, Sem], scalar: A): SomeMaskVolume =
    compare(x, scalar, Predicate.LT)

  def gte[A: Order, Sem](x: SomeNeuroVolume[A, Sem], scalar: A): SomeMaskVolume =
    compare(x, scalar, Predicate.GTE)

  def lte[A: Order, Sem](x: SomeNeuroVolume[A, Sem], scalar: A): SomeMaskVolume =
    compare(x, scalar, Predicate.LTE)

  def eqv[A: Order, Sem](x: SomeNeuroVolume[A, Sem], y: SomeNeuroVolume[A, Sem]): SomeMaskVolume =
    compare(x, y, Predicate.EQV)

  def neq[A: Order, Sem](x: SomeNeuroVolume[A, Sem], y: SomeNeuroVolume[A, Sem]): SomeMaskVolume =
    compare(x, y, Predicate.NEQ)

  private def test[A: Order](left: A, right: A, predicate: Predicate): Boolean =
    val ord = summon[Order[A]]
    predicate match
      case Predicate.LT => ord.lt(left, right)
      case Predicate.LTE => ord.lteqv(left, right)
      case Predicate.GT => ord.gt(left, right)
      case Predicate.GTE => ord.gteqv(left, right)
      case Predicate.EQV => ord.eqv(left, right)
      case Predicate.NEQ => !ord.eqv(left, right)

  private def requireSameSpace(a: SomeSampleSpace, b: SomeSampleSpace): Unit =
    val checked =
      for
        left <- SampleSpaces.requireD3(a).left.map(NeuroImageError.Space.apply)
        right <- SampleSpaces.requireD3(b).left.map(NeuroImageError.Space.apply)
        _ <- SamplingAlignment
          .exact(left, right)
          .left
          .map(NeuroImageError.Image.apply)
      yield ()
    checked.fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )
