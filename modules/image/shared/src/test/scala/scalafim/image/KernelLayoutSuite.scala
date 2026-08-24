package scalafim.image

import SampleSpaces.*

import image4s.SamplingAlignment
import image4s.geometry.Grid
import ravel.NDArray

class KernelLayoutSuite extends munit.FunSuite:

  test("view-safe kernels agree with whole-canonical 2x3x5x7 oracles"):
    val volumeSpace = SampleSpaces(Vector(2, 3, 5))
    val volumeData =
      NDArray
        .tabulate[Double](2, 3, 5): (x, y, z) =>
          100.0 * x + 10.0 * y + z
        .reverse(1)
    val viewVolume =
      SomeScalarVolume.unsafeFromRavel(volumeData, volumeSpace, "asymmetric-view")
    val canonicalVolume =
      SomeScalarVolume.unsafeFromRavel(volumeData.copy, volumeSpace, "asymmetric-view")

    assert(!viewVolume.values.isCanonicalLayout)
    assert(canonicalVolume.values.isCanonicalLayout)
    assert(canonicalVolume.values.isWholeBuffer)

    val viewMapped = viewVolume.mapValues(value => value * 1.5 - 2.0)
    val canonicalMapped =
      canonicalVolume.mapValues(value => value * 1.5 - 2.0)
    assertSameVolume(viewMapped, canonicalMapped)
    assertWholeCanonical(viewMapped)

    val viewZipped =
      viewVolume
        .zipExact(viewMapped)(_ + _)
        .fold(error => fail(error.message), identity)
    val canonicalZipped =
      canonicalVolume
        .zipExact(canonicalMapped)(_ + _)
        .fold(error => fail(error.message), identity)
    assertSameVolume(viewZipped, canonicalZipped)
    assertWholeCanonical(viewZipped)

    val viewBlurred =
      SpatialFilters.gaussianBlur(viewVolume, sigma = 0.8, window = 1)
    val canonicalBlurred =
      SpatialFilters.gaussianBlur(canonicalVolume, sigma = 0.8, window = 1)
    assertSameVolume(viewBlurred, canonicalBlurred, tolerance = 1e-12)
    assertWholeCanonical(viewBlurred)

    val grid = GridSpec.fromSpace(volumeSpace)
    val identityMorphism =
      SpatialPullbacks.worldAligned(grid, grid)
    val resampling =
      ResamplingPlan
        .make(grid, grid, identityMorphism, Resample.Method.Linear)
        .fold(error => fail(error.message), identity)
    assertEquals(
      resampling.executionModel,
      ResamplingExecutionModel.ProviderAffine
    )
    assertEquals(resampling.materializedCoordinateCount, 0)
    val viewResampled =
      resampling(viewVolume).fold(error => fail(error.message), identity)
    val canonicalResampled =
      resampling(canonicalVolume).fold(error => fail(error.message), identity)
    assertSameVolume(viewResampled, canonicalResampled, tolerance = 1e-12)
    assertWholeCanonical(viewResampled)

    val seriesSpace = volumeSpace.addDim(ProviderAxes.time(7))
    val seriesData =
      NDArray
        .tabulate[Double](2, 3, 5, 7): (x, y, z, time) =>
          1000.0 * x + 100.0 * y + 10.0 * z + time
        .reverse(0)
    val viewSeries =
      SomeScalarSeries.unsafeFromRavel(seriesData, seriesSpace, "asymmetric-series-view")
    val canonicalSeries =
      SomeScalarSeries.unsafeFromRavel(
        seriesData.copy,
        seriesSpace,
        "asymmetric-series-view"
      )

    assert(!viewSeries.values.isCanonicalLayout)
    assert(canonicalSeries.values.isCanonicalLayout)

    val viewSeriesResampled =
      resampling(viewSeries).fold(error => fail(error.message), identity)
    val canonicalSeriesResampled =
      resampling(canonicalSeries).fold(error => fail(error.message), identity)
    assertSameSeries(
      viewSeriesResampled,
      canonicalSeriesResampled,
      tolerance = 1e-12
    )
    assertWholeCanonical(viewSeriesResampled)

    val viewSummary = NeuroStats.summarize(viewSeries)
    val canonicalSummary = NeuroStats.summarize(canonicalSeries)
    assertEquals(
      viewSummary.copy(
        global = viewSummary.global.copy(
          product = normalizeNaN(viewSummary.global.product)
        )
      ),
      canonicalSummary.copy(
        global = canonicalSummary.global.copy(
          product = normalizeNaN(canonicalSummary.global.product)
        )
      )
    )

    val viewMean = NeuroStats.temporalMean(viewSeries)
    val canonicalMean = NeuroStats.temporalMean(canonicalSeries)
    assertSameVolume(viewMean, canonicalMean)
    assertWholeCanonical(viewMean)

    val viewSeriesMapped = viewSeries.mapValues(_ / 3.0)
    val canonicalSeriesMapped = canonicalSeries.mapValues(_ / 3.0)
    assertSameSeries(viewSeriesMapped, canonicalSeriesMapped)
    assertWholeCanonical(viewSeriesMapped)

    val viewSeriesZipped =
      viewSeries
        .zipExact(viewSeriesMapped)(_ - _)
        .fold(error => fail(error.message), identity)
    val canonicalSeriesZipped =
      canonicalSeries
        .zipExact(canonicalSeriesMapped)(_ - _)
        .fold(error => fail(error.message), identity)
    assertSameSeries(viewSeriesZipped, canonicalSeriesZipped)
    assertWholeCanonical(viewSeriesZipped)

    val voxel = volumeSpace.gridToIndex3D(1, 2, 4)
    val viewTimeCourse = viewSeries.timeSeries(voxel)
    val canonicalTimeCourse = canonicalSeries.timeSeries(voxel)
    assert(!viewTimeCourse.isWholeBuffer)
    assertEquals(viewTimeCourse.size, 7)
    var time = 0
    while time < 7 do
      assertEqualsDouble(
        viewTimeCourse(time),
        canonicalTimeCourse(time),
        0.0
      )
      time += 1

  private def assertWholeCanonical[A, Sem](volume: SomeNeuroVolume[A, Sem]): Unit =
    assert(volume.values.isCanonicalLayout)
    assert(volume.values.isWholeBuffer)

  @scala.annotation.targetName("assertWholeCanonicalSeries")
  private def assertWholeCanonical[A, Sem](series: SomeNeuroSeries[A, Sem]): Unit =
    assert(series.values.isCanonicalLayout)
    assert(series.values.isWholeBuffer)

  private def normalizeNaN(value: Double): Double =
    if value.isNaN then 0.0 else value

  private def assertSameVolume(
      actual: SomeScalarVolume[Double],
      expected: SomeScalarVolume[Double],
      tolerance: Double = 0.0
  ): Unit =
    assert(Grid.exactCongruence(actual.grid, expected.grid).isRight)
    val shape = actual.space.spatialDims
    var x = 0
    while x < shape(0) do
      var y = 0
      while y < shape(1) do
        var z = 0
        while z < shape(2) do
          assertEqualsDouble(actual(x, y, z), expected(x, y, z), tolerance)
          z += 1
        y += 1
      x += 1

  private def assertSameSeries(
      actual: SomeScalarSeries[Double],
      expected: SomeScalarSeries[Double],
      tolerance: Double = 0.0
  ): Unit =
    val alignment =
      for
        left <- SampleSpaces.requireD3(actual.space).left.map(_.message)
        right <- SampleSpaces.requireD3(expected.space).left.map(_.message)
        evidence <- SamplingAlignment.exact(left, right).left.map(_.message)
      yield evidence
    assert(alignment.isRight)
    val shape = actual.space.spatialDims
    var x = 0
    while x < shape(0) do
      var y = 0
      while y < shape(1) do
        var z = 0
        while z < shape(2) do
          var time = 0
          while time < actual.nVolumes do
            assertEqualsDouble(
              actual(x, y, z, time),
              expected(x, y, z, time),
              tolerance
            )
            time += 1
          z += 1
        y += 1
      x += 1
