package scalafim.image

import SampleSpaces.*

import Ops.*
import image4s.geometry.Grid
import ravel.NDArray as RavelArray
import spire.std.double.given
import spire.std.int.given

class StatsMaskCompareSuite extends munit.FunSuite:

  test("NeuroStats summarizes dense volumes") {
    val sp = SampleSpaces(Vector(2, 2, 2))
    val vol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), sp)
    val summary = NeuroStats.summarize(vol)

    assertEquals(summary.dims, Vector(2, 2, 2), clue = "")
    assertEquals(summary.stats.count, 8, clue = "")
    assertEquals(summary.stats.zeros, 1, clue = "")
    assertEquals(summary.stats.nonZeros, 7, clue = "")
    assertEquals(summary.stats.min, 0.0, clue = "")
    assertEquals(summary.stats.max, 7.0, clue = "")
    assertEquals(summary.stats.sum, 28.0, clue = "")
    assert(math.abs(summary.stats.mean - 3.5) < 1e-12, clue = "")

    assertEquals(vol.summary, summary, clue = "")
  }

  test("temporalMean for dense SomeNeuroSeries matches row means") {
    val sp = SampleSpaces(Vector(2, 1, 1, 3))
    val vec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](Array(0.0, 2.0, 4.0, 1.0, 3.0, 5.0), sp)
    val mean = NeuroStats.temporalMean(vec)
    val vals = Vector.tabulate(mean.copyToCanonicalArray.length)(i => mean.copyToCanonicalArray(i))

    assertEquals(mean.space.dims, Vector(2, 1, 1), clue = "")
    assertEquals(vals, Vector(2.0, 3.0), clue = "")
    assert(Grid.exactCongruence(vec.temporalMean.grid, mean.grid).isRight)
    assertEquals(Vector.tabulate(vec.temporalMean.copyToCanonicalArray.length)(i => vec.temporalMean.copyToCanonicalArray(i)), vals, clue = "")
  }

  test("temporalMean for selected series preserves exact support") {
    val sp = SampleSpaces(Vector(3, 1, 1, 3))
    val packed =
      GridDomain
        .register(
          ProviderSpaces.grid(sp.spatialSpace),
          "stats selected temporal mean",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain = packed.value
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val data =
      RavelArray.tabulate[Double](2, 3) { (position, time) =>
        if position == 0 then 1.0 + 2.0 * time
        else 2.0 + 2.0 * time
      }
    val selected =
      SelectedSeries
        .create(
          domain,
          selection,
          image4s.Axis
            .ordinal("time", image4s.AxisKind.Time, 3)
            .toOption
            .get,
          data
        )
        .toOption
        .get
    val mean = selected.temporalMean.toOption.get
    val summary = selected.summary

    assertEquals(mean.selection.ordinals.toVector, Vector(0, 2), clue = "")
    assertEquals(mean.data.iterator.toVector, Vector(3.0, 4.0), clue = "")
    assertEquals(summary.kind, "SelectedSeries", clue = "")
    assertEquals(summary.timePoints, 3, clue = "")
    assertEquals(summary.global.count, 6, clue = "")
  }

  test("mask images and exact selected support convert only through named operations") {
    val sp = SampleSpaces(Vector(3, 1, 1, 2))
    val vec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](6)(_.toDouble), sp)
    val denseMask = Mask.of(vec)
    assertEquals(Vector.tabulate(denseMask.copyToCanonicalArray.length)(i => denseMask.copyToCanonicalArray(i)), Vector(true, true, true), clue = "")
    val packed =
      GridDomain
        .register(
          ProviderSpaces.grid(sp.spatialSpace),
          "stats mask region",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain = packed.value
    val support =
      locus4s.Region.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val semanticMask = Mask.fromRegion(domain, support).toOption.get
    val recovered = Mask.region(domain, semanticMask).toOption.get

    assertEquals(recovered.ordinalsInDomainOrder.toVector, Vector(0, 2), clue = "")
  }

  test("NeuroCompare builds logical volumes and vectors") {
    val sp = SampleSpaces(Vector(2, 2, 1))
    val vol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](Array(0.0, 1.0, 2.0, 3.0), sp)
    val gt = vol.gt(1.0)

    assertEquals(gt.space, sp, clue = "")
    assertEquals(Vector.tabulate(gt.copyToCanonicalArray.length)(i => gt.copyToCanonicalArray(i)), Vector(false, false, true, true), clue = "")
    assertEquals(Vector.tabulate(vol.eqv(vol).copyToCanonicalArray.length)(i => vol.eqv(vol).copyToCanonicalArray(i)), Vector.fill(4)(true), clue = "")

    val sp4 = SampleSpaces(Vector(2, 1, 1, 2))
    val vec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](Array(0.0, 2.0, 4.0, 6.0), sp4)
    val lt = vec.lt(5.0)
    assertEquals(Vector.tabulate(lt.copyToCanonicalArray.length)(i => lt.copyToCanonicalArray(i)), Vector(true, true, true, false), clue = "")
  }

  test("NeuroCompare preserves selected support") {
    val sp = SampleSpaces(Vector(3, 1, 1))
    val packed =
      GridDomain
        .register(
          ProviderSpaces.grid(sp),
          "stats selected comparison",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain = packed.value
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val selected =
      SelectedVolume
        .create(
          domain,
          selection,
          RavelArray.fromSeq(ravel.Shape(2), Vector(2.0, 5.0))
        )
        .toOption
        .get
    val compared = selected.gt(1.0)
    val denseCompared = compared.toDense(false).toOption.get
    assertEquals(denseCompared.data.iterator.toVector, Vector(true, false, true), clue = "")

  }

  test("NeuroStats summarizes SomeNeuroSeries temporal ranges") {
    val sp = SampleSpaces(Vector(2, 1, 1, 3))
    val vec = SomeScalarSeries.unsafeCopyFromCanonicalArray[Double](Array(0.0, 2.0, 4.0, 1.0, 3.0, 5.0), sp)
    val summary = vec.summary

    assertEquals(summary.timePoints, 3, clue = "")
    assertEquals(summary.totalVoxels, 2, clue = "")
    assertEquals(summary.temporalMeanRange, (2.0, 3.0), clue = "")
    assertEquals(summary.nonZeroVoxels, 2, clue = "")
    assertEquals(summary.global.count, 6, clue = "")
  }
