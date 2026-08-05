package scalafim.image

import Ops.*
import ravel.NDArray as RavelArray
import spire.std.double.given
import spire.std.int.given

class StatsMaskCompareSuite extends munit.FunSuite:

  test("NeuroStats summarizes dense volumes") {
    val sp = NeuroSpace(Vector(2, 2, 2))
    val vol = NeuroVol.fromLinear[Double](Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), sp)
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

  test("temporalMean for dense NeuroVec matches row means") {
    val sp = NeuroSpace(Vector(2, 1, 1, 3))
    val vec = NeuroVec.fromLinear[Double](Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0), sp)
    val mean = NeuroStats.temporalMean(vec)
    val vals = Vector.tabulate(mean.copyLegacyLinear.length)(i => mean.copyLegacyLinear(i))

    assertEquals(mean.space.dims, Vector(2, 1, 1), clue = "")
    assertEquals(vals, Vector(2.0, 3.0), clue = "")
    assertEquals(
      GridCompatibility.exact(vec.temporalMean.space, mean.space),
      Right(()),
      clue = ""
    )
    assertEquals(Vector.tabulate(vec.temporalMean.copyLegacyLinear.length)(i => vec.temporalMean.copyLegacyLinear(i)), vals, clue = "")
  }

  test("temporalMean for sparse NeuroVec preserves active indices") {
    val sp = NeuroSpace(Vector(3, 1, 1, 3))
    val mask = Mask.fromIndices(sp.spatialSpace, Array(0, 2))
    val data =
      RavelArray.tabulate[Double](3, 2) { (time, position) =>
        if position == 0 then 1.0 + 2.0 * time
        else 2.0 + 2.0 * time
      }
    val svec = SparseNeuroVec(data, sp, mask, IndexLookupVol(sp, Array(0, 2)))

    val mean = svec.temporalMean
    val idx = Vector.tabulate(mean.indices.size)(i => mean.indices(i))
    val vals = Vector.tabulate(mean.data.size)(i => mean.data(i))

    assertEquals(idx, Vector(0, 2), clue = "")
    assertEquals(vals, Vector(3.0, 4.0), clue = "")
  }

  test("Mask.of returns full masks for dense objects and stored masks for sparse objects") {
    val sp = NeuroSpace(Vector(3, 1, 1, 2))
    val vec = NeuroVec.fromLinear[Double](PrimitiveBuffers.tabulate[Double](6)(_.toDouble), sp)
    val denseMask = Mask.of(vec)
    assertEquals(Vector.tabulate(denseMask.copyLegacyLinear.length)(i => denseMask.copyLegacyLinear(i)), Vector(true, true, true), clue = "")

    val sparseMask = Mask.fromIndices(sp.spatialSpace, Array(0, 2))
    val svec = vec.asSparse(sparseMask)
    val recovered = Mask.of(svec)
    assertEquals(
      GridCompatibility.exact(recovered.space, sparseMask.space),
      Right(()),
      clue = ""
    )
    assertEquals(
      recovered.copyLegacyLinear.toVector,
      sparseMask.copyLegacyLinear.toVector,
      clue = ""
    )
  }

  test("NeuroCompare builds logical volumes and vectors") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Double](Array(0.0, 1.0, 2.0, 3.0), sp)
    val gt = vol.gt(1.0)

    assertEquals(gt.space, sp, clue = "")
    assertEquals(Vector.tabulate(gt.copyLegacyLinear.length)(i => gt.copyLegacyLinear(i)), Vector(false, false, true, true), clue = "")
    assertEquals(Vector.tabulate(vol.eqv(vol).copyLegacyLinear.length)(i => vol.eqv(vol).copyLegacyLinear(i)), Vector.fill(4)(true), clue = "")

    val sp4 = NeuroSpace(Vector(2, 1, 1, 2))
    val vec = NeuroVec.fromLinear[Double](Array(0.0, 2.0, 4.0, 6.0), sp4)
    val lt = vec.lt(5.0)
    assertEquals(Vector.tabulate(lt.copyLegacyLinear.length)(i => lt.copyLegacyLinear(i)), Vector(true, true, true, false), clue = "")
  }

  test("NeuroCompare supports sparse and clustered volumes") {
    val sp = NeuroSpace(Vector(3, 1, 1))
    val sparse = SparseNeuroVol(Array(2.0, 5.0), Array(0, 2), sp)
    val sgt = sparse.gt(1.0)
    assertEquals(Vector.tabulate(sgt.copyLegacyLinear.length)(i => sgt.copyLegacyLinear(i)), Vector(true, false, true), clue = "")

    val mask = Mask.fromIndices(sp, Array(0, 2))
    val cvol = ClusteredNeuroVol(mask, Array(1, 2))
    val cgt = cvol.gt(1)
    assertEquals(Vector.tabulate(cgt.copyLegacyLinear.length)(i => cgt.copyLegacyLinear(i)), Vector(false, false, true), clue = "")
  }

  test("NeuroStats summarizes NeuroVec temporal ranges") {
    val sp = NeuroSpace(Vector(2, 1, 1, 3))
    val vec = NeuroVec.fromLinear[Double](Array(0.0, 1.0, 2.0, 3.0, 4.0, 5.0), sp)
    val summary = vec.summary

    assertEquals(summary.timePoints, 3, clue = "")
    assertEquals(summary.totalVoxels, 2, clue = "")
    assertEquals(summary.temporalMeanRange, (2.0, 3.0), clue = "")
    assertEquals(summary.nonZeroVoxels, 2, clue = "")
    assertEquals(summary.global.count, 6, clue = "")
  }
