package scalafim.image

import Ops.*
import narr.NArray
import spire.std.double.given
import spire.std.int.given

class StatsMaskCompareSuite extends munit.FunSuite:

  test("NeuroStats summarizes dense volumes") {
    val sp = NeuroSpace(Vector(2, 2, 2))
    val vol = NeuroVol.fromLinear[Double](NArray(0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0), sp)
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
    val vec = NeuroVec.fromLinear[Double](NArray(0.0, 1.0, 2.0, 3.0, 4.0, 5.0), sp)
    val mean = NeuroStats.temporalMean(vec)
    val vals = Vector.tabulate(mean.values.data.length)(i => mean.values.data(i))

    assertEquals(mean.space.dims, Vector(2, 1, 1), clue = "")
    assertEquals(vals, Vector(2.0, 3.0), clue = "")
    assertEquals(vec.temporalMean.space, mean.space, clue = "")
    assertEquals(Vector.tabulate(vec.temporalMean.values.data.length)(i => vec.temporalMean.values.data(i)), vals, clue = "")
  }

  test("temporalMean for sparse NeuroVec preserves active indices") {
    val sp = NeuroSpace(Vector(3, 1, 1, 3))
    val mask = Mask.fromIndices(sp.spatialSpace, NArray(0, 2))
    val data = NDArray[Double](NArray(1.0, 3.0, 5.0, 2.0, 4.0, 6.0), Vector(3, 2))
    val svec = SparseNeuroVec(data, sp, mask, IndexLookupVol(sp, NArray(0, 2)))

    val mean = svec.temporalMean
    val idx = Vector.tabulate(mean.indices.length)(i => mean.indices(i))
    val vals = Vector.tabulate(mean.data.length)(i => mean.data(i))

    assertEquals(idx, Vector(0, 2), clue = "")
    assertEquals(vals, Vector(3.0, 4.0), clue = "")
  }

  test("Mask.of returns full masks for dense objects and stored masks for sparse objects") {
    val sp = NeuroSpace(Vector(3, 1, 1, 2))
    val vec = NeuroVec.fromLinear[Double](NArrayUtil.tabulate[Double](6)(_.toDouble), sp)
    val denseMask = Mask.of(vec)
    assertEquals(Vector.tabulate(denseMask.values.data.length)(i => denseMask.values.data(i)), Vector(true, true, true), clue = "")

    val sparseMask = Mask.fromIndices(sp.spatialSpace, NArray(0, 2))
    val svec = vec.asSparse(sparseMask)
    assertEquals(Mask.of(svec), sparseMask, clue = "")
  }

  test("NeuroCompare builds logical volumes and vectors") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Double](NArray(0.0, 1.0, 2.0, 3.0), sp)
    val gt = vol.gt(1.0)

    assertEquals(gt.space, sp, clue = "")
    assertEquals(Vector.tabulate(gt.values.data.length)(i => gt.values.data(i)), Vector(false, false, true, true), clue = "")
    assertEquals(Vector.tabulate(vol.eqv(vol).values.data.length)(i => vol.eqv(vol).values.data(i)), Vector.fill(4)(true), clue = "")

    val sp4 = NeuroSpace(Vector(2, 1, 1, 2))
    val vec = NeuroVec.fromLinear[Double](NArray(0.0, 2.0, 4.0, 6.0), sp4)
    val lt = vec.lt(5.0)
    assertEquals(Vector.tabulate(lt.values.data.length)(i => lt.values.data(i)), Vector(true, true, true, false), clue = "")
  }

  test("NeuroCompare supports sparse and clustered volumes") {
    val sp = NeuroSpace(Vector(3, 1, 1))
    val sparse = SparseNeuroVol(NArray(2.0, 5.0), NArray(0, 2), sp)
    val sgt = sparse.gt(1.0)
    assertEquals(Vector.tabulate(sgt.values.data.length)(i => sgt.values.data(i)), Vector(true, false, true), clue = "")

    val mask = Mask.fromIndices(sp, NArray(0, 2))
    val cvol = ClusteredNeuroVol(mask, NArray(1, 2))
    val cgt = cvol.gt(1)
    assertEquals(Vector.tabulate(cgt.values.data.length)(i => cgt.values.data(i)), Vector(false, false, true), clue = "")
  }

  test("NeuroStats summarizes NeuroVec temporal ranges") {
    val sp = NeuroSpace(Vector(2, 1, 1, 3))
    val vec = NeuroVec.fromLinear[Double](NArray(0.0, 1.0, 2.0, 3.0, 4.0, 5.0), sp)
    val summary = vec.summary

    assertEquals(summary.timePoints, 3, clue = "")
    assertEquals(summary.totalVoxels, 2, clue = "")
    assertEquals(summary.temporalMeanRange, (2.0, 3.0), clue = "")
    assertEquals(summary.nonZeroVoxels, 2, clue = "")
    assertEquals(summary.global.count, 6, clue = "")
  }
