package scalafim.image

import narr.NArray
import spire.std.double.given

class SubVectorSyntaxSuite extends munit.FunSuite:

  test("NeuroVec apply overloads select one volume or a time subset") {
    val sp = NeuroSpace(Vector(2, 1, 1, 4))
    val vec = NeuroVec.fromLinear[Double](NArrayUtil.tabulate[Double](8)(_.toDouble), sp)

    val v2 = vec(2)
    assertEquals(v2.space.dims, Vector(2, 1, 1), clue = "")
    assertEquals(Vector.tabulate(v2.values.data.length)(i => v2.values.data(i)), Vector(4.0, 5.0), clue = "")

    val sub = vec(1 to 3)
    assertEquals(sub.space.dims, Vector(2, 1, 1, 3), clue = "")
    assertEquals(Vector.tabulate(sub.values.data.length)(i => sub.values.data(i)), Vector(2.0, 3.0, 4.0, 5.0, 6.0, 7.0), clue = "")
  }

  test("SparseNeuroVec supports volume and subVector apply syntax") {
    val sp = NeuroSpace(Vector(3, 1, 1, 4))
    val mask = Mask.fromIndices(sp.spatialSpace, NArray(0, 2))
    val dense = NArray(1.0, 0.0, 10.0, 2.0, 0.0, 20.0, 3.0, 0.0, 30.0, 4.0, 0.0, 40.0)
    val svec = SparseNeuroVec.fromDense[Double](dense, sp, mask)

    val vol = svec(2)
    assertEquals(Vector.tabulate(vol.indices.length)(i => vol.indices(i)), Vector(0, 2), clue = "")
    assertEquals(Vector.tabulate(vol.data.length)(i => vol.data(i)), Vector(3.0, 30.0), clue = "")

    val sub = svec(Vector(1, 3))
    assertEquals(sub.space.dims, Vector(3, 1, 1, 2), clue = "")
    assertEquals(Vector.tabulate(sub.data.data.length)(i => sub.data.data(i)), Vector(2.0, 4.0, 20.0, 40.0), clue = "")
  }

  test("ClusteredNeuroVec and NeuroVecSeq support apply subsetting syntax") {
    val sp = NeuroSpace(Vector(2, 2, 1, 3))
    val vec = NeuroVec.fromLinear[Double](NArrayUtil.tabulate[Double](12)(_.toDouble), sp)
    val mask = Mask.fromIndices(sp.spatialSpace, NArray(0, 1, 2, 3))
    val cvol = ClusteredNeuroVol(mask, NArray(1, 1, 2, 2))
    val cvec = ClusteredNeuroVec.fromNeuroVecMean(vec, cvol)
    val csub = cvec(Seq(0, 2))

    assertEquals(csub.space.dims, Vector(2, 2, 1, 2), clue = "")
    assertEquals(csub.ts.shape, Vector(2, 2), clue = "")

    val seq = NeuroVecSeq(Vector(vec.subVector(Seq(0, 1)), vec.subVector(Seq(2))))
    val seqSub = seq(Seq(1, 2))
    assertEquals(seqSub.length, 2, clue = "")
    assertEquals(Vector.tabulate(seqSub(0).values.data.length)(i => seqSub(0).values.data(i)), Vector(4.0, 5.0, 6.0, 7.0), clue = "")
    assertEquals(Vector.tabulate(seqSub(1).values.data.length)(i => seqSub(1).values.data(i)), Vector(8.0, 9.0, 10.0, 11.0), clue = "")
  }
