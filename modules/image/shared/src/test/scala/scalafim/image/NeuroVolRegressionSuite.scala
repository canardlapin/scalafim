package scalafim.image

import Ops.*
import spire.std.double.given

class NeuroVolRegressionSuite extends munit.FunSuite:

  test("SparseNeuroVol.toDense preserves indices (others are zero)") {
    val sp = NeuroSpace(Vector(3, 3, 3))
    val idx = Array[Int](1, 4, 9)
    val vals = Array[Double](1.0, 2.0, 3.0)
    val sv = SparseNeuroVol[Double](vals, idx, sp)
    val dense = sv.toDense

    val out = Vector.tabulate(dense.copyLegacyLinear.length)(i => dense.copyLegacyLinear(i))
    assertEquals(out(1), 1.0, clue = "")
    assertEquals(out(4), 2.0, clue = "")
    assertEquals(out(9), 3.0, clue = "")
    assertEquals(out.sum, 6.0, clue = "")
  }

  test("NeuroVol.asLogical flags all non-zero values") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Double](Array[Double](0.0, -1.0, 2.0, 0.0), sp)
    val m = vol.asLogical
    val flags = Vector.tabulate(m.copyLegacyLinear.length)(i => m.copyLegacyLinear(i))
    assertEquals(flags, Vector(false, true, true, false), clue = "")
  }

  test("NeuroVol.asMask flags positive values only") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Double](Array[Double](0.0, -1.0, 2.0, 0.0), sp)
    val m = vol.asMask
    val flags = Vector.tabulate(m.copyLegacyLinear.length)(i => m.copyLegacyLinear(i))
    assertEquals(flags, Vector(false, false, true, false), clue = "")
  }

  test("NeuroVol.asMask(indices) sets specified indices true") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Double](PrimitiveBuffers.fillConst[Double](4, 0.0), sp)
    val m = vol.asMask(Array[Int](1, 3))
    val flags = Vector.tabulate(m.copyLegacyLinear.length)(i => m.copyLegacyLinear(i))
    assertEquals(flags, Vector(false, true, false, true), clue = "")
  }

  test("NeuroVol.asSparse from mask/indices roundtrips when outside-mask values are zero") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Double](Array[Double](0.0, 1.0, 0.0, 1.0), sp)
    val mask = vol.asMask
    val idx = Mask.indices(mask)

    val svol1 = vol.asSparse(mask)
    val svol2 = vol.asSparse(idx)

    val dense1 = svol1.toDense
    val dense2 = svol2.toDense

    val v0 = Vector.tabulate(vol.copyLegacyLinear.length)(i => vol.copyLegacyLinear(i))
    val v1 = Vector.tabulate(dense1.copyLegacyLinear.length)(i => dense1.copyLegacyLinear(i))
    val v2 = Vector.tabulate(dense2.copyLegacyLinear.length)(i => dense2.copyLegacyLinear(i))
    assertEquals(v1, v0, clue = "")
    assertEquals(v2, v0, clue = "")
  }

  test("SparseNeuroVol.toMask matches stored indices") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val idx = Array[Int](1, 3)
    val vals = Array[Double](10.0, 20.0)
    val sv = SparseNeuroVol[Double](vals, idx, sp)
    val mask = sv.toMask
    val back = Mask.indices(mask)
    val got = Vector.tabulate(back.size)(i => back(i))
    assertEquals(got, Vector(1, 3), clue = "")
  }

  test("SparseNeuroVol rejects out-of-range indices") {
    val sp = NeuroSpace(Vector(2, 2, 2))
    intercept[IllegalArgumentException] {
      SparseNeuroVol[Double](Array[Double](1.0), Array[Int](8), sp)
    }
  }

  test("SparseNeuroVol rejects mismatched data/indices lengths") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    intercept[IllegalArgumentException] {
      SparseNeuroVol[Double](Array[Double](1.0, 2.0), Array[Int](0), sp)
    }
  }

  test("NeuroVol.asMatrix matches linear ordering") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Int](Array[Int](1, 2, 3, 4), sp)
    val m = vol.asMatrix
    assertEquals(
      Vector.tabulate(m.shape.rank)(m.shape.apply),
      Vector(4, 1),
      clue = ""
    )
    val mv = Vector.tabulate(m.shape(0))(i => m(i, 0))
    assertEquals(mv, Vector(1, 2, 3, 4), clue = "")
  }

  test("mapValues maps label ids with numeric keys and defaults missing to 0") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Int](Array[Int](1, 2, 1, 2), sp)

    val out = vol.mapValues(Map(1 -> 10, 2 -> 20), default = 0)
    val vals = Vector.tabulate(out.copyLegacyLinear.length)(i => out.copyLegacyLinear(i)).distinct.sorted
    assertEquals(vals, Vector(10, 20), clue = "")

    val out2 = vol.mapValues(Map(1 -> 10), default = 0)
    val vals2 = Vector.tabulate(out2.copyLegacyLinear.length)(i => out2.copyLegacyLinear(i)).distinct.sorted
    assertEquals(vals2, Vector(0, 10), clue = "")
  }

  test("mapValues accepts string keys when parseable and rejects non-numeric keys") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Int](Array[Int](1, 2, 1, 2), sp)

    val out = vol.mapValues(Map("1" -> 10, "2" -> 20), default = 0)
    val vals = Vector.tabulate(out.copyLegacyLinear.length)(i => out.copyLegacyLinear(i)).distinct.sorted
    assertEquals(vals, Vector(10, 20), clue = "")

    intercept[IllegalArgumentException] {
      vol.mapValues(Map("a" -> 1, "b" -> 2), default = 0)
    }
  }

  test("mapf produces expected size and respects mask") {
    val sp = NeuroSpace(Vector(5, 5, 5))
    val nels = sp.spatialDims.product
    val vol = NeuroVol.fromLinear[Double](PrimitiveBuffers.fillConst[Double](nels, 1.0), sp)

    val kdim = Vector(3, 3, 3)
    val ker = Kernel3D(kdim, vdim = Vector(1.0, 1.0, 1.0)) { d =>
      if d == 0.0 then 1.0 else 0.0
    }

    val maskFlags = PrimitiveBuffers.fillConst[Boolean](nels, false)
    val center = Vector(2, 2, 2)
    val centerLin = Indexing.gridToIndex3D(sp.spatialDims, center(0), center(1), center(2))
    maskFlags(centerLin) = true
    val mask = NeuroVol.fromLinear[Boolean](maskFlags, sp)

    val out = SpatialFilters.mapf(vol, ker, mask = Some(mask))
    assertEquals(out.space.dims, vol.space.dims, clue = "")
    assertEquals(out(center(0), center(1), center(2)), 1.0, clue = "")
    val sum = Vector.tabulate(out.copyLegacyLinear.length)(i => out.copyLegacyLinear(i)).sum
    assertEquals(sum, 1.0, clue = "")
  }

  test("NeuroVol(ROIVol) extracts values at ROI coords") {
    val sp = NeuroSpace(Vector(4, 4, 4))
    val nels = sp.spatialDims.product
    val vol = NeuroVol.fromLinear[Int](PrimitiveBuffers.tabulate[Int](nels)(i => i + 1), sp)

    val coords = Vector(Vector(0, 0, 0), Vector(1, 0, 0), Vector(0, 1, 0), Vector(3, 3, 3))
    val roi = ROIVol[Int](sp, coords, PrimitiveBuffers.fillConst[Int](coords.length, 0))
    val out = vol(roi)

    val got = Vector.tabulate(out.size)(i => out(i))
    def lin(c: Vector[Int]): Int =
      Indexing.gridToIndex3D(sp.spatialDims, c(0), c(1), c(2))
    val exp = coords.map(c => vol.linear(lin(c)))
    assertEquals(got, exp, clue = "")
  }

  test("NeuroVol.toVec produces a 4D NeuroVec with one volume") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.fromLinear[Double](Array[Double](1.0, 2.0, 3.0, 4.0), sp)
    val vec = vol.toVec
    assertEquals(vec.space.dims, Vector(2, 2, 1, 1), clue = "")
    val v0 = vec.volume(0)
    val got = Vector.tabulate(v0.copyLegacyLinear.length)(i => v0.copyLegacyLinear(i))
    assertEquals(got, Vector(1.0, 2.0, 3.0, 4.0), clue = "")
  }

  test("NeuroVol.concat stacks volumes along time into a NeuroVec") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val v1 = NeuroVol.fromLinear[Double](Array[Double](1.0, 2.0, 3.0, 4.0), sp)
    val v2 = NeuroVol.fromLinear[Double](Array[Double](10.0, 20.0, 30.0, 40.0), sp)
    val vec = v1.concat(v2)
    assertEquals(vec.space.dims, Vector(2, 2, 1, 2), clue = "")
    val lin = Vector.tabulate(vec.copyLegacyLinear.length)(i => vec.copyLegacyLinear(i))
    assertEquals(lin, Vector(1.0, 2.0, 3.0, 4.0, 10.0, 20.0, 30.0, 40.0), clue = "")
  }

  test("NeuroVol planes are zero-copy singleton-D3 image views") {
    val sp = NeuroSpace(Vector(2, 2, 3))
    val nx = sp.dims(0); val ny = sp.dims(1); val nz = sp.dims(2)
    val data = PrimitiveBuffers.ofSize[Double](nx * ny * nz)
    var z = 0
    while z < nz do
      var y = 0
      while y < ny do
        var x = 0
        while x < nx do
          val lin = Indexing.gridToIndex3D(sp.spatialDims, x, y, z)
          data(lin) = (z + 1).toDouble
          x += 1
        y += 1
      z += 1

    val vol = NeuroVol.fromLinear[Double](data, sp)
    val planes =
      Vector.tabulate(nz): index =>
        vol
          .plane(SpatialAxis.Z, index)
          .fold(error => fail(error.message), identity)
    assertEquals(planes.length, nz, clue = "")
    assertEquals(planes.head.grid.shape, Vector(2, 2, 1), clue = "")
    val means =
      planes.map { plane =>
        var sum = 0.0
        var x = 0
        while x < plane.grid.shape(0) do
          var y = 0
          while y < plane.grid.shape(1) do
            sum += plane(x, y, 0)
            y += 1
          x += 1
        sum / plane.grid.shape.take(2).product.toDouble
      }
    assertEquals(means, Vector(1.0, 2.0, 3.0), clue = "")
  }
