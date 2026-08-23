package scalafim.image

import Ops.*
import ravel.{NDArray as RavelArray, Rank, Shape}
import spire.std.double.given
import spire.std.int.given

class CoreSuite extends munit.FunSuite:

  private def columnMajor2[A](array: RavelArray[A, Rank[2]]): Vector[A] =
    Vector
      .tabulate(array.shape(1)): column =>
        Vector.tabulate(array.shape(0))(row => array(row, column))
      .flatten

  private def assertPointClose(actual: SpatialPoint, expected: SpatialPoint, tol: Double): Unit =
    assertEqualsDouble(actual.x, expected.x, tol)
    assertEqualsDouble(actual.y, expected.y, tol)
    assertEqualsDouble(actual.z, expected.z, tol)

  test("Ravel indices expose logical axis coordinates independent of storage order") {
    val arr =
      RavelArray.tabulate[Int](2, 3) { (x, y) =>
        1 + x + 2 * y
      }
    assertEquals(arr(0, 0), 1, clue = "")
    assertEquals(arr(1, 0), 2, clue = "")
    assertEquals(arr(0, 1), 3, clue = "")
    assertEquals(arr(1, 2), 6, clue = "")
  }

  test("NeuroSpace default affine matches spacing/origin") {
    val sp = NeuroSpace(
      dims = Vector(2, 2, 2),
      spacing = Some(Vector(2.0, 3.0, 4.0)),
      origin = Some(Vector(10.0, 20.0, 30.0))
    )
    val coord = sp.indexToCoord(Vector(1.0, 1.0, 1.0))
    assertEquals(coord, Vector(12.0, 23.0, 34.0), clue = "")
  }

  test("NeuroSpace smart constructor reports invalid geometry directly") {
    val shortSpacing = NeuroSpace.make(Vector(2, 2, 2), spacing = Some(Vector(1.0, 2.0)))
    assertEquals(shortSpacing.left.map(_.message), Left("'spacing' must contain 3 spatial values; got 2"), clue = "")

    val badTransform = NeuroSpace.make(Vector(2, 2, 2), trans = Some(DMat.eye(3)))
    assertEquals(badTransform.left.map(_.message), Left("spatial transform must be 4x4; got 3x3"), clue = "")

    val badAxes = NeuroSpace.make(Vector(2, 2, 2, 4), axes = Some(AxisSet.standard(3)))
    assertEquals(badAxes.left.map(_.message), Left("axis count must match dimensionality: expected 4, got 3"), clue = "")
  }

  test("NeuroSpace exposes typed 3D coordinate roundtrip") {
    val sp = NeuroSpace(
      dims = Vector(2, 2, 2),
      spacing = Some(Vector(2.0, 3.0, 4.0)),
      origin = Some(Vector(10.0, 20.0, 30.0))
    )
    val voxel = SpatialPoint(1.0, 1.0, 1.0)
    val world = sp.indexToPoint(voxel)

    assertPointClose(world, SpatialPoint(12.0, 23.0, 34.0), 1e-12)
    assertPointClose(sp.coordToIndexPoint(world), voxel, 1e-12)
  }

  test("NeuroVol arithmetic is elementwise") {
    val sp = NeuroSpace(Vector(2, 2, 2))
    val v1 = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](8, 1.0), sp)
    val v2 = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](8, 2.0), sp)

    val v3 = v1 + v2
    val vals = (0 until v3.copyToCanonicalArray.length).map(i => v3.copyToCanonicalArray(i)).toVector
    assertEquals(vals, Vector.fill(8)(3.0), clue = "")
  }

  test("Mask indices roundtrip") {
    val sp = NeuroSpace(Vector(3, 3, 3))
    val idx = Array[Int](0, 13, 26)
    val mask = Mask.fromIndices(sp, idx)
    val back = Mask.indices(mask)
    val backValues = Vector.tabulate(back.size)(i => back(i)).sorted
    val inputValues = Vector.tabulate(idx.length)(i => idx(i)).sorted
    assertEquals(backValues, inputValues, clue = "")
  }

  test("ROIVol computes linear indices") {
    val sp = NeuroSpace(Vector(2, 3, 4))
    val coords = Vector(Vector(0, 0, 0), Vector(1, 0, 0), Vector(0, 1, 0))
    val roi = ROIVol[Double](sp, coords, Array[Double](1.0, 2.0, 3.0))
    val lin = Vector.tabulate(roi.linearIndices.size)(i => roi.linearIndices(i))
    assertEquals(lin, Vector(0, 12, 4), clue = "")
  }

  test("NeuroVec volume and series") {
    val sp = NeuroSpace(Vector(2, 2, 1, 3))
    // Whole-array input follows canonical Ravel order: the last axis is fastest.
    val data = PrimitiveBuffers.tabulate[Double](12)(_.toDouble)
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp)

    val v1 = vec.volume(1)
    val linVol1 = Vector.tabulate(v1.copyToCanonicalArray.length)(i => v1.copyToCanonicalArray(i))
    assertEquals(linVol1, Vector(1.0, 4.0, 7.0, 10.0), clue = "")

    val ts = vec.series(2)
    val tsVec = Vector.tabulate(ts.length)(i => ts(i))
    assertEquals(tsVec, Vector(6.0, 7.0, 8.0), clue = "")
  }

  test("NeuroVec subVector keeps spatial layout") {
    val sp = NeuroSpace(Vector(2, 2, 1, 4))
    val data = PrimitiveBuffers.tabulate[Double](16)(_.toDouble)
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp)
    val sub = vec.subVector(Seq(1, 3))
    assertEquals(sub.space.dims, Vector(2, 2, 1, 2), clue = "")
    val subData = Vector.tabulate(sub.copyToCanonicalArray.length)(i => sub.copyToCanonicalArray(i))
    assertEquals(subData, Vector(1.0, 3.0, 5.0, 7.0, 9.0, 11.0, 13.0, 15.0), clue = "")
  }

  test("NeuroVol plane retains an affine-honest singleton-D3 view") {
    val sp = NeuroSpace(Vector(2, 3, 1))
    val data = PrimitiveBuffers.tabulate[Int](6)(i => i + 1)
    val vol = NeuroVol.copyFromCanonicalArray[Int](data, sp)
    val plane =
      vol
        .plane(SpatialAxis.X, 1)
        .fold(error => fail(error.message), identity)
    val planeData = Vector.tabulate(3)(y => plane(0, y, 0))
    assertEquals(plane.grid.shape, Vector(1, 3, 1), clue = "")
    assertEquals(planeData, Vector(4, 5, 6), clue = "")
  }

  test("NeuroVecSeq indexes across runs") {
    val sp1 = NeuroSpace(Vector(2, 1, 1, 2))
    val sp2 = NeuroSpace(Vector(2, 1, 1, 3), spacing = Some(sp1.spacing), origin = Some(sp1.origin), trans = Some(sp1.trans))
    val v1 = NeuroVec.copyFromCanonicalArray[Int](PrimitiveBuffers.tabulate[Int](4)(identity), sp1)
    val v2 = NeuroVec.copyFromCanonicalArray[Int](PrimitiveBuffers.tabulate[Int](6)(i => i + 100), sp2)
    val seq = NeuroVecSeq(Vector(v1, v2))
    assertEquals(seq.length, 5, clue = "")
    val vol3 = seq(3)
    val vals3 = Vector.tabulate(vol3.copyToCanonicalArray.length)(i => vol3.copyToCanonicalArray(i))
    assertEquals(vals3, Vector(101, 104), clue = "")
  }

  test("NeuroSpace grid/index conversions") {
    val sp = NeuroSpace(Vector(2, 3, 4))
    assertEquals(sp.gridToIndex3D(1, 0, 0), 12, clue = "")
    assertEquals(sp.indexToGrid3D(5), Vector(0, 1, 1), clue = "")
  }

  test("typed spatial dims and voxel coords protect 3D indexing") {
    val dims = SpatialDims(2, 3, 4)
    val coord = VoxelCoord(1, 2, 0)

    assertEquals(Indexing.gridToIndex3D(dims, coord), 20, clue = "")
    assertEquals(Indexing.indexToGrid3D(dims, 20), coord, clue = "")
    assert(Indexing.gridToIndexChecked(dims, VoxelCoord(2, 0, 0)).isLeft, clue = "x == dim should be out of bounds")
    assert(SpatialDims.fromVector(Vector(2, 3)).isLeft, clue = "SpatialDims should reject non-3D input")
  }

  test("NeuroVec seriesRoi creates ROIVec") {
    val sp = NeuroSpace(Vector(2, 1, 1, 3))
    val data = PrimitiveBuffers.tabulate[Int](6)(identity)
    val vec = NeuroVec.copyFromCanonicalArray[Int](data, sp)
    val roi = ROICoords(Vector(Vector(0, 0, 0), Vector(1, 0, 0)))
    val rvec = vec.seriesRoi(roi)
    assertEquals(rvec.data.shape, Shape(3, 2), clue = "")
    val s0 = rvec.seriesAt(0)
    val s0v = Vector.tabulate(s0.length)(i => s0(i))
    assertEquals(s0v, Vector(0, 1, 2), clue = "")
  }

  test("Dense to sparse and back roundtrip") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(2, 2, 1, 3))
    val data = PrimitiveBuffers.tabulate[Double](12)(_.toDouble)
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp)
    val mask = Mask.fromIndices(sp.spatialSpace, Array(0, 3))
    val svec = vec.asSparse(mask)
    val dense2 = svec.toDense
    val dVals = Vector.tabulate(dense2.copyToCanonicalArray.length)(i => dense2.copyToCanonicalArray(i))
    assertEquals(dVals, Vector(0.0, 1.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0, 10.0, 11.0), clue = "")
  }

  test("asMatrix(SparseNeuroVec) matches dense with mask zeros") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(10, 10, 10, 3))
    val spatialNels = sp.spatialDims.product
    val tLen = sp.dims(3)

    val data = PrimitiveBuffers.tabulate[Double](spatialNels * tLen)(i => (i.toDouble + 1.0) / 10.0)
    val dense = NeuroVec.copyFromCanonicalArray[Double](data, sp)

    // ~30% mask (deterministic)
    val maskFlags = PrimitiveBuffers.fillConst[Boolean](spatialNels, false)
    var lin = 0
    while lin < spatialNels do
      if (lin % 10) < 3 then maskFlags(lin) = true
      lin += 1
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](maskFlags, sp.spatialSpace)

    val sparse = dense.asSparse(mask)
    val md = dense.asMatrix
    val ms = sparse.asMatrix
    assertEquals(ms.shape, md.shape, clue = "")

    lin = 0
    while lin < spatialNels do
      var t = 0
      while t < tLen do
        val expected = if maskFlags(lin) then md(lin, t) else 0.0
        val got = ms(lin, t)
        assert(math.abs(got - expected) < 1e-7, clue = "")
        t += 1
      lin += 1
  }

  test("SparseNeuroVec subArray + linear access parity") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(2, 2, 2, 2))
    val spatialNels = sp.spatialDims.product

    // mask pattern: 1,0,1,0, 0,1,0,1  (R column-major order)
    val maskIdx = Array(0, 2, 5, 7)
    val mask = Mask.fromIndices(sp.spatialSpace, maskIdx)
    val map = IndexLookupVol(sp, maskIdx)

    val nvox = maskIdx.length
    val dat = PrimitiveBuffers.tabulate[Double](2 * nvox)(i => (i + 1).toDouble) // time x voxels
    val compact =
      RavelArray.tabulate[Double](2, nvox) { (time, position) =>
        dat(time + position * 2)
      }
    val svec = SparseNeuroVec[Double](compact, sp, mask, map)
    val dvec = svec.toDense

    val fullS = svec.subArray(0 until 2, 0 until 2, 0 until 2, 0 until 2)
    val fullD = dvec.subArray(0 until 2, 0 until 2, 0 until 2, 0 until 2)
    assertEquals(fullS.shape, fullD.shape, clue = "")
    var tt = 0
    while tt < 2 do
      var kk = 0
      while kk < 2 do
        var jj = 0
        while jj < 2 do
          var ii = 0
          while ii < 2 do
            assertEquals(fullS(ii, jj, kk, tt), fullD(ii, jj, kk, tt), clue = "")
            ii += 1
          jj += 1
        kk += 1
      tt += 1

    assertEquals(svec(0, 0, 0, 0), dvec(0, 0, 0, 0), clue = "")
    val nodropS = svec.subArray(Seq(0), Seq(0), Seq(0), Seq(0))
    val nodropD = dvec.subArray(Seq(0), Seq(0), Seq(0), Seq(0))
    assertEquals(
      Vector.tabulate(nodropS.shape.rank)(nodropS.shape.apply),
      Vector(1, 1, 1, 1),
      clue = ""
    )
    assertEquals(nodropS(0, 0, 0, 0), nodropD(0, 0, 0, 0), clue = "")

    // masked-out spatial index: (1,1,0) => linSpatial=3
    val maskedOutSpatial = Indexing.gridToIndex3D(sp.spatialDims, 1, 1, 0)
    assertEquals(svec(1, 1, 0, 0), 0.0, clue = "")

    // dense matrix has zeros outside mask rows
    val mat = dvec.asMatrix
    val maskFlags = mask.copyToCanonicalArray
    var lin = 0
    while lin < spatialNels do
      var t = 0
      while t < sp.dims(3) do
        val v = mat(lin, t)
        if !maskFlags(lin) then assertEquals(v, 0.0, clue = "")
        t += 1
      lin += 1
  }

  test("Sparse series fills zeros for missing voxels") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(2, 2, 1, 2))
    val data = PrimitiveBuffers.tabulate[Double](8)(_.toDouble)
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp)
    val mask = Mask.fromIndices(sp.spatialSpace, Array(0, 2))
    val svec = vec.asSparse(mask)
    val ts = svec.series(Array(0, 1, 2))
    val tsVals = columnMajor2(ts)
    // voxel 1 is missing => zeros in its column
    assertEquals(ts.shape, Shape(2, 3), clue = "")
    assertEquals(tsVals, Vector(0.0, 1.0, 0.0, 0.0, 4.0, 5.0), clue = "")
  }

  test("SparseNeuroVec union arithmetic") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(2, 2, 1, 2))
    val v1 = NeuroVec.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](8)(_.toDouble), sp)
    val v2 = NeuroVec.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](8)(i => (i + 10).toDouble), sp)
    val m1 = Mask.fromIndices(sp.spatialSpace, Array(0, 1))
    val m2 = Mask.fromIndices(sp.spatialSpace, Array(1, 3))
    val s1 = v1.asSparse(m1)
    val s2 = v2.asSparse(m2)
    val s3 = s1 + s2
    val d3 = s3.toDense
    val vals = Vector.tabulate(d3.copyToCanonicalArray.length)(i => d3.copyToCanonicalArray(i))
    assertEquals(vals, Vector(0.0, 1.0, 14.0, 16.0, 0.0, 0.0, 16.0, 17.0), clue = "")
  }

  test("ClusteredNeuroVol dense reconstruction") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val mask = Mask.fromIndices(sp, Array(0, 2, 3))
    val clusters = Array(1, 2, 1)
    val cvol = ClusteredNeuroVol(mask, clusters)
    assertEquals(cvol.numClusters, 2, clue = "")
    val dense = cvol.toDense
    val vals = Vector.tabulate(dense.copyToCanonicalArray.length)(i => dense.copyToCanonicalArray(i))
    assertEquals(vals, Vector(1, 0, 2, 1), clue = "")
  }

  test("Searchlight sphericalRoi includes center and respects radius") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(5, 5, 5))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](125, 1.0), sp)
    val center = Vector(2, 2, 2)
    val roi = Searchlight.sphericalRoi(vol, center, radius = 1.5)
    assert(roi.coords.coords.contains(center), clue = "")
    val spacing = sp.spacing
    roi.coords.coords.foreach { c =>
      val dx = (c(0) - center(0)) * spacing(0)
      val dy = (c(1) - center(1)) * spacing(1)
      val dz = (c(2) - center(2)) * spacing(2)
      assert(dx * dx + dy * dy + dz * dz <= 1.5 * 1.5 + 1e-9, clue = "")
    }
  }

  test("NeuroVec splitClusters yields ROIVecs") {
    val sp = NeuroSpace(Vector(2, 2, 1, 2))
    val vec = NeuroVec.copyFromCanonicalArray[Int](PrimitiveBuffers.tabulate[Int](8)(identity), sp)
    val mask = Mask.fromIndices(sp.spatialSpace, Array(0, 2, 3))
    val cvol = ClusteredNeuroVol(mask, Array(1, 2, 1))
    val rois = vec.splitClusters(cvol)
    assertEquals(rois.length, 2, clue = "")
    assertEquals(rois.head.data.shape, Shape(2, 2), clue = "") // time x voxels in cluster1
    assertEquals(rois(1).data.shape, Shape(2, 1), clue = "")
  }

  test("Downsample byFactor uses box averaging") {
    val sp = NeuroSpace(Vector(4, 4, 1), spacing = Some(Vector(1.0, 1.0, 1.0)))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](16)(_.toDouble), sp)
    val ds = Downsample.byFactor(vol, 0.5)
    assertEquals(ds.space.dims, Vector(2, 2, 1), clue = "")
    val vals = Vector.tabulate(ds.copyToCanonicalArray.length)(i => ds.copyToCanonicalArray(i))
    assertEquals(vals, Vector(2.5, 4.5, 10.5, 12.5), clue = "")
  }

  test("Downsample updates affine by preserving center world coordinate") {
    val sp =
      NeuroSpace(
        Vector(4, 6, 8),
        spacing = Some(Vector(2.0, 3.0, 4.0)),
        origin = Some(Vector(10.0, 20.0, 30.0))
      )
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](sp.spatialDims.product)(_.toDouble), sp)
    val ds = Downsample.byFactor(vol, 0.5)
    val expectedTrans =
      Affine.rescaleAffine(
        sp.trans,
        shape = Vector(4, 6, 8),
        zooms = Vector(4.0, 6.0, 8.0),
        newShape = Some(Vector(2, 3, 4))
      )

    assertEquals(ds.space.dims, Vector(2, 3, 4), clue = "")
    assertEquals(ds.space.spacing, Vector(4.0, 6.0, 8.0), clue = "")
    assertEquals(ds.space.origin, Vector(12.0, 20.0, 34.0), clue = "")
    assertEquals(ds.space.trans, expectedTrans, clue = "")

    val vec = vol.concat(vol)
    val vds = Downsample.byFactor(vec, 0.5)
    assertEquals(vds.space.dims, Vector(2, 3, 4, 2), clue = "")
    assertEquals(vds.space.trans, expectedTrans, clue = "")
  }

  test("Resample.nearest preserves data when spaces match") {
    val sp = NeuroSpace(Vector(3, 3, 1), spacing = Some(Vector(1.0, 1.0, 1.0)))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](9)(_.toDouble), sp)
    val res = Resample.nearest(vol, sp)
    val vals = Vector.tabulate(res.copyToCanonicalArray.length)(i => res.copyToCanonicalArray(i))
    assertEquals(vals, Vector.tabulate(9)(_.toDouble), clue = "")
  }

  test("Gaussian blur uses 0-padding at volume boundaries") {
    val sp = NeuroSpace(Vector(3, 3, 3))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](27, 5.0), sp)
    val blurred = SpatialFilters.gaussianBlur(vol, sigma = 1.0, window = 1)
    assert(math.abs(blurred(1, 1, 1) - 5.0) < 1e-9, clue = "")
    assert(blurred(0, 0, 0) < 5.0, clue = "")
  }

  test("Gaussian blur respects mask (zeros outside mask)") {
    val sp = NeuroSpace(Vector(5, 5, 5))
    val nels = sp.spatialDims.product
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](nels)(i => (i % 11).toDouble), sp)

    val maskFlags = PrimitiveBuffers.fillConst[Boolean](nels, false)
    var i = 0
    while i < nels do
      if (i % 7) < 3 then maskFlags(i) = true
      i += 1
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](maskFlags, sp)

    val blurred = SpatialFilters.gaussianBlur(vol, sigma = 2.0, window = 1, mask = Some(mask))
    assertEquals(blurred.space, sp, clue = "")
    i = 0
    while i < nels do
      if !maskFlags(i) then
        assertEquals(
          blurred.valueAtCanonicalOrdinal(i),
          0.0,
          clue = ""
        )
      i += 1
  }

  test("Bilateral filter handles missing mask") {
    val sp = NeuroSpace(Vector(6, 6, 6))
    val nels = sp.spatialDims.product
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](nels)(i => (i % 13).toDouble), sp)
    val filtered = SpatialFilters.bilateralFilter(vol, spatialSigma = 2.0, intensitySigma = 1.0, window = 1)
    assertEquals(filtered.space, sp, clue = "")
    assertEquals(filtered.copyToCanonicalArray.length, vol.copyToCanonicalArray.length, clue = "")
    assert(filtered.copyToCanonicalArray.forall(_.isFinite), clue = "")
  }

  test("Bilateral filter 4D is identity for zero windows") {
    val sp = NeuroSpace(Vector(3, 4, 2, 5))
    val nels = sp.spatialDims.product
    val tLen = sp.dims(3)
    val data = PrimitiveBuffers.tabulate[Double](nels * tLen)(i => (i.toDouble - 50.0) / 7.0)
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp)
    val out =
      SpatialFilters.bilateralFilter4D(
        vec,
        spatialWindow = 0,
        temporalWindow = 0,
        spatialSigma = 1.0,
        intensitySigma = 1.0,
        temporalSigma = 1.0,
        temporalSpacing = 1.0
      )
    val outData = out.copyToCanonicalArray
    var i = 0
    while i < outData.length do
      assertEquals(outData(i), data(i), clue = "")
      i += 1
  }

  test("Bilateral filter 4D preserves constant arrays without NaNs") {
    val sp = NeuroSpace(Vector(3, 3, 3, 4))
    val nels = sp.spatialDims.product
    val tLen = sp.dims(3)
    val data = PrimitiveBuffers.fillConst[Double](nels * tLen, 5.0)
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp)
    val out =
      SpatialFilters.bilateralFilter4D(
        vec,
        spatialWindow = 1,
        temporalWindow = 1,
        spatialSigma = 1.0,
        intensitySigma = 1.0,
        temporalSigma = 1.0,
        temporalSpacing = 1.0
      )
    val outData = out.copyToCanonicalArray
    assert(outData.forall(_.isFinite), clue = "")
    var i = 0
    while i < outData.length do
      assert(math.abs(outData(i) - 5.0) < 1e-12, clue = "")
      i += 1
  }

  test("NeuroVec-NeuroVol arithmetic broadcasts spatially") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(2, 2, 1, 2))
    val vec = NeuroVec.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](8)(_.toDouble), sp)
    val vol = NeuroVol.copyFromCanonicalArray[Double](Array[Double](10.0, 20.0, 30.0, 40.0), sp.spatialSpace)
    val out = vec + vol
    val vals = Vector.tabulate(out.copyToCanonicalArray.length)(i => out.copyToCanonicalArray(i))
    assertEquals(vals, Vector(10, 11, 22, 23, 34, 35, 46, 47).map(_.toDouble), clue = "")
  }

  test("SparseNeuroVec concat unions masks") {
    import spire.std.double.given
    val sp1 = NeuroSpace(Vector(2, 2, 1, 1))
    val sp2 = NeuroSpace(Vector(2, 2, 1, 2), spacing = Some(sp1.spacing), origin = Some(sp1.origin), trans = Some(sp1.trans))
    val v1 = NeuroVec.copyFromCanonicalArray[Double](Array[Double](1.0, 2.0, 3.0, 4.0), sp1)
    val v2 = NeuroVec.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](8)(i => (i + 10).toDouble), sp2)
    val m1 = Mask.fromIndices(sp1.spatialSpace, Array(0, 1))
    val m2 = Mask.fromIndices(sp2.spatialSpace, Array(1, 3))
    val s1 = v1.asSparse(m1)
    val s2 = v2.asSparse(m2)
    val s3 = s1.concat(s2)
    assertEquals(s3.space.dims, Vector(2, 2, 1, 3), clue = "")
    val dense = s3.toDense
    val vals = Vector.tabulate(dense.copyToCanonicalArray.length)(i => dense.copyToCanonicalArray(i))
    assertEquals(vals, Vector(1, 0, 0, 2, 12, 13, 0, 0, 0, 0, 16, 17).map(_.toDouble), clue = "")
  }

  test("Ellipsoid ROI is subset of spherical") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(9, 9, 9))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](9 * 9 * 9, 1.0), sp)
    val center = Vector(4, 4, 4)
    val sphere = Searchlight.sphericalRoi(vol, center, radius = 3.0)
    val ellip = Searchlight.ellipsoidRoi(
      vol,
      center,
      radius = 3.0,
      scales = Vector(2.0, 1.0, 1.0),
      rng = new scala.util.Random(0L)
    )
    assert(ellip.coords.size < sphere.coords.size, clue = "")
    assert(ellip.coords.coords.contains(center), clue = "")
  }

  test("Cube ROI contains all voxels in bounding cube") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(5, 5, 5))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](125, 1.0), sp)
    val center = Vector(2, 2, 2)
    val cube = Searchlight.cubeRoi(vol, center, radius = 1.0)
    assertEquals(cube.coords.size, 27, clue = "")
  }

  test("Blobby ROI drops edge voxels but keeps center") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(7, 7, 7))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](343, 1.0), sp)
    val center = Vector(3, 3, 3)
    val sphere = Searchlight.sphericalRoi(vol, center, radius = 2.5)
    val blob = Searchlight.blobbyRoi(
      vol,
      center,
      radius = 2.5,
      drop = 1.0,
      edgeFraction = 1.0,
      rng = new scala.util.Random(0L)
    )
    assert(blob.coords.size < sphere.coords.size, clue = "")
    assert(blob.coords.coords.contains(center), clue = "")
  }

  test("Resample.trilinear preserves data when spaces match") {
    val sp = NeuroSpace(Vector(3, 3, 1), spacing = Some(Vector(1.0, 1.0, 1.0)))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](9)(_.toDouble), sp)
    val res = Resample.trilinear(vol, sp)
    val vals = Vector.tabulate(res.copyToCanonicalArray.length)(i => res.copyToCanonicalArray(i))
    assertEquals(vals, Vector.tabulate(9)(_.toDouble), clue = "")
  }

  test("ConnComp labels components by size and connectivity") {
    val sp = NeuroSpace(Vector(3, 3, 1))
    val linA = sp.gridToIndex3D(0, 0, 0)
    val linB = sp.gridToIndex3D(1, 0, 0)
    val linC = sp.gridToIndex3D(2, 2, 0)
    val mask = Mask.fromIndices(sp, Array(linA, linB, linC))
    val (idxVol, sizeVol) = ConnComp.connComp3D(mask, ConnComp.Connectivity.Connect6)
    assertEquals(idxVol.valueAtCanonicalOrdinal(linA), 1, clue = "")
    assertEquals(idxVol.valueAtCanonicalOrdinal(linB), 1, clue = "")
    assertEquals(idxVol.valueAtCanonicalOrdinal(linC), 2, clue = "")
    assertEquals(sizeVol.valueAtCanonicalOrdinal(linA), 2, clue = "")
    assertEquals(sizeVol.valueAtCanonicalOrdinal(linC), 1, clue = "")

    val diagMask = Mask.fromIndices(sp, Array(linA, sp.gridToIndex3D(1, 1, 0)))
    val (idx6, _) = ConnComp.connComp3D(diagMask, ConnComp.Connectivity.Connect6)
    val (idx26, _) = ConnComp.connComp3D(diagMask, ConnComp.Connectivity.Connect26)
    def nclus(v: NeuroVol[Int]) =
      Vector.tabulate(v.copyToCanonicalArray.length)(i => v.copyToCanonicalArray(i)).filter(_ > 0).distinct.length
    assertEquals(nclus(idx6), 2, clue = "")
    assertEquals(nclus(idx26), 1, clue = "")
  }

  test("ClusteredNeuroVol.fromMask and centroids") {
    val sp = NeuroSpace(Vector(3, 3, 1))
    val linA = sp.gridToIndex3D(0, 0, 0)
    val linB = sp.gridToIndex3D(1, 0, 0)
    val linC = sp.gridToIndex3D(2, 2, 0)
    val mask = Mask.fromIndices(sp, Array(linA, linB, linC))
    val cvol = ClusteredNeuroVol.fromMask(mask, ClusteredNeuroVol.Connectivity.Connect6)
    assertEquals(cvol.numClusters, 2, clue = "")
    val dense = cvol.toDense
    assertEquals(dense.valueAtCanonicalOrdinal(linA), 1, clue = "")
    assertEquals(dense.valueAtCanonicalOrdinal(linC), 2, clue = "")
    val cents = cvol.centroids()
    assertEquals(cents.length, 2, clue = "")
    assert(math.abs(cents(0)(0) - 0.5) < 1e-9, clue = "")
    assert(math.abs(cents(0)(1)) < 1e-9, clue = "")
    assert(math.abs(cents(1)(0) - 2.0) < 1e-9, clue = "")
  }

  test("ClusteredNeuroVec reduces by clusters and broadcasts") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(2, 1, 1, 3))
    val data = Array[Double](1.0, 2.0, 3.0, 3.0, 4.0, 5.0) // voxel series: [1,2,3], [3,4,5]
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp)
    val mask = Mask.fromIndices(sp.spatialSpace, Array(0, 1))
    val cvol = ClusteredNeuroVol(mask, Array(1, 1))
    val cv = ClusteredNeuroVec.fromNeuroVecMean(vec, cvol)
    val tsVals = columnMajor2(cv.ts)
    assertEquals(tsVals, Vector(2.0, 3.0, 4.0), clue = "") // mean across voxels
    val s0 = cv.series(0)
    val s0v = Vector.tabulate(s0.length)(i => s0(i))
    assertEquals(s0v, Vector(2.0, 3.0, 4.0), clue = "")
  }

  test("Searchlight iterators respect center selection") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val mask = Mask.fromIndices(sp, Array(0, 3))
    val all = Searchlight.searchlightCoords(mask, radius = 1.0, nonzero = false).toVector
    val nz = Searchlight.searchlightCoords(mask, radius = 1.0, nonzero = true).toVector
    assertEquals(all.length, 4, clue = "")
    assertEquals(nz.length, 2, clue = "")
  }

  test("ClusteredNeuroVec toDense/toSparse and arithmetic") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(2, 1, 1, 2))
    val vec = NeuroVec.copyFromCanonicalArray[Double](Array[Double](1.0, 2.0, 3.0, 4.0), sp)
    val mask = Mask.fromIndices(sp.spatialSpace, Array(0, 1))
    val cvol = ClusteredNeuroVol(mask, Array(1, 2))
    val cv = ClusteredNeuroVec.fromNeuroVecMean(vec, cvol)
    val dense = cv.toDense
    val dVals = Vector.tabulate(dense.copyToCanonicalArray.length)(i => dense.copyToCanonicalArray(i))
    assertEquals(dVals, Vector(1.0, 2.0, 3.0, 4.0), clue = "")
    val sparse = cv.toSparse
    val dense2 = sparse.toDense
    val d2Vals = Vector.tabulate(dense2.copyToCanonicalArray.length)(i => dense2.copyToCanonicalArray(i))
    assertEquals(d2Vals, dVals, clue = "")

    val cv2 = cv + cv
    val tsVals = columnMajor2(cv2.ts)
    assertEquals(tsVals, Vector(2.0, 4.0, 6.0, 8.0), clue = "")
  }

  test("Clustered searchlight yields one ROI per cluster") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val mask = Mask.fromIndices(sp, Array(0, 1, 2, 3))
    val cvol = ClusteredNeuroVol(mask, Array(1, 1, 2, 2))
    val rois = Searchlight.clusteredSearchlight(cvol).toVector
    assertEquals(rois.length, 2, clue = "")
    assertEquals(rois.head.coords.size, 2, clue = "")
  }

  test("ClusteredNeuroVol medoid centroids use geometric median") {
    val sp = NeuroSpace(Vector(3, 3, 1))
    val idx = Array[Int](
      sp.gridToIndex3D(0, 0, 0),
      sp.gridToIndex3D(2, 0, 0),
      sp.gridToIndex3D(0, 2, 0),
      sp.gridToIndex3D(2, 2, 0)
    )
    val mask = Mask.fromIndices(sp, idx)
    val cvol = ClusteredNeuroVol(mask, Array(1, 1, 1, 1))
    val med = cvol.centroids(ClusteredNeuroVol.CentroidType.Medoid)
    assertEquals(med.length, 1, clue = "")
    assert(math.abs(med.head(0) - 1.0) < 1e-3, clue = "")
    assert(math.abs(med.head(1) - 1.0) < 1e-3, clue = "")
  }

  test("KMeans.partition creates requested number of clusters") {
    val sp = NeuroSpace(Vector(4, 1, 1))
    val mask = Mask.fromIndices(sp, Array(0, 1, 2, 3))
    val cvol = KMeans.partition(mask, k = 2, seed = 42, init = KMeans.Init.KMeansPlusPlus)
    assertEquals(cvol.numClusters, 2, clue = "")
    assert(
      Vector.tabulate(cvol.clusterIds.size)(cvol.clusterIds(_)).forall(id => id >= 1 && id <= 2),
      clue = ""
    )
  }

  test("ConnComp handles empty, single-voxel, and full masks") {
    val sp = NeuroSpace(Vector(5, 5, 5))
    val nels = sp.spatialDims.product

    val emptyFlags = PrimitiveBuffers.fillConst[Boolean](nels, false)
    val emptyMask = NeuroVol.copyFromCanonicalArray[Boolean](emptyFlags, sp)
    val (idxEmpty, sizeEmpty) = ConnComp.connComp3D(emptyMask)
    assert(idxEmpty.copyToCanonicalArray.forall(_ == 0), clue = "")
    assert(sizeEmpty.copyToCanonicalArray.forall(_ == 0), clue = "")

    val singleMask = Mask.fromIndices(sp, Array(sp.gridToIndex3D(2, 2, 2)))
    val (idxSingle, sizeSingle) = ConnComp.connComp3D(singleMask)
    assertEquals(idxSingle.copyToCanonicalArray.max, 1, clue = "")
    assertEquals(sizeSingle.copyToCanonicalArray.max, 1, clue = "")

    val fullFlags = PrimitiveBuffers.fillConst[Boolean](nels, true)
    val fullMask = NeuroVol.copyFromCanonicalArray[Boolean](fullFlags, sp)
    val (idxFull, sizeFull) = ConnComp.connComp3D(fullMask)
    assertEquals(idxFull.copyToCanonicalArray.max, 1, clue = "")
    assertEquals(sizeFull.copyToCanonicalArray.max, nels, clue = "")
  }

  test("ClusteredNeuroVol splitClusters supports non-contiguous ids") {
    val sp = NeuroSpace(Vector(2, 2, 2))
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](PrimitiveBuffers.fillConst[Boolean](8, true), sp)
    val clusters = Array(2, 4, 6, 2, 4, 6, 2, 4)
    val cvol = ClusteredNeuroVol(mask, clusters)
    val rois = cvol.splitClusters
    assertEquals(rois.length, 3, clue = "")
    val roiIds = rois.map(r => r.data(0)).sorted
    assertEquals(roiIds, Vector(2, 4, 6), clue = "")
    rois.foreach { r =>
      assert(Vector.tabulate(r.data.size)(r.data(_)).forall(_ == r.data(0)), clue = "")
    }
  }

  test("NeuroVec splitClusters supports non-contiguous ids") {
    val sp = NeuroSpace(Vector(2, 2, 2, 2))
    val data = PrimitiveBuffers.tabulate[Double](16)(i => (i + 1).toDouble)
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp)
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](PrimitiveBuffers.fillConst[Boolean](8, true), sp.spatialSpace)
    val clusters = Array(2, 4, 6, 2, 4, 6, 2, 4)
    val cvol = ClusteredNeuroVol(mask, clusters)
    val splits = vec.splitClusters(cvol)
    assertEquals(splits.length, 3, clue = "")

    val vol0 = vec.volume(0)
    def meanAt(idx: ravel.Array1[Int]): Double =
      var s = 0.0
      var i = 0
      while i < idx.size do
        s += vol0.valueAtCanonicalOrdinal(idx(i))
        i += 1
      s / idx.size.toDouble

    val expectedMeans = cvol.clusterIds.map(id => meanAt(cvol.clusterMap(id)))
    val gotMeans =
      splits.map { rv =>
        var s = 0.0
        var i = 0
        while i < rv.nVoxels do
          s += rv.data(0, i).asInstanceOf[Double]
          i += 1
        s / rv.nVoxels.toDouble
      }
    assertEquals(gotMeans, expectedMeans, clue = "")
  }

  test("ClusteredNeuroVec construction matches neuroim2 broadcast and ts") {
    import spire.std.double.given
    val sp3 = NeuroSpace(Vector(2, 2, 2))
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](PrimitiveBuffers.fillConst[Boolean](8, true), sp3)
    val cids = Array(1, 1, 1, 1, 2, 2, 2, 2)
    val cvol = ClusteredNeuroVol(mask, cids)

    val sp4 = NeuroSpace(Vector(2, 2, 2, 3))
    val v1 = Vector(10.0, 12.0, 8.0, 6.0, 1.0, 3.0, 5.0, 7.0)
    val v2 = Vector(20.0, 18.0, 22.0, 20.0, 2.0, 4.0, 6.0, 8.0)
    val v3 = Vector(0.0, 0.0, 0.0, 10.0, 10.0, 10.0, 10.0, 10.0)
    val valuesByTime = Vector(v1, v2, v3)
    val canonical =
      RavelArray.tabulate[Double](2, 2, 2, 3) { (x, y, z, time) =>
        val voxel = Indexing.gridToIndex3D(sp3.spatialDims, x, y, z)
        valuesByTime(time)(voxel)
      }
    val vec = NeuroVec.fromRavel(canonical, sp4)

    val cv = ClusteredNeuroVec.fromNeuroVecMean(vec, cvol)
    assertEquals(cv.space.dims, Vector(2, 2, 2, 3), clue = "")

    val m1 = v1.take(4).sum / 4.0; val m2 = v1.drop(4).sum / 4.0
    val n1 = v2.take(4).sum / 4.0; val n2 = v2.drop(4).sum / 4.0
    val p1 = v3.take(4).sum / 4.0; val p2 = v3.drop(4).sum / 4.0

    val vol1 = cv.volume(0)
    val vol2 = cv.volume(1)
    val vol3 = cv.volume(2)
    assert(vol1.copyToCanonicalArray.take(4).forall(_ == m1) && vol1.copyToCanonicalArray.drop(4).forall(_ == m2), clue = "")
    assert(vol2.copyToCanonicalArray.take(4).forall(_ == n1) && vol2.copyToCanonicalArray.drop(4).forall(_ == n2), clue = "")
    assert(vol3.copyToCanonicalArray.take(4).forall(_ == p1) && vol3.copyToCanonicalArray.drop(4).forall(_ == p2), clue = "")

    val expectedTs = Vector(m1, n1, p1, m2, n2, p2)
    val tsVec = columnMajor2(cv.ts)
    assertEquals(tsVec, expectedTs, clue = "")

    var t = 0
    while t < 3 do
      assertEquals(cv(0, 0, 0, t), cv(0, 1, 0, t), clue = "")
      t += 1
  }

  test("clusterSearchlightSeries k-NN and radius parity") {
    import spire.std.double.given
    val sp3 = NeuroSpace(Vector(2, 2, 1))
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](PrimitiveBuffers.fillConst[Boolean](4, true), sp3)
    val cvol = ClusteredNeuroVol(mask, Array(1, 2, 3, 4))

    val sp4 = NeuroSpace(Vector(2, 2, 1, 2))
    val data = Array[Double](10.0, 20.0, 11.0, 21.0, 12.0, 22.0, 13.0, 23.0)
    val vec = NeuroVec.copyFromCanonicalArray[Double](data, sp4)
    val cv = ClusteredNeuroVec.fromNeuroVecMean(vec, cvol)

    val wins = Searchlight.clusterSearchlightSeries(cv, k = 2)
    assertEquals(wins.length, 4, clue = "")
    val roi1 = wins.head
    assertEquals(roi1.data.shape, Shape(2, 2), clue = "")
    val s1 = roi1.seriesAt(0)
    val s1v = Vector.tabulate(s1.length)(i => s1(i))
    assertEquals(s1v, Vector(10.0, 20.0), clue = "")

    val winsr = Searchlight.clusterSearchlightSeries(cv, radius = Some(1.1))
    assertEquals(winsr.head.data.shape(1), 3, clue = "")
  }

  test("Searchlight nonzero filtering yields singleton ROI") {
    val sp = NeuroSpace(Vector(5, 5, 5))
    val lin = sp.gridToIndex3D(2, 2, 2)
    val mask = Mask.fromIndices(sp, Array(lin))
    val fullRoi = Searchlight.searchlight(mask, radius = 2.0, nonzero = false).next()
    val nzRoi = Searchlight.searchlight(mask, radius = 2.0, nonzero = true).next()
    assert(fullRoi.coords.size > nzRoi.coords.size, clue = "")
    assertEquals(nzRoi.coords.size, 1, clue = "")
    assertEquals(nzRoi.coords.coords(nzRoi.centerIndex), Vector(2, 2, 2), clue = "")
  }

  test("SparseNeuroVec validity catches mismatched mask/space") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(8, 8, 8, 5), spacing = Some(Vector(2.0, 2.0, 2.0)))
    val badSp = NeuroSpace(Vector(4, 4, 4), spacing = Some(Vector(2.0, 2.0, 2.0)))
    val badMask = NeuroVol.copyFromCanonicalArray[Boolean](PrimitiveBuffers.fillConst[Boolean](4 * 4 * 4, true), badSp)
    val map = IndexLookupVol(sp, Array(0))
    val dat = RavelArray.zeros[Double](5, 1)
    intercept[IllegalArgumentException] {
      SparseNeuroVec(dat, sp, badMask, map)
    }
  }

  test("SparseNeuroVec validity enforces time x mask-cardinality shapes") {
    import spire.std.double.given
    val sp = NeuroSpace(Vector(6, 6, 6, 4), spacing = Some(Vector(2.0, 2.0, 2.0)))
    val maskIdx = Array(0, 10, 20, 30, 40)
    val mask = Mask.fromIndices(sp.spatialSpace, maskIdx)
    val map = IndexLookupVol(sp, maskIdx)

    val wrongTime =
      RavelArray.zeros[Double](3, maskIdx.length)
    intercept[IllegalArgumentException] {
      SparseNeuroVec(wrongTime, sp, mask, map)
    }

    val wrongCols =
      RavelArray.zeros[Double](4, maskIdx.length + 1)
    intercept[IllegalArgumentException] {
      SparseNeuroVec(wrongCols, sp, mask, map)
    }
  }

  test("NeuroVec preserves input shape and linearization") {
    val sp = NeuroSpace(Vector(2, 2, 2, 2))
    val data = PrimitiveBuffers.tabulate[Int](16)(i => i + 1)
    val vec = NeuroVec.copyFromCanonicalArray[Int](data, sp)
    assertEquals(vec.space.dims, Vector(2, 2, 2, 2), clue = "")
    assertEquals(
      Vector.tabulate(vec.values.shape.rank)(vec.values.shape.apply),
      Vector(2, 2, 2, 2),
      clue = ""
    )
    val back = Vector.tabulate(vec.copyToCanonicalArray.length)(i => vec.copyToCanonicalArray(i))
    assertEquals(back, Vector.tabulate(16)(i => i + 1), clue = "")
  }

  test("NeuroVec series at voxel matches ROI drop semantics") {
    val sp = NeuroSpace(Vector(2, 2, 2, 3))
    val data = PrimitiveBuffers.tabulate[Int](24)(i => i + 1)
    val vec = NeuroVec.copyFromCanonicalArray[Int](data, sp)

    val ts = vec.series(0, 0, 0)
    val tsVals = Vector.tabulate(ts.length)(i => ts(i))
    assertEquals(tsVals, Vector(1, 2, 3), clue = "")

    val lin0 = sp.gridToIndex3D(0, 0, 0)
    val mat = vec.series(Array(lin0))
    assertEquals(mat.shape, Shape(3, 1), clue = "")
    val matVals = columnMajor2(mat)
    assertEquals(matVals, Vector(1, 2, 3), clue = "")
  }
