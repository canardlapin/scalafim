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

  test("ordered voxel selections use canonical grid ordinals") {
    val sp = NeuroSpace(Vector(2, 3, 4))
    val coords = Vector(Vector(0, 0, 0), Vector(1, 0, 0), Vector(0, 1, 0))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "core ordered voxel selection",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val ordinals = coords.map: coord =>
      val index =
        image4s.geometry.LatticeIndex
          .fromVector[image4s.geometry.D3](coord)
          .toOption
          .get
      domain.ordinalOf(index).toOption.get
    val selection =
      locus4s.Selection
        .fromOrdinals(domain.space, ordinals)
        .toOption
        .get

    assertEquals(selection.ordinals.toVector, Vector(0, 12, 4), clue = "")
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

  test("selected series retain position-first storage and contiguous time") {
    val sp = NeuroSpace(Vector(2, 1, 1, 3))
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val data = RavelArray.tabulate[Int](2, 1, 1, 3):
      (x, _, _, time) => x * 3 + time
    val series =
      NeuroSeries
        .categorical(sampleSpace, data)
        .toOption
        .get
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core selected series",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection
        .fromOrdinals(domain.space, Vector(0, 1))
        .toOption
        .get
    val selected =
      SelectedSeries.gather(domain, series, selection).toOption.get
    val provider = selected.selected
    val first =
      provider.seriesAt(
        provider.selection.positions.indexAtValidatedOrdinal(0)
      )

    assertEquals(selected.data.shape, Shape(2, 3), clue = "")
    assertEquals(first.iterator.toVector, Vector(0, 1, 2), clue = "")
  }

  test("dense series gather and explicit fill scatter roundtrip") {
    val sp = NeuroSpace(Vector(2, 2, 1, 3))
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val series =
      NeuroSeries
        .continuous(
          sampleSpace,
          RavelArray.tabulate[Double](2, 2, 1, 3):
            (x, y, _, time) =>
              (x * 2 * 1 * 3 + y * 1 * 3 + time).toDouble
        )
        .toOption
        .get
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core dense selected roundtrip",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection
        .fromOrdinals(domain.space, Vector(0, 3))
        .toOption
        .get
    val selected =
      SelectedSeries.gather(domain, series, selection).toOption.get
    val dense2 = selected.toDense(0.0).toOption.get
    val dVals = dense2.data.iterator.toVector
    assertEquals(dVals, Vector(0.0, 1.0, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0, 10.0, 11.0), clue = "")
  }

  test("selected series rows match the canonical dense voxel-time view") {
    val sp = NeuroSpace(Vector(10, 10, 10, 3))
    val spatialNels = sp.spatialDims.product
    val tLen = sp.dims(3)
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val dense =
      NeuroSeries
        .continuous(
          sampleSpace,
          RavelArray.tabulate[Double](10, 10, 10, 3):
            (x, y, z, time) =>
              val ordinal = ((x * 10 + y) * 10 + z) * 3 + time
              (ordinal.toDouble + 1.0) / 10.0
        )
        .toOption
        .get
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core selected matrix",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val ordinals = Vector.range(0, spatialNels).filter(ordinal => ordinal % 10 < 3)
    val selection =
      locus4s.Selection
        .fromOrdinals(domain.space, ordinals)
        .toOption
        .get
    val selected =
      SelectedSeries.gather(domain, dense, selection).toOption.get
    val matrix = dense.voxelTimeMatrix.toOption.get

    assertEquals(selected.data.shape, Shape(ordinals.length, tLen), clue = "")
    var position = 0
    while position < ordinals.length do
      var t = 0
      while t < tLen do
        assertEqualsDouble(
          selected(position, t),
          matrix(ordinals(position), t),
          1e-12
        )
        t += 1
      position += 1
  }

  test("selected series preserve explicit support order and dense parity") {
    val sp = NeuroSpace(Vector(2, 2, 2, 2))
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val series =
      NeuroSeries
        .continuous(
          sampleSpace,
          RavelArray.tabulate[Double](2, 2, 2, 2):
            (x, y, z, time) =>
              (((x * 2 + y) * 2 + z) * 2 + time + 1).toDouble
        )
        .toOption
        .get
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core selected parity",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val ordinals = Vector(0, 2, 5, 7)
    val selection =
      locus4s.Selection
        .fromOrdinals(domain.space, ordinals)
        .toOption
        .get
    val selected =
      SelectedSeries.gather(domain, series, selection).toOption.get
    val dense = selected.toDense(0.0).toOption.get

    assertEquals(selected.selection.ordinals.toVector, ordinals, clue = "")
    assertEquals(selected.data.shape, Shape(4, 2), clue = "")
    var position = 0
    while position < ordinals.length do
      val lattice = domain.indexOfOrdinal(ordinals(position)).toOption.get.values
      var time = 0
      while time < 2 do
        assertEqualsDouble(
          selected(position, time),
          dense.data(lattice(0), lattice(1), lattice(2), time),
          1e-12
        )
        time += 1
      position += 1
  }

  test("selected-series fill policy is explicit for missing voxels") {
    val sp = NeuroSpace(Vector(2, 2, 1, 2))
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val series =
      NeuroSeries
        .continuous(
          sampleSpace,
          RavelArray.tabulate[Double](2, 2, 1, 2):
            (x, y, _, time) => ((x * 2 + y) * 2 + time).toDouble
        )
        .toOption
        .get
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core selected fill",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val support =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val requested =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 1, 2)).toOption.get
    val selected =
      SelectedSeries.gather(domain, series, support).toOption.get
    val filled =
      selected.reselect(requested, MissingVoxelPolicy.Fill(0.0)).toOption.get

    assertEquals(filled.data.shape, Shape(3, 2), clue = "")
    assertEquals(
      filled.data.iterator.toVector,
      Vector(0.0, 1.0, 0.0, 0.0, 4.0, 5.0),
      clue = ""
    )
  }

  test("selected-series union arithmetic requires an explicit fill policy") {
    val sp = NeuroSpace(Vector(2, 2, 1, 2))
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val first =
      NeuroSeries
        .continuous(
          sampleSpace,
          RavelArray.tabulate[Double](2, 2, 1, 2):
            (x, y, _, time) => ((x * 2 + y) * 2 + time).toDouble
        )
        .toOption
        .get
    val second =
      NeuroSeries
        .continuous(
          sampleSpace,
          RavelArray.tabulate[Double](2, 2, 1, 2):
            (x, y, _, time) => ((x * 2 + y) * 2 + time + 10).toDouble
        )
        .toOption
        .get
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core selected union",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val leftSupport =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 1)).toOption.get
    val rightSupport =
      locus4s.Selection.fromOrdinals(domain.space, Vector(1, 3)).toOption.get
    val union =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 1, 3)).toOption.get
    val left =
      SelectedSeries
        .gather(domain, first, leftSupport)
        .toOption
        .get
        .reselect(union, MissingVoxelPolicy.Fill(0.0))
        .toOption
        .get
    val right =
      SelectedSeries
        .gather(domain, second, rightSupport)
        .toOption
        .get
        .reselect(union, MissingVoxelPolicy.Fill(0.0))
        .toOption
        .get
    val summedData =
      RavelArray.tabulate[Double](union.size, 2): (position, time) =>
        left(position, time) + right(position, time)
    val summed =
      left.selected
        .withSelectionData(union, summedData)
        .toOption
        .get
        .requireDataRank[2]
        .toOption
        .get
    val selectedSum = SelectedSeries.fromSelected(summed).toOption.get
    val vals = selectedSum.toDense(0.0).toOption.get.data.iterator.toVector
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

  test("cluster supports gather exact position-first selected series") {
    val sp = NeuroSpace(Vector(2, 2, 1, 2))
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val series =
      NeuroSeries
        .categorical(
          sampleSpace,
          RavelArray.tabulate[Int](2, 2, 1, 2):
            (x, y, _, time) => (x * 2 + y) * 2 + time
        )
        .toOption
        .get
    val mask = Mask.fromIndices(sp.spatialSpace, Array(0, 2, 3))
    val cvol = ClusteredNeuroVol(mask, Array(1, 2, 1))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core cluster selected series",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selected = cvol.clusterIds.map: id =>
      val ordinals = cvol.clusterMap(id).iterator.toVector
      val selection =
        locus4s.Selection.fromOrdinals(domain.space, ordinals).toOption.get
      SelectedSeries.gather(domain, series, selection).toOption.get

    assertEquals(selected.length, 2, clue = "")
    assertEquals(selected.head.data.shape, Shape(2, 2), clue = "")
    assertEquals(selected(1).data.shape, Shape(1, 2), clue = "")
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

  test("selected-series time concatenation uses an explicit union support") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "core selected time concatenation",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val timeOne =
      image4s.Axis
        .ordinal("time", image4s.AxisKind.Time, 1)
        .toOption
        .get
    val timeTwo =
      image4s.Axis
        .ordinal("time", image4s.AxisKind.Time, 2)
        .toOption
        .get
    val firstSpace =
      image4s.SampleSpace.create(
        domain.grid,
        image4s.NonSpatialAxes.from(Vector(timeOne)).toOption.get
      )
    val secondSpace =
      image4s.SampleSpace.create(
        domain.grid,
        image4s.NonSpatialAxes.from(Vector(timeTwo)).toOption.get
      )
    val first =
      NeuroSeries
        .continuous(
          firstSpace,
          RavelArray.tabulate[Double](2, 2, 1, 1):
            (x, y, _, _) => (x * 2 + y + 1).toDouble
        )
        .toOption
        .get
    val second =
      NeuroSeries
        .continuous(
          secondSpace,
          RavelArray.tabulate[Double](2, 2, 1, 2):
            (x, y, _, time) => ((x * 2 + y) * 2 + time + 10).toDouble
        )
        .toOption
        .get
    val leftSupport =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 1)).toOption.get
    val rightSupport =
      locus4s.Selection.fromOrdinals(domain.space, Vector(1, 3)).toOption.get
    val union =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 1, 3)).toOption.get
    val left =
      SelectedSeries
        .gather(domain, first, leftSupport)
        .toOption
        .get
        .reselect(union, MissingVoxelPolicy.Fill(0.0))
        .toOption
        .get
    val right =
      SelectedSeries
        .gather(domain, second, rightSupport)
        .toOption
        .get
        .reselect(union, MissingVoxelPolicy.Fill(0.0))
        .toOption
        .get
    val concatenated =
      RavelArray.tabulate[Double](union.size, 3): (position, time) =>
        if time == 0 then left(position, 0)
        else right(position, time - 1)
    val selected =
      SelectedSeries
        .create(
          domain,
          union,
          image4s.Axis
            .ordinal("time", image4s.AxisKind.Time, 3)
            .toOption
            .get,
          concatenated
        )
        .toOption
        .get
    val vals = selected.toDense(0.0).toOption.get.data.iterator.toVector
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

  test("cluster fibers support non-contiguous ids as exact regions") {
    val sp = NeuroSpace(Vector(2, 2, 2))
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](PrimitiveBuffers.fillConst[Boolean](8, true), sp)
    val clusters = Array(2, 4, 6, 2, 4, 6, 2, 4)
    val cvol = ClusteredNeuroVol(mask, clusters)
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "core non-contiguous cluster fibers",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val fibers = cvol.clusterIds.map: id =>
      locus4s.Region
        .fromOrdinals(domain.space, cvol.clusterMap(id).iterator)
        .toOption
        .get

    assertEquals(cvol.clusterIds, Vector(2, 4, 6), clue = "")
    assertEquals(fibers.map(_.cardinality), Vector(3, 3, 2), clue = "")
    assertEquals(
      fibers.reduce(_.union(_)).ordinalsInDomainOrder.toVector,
      Vector.range(0, 8),
      clue = ""
    )
  }

  test("non-contiguous cluster fibers gather correct selected-series means") {
    val sp = NeuroSpace(Vector(2, 2, 2, 2))
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val series =
      NeuroSeries
        .continuous(
          sampleSpace,
          RavelArray.tabulate[Double](2, 2, 2, 2):
            (x, y, z, time) =>
              (((x * 2 + y) * 2 + z) * 2 + time + 1).toDouble
        )
        .toOption
        .get
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](PrimitiveBuffers.fillConst[Boolean](8, true), sp.spatialSpace)
    val clusters = Array(2, 4, 6, 2, 4, 6, 2, 4)
    val cvol = ClusteredNeuroVol(mask, clusters)
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core non-contiguous cluster series",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val means = cvol.clusterIds.map: id =>
      val ordinals = cvol.clusterMap(id).iterator.toVector
      val selection =
        locus4s.Selection.fromOrdinals(domain.space, ordinals).toOption.get
      val selected =
        SelectedSeries.gather(domain, series, selection).toOption.get
      var sum = 0.0
      var position = 0
      while position < selection.size do
        sum += selected(position, 0)
        position += 1
      sum / selection.size.toDouble

    assertEquals(means, Vector(7.0, 9.0, 8.0), clue = "")
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

  test("selected-series gather rejects a foreign support owner") {
    val sp = NeuroSpace(Vector(8, 8, 8, 5), spacing = Some(Vector(2.0, 2.0, 2.0)))
    val badSp = NeuroSpace(Vector(4, 4, 4), spacing = Some(Vector(2.0, 2.0, 2.0)))
    val sampleSpace = NeuroSpace.requireD3(sp).toOption.get
    val series =
      NeuroSeries
        .continuous(sampleSpace, RavelArray.zeros[Double](8, 8, 8, 5))
        .toOption
        .get
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core selected owner",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val foreignPacked =
      VolumeDomain
        .register(
          VolumeSpace(badSp),
          "core foreign selected owner",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    val foreignSelection =
      locus4s.Selection
        .fromOrdinals(foreignPacked.value.space, Vector(0))
        .toOption
        .get

    assert(SelectedSeries.gather(domain, series, foreignSelection).isLeft)
  }

  test("selected-series validity enforces position x time shape") {
    val sp = NeuroSpace(Vector(6, 6, 6, 4), spacing = Some(Vector(2.0, 2.0, 2.0)))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp.spatialSpace),
          "core selected shape",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection
        .fromOrdinals(domain.space, Vector(0, 10, 20, 30, 40))
        .toOption
        .get
    val time =
      image4s.Axis
        .ordinal("time", image4s.AxisKind.Time, 4)
        .toOption
        .get

    assert(
      SelectedSeries
        .create(domain, selection, time, RavelArray.zeros[Double](5, 3))
        .isLeft
    )
    assert(
      SelectedSeries
        .create(domain, selection, time, RavelArray.zeros[Double](6, 4))
        .isLeft
    )
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
