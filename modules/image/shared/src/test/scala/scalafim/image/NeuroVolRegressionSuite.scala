package scalafim.image

import Ops.*
import spire.std.double.given

class NeuroVolRegressionSuite extends munit.FunSuite:

  test("selected-volume scatter preserves indices and explicit fill") {
    val sp = NeuroSpace(Vector(3, 3, 3))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "selected volume scatter regression",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection
        .fromOrdinals(domain.space, Vector(1, 4, 9))
        .toOption
        .get
    val selected =
      SelectedVolume
        .create(
          domain,
          selection,
          ravel.NDArray.fromSeq(ravel.Shape(3), Vector(1.0, 2.0, 3.0))
        )
        .toOption
        .get
    val out = selected.toDense(0.0).toOption.get.data.iterator.toVector
    assertEquals(out(1), 1.0, clue = "")
    assertEquals(out(4), 2.0, clue = "")
    assertEquals(out(9), 3.0, clue = "")
    assertEquals(out.sum, 6.0, clue = "")
  }

  test("NeuroVol.asLogical flags all non-zero values") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.copyFromCanonicalArray[Double](Array[Double](0.0, -1.0, 2.0, 0.0), sp)
    val m = vol.asLogical
    val flags = Vector.tabulate(m.copyToCanonicalArray.length)(i => m.copyToCanonicalArray(i))
    assertEquals(flags, Vector(false, true, true, false), clue = "")
  }

  test("NeuroVol.asMask flags positive values only") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.copyFromCanonicalArray[Double](Array[Double](0.0, -1.0, 2.0, 0.0), sp)
    val m = vol.asMask
    val flags = Vector.tabulate(m.copyToCanonicalArray.length)(i => m.copyToCanonicalArray(i))
    assertEquals(flags, Vector(false, false, true, false), clue = "")
  }

  test("NeuroVol.asMask(indices) sets specified indices true") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](4, 0.0), sp)
    val m = vol.asMask(Array[Int](1, 3))
    val flags = Vector.tabulate(m.copyToCanonicalArray.length)(i => m.copyToCanonicalArray(i))
    assertEquals(flags, Vector(false, true, false, true), clue = "")
  }

  test("native volume gather and scatter roundtrip on an exact selection") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "selected volume roundtrip regression",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val sampleSpace =
      image4s.SampleSpace.create(domain.grid, image4s.NonSpatialAxes.empty)
    val volume =
      NeuroVolume
        .continuous(
          sampleSpace,
          ravel.NDArray.fromSeq(
            ravel.Shape(2, 2, 1),
            Vector(0.0, 1.0, 0.0, 1.0)
          )
        )
        .toOption
        .get
    val selection =
      locus4s.Selection
        .fromOrdinals(domain.space, Vector(1, 3))
        .toOption
        .get
    val selected =
      SelectedVolume.gather(domain, volume, selection).toOption.get
    val dense = selected.toDense(0.0).toOption.get

    assertEquals(
      dense.data.iterator.toVector,
      volume.data.iterator.toVector,
      clue = ""
    )
  }

  test("dense semantic masks convert explicitly to and from exact regions") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "mask region regression",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val region =
      locus4s.Region.fromOrdinals(domain.space, Vector(1, 3)).toOption.get
    val mask = Mask.fromRegion(domain, region).toOption.get
    val back = Mask.region(domain, mask).toOption.get

    assertEquals(back.ordinalsInDomainOrder.toVector, Vector(1, 3), clue = "")
  }

  test("exact selection rejects out-of-range voxel ordinals") {
    val sp = NeuroSpace(Vector(2, 2, 2))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "selection bounds regression",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get

    assert(locus4s.Selection.fromOrdinals(packed.value.space, Vector(8)).isLeft)
  }

  test("selected-volume construction rejects mismatched data and support lengths") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "selected shape regression",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0)).toOption.get

    assert(
      SelectedVolume
        .create(
          domain,
          selection,
          ravel.NDArray.fromSeq(ravel.Shape(2), Vector(1.0, 2.0))
        )
        .isLeft
    )
  }

  test("NeuroVol.asMatrix matches linear ordering") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.copyFromCanonicalArray[Int](Array[Int](1, 2, 3, 4), sp)
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
    val vol = NeuroVol.copyFromCanonicalArray[Int](Array[Int](1, 2, 1, 2), sp)

    val out = vol.mapValues(Map(1 -> 10, 2 -> 20), default = 0)
    val vals = Vector.tabulate(out.copyToCanonicalArray.length)(i => out.copyToCanonicalArray(i)).distinct.sorted
    assertEquals(vals, Vector(10, 20), clue = "")

    val out2 = vol.mapValues(Map(1 -> 10), default = 0)
    val vals2 = Vector.tabulate(out2.copyToCanonicalArray.length)(i => out2.copyToCanonicalArray(i)).distinct.sorted
    assertEquals(vals2, Vector(0, 10), clue = "")
  }

  test("mapValues accepts string keys when parseable and rejects non-numeric keys") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.copyFromCanonicalArray[Int](Array[Int](1, 2, 1, 2), sp)

    val out = vol.mapValues(Map("1" -> 10, "2" -> 20), default = 0)
    val vals = Vector.tabulate(out.copyToCanonicalArray.length)(i => out.copyToCanonicalArray(i)).distinct.sorted
    assertEquals(vals, Vector(10, 20), clue = "")

    intercept[IllegalArgumentException] {
      vol.mapValues(Map("a" -> 1, "b" -> 2), default = 0)
    }
  }

  test("mapf produces expected size and respects mask") {
    val sp = NeuroSpace(Vector(5, 5, 5))
    val nels = sp.spatialDims.product
    val vol = NeuroVol.copyFromCanonicalArray[Double](PrimitiveBuffers.fillConst[Double](nels, 1.0), sp)

    val kdim = Vector(3, 3, 3)
    val ker = Kernel3D(kdim, vdim = Vector(1.0, 1.0, 1.0)) { d =>
      if d == 0.0 then 1.0 else 0.0
    }

    val maskFlags = PrimitiveBuffers.fillConst[Boolean](nels, false)
    val center = Vector(2, 2, 2)
    val centerLin = Indexing.gridToIndex3D(sp.spatialDims, center(0), center(1), center(2))
    maskFlags(centerLin) = true
    val mask = NeuroVol.copyFromCanonicalArray[Boolean](maskFlags, sp)

    val out = SpatialFilters.mapf(vol, ker, mask = Some(mask))
    assertEquals(out.space.dims, vol.space.dims, clue = "")
    assertEquals(out(center(0), center(1), center(2)), 1.0, clue = "")
    val sum = Vector.tabulate(out.copyToCanonicalArray.length)(i => out.copyToCanonicalArray(i)).sum
    assertEquals(sum, 1.0, clue = "")
  }

  test("selected-volume gather extracts values at exact voxel coordinates") {
    val sp = NeuroSpace(Vector(4, 4, 4))
    val nels = sp.spatialDims.product
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(sp),
          "coordinate gather regression",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val sampleSpace =
      image4s.SampleSpace.create(domain.grid, image4s.NonSpatialAxes.empty)
    val volume =
      NeuroVolume
        .categorical(
          sampleSpace,
          ravel.NDArray.tabulate[Int](4, 4, 4):
            (x, y, z) => ((x * 4 + y) * 4 + z) + 1
        )
        .toOption
        .get
    val coords = Vector(Vector(0, 0, 0), Vector(1, 0, 0), Vector(0, 1, 0), Vector(3, 3, 3))
    val ordinals = coords.map: coord =>
      domain
        .ordinalOf(
          image4s.geometry.LatticeIndex
            .fromVector[image4s.geometry.D3](coord)
            .toOption
            .get
        )
        .toOption
        .get
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, ordinals).toOption.get
    val selected =
      SelectedVolume.gather(domain, volume, selection).toOption.get
    val got = selected.data.iterator.toVector
    val exp = coords.map(c => volume.data(c(0), c(1), c(2)))
    assertEquals(got, exp, clue = "")
  }

  test("NeuroVol.toVec produces a 4D NeuroVec with one volume") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val vol = NeuroVol.copyFromCanonicalArray[Double](Array[Double](1.0, 2.0, 3.0, 4.0), sp)
    val vec = vol.toVec
    assertEquals(vec.space.dims, Vector(2, 2, 1, 1), clue = "")
    val v0 = vec.volume(0)
    val got = Vector.tabulate(v0.copyToCanonicalArray.length)(i => v0.copyToCanonicalArray(i))
    assertEquals(got, Vector(1.0, 2.0, 3.0, 4.0), clue = "")
  }

  test("NeuroVol.concat stacks volumes along time into a NeuroVec") {
    val sp = NeuroSpace(Vector(2, 2, 1))
    val v1 = NeuroVol.copyFromCanonicalArray[Double](Array[Double](1.0, 2.0, 3.0, 4.0), sp)
    val v2 = NeuroVol.copyFromCanonicalArray[Double](Array[Double](10.0, 20.0, 30.0, 40.0), sp)
    val vec = v1.concat(v2)
    assertEquals(vec.space.dims, Vector(2, 2, 1, 2), clue = "")
    val lin = Vector.tabulate(vec.copyToCanonicalArray.length)(i => vec.copyToCanonicalArray(i))
    assertEquals(lin, Vector(1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 4.0, 40.0), clue = "")
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

    val vol = NeuroVol.copyFromCanonicalArray[Double](data, sp)
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
