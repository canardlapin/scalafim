package scalafim.image

import ravel.DType.given
import ravel.NDArray as RavelArray

class OwnershipSuite extends munit.FunSuite:

  test("canonical array construction copies its input buffer") {
    val space = VolumeSpace(NeuroSpace(Vector(2, 2, 1))).sampleSpace
    val input = Array[Int](1, 2, 3, 4)
    val volume =
      NeuroVolume
        .copyCategoricalFromCanonicalArray[Int](space, input)
        .fold(error => fail(error.message), identity)

    input(0) = 99
    assertEquals(volume(0, 0, 0), 1, clue = "")
  }

  test("canonical ingress retains one Sampled and Ravel value") {
    val space = VolumeSpace(NeuroSpace(Vector(2, 3, 2))).sampleSpace
    val input =
      PrimitiveBuffers.tabulate[Int](12)(index => index + 1)
    val volume =
      NeuroVolume
        .copyCategoricalFromCanonicalArray[Int](space, input)
        .fold(error => fail(error.message), identity)

    assert(
      volume.asInstanceOf[AnyRef].eq(volume.sampled.asInstanceOf[AnyRef]),
      clue = "NeuroVolume must be the Sampled value, not an allocating wrapper"
    )
    assert(volume.data.eq(volume.sampled.data), clue = "")
    assert(volume.data.isCanonicalLayout, clue = "")
    input(0) = 99
    assertEquals(volume(0, 0, 0), 1, clue = "")
    assertEquals(volume(0, 0, 1), 2, clue = "")
    assertEquals(volume(1, 0, 0), 7, clue = "")
  }

  test("dense compatibility geometry is the sampled SampleSpace itself") {
    val volumeSpace = NeuroSpace(Vector(2, 3, 2))
    val volume =
      NeuroVol.fromRavel(
        RavelArray.tabulate[Int](2, 3, 2)((i, j, k) => i + 10 * j + 100 * k),
        volumeSpace,
        "volume"
      )
    assert(
      NeuroSpace.canonical(volume.space).eq(volume.sampled.sampleSpace),
      clue = "NeuroVol.space must not retain or reconstruct geometry"
    )
    assert(
      NeuroSpace.canonical(volumeSpace).eq(volume.sampled.sampleSpace),
      clue = "ranked Ravel construction must reuse the admitted SampleSpace"
    )

    val seriesSpace = volumeSpace.addDim(2, Some(Axis.Time))
    val series =
      NeuroVec.fromRavel(
        RavelArray.tabulate[Int](2, 3, 2, 2) {
          (i, j, k, t) => i + 10 * j + 100 * k + 1000 * t
        },
        seriesSpace,
        "series"
      )
    assert(
      series.asInstanceOf[AnyRef] eq series.sampled.asInstanceOf[AnyRef],
      clue = "NeuroVec must be the Sampled value, not an allocating wrapper"
    )
    assert(
      NeuroSpace.canonical(series.space).eq(series.sampled.sampleSpace),
      clue = "NeuroVec.space must be a zero-wrapper compatibility name"
    )
    assert(
      NeuroSpace.canonical(seriesSpace).eq(series.sampled.sampleSpace),
      clue = "series construction must reuse the admitted SampleSpace"
    )
  }

  test("series volume selection is a zero-copy Ravel view") {
    val space = NeuroSpace(Vector(2, 2, 2, 3))
    val series =
      NeuroVec.copyFromCanonicalArray[Int](
        PrimitiveBuffers.tabulate[Int](24)(index => index + 1),
        space,
        "series"
      )
    val volume = series.volume(1)

    assert(series.values.isWholeBuffer, clue = "")
    assert(!volume.values.isWholeBuffer, clue = "")
    assert(!volume.values.isCanonicalLayout, clue = "")
    assert(volume.values.eq(volume.sampled.data), clue = "")
    assertEquals(volume(1, 1, 1), series(1, 1, 1, 1), clue = "")
  }

  test("component fields retain one image4s and Ravel value") {
    val grid = GridSpec.identity(Vector(2, 2, 2))
    val data =
      RavelArray.tabulate[Double](2, 2, 2, 3) {
        (i, j, k, component) =>
          i.toDouble + 10.0 * j + 100.0 * k + 1000.0 * component
      }
    val field =
      DenseVectorField.sourceCoordinates(
        grid,
        data
      )

    assert(field.values.eq(data), clue = "")
    assert(field.values.eq(field.sampled.data), clue = "")
    assert(
      field.sampled.asInstanceOf[AnyRef] eq field.asInstanceOf[AnyRef],
      clue = "DenseVectorField must be the exact Sampled object"
    )
    assertEquals(field.sampled.nonSpatialAxes.shape, Vector(3), clue = "")
    assertEquals(field.sampled.grid.shape, grid.dims, clue = "")
    assertEquals(field(1, 1, 1, 2), 2111.0, clue = "")
  }

  test("sparse series retains one compact Ravel value and ordered typed support") {
    import spire.std.double.given
    val space = NeuroSpace(Vector(3, 1, 1, 2))
    val indexSet =
      VoxelIndexSet.unique(space.spatialSpace, Array(2, 0))
    val support = SparseSupport.fromIndexSet(indexSet)
    val compact =
      RavelArray.tabulate[Double](2, 2) { (time, position) =>
        10.0 * position.toDouble + time.toDouble
      }
    val sparse =
      SparseNeuroVec(compact, space, support, "compact")

    assert(sparse.data.eq(compact), clue = "")
    assert(sparse.support.eq(support), clue = "")
    assertEquals(sparse.support.indexSet.toVector, Vector(2, 0), clue = "")
    assertEquals(sparse.support.positionOf(2), 0, clue = "")
    assertEquals(sparse.support.positionOf(0), 1, clue = "")
    assertEquals(sparse(2, 0, 0, 1), 1.0, clue = "")
    assertEquals(sparse(0, 0, 0, 1), 11.0, clue = "")
    assertEquals(sparse(1, 0, 0, 1), 0.0, clue = "")
  }

  test("ordered sparse support survives time slicing and dense roundtrips") {
    import spire.std.double.given
    val space = NeuroSpace(Vector(3, 1, 1, 3))
    val support =
      SparseSupport.fromIndexSet(
        VoxelIndexSet.unique(space.spatialSpace, Array(2, 0))
      )
    val compact =
      RavelArray.tabulate[Double](3, 2) { (time, position) =>
        100.0 * position + time.toDouble
      }
    val sparse = SparseNeuroVec(compact, space, support, "ordered")

    val sliced = sparse.subVector(Seq(2, 0))
    assertEquals(sliced.support.indexSet.toVector, Vector(2, 0), clue = "")
    assertEquals(sliced.data(0, 0), 2.0, clue = "")
    assertEquals(sliced.data(0, 1), 102.0, clue = "")
    assertEquals(sliced.data(1, 0), 0.0, clue = "")
    assertEquals(sliced.data(1, 1), 100.0, clue = "")

    val dense = sparse.toDense
    val gathered = dense.asSparse(support.indexSet)
    assertEquals(gathered.support.indexSet.toVector, Vector(2, 0), clue = "")
    assertEquals(gathered.data.shape, compact.shape, clue = "")
    var time = 0
    while time < 3 do
      var position = 0
      while position < 2 do
        assertEquals(gathered.data(time, position), compact(time, position), clue = "")
        position += 1
      time += 1
    assertEquals(gathered.data.size, 6, clue = "")
    assert(gathered.data.size < dense.values.size, clue = "")
  }

  test("checked dense reconstruction reports shape failures") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val result = NeuroVol.copyFromCanonicalArrayChecked(Array[Int](1, 2, 3), space)

    assertEquals(
      result,
      Left(NeuroImageError.LinearSizeMismatch("NeuroVolume canonical array", 4, 3)),
      clue = ""
    )
  }

  test("ROI construction copies mutable ingress into immutable canonical storage") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val roi =
      VoxelRoi(
        VolumeSpace(space),
        Vector(VoxelCoord(0, 0, 0), VoxelCoord(1, 0, 0))
      )
    val input = Array[Int](10, 20)
    val values =
      ROIVol.make[Int](space, roi, input).fold(error => fail(error.message), identity)

    input(0) = 99
    assertEquals(values(0), 10, clue = "")

    val exported = values.values
    assertEquals(Vector.tabulate(exported.size)(i => exported(i)), Vector(10, 20), clue = "")
  }

  test("ROI windows own public input and reconstruct only compatible geometry") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val coords = ROICoords(Vector(Vector(0, 0, 0), Vector(1, 0, 0)))
    val input = Array[Int](10, 20)
    val window =
      ROIVolWindow
        .make[Int](space, coords, input, centerIndex = 1, parentIndex = space.gridToIndex3D(1, 0, 0))
        .fold(error => fail(error.message), identity)

    input(1) = 99
    assertEquals(window(1), 20, clue = "")

    val exported = window.values
    assertEquals(Vector.tabulate(exported.size)(i => exported(i)), Vector(10, 20), clue = "")
    assertEquals(window.toROIVol.roi, window.selection.toVoxelRoi, clue = "")
  }
