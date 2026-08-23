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

  test("selected series retains one compact Ravel value and exact ordered support") {
    val space = NeuroSpace(Vector(3, 1, 1, 2))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(space.spatialSpace),
          "ownership selected series",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(2, 0)).toOption.get
    val compact =
      RavelArray.tabulate[Double](2, 2) { (position, time) =>
        10.0 * position.toDouble + time.toDouble
      }
    val selected =
      SelectedSeries
        .create(
          domain,
          selection,
          image4s.Axis
            .ordinal("time", image4s.AxisKind.Time, 2)
            .toOption
            .get,
          compact,
          image4s.ImageMetadata.named("compact")
        )
        .toOption
        .get

    assert(selected.data.eq(compact), clue = "")
    assert(selected.selection.eq(selection), clue = "")
    assertEquals(selected.selection.ordinals.toVector, Vector(2, 0), clue = "")
    assertEquals(selected(0, 1), 1.0, clue = "")
    assertEquals(selected(1, 1), 11.0, clue = "")
    val dense = selected.toDense(0.0).toOption.get
    assertEquals(dense.data(1, 0, 0, 1), 0.0, clue = "")
  }

  test("ordered selected support survives time selection and dense roundtrips") {
    val space = NeuroSpace(Vector(3, 1, 1, 3))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(space.spatialSpace),
          "ownership ordered selected series",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(2, 0)).toOption.get
    val compact =
      RavelArray.tabulate[Double](2, 3) { (position, time) =>
        100.0 * position + time.toDouble
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
          compact,
          image4s.ImageMetadata.named("ordered")
        )
        .toOption
        .get
    val slicedData =
      RavelArray.tabulate[Double](2, 2): (position, time) =>
        selected(position, if time == 0 then 2 else 0)
    val sliced =
      SelectedSeries
        .create(
          domain,
          selection,
          image4s.Axis
            .ordinal("time", image4s.AxisKind.Time, 2)
            .toOption
            .get,
          slicedData,
          selected.metadata
        )
        .toOption
        .get

    assertEquals(sliced.selection.ordinals.toVector, Vector(2, 0), clue = "")
    assertEquals(sliced.data.iterator.toVector, Vector(2.0, 0.0, 102.0, 100.0), clue = "")

    val dense = selected.toDense(0.0).toOption.get
    val gathered = SelectedSeries.gather(domain, dense, selection).toOption.get
    assertEquals(gathered.selection.ordinals.toVector, Vector(2, 0), clue = "")
    assertEquals(gathered.data.shape, compact.shape, clue = "")
    var position = 0
    while position < 2 do
      var time = 0
      while time < 3 do
        assertEquals(gathered(position, time), compact(position, time), clue = "")
        time += 1
      position += 1
    assertEquals(gathered.data.size, 6, clue = "")
    assert(gathered.data.size < dense.data.size, clue = "")
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

  test("selected-volume construction copies mutable ingress into immutable storage") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(space),
          "ownership selected volume",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val input = Array[Int](10, 20)
    val values =
      SelectedVolume
        .categorical(
          domain,
          selection,
          RavelArray.fromSeq(ravel.Shape(2), input)
        )
        .fold(error => fail(error.message), identity)

    input(0) = 99
    assertEquals(values(0), 10, clue = "")
    assertEquals(values.data.iterator.toVector, Vector(10, 20), clue = "")
  }

  test("selected windows add only a certified center to selected storage") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val packed =
      VolumeDomain
        .register(
          VolumeSpace(space),
          "ownership selected window",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain: VolumeDomain[Voxel] = packed.value
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val input = Array[Int](10, 20)
    val selected =
      SelectedVolume
        .categorical(
          domain,
          selection,
          RavelArray.fromSeq(ravel.Shape(2), input)
        )
        .fold(error => fail(error.message), identity)
    val center = domain.space.indexAtValidatedOrdinal(2)
    val window =
      SelectedVolumeWindow
        .make(selected, center, centerPosition = 1)
        .fold(error => fail(error.message), identity)

    input(1) = 99
    assertEquals(window(1), 20, clue = "")
    assert(
      window.values.asInstanceOf[AnyRef].eq(selected.asInstanceOf[AnyRef]),
      clue = ""
    )
    assertEquals(window.center.ordinal, 2, clue = "")
  }
