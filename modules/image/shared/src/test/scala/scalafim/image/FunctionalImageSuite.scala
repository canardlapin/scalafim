package scalafim.image

import cats.implicits.*

class FunctionalImageSuite extends munit.FunSuite:

  private val volumeSpace =
    VolumeSpace(SampleSpaces(Vector(2, 2, 1)))

  private val translatedSpace =
    VolumeSpace(
      SampleSpaces(
        Vector(2, 2, 1),
        trans = Some(
          DMat.fromRows(
            Vector(
              Vector(1.0, 0.0, 0.0, 10.0),
              Vector(0.0, 1.0, 0.0, 0.0),
              Vector(0.0, 0.0, 1.0, 0.0),
              Vector(0.0, 0.0, 0.0, 1.0)
            )
          )
        )
      )
    )

  private val packedDomain =
    GridDomain
      .register(
        volumeSpace.sampleSpace.grid,
        "functional image voxels",
        locus4s.DomainRegistry.empty
      )
      .toOption
      .get
  private type Voxel = packedDomain.S
  private val domain = packedDomain.value
  private val timeAxis =
    image4s.Axis
      .ordinal("time", image4s.AxisKind.Time, 2)
      .toOption
      .get
  private val seriesSpace =
    image4s.SampleSpace.create(
      domain.grid,
      image4s.NonSpatialAxes.from(Vector(timeAxis)).toOption.get
    )

  private def nativeSeries: LabelSeries[seriesSpace.type, Int] =
    NeuroSeries
      .categorical(
        seriesSpace,
        ravel.NDArray.fromSeq(
          ravel.Shape(2, 2, 1, 2),
          Vector(0, 10, 1, 11, 2, 12, 3, 13)
        )
      )
      .toOption
      .get

  private def vector(values: Array[Int]): Vector[Int] =
    Vector.tabulate(values.length)(i => values(i))

  private def vector(values: ravel.Array1[Int]): Vector[Int] =
    Vector.tabulate(values.size)(i => values(i))

  test("mapValues and coordinate-aware volume mapping preserve geometry") {
    val volume = SomeLabelVolume.unsafeCopyFromCanonicalArray(Array[Int](10, 20, 30, 40), volumeSpace.toSampleSpace)
    val mapped = volume.mapValues[Int, image4s.Categorical](_ + 1)
    val located = volume.mapVoxels[Int, image4s.Categorical]: (coord, value) =>
      value + coord.x + 10 * coord.y

    assertEquals(mapped.space, volume.space, clue = "")
    assertEquals(vector(mapped.copyToCanonicalArray), Vector(11, 21, 31, 41), clue = "")
    assertEquals(vector(located.copyToCanonicalArray), Vector(10, 30, 31, 51), clue = "")
  }

  test("SomeNeuroSeries mapping distinguishes voxel coordinates from time samples") {
    val space = volumeSpace.addTime(2)
    val series = SomeScalarSeries.unsafeCopyFromCanonicalArray(Array[Int](0, 0, 0, 0, 0, 0, 0, 0), space.toSampleSpace)
    val mapped = series.mapSamples[Int, image4s.Continuous]: (coord, time, _) =>
      coord.x + 10 * coord.y + 100 * time

    assertEquals(vector(mapped.copyToCanonicalArray), Vector(0, 100, 10, 110, 1, 101, 11, 111), clue = "")
  }

  test("checked zipExact rejects a different physical grid") {
    val left = SomeLabelVolume.unsafeCopyFromCanonicalArray(Array[Int](1, 2, 3, 4), volumeSpace.toSampleSpace)
    val right = SomeLabelVolume.unsafeCopyFromCanonicalArray(Array[Int](10, 20, 30, 40), volumeSpace.toSampleSpace)
    val translated = SomeLabelVolume.unsafeCopyFromCanonicalArray(Array[Int](10, 20, 30, 40), translatedSpace.toSampleSpace)

    val summed =
      left
        .zipExact[Int, image4s.Categorical, Int, image4s.Categorical](right)(_ + _)
        .fold(error => fail(error.message), identity)
    assertEquals(vector(summed.copyToCanonicalArray), Vector(11, 22, 33, 44), clue = "")
    assert(
      left
        .zipExact[Int, image4s.Categorical, Int, image4s.Categorical](translated)(_ + _)
        .isLeft
    )
  }

  test("effectful traversal sequences failures without adding an image monad") {
    val volume = SomeLabelVolume.unsafeCopyFromCanonicalArray(Array[Int](1, 2, 3, 4), volumeSpace.toSampleSpace)
    val success =
      volume.traverseValues[Option, Int, image4s.Categorical](value => Some(value * 2))
    val failure =
      volume.traverseValues[Option, Int, image4s.Categorical](value => Option.when(value < 3)(value))

    assertEquals(success.map(result => vector(result.copyToCanonicalArray)), Some(Vector(2, 4, 6, 8)), clue = "")
    assertEquals(failure, None, clue = "")
  }

  test("selected-series mapping retains exact selection and exposes coordinates") {
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(2, 0)).toOption.get
    val selected =
      SelectedSeries.gather(domain, nativeSeries, selection).toOption.get
    val mappedData =
      ravel.NDArray.tabulate[Int](selection.size, 2): (position, time) =>
        val ordinal = selection.ordinals(position)
        val coord = domain.indexOfOrdinal(ordinal).toOption.get.values
        selected(position, time) + coord(1) * 100 + time * 1000
    val mapped =
      selected.selected
        .withSelectionData(selection, mappedData)
        .toOption
        .get
        .requireDataRank[2]
        .toOption
        .get
    val result = SelectedSeries.fromSelected(mapped).toOption.get

    assert(result.selection eq selection)
    assertEquals(result(0, 0), 2)
    assertEquals(result(0, 1), 1012)
    assertEquals(result(1, 0), 0)
    assertEquals(result(1, 1), 1010)
  }

  test("region algebra and ordered selection compose without duplicate wrappers") {
    val left =
      locus4s.Region.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val right =
      locus4s.Region.fromOrdinals(domain.space, Vector(1, 2)).toOption.get
    val selection =
      locus4s.Selection.fromRegion(left.union(right)).toOption.get
    val selected =
      SelectedSeries.gather(domain, nativeSeries, selection).toOption.get
    val mapped =
      selected.selected
        .mapValues[Int, image4s.Categorical](_ + 1)
    val result = SelectedSeries.fromSelected(mapped).toOption.get

    assertEquals(result.selection.ordinals.toVector, Vector(0, 1, 2), clue = "")
    assertEquals(result(0, 0), 1)
    assertEquals(result(2, 1), 13)
  }

  test("selected data requires an explicit missing-voxel policy") {
    val support =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(2, 1, 0)).toOption.get
    val sparse =
      SelectedSeries.gather(domain, nativeSeries, support).toOption.get

    val required = sparse.reselect(selection, MissingVoxelPolicy.RequireCovered)
    required match
      case Left(SelectedImageError.OutsideSupport(missing)) =>
        assertEquals(missing.ordinalsInDomainOrder.toVector, Vector(1), clue = "")
      case other =>
        fail(s"expected typed missing-support error, got $other")

    val dropped =
      sparse
        .reselect(selection, MissingVoxelPolicy.DropMissing)
        .fold(error => fail(error.message), identity)
    assertEquals(dropped.selection.ordinals.toVector, Vector(2, 0), clue = "")
    assertEquals(dropped(0, 0), 2)
    assertEquals(dropped(0, 1), 12)
    assertEquals(dropped(1, 0), 0)
    assertEquals(dropped(1, 1), 10)

    val filled =
      sparse
        .reselect(selection, MissingVoxelPolicy.Fill(-1))
        .fold(error => fail(error.message), identity)
    assertEquals(filled(0, 0), 2)
    assertEquals(filled(0, 1), 12)
    assertEquals(filled(1, 0), -1)
    assertEquals(filled(1, 1), -1)
    assertEquals(filled(2, 0), 0)
    assertEquals(filled(2, 1), 10)

    val foreignPacked =
      GridDomain
        .register(
          translatedSpace.sampleSpace.grid,
          "functional translated voxels",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    val translatedSelection =
      locus4s.Selection
        .fromOrdinals(foreignPacked.value.space, Vector(0))
        .toOption
        .get
    sparse.reselect(translatedSelection, MissingVoxelPolicy.RequireCovered) match
      case Left(SelectedImageError.SelectionSpace(_)) => ()
      case other => fail(s"expected typed sparse grid mismatch, got $other")
  }
