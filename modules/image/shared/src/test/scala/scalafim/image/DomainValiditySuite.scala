package scalafim.image

import ravel.NDArray as RavelArray
import spire.std.int.given

class DomainValiditySuite extends munit.FunSuite:

  private val space = SampleSpaces(Vector(3, 3, 1))
  private val volumeSpace = VolumeSpace(space)
  private val packedDomain =
    GridDomain
      .register(
        volumeSpace.sampleSpace.grid,
        "domain validity voxels",
        locus4s.DomainRegistry.empty
      )
      .toOption
      .get
  private type Voxel = packedDomain.S
  private val domain = packedDomain.value

  test("selected windows validate that the selected center is the exact voxel") {
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 3)).toOption.get
    val selected =
      SelectedVolume
        .categorical(
          domain,
          selection,
          RavelArray.fromSeq(ravel.Shape(2), Vector(1, 1))
        )
        .toOption
        .get
    val result = SelectedVolumeWindow.make(
      selected,
      domain.space.indexAtValidatedOrdinal(0),
      centerPosition = 1
    )

    assertEquals(
      result,
      Left(SelectedVolumeWindowError.CenterMismatch(1, 0, 3)),
      clue = ""
    )
  }

  test("exact searchlight materialization reports an excluded center") {
    val sampleSpace =
      image4s.SampleSpace.create(domain.grid, image4s.NonSpatialAxes.empty)
    val volume =
      NeuroVolume
        .categorical(
          sampleSpace,
          RavelArray.tabulate[Int](3, 3, 1): (x, y, _) =>
            if x == 1 && y == 1 then 0 else 1
        )
        .toOption
        .get
    val field = domain.spatialField(volume).toOption.get
    val center = domain.space.indexAtValidatedOrdinal(4)
    val radius =
      SearchlightRadius.make(1.0).fold(error => fail(error.message), identity)
    val searchlight =
      ExactVolumeSearchlight.metricBalls(domain, radius).toOption.get
    val support =
      locus4s.Region.tabulate(domain.space)(index => field(index) != 0)

    val result =
      ExactVolumeSearchlight.materializeCategorical(
        domain,
        searchlight,
        center,
        field,
        support = Some(support)
      )

    assertEquals(
      result,
      Left(ExactVolumeSearchlightError.CenterExcluded(4)),
      clue = ""
    )
  }

  test("exact searchlight support produces valid region-bound windows") {
    val support =
      locus4s.Region.fromOrdinals(domain.space, Vector(0, 4)).toOption.get
    val radius =
      SearchlightRadius.make(1.0).fold(error => fail(error.message), identity)
    val searchlight =
      ExactVolumeSearchlight.metricBalls(domain, radius, support).toOption.get
    val field = locus4s.data.Field.view(domain.space)(_ => 1)
    val windows = support.indicesInDomainOrder.map: center =>
      ExactVolumeSearchlight
        .materializeCategorical(
          domain,
          searchlight,
          center,
          field,
          support = Some(support)
        )
        .toOption
        .get
    val materialized = windows.toVector

    assertEquals(materialized.length, 2, clue = "")
    assert(materialized.forall(window => window.values.selection.size == 1), clue = "")
    assert(materialized.forall(window => window.values.selection.region.contains(window.center)), clue = "")
  }

  test("exact searchlight relations reject non-empty rows outside centers") {
    val centers =
      locus4s.Region.fromOrdinals(domain.space, Vector(0)).toOption.get
    val rows = Vector.tabulate(domain.space.size): ordinal =>
      if ordinal == 0 then Vector(0)
      else if ordinal == 1 then Vector(1)
      else Vector.empty[Int]
    val relation =
      locus4s.Relation
        .fromOrdinalRows(domain.space, domain.space, rows)
        .toOption
        .get

    assertEquals(
      ExactVolumeSearchlight.fromRelation(centers, relation),
      Left(ExactVolumeSearchlightError.NonEmptyOutsideCenters(1)),
      clue = ""
    )
  }

  test("searchlight radii admit zero and reject negative or non-finite values") {
    assertEquals(SearchlightRadius.make(0.0).map(_.millimeters), Right(0.0), clue = "")
    assert(SearchlightRadius.make(-0.1).isLeft, clue = "")
    assert(SearchlightRadius.make(Double.PositiveInfinity).isLeft, clue = "")
    assert(SearchlightRadius.make(Double.NaN).isLeft, clue = "")
  }
