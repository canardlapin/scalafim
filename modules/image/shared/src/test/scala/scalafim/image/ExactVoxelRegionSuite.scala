package scalafim.image

class ExactVoxelRegionSuite extends munit.FunSuite:

  private val volumeSpace =
    VolumeSpace(NeuroSpace(Vector(2, 2, 1)))

  private val translatedSpace =
    VolumeSpace(
      NeuroSpace(
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
    VolumeDomain
      .register(
        volumeSpace,
        "exact voxel region suite",
        locus4s.DomainRegistry.empty
      )
      .toOption
      .get
  private type Voxel = packedDomain.S
  private val domain: VolumeDomain[Voxel] = packedDomain.value

  private def region(indices: Int*): locus4s.Region[Voxel] =
    locus4s.Region.fromOrdinals(domain.space, indices).toOption.get

  test("region construction canonicalizes membership and implements set algebra"):
    val left = region(3, 1, 1)
    val right = region(1, 2)

    assertEquals(left.ordinalsInDomainOrder.toVector, Vector(1, 3))
    assertEquals(left, region(1, 3))
    assertEquals(left.union(right).ordinalsInDomainOrder.toVector, Vector(1, 2, 3))
    assertEquals(left.intersect(right).ordinalsInDomainOrder.toVector, Vector(1))
    assertEquals(left.diff(right).ordinalsInDomainOrder.toVector, Vector(3))
    assertEquals(left.xor(right).ordinalsInDomainOrder.toVector, Vector(2, 3))
    assertEquals(left.complement.ordinalsInDomainOrder.toVector, Vector(0, 2))

  test("checked region algebra rejects a same-size foreign grid owner"):
    val foreign =
      VolumeDomain
        .register(
          translatedSpace,
          "translated exact voxel region suite",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    val expected = region(0, 1)
    val translated =
      locus4s.Region.fromOrdinals(foreign.value.space, Vector(0, 1)).toOption.get

    assert(expected.unionChecked(translated).isLeft)
    assert(expected.intersectChecked(translated).isLeft)

  test("ordered selections preserve feature order separately from membership"):
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(2, 0)).toOption.get
    val reversed =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get

    assertEquals(selection.ordinals.toVector, Vector(2, 0))
    assertEquals(selection.region.ordinalsInDomainOrder.toVector, Vector(0, 2))
    assertNotEquals(selection, reversed)
    assert(locus4s.Selection.fromOrdinals(domain.space, Vector(2, 2)).isLeft)

  test("selected-volume gather preserves order and rejects foreign owners"):
    val sampleSpace =
      image4s.SampleSpace.create(domain.grid, image4s.NonSpatialAxes.empty)
    val volume =
      NeuroVolume
        .categorical(
          sampleSpace,
          ravel.NDArray.fromSeq(
            ravel.Shape(2, 2, 1),
            Vector(10, 11, 12, 13)
          )
        )
        .toOption
        .get
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(2, 0)).toOption.get
    val selected =
      SelectedVolume.gather(domain, volume, selection).toOption.get

    assertEquals(selected.data.iterator.toVector, Vector(12, 10))
    assert(selected.selection.eq(selection))

    val foreign =
      VolumeDomain
        .register(
          translatedSpace,
          "translated exact selected volume suite",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    val foreignSelection =
      locus4s.Selection.fromOrdinals(foreign.value.space, Vector(0)).toOption.get
    assert(SelectedVolume.gather(domain, volume, foreignSelection).isLeft)

  test("selected series use position by time storage"):
    val time =
      image4s.Axis.ordinal("time", image4s.AxisKind.Time, 2).toOption.get
    val sampleSpace =
      image4s.SampleSpace.create(
        domain.grid,
        image4s.NonSpatialAxes.from(Vector(time)).toOption.get
      )
    val series =
      NeuroSeries
        .categorical(
          sampleSpace,
          ravel.NDArray.fromSeq(
            ravel.Shape(2, 2, 1, 2),
            Vector(0, 10, 1, 11, 2, 12, 3, 13)
          )
        )
        .toOption
        .get
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(2, 0)).toOption.get
    val selected =
      SelectedSeries.gather(domain, series, selection).toOption.get
    val mapped =
      selected.selected.mapValues[Int, image4s.Categorical](_ + 1)
    val result = SelectedSeries.fromSelected(mapped).toOption.get

    assertEquals(result.data.shape, ravel.Shape(2, 2))
    assertEquals(result.data.iterator.toVector, Vector(3, 13, 1, 11))

  test("grid coordinate adapters validate bounds in the exact domain"):
    val first =
      image4s.geometry.LatticeIndex
        .fromVector[image4s.geometry.D3](Vector(0, 0, 0))
        .toOption
        .get
    val last =
      image4s.geometry.LatticeIndex
        .fromVector[image4s.geometry.D3](Vector(1, 1, 0))
        .toOption
        .get
    val outside =
      image4s.geometry.LatticeIndex
        .fromVector[image4s.geometry.D3](Vector(2, 0, 0))
        .toOption
        .get

    assertEquals(domain.ordinalOf(first), Right(0))
    assertEquals(domain.ordinalOf(last), Right(3))
    assert(domain.ordinalOf(outside).isLeft)
