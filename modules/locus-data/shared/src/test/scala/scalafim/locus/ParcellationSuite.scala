package scalafim.locus

class ParcellationSuite extends munit.FunSuite:
  private sealed trait X
  private sealed trait P
  private sealed trait Q
  private sealed trait N

  private val ambient =
    FiniteSpace.make[X](SpaceKey.unsafe("partition:ambient"), 6).toOption.get
  private val parcels =
    FiniteSpace.make[P](SpaceKey.unsafe("partition:parcels"), 3).toOption.get
  private val parcellation =
    Parcellation.fromAssignments(
      ambient,
      parcels,
      Vector(Some(0), Some(0), None, Some(1), Some(2), Some(2))
    ).toOption.get

  test("fibers are disjoint, cover support, and every parcel is inhabited"):
    val fibers = parcels.points.map(parcellation.fiber).toVector
    assertEquals(parcellation.support.ordinalsInDomainOrder.toVector, Vector(0, 1, 3, 4, 5))
    assertEquals(fibers.map(_.ordinalsInDomainOrder.toVector), Vector(
      Vector(0, 1),
      Vector(3),
      Vector(4, 5)
    ))

    fibers.indices.foreach: left =>
      fibers.indices.foreach: right =>
        if left != right then
          assert(fibers(left).intersect(fibers(right)).toOption.get.isEmpty)

    val union = fibers.foldLeft(Region.empty(ambient))((acc, fiber) => acc.union(fiber).toOption.get)
    assertEquals(union, parcellation.support)

  test("background remains None and quotient rows contain at most one parcel"):
    assertEquals(parcellation.parcelAt(ambient.point(2).get), None)
    assertEquals(parcellation.parcelAt(ambient.point(4).get).map(_.ordinal), Some(2))
    assertEquals(
      parcellation.quotientRelation.ordinalRows.map(_.toVector).toVector,
      Vector(Vector(0), Vector(0), Vector(), Vector(1), Vector(2), Vector(2))
    )

  test("construction rejects invalid and unused parcel ordinals"):
    assertEquals(
      Parcellation.fromAssignments(
        ambient,
        parcels,
        Vector(Some(0), Some(0), None, Some(1), None, None)
      ),
      Left(ParcellationError.UnusedParcel(2))
    )
    assertEquals(
      Parcellation.fromAssignments(
        ambient,
        parcels,
        Vector(Some(0), Some(0), None, Some(1), Some(3), Some(2))
      ),
      Left(ParcellationError.ParcelOutOfBounds(4, 3, 3))
    )

  test("partition equality is invariant to bijective relabeling"):
    val relabeledParcels =
      FiniteSpace.make[Q](SpaceKey.unsafe("partition:relabeled"), 3).toOption.get
    val relabeled =
      Parcellation.fromAssignments(
        ambient,
        relabeledParcels,
        Vector(Some(2), Some(2), None, Some(0), Some(1), Some(1))
      ).toOption.get
    val changed =
      Parcellation.fromAssignments(
        ambient,
        relabeledParcels,
        Vector(Some(2), Some(0), None, Some(0), Some(1), Some(1))
      ).toOption.get

    assert(parcellation.sameBlocksAs(relabeled).toOption.get)
    assert(!parcellation.sameBlocksAs(changed).toOption.get)
    assert(!parcellation.equals(relabeled))

  test("coarsening is functorial and coarsened fibers are source-fiber unions"):
    val networks =
      FiniteSpace.make[Q](SpaceKey.unsafe("partition:networks"), 2).toOption.get
    val systems =
      FiniteSpace.make[N](SpaceKey.unsafe("partition:systems"), 1).toOption.get
    val parcelToNetwork =
      Surjection.validate(
        TotalMap.fromTargetOrdinals(parcels, networks, Array(0, 0, 1)).toOption.get
      ).toOption.get
    val networkToSystem =
      Surjection.validate(
        TotalMap.fromTargetOrdinals(networks, systems, Array(0, 0)).toOption.get
      ).toOption.get
    val parcelToSystem =
      Surjection.validate(
        parcelToNetwork.mapping.andThen(networkToSystem.mapping).toOption.get
      ).toOption.get

    val stepwise =
      parcellation.coarsen(parcelToNetwork).toOption.get
        .coarsen(networkToSystem)
        .toOption
        .get
    val direct = parcellation.coarsen(parcelToSystem).toOption.get
    assertEquals(stepwise, direct)

    val networkPartition = parcellation.coarsen(parcelToNetwork).toOption.get
    val firstNetworkFiber = networkPartition.fiber(networks.point(0).get)
    val expected =
      parcellation.fiber(parcels.point(0).get)
        .union(parcellation.fiber(parcels.point(1).get))
        .toOption
        .get
    assertEquals(firstNetworkFiber, expected)

  test("identity coarsening preserves the label field"):
    val identity = Surjection.validate(TotalMap.identity(parcels)).toOption.get
    assertEquals(parcellation.coarsen(identity).toOption.get, parcellation)
