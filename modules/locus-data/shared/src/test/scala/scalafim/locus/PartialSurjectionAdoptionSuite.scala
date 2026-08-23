package scalafim.locus

import locus4s.{
  CertifiedMapError,
  DomainRegistry,
  PartialMapError,
  PartialSurjection,
  Surjection
}

class PartialSurjectionAdoptionSuite extends munit.FunSuite:
  private val ambientResolution =
    DomainFactory.unsafeRestore(
      DomainRegistry.empty,
      SpaceKey.unsafe("partition:ambient"),
      6
    )
  private type X = ambientResolution.S
  private val ambient: FiniteSpace[X] = ambientResolution.space

  private val parcelResolution =
    DomainFactory.unsafeRestore(
      ambientResolution.registry,
      SpaceKey.unsafe("partition:parcels"),
      3
    )
  private type P = parcelResolution.S
  private val parcels: FiniteSpace[P] = parcelResolution.space

  private val assignment =
    PartialSurjection
      .fromOptionalTargetOrdinals(
        ambient,
        parcels,
        Vector(Some(0), Some(0), None, Some(1), Some(2), Some(2))
      )
      .toOption
      .get

  test("fibers are disjoint, cover support, and every parcel is inhabited"):
    val fibers = assignment.fibers.toOption.get
    val regions = parcels.indices.map(fibers.row).toVector
    assertEquals(
      assignment.support.ordinalsInDomainOrder.toVector,
      Vector(0, 1, 3, 4, 5)
    )
    assertEquals(
      regions.map(_.ordinalsInDomainOrder.toVector),
      Vector(Vector(0, 1), Vector(3), Vector(4, 5))
    )

    regions.indices.foreach: left =>
      regions.indices.foreach: right =>
        if left != right then
          assert(regions(left).intersect(regions(right)).isEmpty)

    val union =
      regions.foldLeft(Region.empty(ambient))((acc, fiber) =>
        acc.union(fiber)
      )
    assertEquals(union, assignment.support)

  test("background remains None and the assignment relation has at most one target"):
    assertEquals(assignment(ambient.indexOption(2).get), None)
    assertEquals(
      assignment(ambient.indexOption(4).get).map(_.ordinal),
      Some(2)
    )
    assertEquals(
      assignment.toRelation.toOption.get.ordinalRows.map(_.toVector).toVector,
      Vector(Vector(0), Vector(0), Vector(), Vector(1), Vector(2), Vector(2))
    )

  test("upstream construction rejects wrong counts, invalid targets, and uncovered parcels"):
    assertEquals(
      PartialSurjection.fromOptionalTargetOrdinals(
        ambient,
        parcels,
        Vector(Some(0), Some(0))
      ),
      Left(PartialMapError.WrongTargetCount(6, 2))
    )
    assertEquals(
      PartialSurjection.fromOptionalTargetOrdinals(
        ambient,
        parcels,
        Vector(Some(0), Some(0), None, Some(1), None, None)
      ),
      Left(CertifiedMapError.NotSurjective(2))
    )
    assertEquals(
      PartialSurjection.fromOptionalTargetOrdinals(
        ambient,
        parcels,
        Vector(Some(0), Some(0), None, Some(1), Some(3), Some(2))
      ),
      Left(PartialMapError.TargetOutOfBounds(4, 3, 3))
    )

  test("checked typed construction rejects a same-sized foreign parcel owner"):
    val foreignResolution =
      DomainFactory.unsafeRestore(
        parcelResolution.registry,
        SpaceKey.unsafe("partition:foreign-parcels"),
        3
      )
    val foreignParcels = foreignResolution.space
    val foreignTargets =
      Vector(
        Some(foreignParcels.indexOption(0).get),
        Some(foreignParcels.indexOption(0).get),
        None,
        Some(foreignParcels.indexOption(1).get),
        Some(foreignParcels.indexOption(2).get),
        Some(foreignParcels.indexOption(2).get)
      )

    assert(
      PartialSurjection
        .fromOptionalTargetsChecked(
          ambient,
          parcels,
          foreignParcels,
          foreignTargets
        )
        .isLeft
    )

  test("partition equality is checked on source identity and ignores target relabeling"):
    val relabeledResolution =
      DomainFactory.unsafeRestore(
        parcelResolution.registry,
        SpaceKey.unsafe("partition:relabeled"),
        3
      )
    type Q = relabeledResolution.S
    val relabeledParcels: FiniteSpace[Q] = relabeledResolution.space
    val relabeled =
      PartialSurjection
        .fromOptionalTargetOrdinals(
          ambient,
          relabeledParcels,
          Vector(Some(2), Some(2), None, Some(0), Some(1), Some(1))
        )
        .toOption
        .get
    val changed =
      PartialSurjection
        .fromOptionalTargetOrdinals(
          ambient,
          relabeledParcels,
          Vector(Some(2), Some(0), None, Some(0), Some(1), Some(1))
        )
        .toOption
        .get

    assert(assignment.equivalentUpToTargetRelabelingChecked(relabeled).toOption.get)
    assert(!assignment.equivalentUpToTargetRelabelingChecked(changed).toOption.get)
    assert(!assignment.equals(relabeled))

    val foreignResolution =
      DomainFactory.unsafeRestore(
        relabeledResolution.registry,
        SpaceKey.unsafe("partition:foreign-ambient"),
        6
      )
    val foreign =
      PartialSurjection
        .fromOptionalTargetOrdinals(
          foreignResolution.space,
          relabeledParcels,
          Vector(Some(2), Some(2), None, Some(0), Some(1), Some(1))
        )
        .toOption
        .get
    assert(assignment.equivalentUpToTargetRelabelingChecked(foreign).isLeft)

  test("coarsening is functorial and coarsened fibers are source-fiber unions"):
    val networkResolution =
      DomainFactory.unsafeRestore(
        parcelResolution.registry,
        SpaceKey.unsafe("partition:networks"),
        2
      )
    type Q = networkResolution.S
    val networks: FiniteSpace[Q] = networkResolution.space
    val systemResolution =
      DomainFactory.unsafeRestore(
        networkResolution.registry,
        SpaceKey.unsafe("partition:systems"),
        1
      )
    type N = systemResolution.S
    val systems: FiniteSpace[N] = systemResolution.space
    val parcelToNetwork =
      Surjection
        .fromTargetOrdinals(parcels, networks, Array(0, 0, 1))
        .toOption
        .get
    val networkToSystem =
      Surjection
        .fromTargetOrdinals(networks, systems, Array(0, 0))
        .toOption
        .get
    val parcelToSystem = parcelToNetwork.andThen(networkToSystem)

    val stepwise = assignment.andThen(parcelToNetwork).andThen(networkToSystem)
    val direct = assignment.andThen(parcelToSystem)
    assertEquals(stepwise, direct)

    val networkAssignment = assignment.andThen(parcelToNetwork)
    val firstNetworkFiber = networkAssignment.fiber(networks.indexOption(0).get)
    val expected =
      assignment.fiber(parcels.indexOption(0).get)
        .union(assignment.fiber(parcels.indexOption(1).get))
    assertEquals(firstNetworkFiber, expected)

  test("identity coarsening preserves the assignment"):
    val identity =
      Surjection
        .fromTargetOrdinals(parcels, parcels, Array(0, 1, 2))
        .toOption
        .get
    assertEquals(assignment.andThen(identity), assignment)
