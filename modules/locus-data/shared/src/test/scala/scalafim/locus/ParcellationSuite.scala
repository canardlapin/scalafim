package scalafim.locus

import locus4s.PartialMap
import locus4s.data.Aggregation

class ParcellationSuite extends munit.FunSuite:
  private val ambientResolution =
    DomainFactory.unsafeRestore(SpaceKey.unsafe("partition:ambient"), 6)
  private type X = ambientResolution.S
  private val ambient: FiniteSpace[X] = ambientResolution.space

  private val parcelResolution =
    DomainFactory.unsafeRestore(SpaceKey.unsafe("partition:parcels"), 3)
  private type P = parcelResolution.S
  private val parcels: FiniteSpace[P] = parcelResolution.space

  private val parcellation =
    Parcellation.fromAssignments(
      ambient,
      parcels,
      Vector(Some(0), Some(0), None, Some(1), Some(2), Some(2))
    ).toOption.get

  test("fibers are disjoint, cover support, and every parcel is inhabited"):
    val fibers = parcels.indices.map(parcellation.fiber).toVector
    assertEquals(parcellation.support.ordinalsInDomainOrder.toVector, Vector(0, 1, 3, 4, 5))
    assertEquals(fibers.map(_.ordinalsInDomainOrder.toVector), Vector(
      Vector(0, 1),
      Vector(3),
      Vector(4, 5)
    ))

    fibers.indices.foreach: left =>
      fibers.indices.foreach: right =>
        if left != right then
          assert(fibers(left).intersect(fibers(right)).isEmpty)

    val union =
      fibers.foldLeft(Region.empty(ambient))((acc, fiber) =>
        acc.union(fiber)
      )
    assertEquals(union, parcellation.support)

  test("background remains None and quotient rows contain at most one parcel"):
    assertEquals(parcellation.parcelAt(ambient.indexOption(2).get), None)
    assertEquals(parcellation.parcelAt(ambient.indexOption(4).get).map(_.ordinal), Some(2))
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
    val relabeledResolution =
      DomainFactory.unsafeRestore(SpaceKey.unsafe("partition:relabeled"), 3)
    type Q = relabeledResolution.S
    val relabeledParcels: FiniteSpace[Q] = relabeledResolution.space
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
    val networkResolution =
      DomainFactory.unsafeRestore(SpaceKey.unsafe("partition:networks"), 2)
    type Q = networkResolution.S
    val networks: FiniteSpace[Q] = networkResolution.space
    val systemResolution =
      DomainFactory.unsafeRestore(SpaceKey.unsafe("partition:systems"), 1)
    type N = systemResolution.S
    val systems: FiniteSpace[N] = systemResolution.space
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
        parcelToNetwork.mapping.andThen(networkToSystem.mapping)
      ).toOption.get

    val stepwise =
      parcellation.coarsen(parcelToNetwork).toOption.get
        .coarsen(networkToSystem)
        .toOption
        .get
    val direct = parcellation.coarsen(parcelToSystem).toOption.get
    assertEquals(stepwise, direct)

    val networkPartition = parcellation.coarsen(parcelToNetwork).toOption.get
    val firstNetworkFiber = networkPartition.fiber(networks.indexOption(0).get)
    val expected =
      parcellation.fiber(parcels.indexOption(0).get)
        .union(parcellation.fiber(parcels.indexOption(1).get))
    assertEquals(firstNetworkFiber, expected)

  test("fromSurjection is total over its source and its fibers are preimages"):
    val networkResolution =
      DomainFactory.unsafeRestore(SpaceKey.unsafe("partition:surjection-networks"), 2)
    type Q = networkResolution.S
    val networks: FiniteSpace[Q] = networkResolution.space
    val parcelToNetwork =
      Surjection.validate(
        TotalMap.fromTargetOrdinals(parcels, networks, Array(1, 0, 1)).toOption.get
      ).toOption.get

    val quotient = Parcellation.fromSurjection(parcelToNetwork)
    assert(quotient.ambient.sameIdentityAs(parcels))
    assert(quotient.parcels.sameIdentityAs(networks))
    assertEquals(quotient.assignmentOrdinals, Vector(Some(1), Some(0), Some(1)))
    assertEquals(quotient.support.ordinalsInDomainOrder.toVector, Vector(0, 1, 2))
    assertEquals(
      networks.indices.map(quotient.fiber(_).ordinalsInDomainOrder.toVector).toVector,
      Vector(Vector(1), Vector(0, 2))
    )

  test("aggregating through fromSurjection fuses with aggregating over coarsen"):
    val networkResolution =
      DomainFactory.unsafeRestore(SpaceKey.unsafe("partition:fusion-networks"), 2)
    type Q = networkResolution.S
    val networks: FiniteSpace[Q] = networkResolution.space
    val parcelToNetwork =
      Surjection.validate(
        TotalMap.fromTargetOrdinals(parcels, networks, Array(0, 0, 1)).toOption.get
      ).toOption.get
    def providerMapping[S, T](partition: Parcellation[S, T]): PartialMap[S, T] =
      PartialMap
        .fromOptionalTargetOrdinals(
          partition.ambient,
          partition.parcels,
          partition.assignmentOrdinals
        )
        .toOption
        .get
    val field = IndexedField.tabulate(ambient)(_.ordinal)

    val direct =
      Aggregation
        .foldMapByChecked(
          providerMapping(parcellation.coarsen(parcelToNetwork).toOption.get),
          field
        )(Set.empty[Int])(value => Set(value))(_ union _)
        .toOption
        .get
    val parcelValues =
      Aggregation
        .foldMapByChecked(providerMapping(parcellation), field)(
          Set.empty[Int]
        )(value => Set(value))(_ union _)
        .toOption
        .get
    val hierarchical =
      Aggregation
        .foldMapByChecked(
          providerMapping(Parcellation.fromSurjection(parcelToNetwork)),
          parcelValues
        )(Set.empty[Int])(identity)(_ union _)
        .toOption
        .get

    assertEquals(
      networks.indices.map(hierarchical.apply).toVector,
      Vector(Set(0, 1, 3), Set(4, 5))
    )
    networks.indices.foreach: network =>
      assertEquals(hierarchical(network), direct(network))

  test("identity coarsening preserves the label field"):
    val identity = Surjection.validate(TotalMap.identity(parcels)).toOption.get
    assertEquals(parcellation.coarsen(identity).toOption.get, parcellation)
