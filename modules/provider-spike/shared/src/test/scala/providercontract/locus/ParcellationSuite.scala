package providercontract.locus

import locus4s.{CertifiedMapError, FiniteSpace, PartialMap, PartialMapError, PartialSurjection, Region, Surjection, TotalMap}
import locus4s.data.{Aggregation, VectorField}

class ParcellationSuite extends munit.FunSuite:
  private val ambientResolution =
    DomainFixtures.restore("partition:ambient", 6)
  private type X = ambientResolution.S
  private val ambient: FiniteSpace[X] = ambientResolution.space

  private val parcelResolution =
    DomainFixtures.restore("partition:parcels", 3)
  private type P = parcelResolution.S
  private val parcels: FiniteSpace[P] = parcelResolution.space

  private val parcellation =
    PartialSurjection.fromOptionalTargetOrdinals(
      ambient,
      parcels,
      Vector(Some(0), Some(0), None, Some(1), Some(2), Some(2))
    ).toOption.get

  private def lift[S, T](mapping: Surjection[S, T]): PartialSurjection[S, T] =
    PartialSurjection.tabulate(mapping.from, mapping.to)(point => Some(mapping(point))).toOption.get

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
    assertEquals(parcellation.apply(ambient.indexOption(2).get), None)
    assertEquals(parcellation.apply(ambient.indexOption(4).get).map(_.ordinal), Some(2))
    assertEquals(
      parcellation.toRelation.toOption.get.ordinalRows.map(_.toVector).toVector,
      Vector(Vector(0), Vector(0), Vector(), Vector(1), Vector(2), Vector(2))
    )

  test("construction rejects invalid and unused parcel ordinals"):
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

  test("partition equality is invariant to bijective relabeling"):
    val relabeledResolution =
      DomainFixtures.restore("partition:relabeled", 3)
    type Q = relabeledResolution.S
    val relabeledParcels: FiniteSpace[Q] = relabeledResolution.space
    val relabeled =
      PartialSurjection.fromOptionalTargetOrdinals(
        ambient,
        relabeledParcels,
        Vector(Some(2), Some(2), None, Some(0), Some(1), Some(1))
      ).toOption.get
    val changed =
      PartialSurjection.fromOptionalTargetOrdinals(
        ambient,
        relabeledParcels,
        Vector(Some(2), Some(0), None, Some(0), Some(1), Some(1))
      ).toOption.get

    assert(parcellation.equivalentUpToTargetRelabelingChecked(relabeled).toOption.get)
    assert(!parcellation.equivalentUpToTargetRelabelingChecked(changed).toOption.get)
    assert(!parcellation.equals(relabeled))

  test("coarsening is functorial and coarsened fibers are source-fiber unions"):
    val networkResolution =
      DomainFixtures.restore("partition:networks", 2)
    type Q = networkResolution.S
    val networks: FiniteSpace[Q] = networkResolution.space
    val systemResolution =
      DomainFixtures.restore("partition:systems", 1)
    type N = systemResolution.S
    val systems: FiniteSpace[N] = systemResolution.space
    val parcelToNetwork =
      Surjection.fromTotalMap(
        TotalMap.fromTargetOrdinals(parcels, networks, Array(0, 0, 1)).toOption.get
      ).toOption.get
    val networkToSystem =
      Surjection.fromTotalMap(
        TotalMap.fromTargetOrdinals(networks, systems, Array(0, 0)).toOption.get
      ).toOption.get
    val parcelToSystem =
      Surjection.fromTotalMap(
        parcelToNetwork.toTotalMap.andThen(networkToSystem.toTotalMap)
      ).toOption.get

    val stepwise =
      parcellation.andThen(parcelToNetwork).andThen(networkToSystem)
    val direct = parcellation.andThen(parcelToSystem)
    assertEquals(stepwise, direct)

    val networkPartition = parcellation.andThen(parcelToNetwork)
    val firstNetworkFiber = networkPartition.fiber(networks.indexOption(0).get)
    val expected =
      parcellation.fiber(parcels.indexOption(0).get)
        .union(parcellation.fiber(parcels.indexOption(1).get))
    assertEquals(firstNetworkFiber, expected)

  test("fromSurjection is total over its source and its fibers are preimages"):
    val networkResolution =
      DomainFixtures.restore("partition:surjection-networks", 2)
    type Q = networkResolution.S
    val networks: FiniteSpace[Q] = networkResolution.space
    val parcelToNetwork =
      Surjection.fromTotalMap(
        TotalMap.fromTargetOrdinals(parcels, networks, Array(1, 0, 1)).toOption.get
      ).toOption.get

    val quotient = lift(parcelToNetwork)
    assert(quotient.from.sameRuntimeOwnerAs(parcels))
    assert(quotient.to.sameRuntimeOwnerAs(networks))
    assertEquals(quotient.toPartialMap.optionalTargetOrdinals, Vector(Some(1), Some(0), Some(1)))
    assertEquals(quotient.support.ordinalsInDomainOrder.toVector, Vector(0, 1, 2))
    assertEquals(
      networks.indices.map(quotient.fiber(_).ordinalsInDomainOrder.toVector).toVector,
      Vector(Vector(1), Vector(0, 2))
    )

  test("aggregating through fromSurjection fuses with aggregating over coarsen"):
    val networkResolution =
      DomainFixtures.restore("partition:fusion-networks", 2)
    type Q = networkResolution.S
    val networks: FiniteSpace[Q] = networkResolution.space
    val parcelToNetwork =
      Surjection.fromTotalMap(
        TotalMap.fromTargetOrdinals(parcels, networks, Array(0, 0, 1)).toOption.get
      ).toOption.get
    def providerMapping[S, T](partition: PartialSurjection[S, T]): PartialMap[S, T] =
      PartialMap
        .fromOptionalTargetOrdinals(
          partition.from,
          partition.to,
          partition.toPartialMap.optionalTargetOrdinals
        )
        .toOption
        .get
    val field = VectorField.tabulate(ambient)(_.ordinal)

    val direct =
      Aggregation
        .foldMapByChecked(
          providerMapping(parcellation.andThen(parcelToNetwork)),
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
          providerMapping(lift(parcelToNetwork)),
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
    val identity = Surjection.fromTotalMap(TotalMap.identity(parcels)).toOption.get
    assertEquals(parcellation.andThen(identity), parcellation)
