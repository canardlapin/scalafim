package scalafim.surface

import scalafim.surface.fixtures.SurfaceTestFixtures

class SurfaceParcelsSuite extends munit.FunSuite:

  private val sheetTopology = SurfaceTestFixtures.sheetTopology
  private val sheetLabels = SurfaceTestFixtures.sheetLabels

  test("units returns contiguous parcels in label order"):
    val parcels = SurfaceParcels.units(sheetLabels, sheetTopology)

    assertEquals(parcels.map(_.label), Vector(1, 2))
    assertEquals(parcels.map(_.size), Vector(2, 2))
    assertEquals(parcels.head.info.map(_.name), Some("A"))

  test("fragmented parcel policies error, keep largest, split, or merge"):
    val topology = SurfaceTestFixtures.disconnectedTopology
    val labels = SurfaceTestFixtures.fragmentedLabels

    interceptMessage[IllegalArgumentException]("requirement failed: parcel label 9 is fragmented"):
      SurfaceParcels.units(labels, topology)

    val largest = SurfaceParcels.units(labels, topology, FragmentedParcelPolicy.Largest)
    assertEquals(largest.map(_.vertices), Vector(Vector(VertexId(0), VertexId(1))))

    val each = SurfaceParcels.units(labels, topology, FragmentedParcelPolicy.Each)
    assertEquals(each.map(_.key), Vector(ParcelKey(9, Some(1)), ParcelKey(9, Some(2))))
    assertEquals(each.map(_.size), Vector(2, 2))

    val merged = SurfaceParcels.units(labels, topology, FragmentedParcelPolicy.Merge)
    assertEquals(merged.length, 1)
    assertEquals(merged.head.vertices, Vector(VertexId(0), VertexId(1), VertexId(3), VertexId(4)))

  test("ParcelKey validates split parcel parts"):
    interceptMessage[IllegalArgumentException]("requirement failed: parcel part must be positive"):
      ParcelKey(1, Some(0))

    interceptMessage[IllegalArgumentException]("requirement failed: parcel label must be non-negative"):
      ParcelLabel(-1)

    val key = ParcelKey.typed(ParcelLabel(9), Some(ParcelPart(2)))
    assertEquals(key.label, 9)
    assertEquals(key.part, Some(2))
    assertEquals(key.display, "9.2")
    assert(ParcelKey.fromEither(1, Some(0)).isLeft)

  test("centroid and geodesic medoid can select different parcel representatives"):
    val topology = SurfaceTestFixtures.skewedCentroidTopology
    val parcel = SurfaceTestFixtures.skewedCentroidParcel

    assertEquals(SurfaceParcels.centroidVertex(topology, parcel), VertexId(1))
    assertEquals(SurfaceParcels.geodesicMedoidVertex(topology, parcel), VertexId(0))

  test("geodesic medoid rejects merged disconnected parcel"):
    val merged =
      SurfaceParcels.units(
        SurfaceTestFixtures.fragmentedLabels,
        SurfaceTestFixtures.disconnectedTopology,
        FragmentedParcelPolicy.Merge
      ).head

    interceptMessage[IllegalArgumentException]("requirement failed: parcel 9 contains unreachable vertices"):
      SurfaceParcels.geodesicMedoidVertex(SurfaceTestFixtures.disconnectedTopology, merged)

  test("distance matrices are symmetric for centroid and minimum methods"):
    val centroid =
      SurfaceParcels.distanceMatrix(
        sheetLabels,
        sheetTopology,
        method = ParcelDistanceMethod.Centroid,
        metric = DistanceMetric.Geodesic
      )
    val minimum =
      SurfaceParcels.distanceMatrix(
        sheetLabels,
        sheetTopology,
        method = ParcelDistanceMethod.Minimum,
        metric = DistanceMetric.Geodesic
      )

    assertEquals(centroid.size, 2)
    assertEqualsDouble(centroid(0, 1), centroid(1, 0), 1e-12)
    assertEqualsDouble(minimum(0, 1), minimum(1, 0), 1e-12)
    assertEqualsDouble(centroid(0, 0), 0.0, 1e-12)
    assertEqualsDouble(minimum(0, 0), 0.0, 1e-12)

  test("medoid distance matrix uses geodesic medoid representatives"):
    val topology = SurfaceTestFixtures.skewedCentroidTopology
    val parcelA = SurfaceTestFixtures.skewedCentroidParcel
    val parcelB = ParcelUnit(ParcelKey(2), Vector(VertexId(5)), Some(LabelInfo(2, "helper")))
    val matrix =
      SurfaceParcels.distanceMatrix(
        Vector(parcelA, parcelB),
        topology,
        method = ParcelDistanceMethod.Medoid,
        metric = DistanceMetric.Geodesic
      )

    assertEquals(matrix.parcels.map(_.key), Vector(ParcelKey(1), ParcelKey(2)))
    assertEqualsDouble(matrix(0, 1), 100.0, 1e-12)
    assertEqualsDouble(matrix(1, 0), 100.0, 1e-12)

  test("boundaryContacts counts shared mesh edges symmetrically"):
    val contacts = SurfaceParcels.boundaryContacts(sheetLabels, sheetTopology)

    assertEquals(contacts.size, 2)
    assertEquals(contacts.count(0, 1), 3)
    assertEquals(contacts.count(1, 0), 3)
    assertEquals(contacts.count(0, 0), 0)
    assert(contacts.touches(0, 1))

  test("ignoredLabels removes labels from parcel outputs and contacts"):
    val parcels = SurfaceParcels.units(sheetLabels, sheetTopology, ignoredLabels = Set(2))
    val contacts = SurfaceParcels.boundaryContacts(sheetLabels, sheetTopology, ignoredLabels = Set(2))

    assertEquals(parcels.map(_.label), Vector(1))
    assertEquals(contacts.size, 1)
    assertEquals(contacts.count(0, 0), 0)
