package scalafim.surface

import locus4s.DomainRegistry
import scalafim.locus.SpaceKey
import scalafim.surface.fixtures.SurfaceTestFixtures

class SurfaceLocusDomainSuite extends munit.FunSuite:

  private def domain(
      geometry: SurfaceGeometry = SurfaceTestFixtures.tetraGeometry
  ): SomeSurfaceLocusDomain =
    SurfaceLocusDomain
      .semantic(SpaceKey.unsafe("subject-01:left-cortex"), geometry)
      .toOption
      .get

  test("surface domain identity includes exact ordered topology"):
    val tetra = domain().value
    val equalSizedDifferentTopology =
      SurfaceField.full(
        SurfaceTestFixtures.sheetGeometry,
        Vector(1.0, 2.0, 3.0, 4.0)
      )

    assert(tetra.fullField(equalSizedDifferentTopology).isLeft)
    assert(tetra.optionalField(equalSizedDifferentTopology).isLeft)

  test("full and sparse surface fields preserve their distinct support semantics"):
    val d = domain().value
    val full =
      d.fullField:
        SurfaceField.full(
          SurfaceTestFixtures.tetraGeometry,
          Vector(10.0, 20.0, 30.0, 40.0)
        )
      .toOption
      .get
    val sparse =
      d.optionalField:
        SurfaceField.fromIndexed(
          SurfaceTestFixtures.tetraGeometry,
          Vector(VertexId(3), VertexId(1)),
          Vector(40.0, 20.0)
        )
      .toOption
      .get

    assertEquals(full(d.finiteSpace.indexOption(2).get), 30.0)
    assertEquals(sparse(d.finiteSpace.indexOption(0).get), None)
    assertEquals(sparse(d.finiteSpace.indexOption(1).get), Some(20.0))
    assertEquals(sparse(d.finiteSpace.indexOption(3).get), Some(40.0))

  test("legacy SurfaceRoi adapts to a region, restricted values, and annotation"):
    val d = domain().value
    val field =
      SurfaceField.full(
        SurfaceTestFixtures.tetraGeometry,
        Vector(1, 2, 3, 4)
      )
    val legacy =
      SurfaceRoi.fromField(
        field,
        Vector(VertexId(3), VertexId(1)),
        "motor"
      )
    val view = d.roi(legacy).toOption.get

    assertEquals(view.region.ordinalsInDomainOrder.toVector, Vector(1, 3))
    assertEquals(view.annotation, "motor")
    assertEquals(view.values(d.finiteSpace.indexOption(1).get).toOption, Some(Some(2)))
    assertEquals(view.values(d.finiteSpace.indexOption(0).get).toOption, None)

  test("labeled surfaces become partial surjections with metadata and explicit display order"):
    val d = domain(SurfaceTestFixtures.sheetGeometry).value
    val parcels = d.parcelAssignment(SurfaceTestFixtures.sheetLabels).toOption.get
    val firstParcel = parcels.assignment.to.indexOption(0).get
    val secondParcel = parcels.assignment.to.indexOption(1).get

    assertEquals(parcels.assignment.toPartialMap.optionalTargetOrdinals, Vector(Some(0), Some(0), Some(1), Some(1)))
    assertEquals(parcels.assignment.fiber(firstParcel).ordinalsInDomainOrder.toVector, Vector(0, 1))
    assertEquals(parcels.assignment.fiber(secondParcel).ordinalsInDomainOrder.toVector, Vector(2, 3))
    assertEquals(parcels.labelIds(firstParcel), 1)
    assertEquals(parcels.metadata(secondParcel).map(_.name), Some("B"))
    assertEquals(parcels.displayOrder.ordinals.toVector, Vector(0, 1))

  test("a disconnected label remains one valid extensional quotient fiber"):
    val geometry =
      SurfaceGeometry(
        SurfaceTestFixtures.disconnectedGeometry.mesh,
        Hemisphere.Left,
        SurfaceKind.Pial
      )
    val labels =
      LabeledSurface.fromIndexed(
        geometry,
        Vector(VertexId(0), VertexId(1), VertexId(3), VertexId(4)),
        Vector(9, 9, 9, 9),
        Vector(LabelInfo(9, "fragmented"))
      )
    val d = domain(geometry).value
    val parcels =
      d.parcelAssignment(labels).toOption.get
    val parcel = parcels.assignment.to.indexOption(0).get

    assertEquals(parcels.assignment.to.size, 1)
    assertEquals(
      parcels.assignment.fiber(parcel).ordinalsInDomainOrder.toVector,
      Vector(0, 1, 3, 4)
    )
    assertEquals(parcels.metadata(parcel).map(_.name), Some("fragmented"))

  test("runtime-loaded domains preserve the hidden semantic space type"):
    val packed =
      SomeSurfaceLocusDomain
        .semantic(
          SpaceKey.unsafe("runtime:left-cortex"),
          SurfaceTestFixtures.tetraGeometry
        )
        .toOption
        .get
    val space = packed.value.finiteSpace
    val field =
      packed.value
        .fullField(
          SurfaceField.full(
            SurfaceTestFixtures.tetraGeometry,
            Vector(1, 2, 3, 4)
          )
        )
        .toOption
        .get

    assertEquals(field(space.indexOption(3).get), 4)

  test("surface and parcel owners canonicalize only in an explicit registry scope"):
    val key = SpaceKey.unsafe("runtime:scoped-left-cortex")
    val first =
      SomeSurfaceLocusDomain
        .semanticIn(DomainRegistry.empty, key, SurfaceTestFixtures.sheetGeometry)
        .toOption
        .get
    val restored =
      SomeSurfaceLocusDomain
        .semanticIn(first.registry, key, SurfaceTestFixtures.sheetGeometry)
        .toOption
        .get
    val independent =
      SomeSurfaceLocusDomain
        .semantic(key, SurfaceTestFixtures.sheetGeometry)
        .toOption
        .get
    val parcels =
      first.value
        .parcelAssignment(SurfaceTestFixtures.sheetLabels)
        .toOption
        .get
    val restoredParcels =
      first.value
        .parcelAssignmentIn(
          parcels.registry,
          SurfaceTestFixtures.sheetLabels
        )
        .toOption
        .get

    assert(first.value.finiteSpace.sameRuntimeOwnerAs(restored.value.finiteSpace))
    assert(!first.value.finiteSpace.sameRuntimeOwnerAs(independent.value.finiteSpace))
    assert(
      parcels.assignment.to.sameRuntimeOwnerAs(restoredParcels.assignment.to)
    )
    assertEquals(parcels.registry.size, 2)
