package scalafim.atlas

import scalafim.atlas.syntax.*
import scalafim.image.*
import scalafim.surface.{
  Hemisphere as SurfaceHemisphere,
  LabelInfo,
  LabeledSurface,
  SurfaceGeometry,
  SurfaceKind,
  TriangleMesh,
  VertexId
}

class AtlasQuotientSuite extends munit.FunSuite:

  private val volumeRef =
    AtlasRef.volume(
      family = "quotient",
      model = "Volume",
      templateSpace = SpaceId.Custom,
      coordSpace = SpaceId.MNI152,
      confidence = Confidence.Exact
    )

  private def volumeAtlas(
      metadata: Vector[AtlasRegionMetadata] = Vector(
        Region(RegionId(2), "Second", network = Some(NetworkId("Visual"))),
        Region(RegionId(1), "First", network = Some(NetworkId("Visual")))
      ),
      space: NeuroSpace = NeuroSpace(Vector(2, 2, 1))
  ): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(
      volumeRef,
      RegionIndex(metadata),
      NeuroVol.fromLinear(
        NArrayUtil.fromArray(Array(1, 1, 2, 2)),
        space
      )
    )

  test("volume atlases expose quotient fibers, parcel metadata, and display order"):
    val atlas = volumeAtlas()
    val quotient = atlas.quotient
    val parcelTwo = quotient.parcelPoint(RegionId(2)).get
    val parcelOne = quotient.parcelPoint(RegionId(1)).get

    assertEquals(quotient.displayOrder.ordinals.toVector, Vector(0, 1))
    assertEquals(quotient.metadata(parcelTwo).label, "Second")
    assertEquals(quotient.metadata(parcelOne).label, "First")
    assertEquals(
      quotient.region(RegionId(1)).get.ordinalsInDomainOrder.toVector,
      Vector(0, 1)
    )
    assertEquals(
      quotient.region(RegionId(2)).get.ordinalsInDomainOrder.toVector,
      Vector(2, 3)
    )

  test("metadata changes do not change extensional region identity"):
    val original = volumeAtlas()
    val renamed =
      volumeAtlas(
        Vector(
          Region(RegionId(2), "Renamed second", network = Some(NetworkId("Visual"))),
          Region(RegionId(1), "Renamed first", network = Some(NetworkId("Visual")))
        )
      )

    Vector(RegionId(1), RegionId(2)).foreach: id =>
      val left = original.quotient.region(id).get
      val right = renamed.quotient.region(id).get
      assert(left.space.sameIdentityAs(right.space))
      assertEquals(
        left.ordinalsInDomainOrder.toVector,
        right.ordinalsInDomainOrder.toVector
      )

  test("network regions are fibers of the composed quotient"):
    val quotient = volumeAtlas().quotient
    val networks = quotient.networkParcellation.get

    assertEquals(networks.parcellation.parcels.size, 1)
    assertEquals(
      quotient
        .networkRegion(NetworkId("Visual"))
        .get
        .ordinalsInDomainOrder
        .toVector,
      Vector(0, 1, 2, 3)
    )

  test("partial network annotations do not claim a total surjection"):
    val atlas =
      volumeAtlas(
        Vector(
          Region(RegionId(2), "Second", network = Some(NetworkId("Visual"))),
          Region(RegionId(1), "First")
        )
      )

    assert(atlas.quotient.networkAssignment.isEmpty)

  test("surface atlases expose the same bilateral quotient operations"):
    val mesh =
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0)
        ),
        Vector((0, 1, 2))
      )
    val leftGeometry =
      SurfaceGeometry(mesh, SurfaceHemisphere.Left, SurfaceKind.Pial)
    val rightGeometry =
      SurfaceGeometry(mesh, SurfaceHemisphere.Right, SurfaceKind.Pial)
    val left =
      LabeledSurface.fromIndexed(
        leftGeometry,
        Vector(VertexId(0), VertexId(1), VertexId(2)),
        Vector(1, 1, 1),
        Vector(LabelInfo(1, "Left"))
      )
    val right =
      LabeledSurface.fromIndexed(
        rightGeometry,
        Vector(VertexId(0), VertexId(1), VertexId(2)),
        Vector(2, 2, 2),
        Vector(LabelInfo(2, "Right"))
      )
    val atlas =
      SurfaceAtlas.fromLabeledSurfaces(
        AtlasRef.surface(
          family = "quotient",
          model = "Surface",
          templateSpace = SpaceId.FsAverage,
          coordSpace = SpaceId.FsAverage,
          confidence = Confidence.Exact
        ),
        RegionIndex(
          Vector(
            Region(RegionId(1), "Left", hemisphere = Some(Hemisphere.Left)),
            Region(RegionId(2), "Right", hemisphere = Some(Hemisphere.Right))
          )
        ),
        left,
        right
      )
    val quotient = atlas.quotient

    assertEquals(quotient.parcellation.ambient.size, 6)
    assertEquals(
      quotient.region(RegionId(1)).get.ordinalsInDomainOrder.toVector,
      Vector(0, 1, 2)
    )
    assertEquals(
      quotient.region(RegionId(2)).get.ordinalsInDomainOrder.toVector,
      Vector(3, 4, 5)
    )
    assertEquals(
      quotient
        .metadata(quotient.parcelPoint(RegionId(2)).get)
        .hemisphere,
      Some(Hemisphere.Right)
    )

  test("overlap requires exact grids unless alignment is explicitly requested"):
    val reference = volumeAtlas()
    val shiftedSpace =
      NeuroSpace(
        dims = Vector(2, 2, 1),
        origin = Some(Vector(10.0, 0.0, 0.0))
      )
    val shifted = volumeAtlas(space = shiftedSpace)

    AtlasOverlap.computeEither(reference, shifted) match
      case Left(AtlasError.ExactGridRequired(_, _)) =>
        ()
      case other =>
        fail(s"expected exact-grid rejection, got $other")
    assert(
      AtlasOverlap
        .computeEither(
          reference,
          shifted,
          AtlasAlignment.NearestNeighborToFirst
        )
        .isRight
    )

  test("one-pass standard reducers retain NaN and non-contiguous id semantics"):
    val atlas = volumeAtlas()
    val data =
      NeuroVol.fromLinear(
        NArrayUtil.fromArray(Array(1.0, Double.NaN, 10.0, 20.0)),
        atlas.space
      )
    val means = atlas.reduce(data, Reducers.mean)
    val sums = atlas.reduce(data, Reducers.sum)

    assertEquals(means.value(RegionId(1)), Some(1.0))
    assertEquals(means.value(RegionId(2)), Some(15.0))
    assertEquals(sums.value(RegionId(1)), Some(1.0))
    assertEquals(sums.value(RegionId(2)), Some(30.0))
