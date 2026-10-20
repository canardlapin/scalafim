package scalafim.atlas

import image4s.SomeSampleSpace

import neuroatlas4s.*

import image4s.geometry.GeometryError
import ravel.Shape
import neuroim4s.image.*
import neuroatlas4s.syntax.*

class AtlasCoverageSuite extends munit.FunSuite:

  private val fixtureWorld = neuroim4s.spaces.WorldSpace.declare("atlas fixture shared world")
    .fold(error => fail(error.message), identity)

  private def fixtureSpace(
      dims: Vector[Int],
      origin: Option[Vector[Double]] = None,
      affine: Option[image4s.geometry.Affine[image4s.geometry.D3]] = None
  ): SomeSampleSpace =
    SampleSpaces.inWorld(SampleSpaces(dims, origin = origin, affine = affine), fixtureWorld)
      .fold(error => fail(error.message), identity)

  private def ref: VolumeAtlasRef =
    AtlasRef.volume(
      family = "toy",
      model = "CoverageAtlas",
      templateSpace = SpaceId.Custom,
      coordSpace = SpaceId.MNI152,
      confidence = Confidence.Exact
    )

  private def twoRegionIndex: RegionIndex =
    RegionIndex(
      Vector(
        AtlasRegionMetadata(RegionId(1), "Left V1", labelFull = Some("Network/Left V1"), hemisphere = Some(Hemisphere.Left)),
        AtlasRegionMetadata(RegionId(2), "Right V1", labelFull = Some("Network/Right V1"), hemisphere = Some(Hemisphere.Right))
      )
    )

  private def labelVolume(values: Vector[Int]): SomeLabelVolume[Int] =
    AtlasTestImages.labelVolume(
      fixtureSpace(Vector(2, 2, 1)),
      (values.toArray).clone(),
      "coverage"
    )

  private def atlas(values: Vector[Int], regions: RegionIndex = twoRegionIndex, space: SomeSampleSpace = fixtureSpace(Vector(2, 2, 1))): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(
      ref,
      regions,
      AtlasTestImages.labelVolume(
        space,
        (values.toArray).clone(),
        "coverage"
      )
    )

  test("ScalaFIM provider failures keep their messages"):
    val gridMismatch = GeometryError.GridsNotCongruent(0.0)
    assertEquals(AtlasError.Provider(GeometryFailure(gridMismatch)).message, gridMismatch.message)
    val graph = scalafim.spatial.SpatialError.UnsupportedGeometry("no route")
    assertEquals(
      AtlasError.Provider(TransformGraphFailure(graph)).message,
      s"transform manifest does not form a valid spatial graph: ${graph.message}"
    )

  test("volume atlas construction rejects unknown and absent payload labels"):
    val regions = twoRegionIndex

    val unknown = intercept[IllegalArgumentException]:
      VolumeAtlas.fromLabelVolume(
        ref,
        regions,
        labelVolume(Vector(1, 2, 99, 0))
      )
    assert(unknown.getMessage.contains("atlas payload is missing region id 99"), clue = unknown.getMessage)

    val absent = intercept[IllegalArgumentException]:
      VolumeAtlas.fromLabelVolume(
        ref,
        regions,
        labelVolume(Vector(1, 1, 1, 0))
      )
    assert(absent.getMessage.contains("label volume is missing region ids: 2"), clue = absent.getMessage)

  test("space transforms expose no-route and non-affine execution limits"):
    assertEquals(SpaceId.kind(SpaceId.MNI152), SpaceKindTag.Volume)
    assertEquals(SpaceId.kind(SpaceId.FsAverage), SpaceKindTag.Surface)
    assertEquals(SpaceId.asVolume(SpaceId.MNI152), Right(SpaceId.MNI152))
    assertEquals(SpaceId.asSurface(SpaceId.FsAverage), Right(SpaceId.FsAverage))
    assertEquals(
      SpaceId.asVolume(SpaceId.FsAverage),
      Left(AtlasError.SpaceKindMismatch(SpaceId.FsAverage, SpaceKindTag.Volume, SpaceKindTag.Surface))
    )

    val noRoute = SpaceTransforms.plan(SpaceId.Custom, SpaceId.FsLR32k)
    assertEquals(noRoute, Left(AtlasError.NoTransformRoute(SpaceId.Custom, SpaceId.FsLR32k)))

    val vertexRoute = SpaceTransforms.plan(SpaceId.FsAverage, SpaceId.FsAverage5, DataKind.Vertex).toOption.get
    assertEquals(vertexRoute.status, TransformStatus.Available)
    assert(vertexRoute.warnings.exists(_.contains("Nearest-neighbor surface resampling")), clue = vertexRoute.warnings.mkString(";"))

    val nonAffine = SpaceTransforms.transformCoords(Vector(Point3D(0.0, 0.0, 0.0)), SpaceId.FsAverage, SpaceId.FsLR32k)
    nonAffine match
      case Left(AtlasError.TransformNotExecutable(from, to, reason)) =>
        assertEquals(from, SpaceId.FsAverage)
        assertEquals(to, SpaceId.FsLR32k)
        assert(reason.contains("unavailable steps") || reason.contains("non-affine steps"), clue = reason)
      case other =>
        fail(s"expected non-executable route, got $other")

  test("parcel data preserves atlas order and rejects invalid record contracts"):
    val a = atlas(Vector(1, 2, 1, 2))
    val data = ParcelData.fromValues(a, Vector("left", "right"))

    assertEquals(data.atlasRef, a.ref)
    assertEquals(data.schemaVersion, "1.0.0")
    assertEquals(data.values, Vector("left", "right"))
    assertEquals(data.regionIndex.labels, Vector("Left V1", "Right V1"))

    interceptMessage[IllegalArgumentException]("requirement failed: values length must match atlas region count"):
      ParcelData.fromValues(a, Vector("only one"))

    interceptMessage[IllegalArgumentException]("requirement failed: parcel data must contain at least one record"):
      ParcelData(a.ref, Vector.empty[ParcelRecord[Double]])

    interceptMessage[IllegalArgumentException]("requirement failed: schemaVersion must be non-empty"):
      ParcelData(a.ref, data.records, schemaVersion = " ")

    val duplicate =
      Vector(
        ParcelRecord(AtlasRegionMetadata(RegionId(1), "A"), 1.0),
        ParcelRecord(AtlasRegionMetadata(RegionId(1), "A duplicate"), 2.0)
      )
    val err =
      intercept[IllegalArgumentException]:
        ParcelData(a.ref, duplicate)
    assert(err.getMessage.contains("atlas region ids must be unique: 1"), clue = err.getMessage)

  test("reducers define NaN handling and volume reductions match grouped oracle sums"):
    assertEquals(Reducers.mean((Array(1.0, Double.NaN, 5.0)).clone()), 3.0)
    assert(Reducers.mean((Array(Double.NaN, Double.NaN)).clone()).isNaN)
    assert(Reducers.mean((Array.empty[Double]).clone()).isNaN)
    assertEquals(Reducers.sum((Array(1.0, Double.NaN, 5.0)).clone()), 6.0)

    val regions =
      RegionIndex(
        Vector(
          AtlasRegionMetadata(RegionId(2), "A", hemisphere = Some(Hemisphere.Left)),
          AtlasRegionMetadata(RegionId(5), "B", hemisphere = Some(Hemisphere.Right))
        )
      )
    val a = atlas(Vector(2, 5, 2, 5), regions)
    val data =
      AtlasTestImages.scalarVolume(
        a,
        (Array(1.0, 10.0, 3.0, 20.0)).clone()
      )
    val reduced = a.reduce(data, Reducers.sum)
    val checkedReduced = AtlasReduce.summarizeVolumeEither(a, data, Reducers.sum)

    assertEquals(reduced.value(RegionId(2)), Some(4.0))
    assertEquals(reduced.value(RegionId(5)), Some(30.0))
    assertEquals(checkedReduced.map(_.value(RegionId(2))), Right(Some(4.0)))

    val malformed =
      intercept[IllegalArgumentException]:
        ParcelValues(a, Vector(ParcelValue(regions.regions.head, 4.0)))
    assert(malformed.getMessage.contains("parcel values must cover atlas regions exactly"), clue = malformed.getMessage)

  test("vector reduction syntax overloads respect custom reducers and masks"):
    val a = atlas(Vector(1, 2, 1, 2))
    val tLen = 2
    val data =
      AtlasTestImages.scalarSeries(
        a,
        (Array(1.0, 2.0, 10.0, 30.0, 3.0, 4.0, 20.0, 40.0)).clone(),
        tLen,
        "timeseries"
      )
    val summed = a.reduce(data, Reducers.sum).data

    assertEquals(summed.shape, Shape(2, 2))
    assertEquals(summed(0, 0), 4.0)
    assertEquals(summed(0, 1), 6.0)
    assertEquals(summed(1, 0), 30.0)
    assertEquals(summed(1, 1), 70.0)

    val mask =
      AtlasTestImages.maskVolume(
        a,
        (Array(true, false, false, true)).clone(),
        "mask"
      )
    val masked = a.reduce(data, mask, Reducers.sum).data
    assertEquals(masked(0, 0), 1.0)
    assertEquals(masked(0, 1), 2.0)
    assertEquals(masked(1, 0), 20.0)
    assertEquals(masked(1, 1), 40.0)

  test("overlap is symmetric by atlas order and self-overlap is identity"):
    val a = atlas(Vector(1, 1, 2, 2))
    val b = atlas(Vector(1, 2, 2, 1))

    val ab = a.overlap(b)
    val ba = b.overlap(a)
    val abByPair = ab.map(o => (o.region1.id.value, o.region2.id.value) -> (o.dice, o.jaccard, o.nOverlap)).toMap
    val baByPair = ba.map(o => (o.region2.id.value, o.region1.id.value) -> (o.dice, o.jaccard, o.nOverlap)).toMap

    assertEquals(abByPair.keySet, baByPair.keySet)
    abByPair.foreach { case (pair, values) =>
      val reversed = baByPair(pair)
      assertEquals(values._3, reversed._3)
      assertEqualsDouble(values._1, reversed._1, 1e-12)
      assertEqualsDouble(values._2, reversed._2, 1e-12)
    }

    val self = a.overlap(a, AtlasAlignment.Exact)
    assert(self.forall(o => o.region1.id == o.region2.id), clue = self.toString)
    assert(self.forall(o => math.abs(o.dice - 1.0) < 1e-12), clue = self.toString)
    assert(self.forall(o => math.abs(o.jaccard - 1.0) < 1e-12), clue = self.toString)

    val mismatched = atlas(Vector(1, 2), space = fixtureSpace(Vector(2, 1, 1)))
    val cause = GeometryError.GridsNotCongruent(0.0)
    AtlasOverlap.computeEither(a, mismatched, AtlasAlignment.Exact) match
      case Left(AtlasError.Provider(GeometryFailure(GeometryError.GridsNotCongruent(tolerance)))) =>
        assertEqualsDouble(tolerance, 0.0, 0.0)
      case result =>
        fail(s"expected typed grid-congruence failure, got $result")
    val err =
      intercept[IllegalArgumentException]:
        AtlasOverlap.compute(a, mismatched, AtlasAlignment.Exact)
    assertEquals(err.getMessage, cause.message)

  test("adjacency connectivity is monotone from faces to corners"):
    val space = fixtureSpace(Vector(2, 2, 2))
    val values = Vector(1, 0, 0, 0, 0, 0, 0, 2)
    val a = atlas(values, space = space)

    assertEquals(a.adjacency(VoxelConnectivity.Connect6), Vector.empty)
    assertEquals(a.adjacency(VoxelConnectivity.Connect18), Vector.empty)

    val connect26 = a.adjacency(VoxelConnectivity.Connect26)
    assertEquals(connect26.map(e => (e.from.id, e.to.id, e.weight)), Vector((RegionId(1), RegionId(2), 1)))

  test("query contracts cover exact lookup, invalid radii, and transform failures"):
    val a = atlas(Vector(1, 2, 1, 2))
    val exact = AtlasQuery.exact(a, Point3D(0.0, 0.0, 0.0), fromSpace = SpaceId.MNI152)
    assertEquals(exact.pointIndex, 0)
    assertEquals(exact.id, Some(RegionId(1)))
    assertEquals(exact.distanceMm, Some(0.0))

    interceptMessage[IllegalArgumentException]("radiusMm must be finite and non-negative"):
      AtlasQuery.query(a, Vector(Point3D(0.0, 0.0, 0.0)), radiusMm = -1.0)

    assertEquals(
      AtlasQuery.queryEither(a, Vector(Point3D(0.0, 0.0, 0.0)), radiusMm = -1.0),
      Left(AtlasError.InvalidQuery("radiusMm must be finite and non-negative"))
    )

    interceptMessage[IllegalArgumentException]("radiusMm must be finite and non-negative"):
      AtlasQuery.query(a, Vector(Point3D(0.0, 0.0, 0.0)), radiusMm = Double.PositiveInfinity)

    val err =
      intercept[IllegalArgumentException]:
        AtlasQuery.query(a, Vector(Point3D(0.0, 0.0, 0.0)), fromSpace = SpaceId.FsLR32k)
    assert(err.getMessage.contains("no transform route found from 'fsLR_32k' to 'MNI152'"), clue = err.getMessage)
