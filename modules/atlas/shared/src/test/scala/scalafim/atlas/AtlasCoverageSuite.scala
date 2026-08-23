package scalafim.atlas

import ravel.Shape
import scalafim.image.*
import scalafim.atlas.syntax.*

class AtlasCoverageSuite extends munit.FunSuite:

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
      NeuroSpace(Vector(2, 2, 1)),
      PrimitiveBuffers.fromArray(values.toArray),
      "coverage"
    )

  private def atlas(values: Vector[Int], regions: RegionIndex = twoRegionIndex, space: NeuroSpace = NeuroSpace(Vector(2, 2, 1))): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(
      ref,
      regions,
      AtlasTestImages.labelVolume(
        space,
        PrimitiveBuffers.fromArray(values.toArray),
        "coverage"
      )
    )

  test("registry normalizes aliases and reports unknown ids with available atlases"):
    val spec = AtlasRegistry.default.find("Glasser 360 Surface").toOption.get
    assertEquals(spec.id, "glasser-surface")

    AtlasRegistry.default.find("not-a-real-atlas") match
      case Left(err @ AtlasError.UnknownAtlas(name, available)) =>
        assertEquals(name, "not-a-real-atlas")
        assert(available.contains("schaefer"), clue = available.mkString(","))
        assert(err.message.contains("available atlases: schaefer, glasser"), clue = err.message)
      case other =>
        fail(s"expected unknown atlas error, got $other")

    val thrown = intercept[NoSuchElementException]:
      AtlasRegistry.default("not-a-real-atlas")
    assert(thrown.getMessage.contains("unknown atlas 'not-a-real-atlas'"), clue = thrown.getMessage)

  test("region index normalizes labels, filters hemispheres, and rejects duplicate ids"):
    val index =
      RegionIndex(
        Vector(
          AtlasRegionMetadata(RegionId(1), "Left V1", labelFull = Some("Network/Left V1"), hemisphere = Some(Hemisphere.Left)),
          AtlasRegionMetadata(RegionId(2), "Left-V1", labelFull = Some("Network/Right V1"), hemisphere = Some(Hemisphere.Right)),
          AtlasRegionMetadata(RegionId(3), "Area 3", labelFull = Some("Full Area 3"), hemisphere = Some(Hemisphere.Bilateral))
        )
      )

    assertEquals(index.find("left v1").map(_.id), Vector(RegionId(1), RegionId(2)))
    assertEquals(index.find("network left v1").map(_.id), Vector(RegionId(1)))
    assertEquals(index.find("LEFT_V1", Some(Hemisphere.Left)).map(_.id), Vector(RegionId(1)))
    assertEquals(index.filter(_.hemisphere.contains(Hemisphere.Left)).ids, Vector(RegionId(1)))
    assertEquals(index.regions.head.typedLabel, RegionLabel.unsafe("Left V1"))
    assertEquals(index.regions.head.typedFullLabel, RegionLabel.unsafe("Network/Left V1"))
    assertEquals(
      AtlasRegionMetadata.checked(RegionId(10), "Area 10", attributes = Map("system" -> "visual")).map(_.typedAttributes.toMap),
      Right(Map("system" -> "visual"))
    )
    assertEquals(
      AtlasRegionMetadata.checked(RegionId(10), " ", attributes = Map.empty),
      Left(AtlasError.InvalidRegionMetadata("region label must be non-empty"))
    )

    val missing = intercept[NoSuchElementException]:
      index.requireRegion(RegionId(99))
    assertEquals(missing.getMessage, "atlas payload is missing region id 99")

    val duplicate = intercept[IllegalArgumentException]:
      RegionIndex(Vector(AtlasRegionMetadata(RegionId(1), "A"), AtlasRegionMetadata(RegionId(1), "B")))
    assert(duplicate.getMessage.contains("atlas region ids must be unique: 1"), clue = duplicate.getMessage)

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
    assertEquals(Reducers.mean(PrimitiveBuffers.fromArray(Array(1.0, Double.NaN, 5.0))), 3.0)
    assert(Reducers.mean(PrimitiveBuffers.fromArray(Array(Double.NaN, Double.NaN))).isNaN)
    assert(Reducers.mean(PrimitiveBuffers.fromArray(Array.empty[Double])).isNaN)
    assertEquals(Reducers.sum(PrimitiveBuffers.fromArray(Array(1.0, Double.NaN, 5.0))), 6.0)

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
        PrimitiveBuffers.fromArray(Array(1.0, 10.0, 3.0, 20.0))
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
        PrimitiveBuffers.fromArray(Array(1.0, 2.0, 10.0, 30.0, 3.0, 4.0, 20.0, 40.0)),
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
        PrimitiveBuffers.fromArray(Array(true, false, false, true)),
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

    val mismatched = atlas(Vector(1, 2), space = NeuroSpace(Vector(2, 1, 1)))
    assertEquals(
      AtlasOverlap.computeEither(a, mismatched, AtlasAlignment.Exact),
      Left(AtlasError.SpaceMismatch(Vector(2, 2, 1), Vector(2, 1, 1)))
    )
    val err =
      intercept[IllegalArgumentException]:
        AtlasOverlap.compute(a, mismatched, AtlasAlignment.Exact)
    assert(err.getMessage.contains("expected spatial dimensions 2x2x1 but got 2x1x1"), clue = err.getMessage)

  test("adjacency connectivity is monotone from faces to corners"):
    val space = NeuroSpace(Vector(2, 2, 2))
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

  test("atlas error messages and provenance helper aliases remain explicit"):
    val errors =
      Vector(
        AtlasError.EmptyAtlas -> "atlas must contain at least one region",
        AtlasError.DuplicateRegionIds(Vector(RegionId(1), RegionId(2))) -> "atlas region ids must be unique: 1, 2",
        AtlasError.MissingRegionId(RegionId(3)) -> "atlas payload is missing region id 3",
        AtlasError.UnknownAtlas("missing", Vector("schaefer", "glasser")) -> "unknown atlas 'missing'; available atlases: schaefer, glasser",
        AtlasError.UnknownSpace(SpaceId("bad")) -> "unknown space 'bad'",
        AtlasError.SpaceKindMismatch(SpaceId.FsAverage, SpaceKindTag.Volume, SpaceKindTag.Surface) -> "space 'fsaverage' has kind Surface; expected Volume",
        AtlasError.NoTransformRoute(SpaceId.Custom, SpaceId.MNI152) -> "no transform route found from 'custom' to 'MNI152'",
        AtlasError.TransformNotExecutable(SpaceId.FsAverage, SpaceId.FsLR32k, "non-affine") -> "transform route from 'fsaverage' to 'fsLR_32k' is not executable: non-affine",
        AtlasError.SpaceMismatch(Vector(2, 2, 1), Vector(2, 1, 1)) -> "expected spatial dimensions 2x2x1 but got 2x1x1",
        AtlasError.ExactGridRequired("expected-grid", "actual-grid") -> "exact atlas grid required; expected expected-grid but got actual-grid",
        AtlasError.InvalidQuery("bad query") -> "bad query",
        AtlasError.InvalidCoordinate("bad coordinate") -> "bad coordinate",
        AtlasError.InvalidRegionMetadata("bad metadata") -> "bad metadata"
      )
    errors.foreach { case (error, expected) => assertEquals(error.message, expected) }

    assertEquals(ArtifactRole.fromLegacy("volume"), ArtifactRole.ParcellationVolume)
    assertEquals(ArtifactRole.fromLegacy("annotation"), ArtifactRole.SurfaceAnnotation)
    assertEquals(ArtifactRole.fromLegacy("lut"), ArtifactRole.LabelTable)
    assertEquals(ArtifactRole.fromLegacy("xfm"), ArtifactRole.Transform)
    assertEquals(ArtifactRole.fromLegacy("docs"), ArtifactRole.Documentation)
    assertEquals(ArtifactRole.fromLegacy("something else"), ArtifactRole.Other)

    val schema = LabelTableSchema("toy labels", Vector("id", "label"))
    assertEquals(schema.columns, Vector("id", "label"))

    interceptMessage[IllegalArgumentException]("requirement failed: label table schema must contain columns"):
      LabelTableSchema("toy labels", Vector.empty)

    val textCitation = Citation(text = Some("A textual citation."))
    assertEquals(textCitation.text, Some("A textual citation."))

    interceptMessage[IllegalArgumentException]("requirement failed: citation requires DOI or text"):
      Citation()
