package scalafim.atlas

import ravel.Shape
import scalafim.image.*
import scalafim.atlas.syntax.*

class AtlasCoreSuite extends munit.FunSuite:

  test("Point3D is an atlas alias for image SpatialPoint"):
    val point: SpatialPoint = Point3D(1.0, 2.0, 3.0)
    val atlasPoint: Point3D = SpatialPoint(1.0, 2.0, 3.0)

    assertEquals(point, atlasPoint)
    assertEquals(Point3D.fromVector(Vector(1.0, 2.0, 3.0)), point)

  private def toyAtlas(): VolumeAtlas =
    val sp = SampleSpaces(
      dims = Vector(5, 5, 5),
      spacing = Some(Vector(2.0, 2.0, 2.0)),
      origin = Some(Vector(0.0, 0.0, 0.0))
    )
    val labels = PrimitiveBuffers.fillConst[Int](sp.spatialDims.product, 0)

    def set(x: Int, y: Int, z: Int, id: Int): Unit =
      labels(Indexing.gridToIndex3D(sp.spatialDims, x, y, z)) = id

    for y <- 0 to 1; z <- 0 to 1 do set(0, y, z, 1)
    for y <- 0 to 1; z <- 0 to 1 do set(1, y, z, 2)
    set(4, 4, 4, 3)

    val regions =
      RegionIndex(
        Vector(
          AtlasRegionMetadata(RegionId(1), "RegionA", hemisphere = Some(Hemisphere.Left), network = Some(NetworkId("NetA"))),
          AtlasRegionMetadata(RegionId(2), "RegionB", hemisphere = Some(Hemisphere.Right), network = Some(NetworkId("NetA"))),
          AtlasRegionMetadata(RegionId(3), "RegionC", hemisphere = Some(Hemisphere.Left), network = Some(NetworkId("NetB")))
        )
      )

    val ref =
      AtlasRef.volume(
        family = "toy",
        model = "ToyAtlas",
        templateSpace = SpaceId.Custom,
        coordSpace = SpaceId.MNI152,
        confidence = Confidence.Exact
      )

    VolumeAtlas.fromLabelVolume(
      ref,
      regions,
      AtlasTestImages.labelVolume(sp, labels, label = "toy")
    )

  test("registry resolves standard atlas aliases") {
    val spec = AtlasRegistry.default("hcp-mmp")
    assertEquals(spec.id, "glasser")
    assertEquals(spec.defaultSpace, SpaceId.MNI152NLin2009cAsym)
    assert(AtlasRegistry.default.ids.contains("schaefer"), clue = "")

    val schaefer = Schaefer2018(SchaeferParcels.P400, YeoNetworks.Seventeen, VoxelResolution.TwoMm)
    val typedVolumeRef: VolumeAtlasRef = schaefer.atlasRef()
    assertEquals(schaefer.id, "schaefer-400-17-2mm")
    assertEquals(typedVolumeRef.templateSpace, SpaceId.MNI152NLin6Asym)

    val schaeferSurface: SurfaceAtlasRef = Schaefer2018Surface.default.atlasRef()
    assertEquals(schaeferSurface.representation, AtlasRepresentation.Surface)
    assertEquals(schaeferSurface.templateSpace, SpaceId.FsAverage6)
    assertEquals(schaeferSurface.density, Some("41k"))

    val glasserSurfaceSpec = AtlasRegistry.default("hcp-mmp-surface")
    assertEquals(glasserSurfaceSpec.id, "glasser-surface")
    assertEquals(glasserSurfaceSpec.defaultSpace, SpaceId.FsLR32k)
  }

  test("space transform plans include direct, identity, two-hop, and planned routes") {
    val direct = SpaceTransforms.plan(SpaceId.MNI305, SpaceId.MNI152).toOption.get
    assertEquals(direct.nSteps, 1)
    assertEquals(direct.confidence, Confidence.Exact)
    assertEquals(direct.status, TransformStatus.Available)
    assert(direct.executableCoordinatePlan.isRight, clue = direct.executableCoordinatePlan.toString)

    val same = SpaceTransforms.plan(SpaceId.MNI152, SpaceId.MNI152).toOption.get
    assertEquals(same.steps.head.kind, TransformKind.Identity)

    val twoHop = SpaceTransforms.plan(SpaceId.FsAverage5, SpaceId.FsAverage6).toOption.get
    assertEquals(twoHop.nSteps, 2)
    assertEquals(twoHop.steps.head.to, SpaceId.FsAverage)
    assertEquals(twoHop.steps(1).from, SpaceId.FsAverage)

    val planned = SpaceTransforms.plan(SpaceId.FsAverage, SpaceId.FsLR32k).toOption.get
    assertEquals(planned.status, TransformStatus.Planned)
    assertEquals(planned.isExecutable, false)
    assert(planned.warnings.exists(_.contains("planned")), clue = planned.warnings.mkString(";"))

    val volToSurf = VolumeSurfaceTransformPlan.volumeToSurface(SpaceId.MNI152NLin6Asym, SpaceId.FsAverage).toOption.get
    assertEquals(volToSurf.direction, VolumeSurfaceDirection.VolumeToSurface)
    assert(volToSurf.steps.exists(_.kind == TransformKind.VolToSurf), clue = volToSurf.steps.mkString(","))
    assertEquals(volToSurf.status, TransformStatus.Planned)

    val surfToVol = VolumeSurfaceTransformPlan.surfaceToVolume(SpaceId.FsAverage, SpaceId.MNI152NLin2009cAsym).toOption.get
    assertEquals(surfToVol.direction, VolumeSurfaceDirection.SurfaceToVolume)
    assert(surfToVol.steps.exists(_.kind == TransformKind.SurfToVol), clue = surfToVol.steps.mkString(","))

    val pt = SpaceTransforms.transformCoords(Vector(Point3D(10.0, -20.0, 35.0)), SpaceId.MNI305, SpaceId.MNI152).toOption.get.head
    assert(math.abs(pt.x - 10.694) < 0.01, clue = pt.toString)
    assert(math.abs(pt.y + 18.406) < 0.01, clue = pt.toString)
    assert(math.abs(pt.z - 36.139) < 0.01, clue = pt.toString)

    val morphism = SpaceTransforms.spatialMorphism(SpaceId.MNI305, SpaceId.MNI152).toOption.get
    val pulledBack = morphism.transform(pt.toVector)
    assertEqualsDouble(pulledBack(0), 10.0, 1e-10)
    assertEqualsDouble(pulledBack(1), -20.0, 1e-10)
    assertEqualsDouble(pulledBack(2), 35.0, 1e-10)
  }

  test("VolumeAtlas validates region ids and supports metadata subset") {
    val atlas = toyAtlas()
    assertEquals(atlas.regions.size, 3)
    assertEquals(atlas.region("RegionA").map(_.id), Vector(RegionId(1)))

    val netA = atlas.subset(_.network.contains(NetworkId("NetA")))
    assertEquals(netA.regions.ids, Vector(RegionId(1), RegionId(2)))
    assertEquals(netA.regions.ids.map(_.value), Vector(1, 2))
    assertEquals(netA.provenance.labels.regionIds, Vector(RegionId(1), RegionId(2)))
  }

  test("query exact and radius lookups return atlas metadata") {
    val atlas = toyAtlas()
    val exact = atlas.query(Point3D(0.0, 0.0, 0.0)).head
    assertEquals(exact.id, Some(RegionId(1)))
    assertEquals(exact.label, Some("RegionA"))

    val background = atlas.query(Point3D(4.0, 4.0, 4.0)).head
    assertEquals(background.region, None)

    val radiusHits = atlas.query(Point3D(2.0, 0.0, 0.0), radiusMm = 2.1)
    assertEquals(radiusHits.flatMap(_.id).toSet, Set(RegionId(1), RegionId(2)))
  }

  test("reduceVolume and reduceVec summarize parcel data") {
    val atlas = toyAtlas()
    val volData = PrimitiveBuffers.fillConst[Double](atlas.space.spatialDims.product, 0.0)
    var lin = 0
    val labelVol = atlas.labelVolume
    while lin < volData.length do
      volData(lin) =
        AtlasTestImages.labelAtCanonicalOrdinal(labelVol, lin).toDouble
      lin += 1
    val vol = AtlasTestImages.scalarVolume(atlas, volData)
    val values = atlas.reduce(vol)
    assertEquals(values.value(RegionId(1)), Some(1.0))
    assertEquals(values.value(RegionId(2)), Some(2.0))
    assertEquals(values.value(RegionId(3)), Some(3.0))

    val tLen = 3
    val vecData = PrimitiveBuffers.fillConst[Double](atlas.space.spatialDims.product * tLen, 0.0)
    var t = 0
    while t < tLen do
      lin = 0
      while lin < atlas.space.spatialDims.product do
        val id = AtlasTestImages.labelAtCanonicalOrdinal(labelVol, lin)
        vecData(lin * tLen + t) = id.toDouble * (t + 1).toDouble
        lin += 1
      t += 1
    val vec = AtlasTestImages.scalarSeries(atlas, vecData, tLen)
    val cvec = atlas.reduce(vec)
    assertEquals(cvec.data.shape, Shape(3, 3))
    assertEquals(cvec.data(0, 0), 1.0)
    assertEquals(cvec.data(0, 1), 2.0)
    assertEquals(cvec.data(1, 0), 2.0)
  }

  test("reduceVec preserves all parcels and writes NaN for parcels outside mask") {
    val atlas = toyAtlas()
    val tLen = 2
    val vecData = PrimitiveBuffers.fillConst[Double](atlas.space.spatialDims.product * tLen, 1.0)
    val vec = AtlasTestImages.scalarSeries(atlas, vecData, tLen)

    val maskFlags = PrimitiveBuffers.fillConst[Boolean](atlas.space.spatialDims.product, false)
    atlas.realization.region(RegionId(1)).get.foreachIndex: voxel =>
      maskFlags(voxel.ordinal) = true
    val mask = AtlasTestImages.maskVolume(atlas, maskFlags)
    val cvec = AtlasReduce.reduceSeries(atlas, vec, Some(mask))

    assertEquals(cvec.data.shape, Shape(3, 2))
    assertEquals(cvec.data(0, 0), 1.0)
    assert(cvec.data(1, 0).isNaN, clue = "region 2 should be NaN at t=0")
    assert(cvec.data(2, 0).isNaN, clue = "region 3 should be NaN at t=0")
  }

  test("overlap and adjacency compute region relationships") {
    val atlas = toyAtlas()
    val overlap = atlas.overlap(atlas, AtlasAlignment.Exact)
    val self = overlap.filter(o => o.region1.id == o.region2.id)
    assertEquals(self.length, 3)
    assert(self.forall(o => math.abs(o.dice - 1.0) < 1e-12), clue = "")

    val edges = atlas.adjacency(VoxelConnectivity.Connect6)
    assertEquals(edges.map(e => (e.from.id, e.to.id)), Vector((RegionId(1), RegionId(2))))
    assert(edges.head.weight > 0, clue = "")
  }
