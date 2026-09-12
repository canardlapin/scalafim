package scalafim.atlas

import scalafim.image.*

class AtlasBoundaryQuerySuite extends munit.FunSuite:
  private val spaceId = SpaceId("analytic-boundary-mm")
  private def atlas(dims: Vector[Int], labels: Array[Int], rows: Vector[Vector[Double]]): VolumeAtlas =
    val space = NeuroSpace(dims, trans = Some(DMat.fromRows(rows)))
    val regions = RegionIndex(Vector(AtlasRegionMetadata(RegionId(1), "same"), AtlasRegionMetadata(RegionId(2), "same")))
    VolumeAtlas.fromLabelVolume(AtlasRef("oracle", "cells", AtlasRepresentation.Volume, spaceId, spaceId),
      regions, NeuroVol.fromLinear(labels, space))
  private val diagonal = Vector(Vector(2.0,0,0,0),Vector(0.0,2,0,0),Vector(0.0,0,2,0),Vector(0.0,0,0,1))
  private def cube = atlas(Vector(1,1,1), Array(1), diagonal)
  private def query(a: VolumeAtlas, p: Point3D, radius: Double) =
    AtlasBoundaryQuery.queryEither(a,p,spaceId,radius).fold(e => fail(e.message),identity)

  test("one 2mm cube centre is 1mm from boundary, not zero from labelled centre"):
    val hit = query(cube,Point3D(0,0,0),2).hits.head
    assertEqualsDouble(hit.distanceMm,1,1e-12)
    assertEquals(hit.face,AtlasBoundaryFace(0,0,0,0,-1))
    assertEquals(hit.nearestPoint,Point3D(-1,0,0))
    assert(query(cube,Point3D(0,0,0),0.99).hits.isEmpty)

  test("edge and corner distances use closed-form Euclidean box oracle"):
    val edge = query(cube,Point3D(2,2,0),3).hits.head
    assertEqualsDouble(edge.distanceMm,math.sqrt(2),1e-12)
    assertEquals(edge.nearestPoint,Point3D(1,1,0))
    val corner = query(cube,Point3D(2,2,2),3).hits.head
    assertEqualsDouble(corner.distanceMm,math.sqrt(3),1e-12)
    assertEquals(corner.nearestPoint,Point3D(1,1,1))

  test("adjacent equal labels remove internal faces; distinct labels retain both zero-distance ties"):
    val same = atlas(Vector(2,1,1),Array(1,1),diagonal)
    assertEqualsDouble(query(same,Point3D(1,0,0),2).hits.head.distanceMm,1,1e-12)
    val different = atlas(Vector(2,1,1),Array(2,1),diagonal)
    val hits = query(different,Point3D(1,0,0),0).hits
    assertEquals(hits.map(_.region.id.value),Vector(1,2))
    hits.foreach(h => assertEqualsDouble(h.distanceMm,0,1e-12))
    assertEquals(hits.map(_.region.label),Vector("same","same"))

  test("rotation translation and anisotropic scale retain world-mm metric"):
    val rows = Vector(Vector(0.0,-3,0,10),Vector(2.0,0,0,20),Vector(0.0,0,4,30),Vector(0.0,0,0,1))
    val hit = query(atlas(Vector(1,1,1),Array(1),rows),Point3D(10,20,30),3).hits.head
    assertEqualsDouble(hit.distanceMm,1,1e-12)
    assertEquals(hit.nearestPoint,Point3D(10,19,30))

  test("shear uses distance to the oblique plane and complete inverse-affine search bounds"):
    val shear = Vector(Vector(2.0,2,0,0),Vector(0.0,2,0,0),Vector(0.0,0,2,0),Vector(0.0,0,0,1))
    val a = atlas(Vector(1,1,1),Array(1),shear)
    // Cell face x-y=-1: orthogonal projection of origin is(-1/2,1/2,0).
    val inside = query(a,Point3D(0,0,0),1).hits.head
    assertEqualsDouble(inside.distanceMm,1/math.sqrt(2),1e-12)
    assertEqualsDouble(inside.nearestPoint.x,-0.5,1e-12)
    assertEqualsDouble(inside.nearestPoint.y,0.5,1e-12)
    // Outside the voxel centre box, but within0.8mm of face x-y=1.
    val outside = query(a,Point3D(2,0,0),0.8).hits.head
    assertEqualsDouble(outside.distanceMm,1/math.sqrt(2),1e-12)
    assertEqualsDouble(outside.nearestPoint.x,1.5,1e-12)
    assertEqualsDouble(outside.nearestPoint.y,0.5,1e-12)

  test("no labels, outside radius, huge finite points and invalid requests are explicit"):
    assert(query(atlas(Vector(1,1,1),Array(0),diagonal),Point3D(0,0,0),2).hits.isEmpty)
    assert(query(cube,Point3D(4294967296.0,0,0),2).hits.isEmpty)
    assert(query(cube,Point3D(100,0,0),2).hits.isEmpty)
    assert(AtlasBoundaryQuery.queryEither(cube,Point3D(0,0,0),SpaceId("other"),2).isLeft)
    assert(AtlasBoundaryQuery.queryEither(cube,Point3D(0,0,0),spaceId,-1).isLeft)
    assert(AtlasBoundaryQuery.queryEither(cube,Point3D(0,0,0),spaceId,Double.NaN).isLeft)

  test("budget and cancellation refuse partial answers"):
    val a = atlas(Vector(3,3,3),Array.fill(27)(1),diagonal)
    assert(AtlasBoundaryQuery.queryEither(a,Point3D(2,2,2),spaceId,10,maxVisitedVoxels=26).isLeft)
    var checks = 0
    val result = AtlasBoundaryQuery.queryEither(a,Point3D(2,2,2),spaceId,10,cancelled=() =>
      checks += 1
      checks == 5)
    assert(result.left.exists(_.message.contains("cancelled")))
    assertEquals(checks,5)

  test("nearly collapsed affine faces are refused rather than inaccurately measured"):
    val rows = Vector(Vector(2.0,2,0,0),Vector(0.0,1e-7,0,0),Vector(0.0,0,2,0),Vector(0.0,0,0,1))
    assert(AtlasBoundaryQuery.queryEither(atlas(Vector(1,1,1),Array(1),rows),Point3D(0,0,0),spaceId,2).isLeft)
