package scalafim.atlas

import scalafim.image.*

class AtlasBoundaryQuerySuite extends munit.FunSuite:
  private val spaceId = SpaceId("analytic-boundary-mm")
  private def atlas(dims: Vector[Int], labels: Array[Int], rows: Vector[Vector[Double]]): VolumeAtlas =
    val space = NeuroSpace(dims, trans = Some(DMat.fromRows(rows)))
    val regions = RegionIndex(labels.iterator.filter(_ != 0).toVector.distinct.sorted.map(id =>
      AtlasRegionMetadata(RegionId(id), "same")))
    VolumeAtlas.fromLabelVolume(AtlasRef("oracle", "cells", AtlasRepresentation.Volume, spaceId, spaceId),
      regions, NeuroVol.fromLinear(labels, space))
  private val diagonal = Vector(Vector(2.0,0,0,0),Vector(0.0,2,0,0),Vector(0.0,0,2,0),Vector(0.0,0,0,1))
  private def cube = atlas(Vector(1,1,1), Array(1), diagonal)
  private def query(a: VolumeAtlas, p: Point3D, radius: Double) =
    AtlasBoundaryQuery.queryEither(a,p,spaceId,radius).fold(e => fail(e.message),identity)

  test("one 2mm cube has a unique off-centre boundary witness"):
    val hit = query(cube,Point3D(0.1,0,0),2).hits.head
    assertEqualsDouble(hit.distanceMm,0.9,1e-12)
    assertEquals(hit.face,AtlasBoundaryFace(0,0,0,0,1))
    assertEquals(hit.nearestPoint,Point3D(1,0,0))
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
    assertEqualsDouble(query(same,Point3D(1,0.1,0),2).hits.head.distanceMm,0.9,1e-12)
    val different = atlas(Vector(2,1,1),Array(2,1),diagonal)
    val hits = query(different,Point3D(1,0,0),0).hits
    assertEquals(hits.map(_.region.id.value),Vector(1,2))
    hits.foreach(h => assertEqualsDouble(h.distanceMm,0,1e-12))
    assertEquals(hits.map(_.region.label),Vector("same","same"))

  test("rotation translation and anisotropic scale retain world-mm metric"):
    val rows = Vector(Vector(0.0,-3,0,10),Vector(2.0,0,0,20),Vector(0.0,0,4,30),Vector(0.0,0,0,1))
    val hit = query(atlas(Vector(1,1,1),Array(1),rows),Point3D(10,20.1,30),3).hits.head
    assertEqualsDouble(hit.distanceMm,0.9,1e-12)
    assertEquals(hit.nearestPoint,Point3D(10,21,30))

  test("shear uses distance to the oblique plane and complete inverse-affine search bounds"):
    val shear = Vector(Vector(2.0,2,0,0),Vector(0.0,2,0,0),Vector(0.0,0,2,0),Vector(0.0,0,0,1))
    val a = atlas(Vector(1,1,1),Array(1),shear)
    // Cell face x-y=1: projection of(0.1,0,0) is(0.55,-0.45,0).
    val inside = query(a,Point3D(0.1,0,0),1).hits.head
    assertEqualsDouble(inside.distanceMm,0.9/math.sqrt(2),1e-12)
    assertEqualsDouble(inside.nearestPoint.x,0.55,1e-12)
    assertEqualsDouble(inside.nearestPoint.y,-0.45,1e-12)
    // Outside the voxel centre box, but within0.8mm of face x-y=1.
    val outside = query(a,Point3D(2,0,0),0.8).hits.head
    assertEqualsDouble(outside.distanceMm,1/math.sqrt(2),1e-12)
    assertEqualsDouble(outside.nearestPoint.x,1.5,1e-12)
    assertEqualsDouble(outside.nearestPoint.y,0.5,1e-12)

  test("no labels, outside radius, huge finite points and invalid requests are explicit"):
    // The valid atlas contains a distant region; the complete queried neighborhood has no label.
    assert(query(atlas(Vector(5,1,1),Array(0,0,0,0,1),diagonal),Point3D(0,0,0),1).hits.isEmpty)
    assert(AtlasBoundaryQuery.queryEither(cube,Point3D(4294967296.0,0,0),spaceId,2)
      .left.exists(_.message == "Boundary distance arithmetic envelope exceeds local voxel resolution"))
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

  test("inclusive non-binary affine radius exposes its arithmetic envelope"):
    // Analytic right face is x=0.3+0.8/2=0.7. From x=1 the exact distance is0.3;
    // ordinary binary subtraction gives0.30000000000000004, one ulp above radius.
    val rows = Vector(Vector(0.8,0,0,0.3),Vector(0.0,2,0,0),Vector(0.0,0,2,0),Vector(0.0,0,0,1))
    val a = atlas(Vector(1,1,1),Array(1),rows)
    val included = query(a,Point3D(1,0,0),0.3)
    assertEquals(included.hits.size,1)
    assertEqualsDouble(included.hits.head.distanceMm,0.3,1e-15)
    assert(included.distanceErrorBoundMm > 0 && included.distanceErrorBoundMm < 1e-10)
    assertEquals(included.hits.head.radiusAdmission,AtlasBoundaryRadiusAdmission.NumericallyBorderline)
    val below = included.hits.head.distanceMm - 2*included.distanceErrorBoundMm
    assert(query(a,Point3D(1,0,0),below).hits.isEmpty)

  test("residual bound encloses a correct inverse and refuses an inaccurate one"):
    val a = DMat.fromRows(diagonal)
    val correct = AtlasBoundaryQuery.discoveryInverseBound(a,Affine3D(a).inverse).toOption.get
    assert(correct.residual >= 0 && correct.residual < 1e-12)
    assert(correct.errorNorm >= 0 && correct.errorNorm < 1e-12)
    assert(AtlasBoundaryQuery.discoveryInverseBound(a,DMat.eye(4)).isLeft)

  test("scale-aware envelope includes the independent 77-ulp plane-distance counterexample"):
    val u = Vector(2.9443676538450223,21.62245611412732,-32.485547046642765)
    val v = Vector(0.00029772776396990125,-2.8706306771289634e-05,0.00015056796535156377)
    val cross = Vector(u(1)*v(2)-u(2)*v(1),u(2)*v(0)-u(0)*v(2),u(0)*v(1)-u(1)*v(0))
    val length = math.sqrt(cross.map(x => x*x).sum)
    val normal = cross.map(_/length)
    // A long strip retains the tiny-v face while removing nearer same-label side
    // faces. Face(0,0,500,axis0,-1) has the oracle's u,v edges and origin0.
    val rows = Vector.tabulate(3)(r => Vector(2*normal(r),u(r),v(r),normal(r)+0.5*u(r)-499.5*v(r))) :+
      Vector(0.0,0,0,1)
    val a = atlas(Vector(1,1,1001),Array.fill(1001)(1),rows)
    val point = Point3D(0.7125110943167478,5.179380365005075,-7.82278772977632)
    // Decimal precision80 over exact input Double values, independent cross/dot oracle.
    val radius = 0.02334134377782575
    val result = query(a,point,radius)
    assertEquals(result.hits.size,1)
    val hit = result.hits.head
    assert(math.abs(hit.distanceMm-radius) <= hit.distanceErrorBoundMm)
    val witnessDistance = math.hypot(math.hypot(point.x-hit.nearestPoint.x,point.y-hit.nearestPoint.y),point.z-hit.nearestPoint.z)
    assert(math.abs(witnessDistance-hit.distanceMm) <= hit.distanceErrorBoundMm+hit.nearestPointErrorBoundMm)
    assert(hit.distanceErrorBoundMm < 1e-10)
    assert(query(a,point,hit.distanceMm-2*hit.distanceErrorBoundMm).hits.isEmpty)

  test("normal image geometry has negligible envelopes; unrepresentable placement refuses"):
    val ordinary = query(cube,Point3D(0.1,0,0),1)
    assert(ordinary.distanceErrorBoundMm < 1e-12)
    assert(ordinary.discovery.paddingVoxels < 1e-12)
    val remote = diagonal.updated(0,Vector(2.0,0,0,1e12))
    assert(AtlasBoundaryQuery.queryEither(atlas(Vector(1,1,1),Array(1),remote),Point3D(1e12,0,0),spaceId,1)
      .left.exists(_.message.contains("envelope")))

  test("inverse-dot cancellation cannot drop a translated exact-interface candidate"):
    // (1/0.3)*1024.15-(1/0.3)*1024 rounds above0.5 by thousands of its ulps.
    val rows = Vector(Vector(0.3,0.3,0,1024.0),Vector(0.0,0.3,0,1024.0),
      Vector(0.0,0,2,0),Vector(0.0,0,0,1))
    val result = query(atlas(Vector(1,1,1),Array(1),rows),Point3D(1024.15,1024,0),0)
    assertEquals(result.hits.size,1)
    assert(result.hits.head.distanceMm <= result.hits.head.distanceErrorBoundMm)
    assert(result.discovery.residualInfinityNorm < 1e-10)
    assert(result.discovery.paddingVoxels < 0.01)

  test("rounded corner-edge distance ambiguity refuses a false coordinate guarantee"):
    val a = atlas(Vector(1,1,1),Array(1),Vector(Vector(1.0,0,0,0),Vector(0.0,1,0,0),
      Vector(0.0,0,1,0),Vector(0.0,0,0,1)))
    val point = Point3D(-100,-100,-0.5+1e-6)
    val ambiguous = AtlasBoundaryQuery.queryEither(a,point,spaceId,141)
    assert(ambiguous.left.exists(_.message == "Boundary nearest-witness ambiguity exceeds local voxel resolution"))
    assert(math.abs(-0.5-point.z) > 1e-8, "The old corner witness exceeds the coordinate materiality budget")
    val separated = Point3D(-100,-100,-0.49)
    val hit = query(a,separated,141).hits.head
    // Cartesian box projection: clamp x,y to-0.5 and preserve the interior z.
    val coordinateError = math.hypot(math.hypot(hit.nearestPoint.x+0.5,hit.nearestPoint.y+0.5),
      hit.nearestPoint.z-separated.z)
    assert(coordinateError <= hit.nearestPointErrorBoundMm)
    assert(hit.nearestPointErrorBoundMm < 1e-8)
    assertEqualsDouble(hit.nearestPoint.z,separated.z,1e-12)

  test("exact symmetric nearest-face ambiguity is explicitly refused"):
    val result = AtlasBoundaryQuery.queryEither(cube,Point3D(0,0,0),spaceId,2)
    assert(result.left.exists(_.message == "Boundary nearest-witness ambiguity exceeds local voxel resolution"))
