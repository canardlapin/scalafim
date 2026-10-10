package scalafim.atlas.reference

import scalafim.atlas.{Fslr32kFrom2009c, StandardRoutePolicy, StandardRouteRefusal}
import scalafim.atlas.io.StandardSurfaceRouteFiles
import scalafim.image.*
import scalafim.surface.*
import scalafim.surface.io.GiftiSurfaceReader
import scalafim.surface.reference.*

import java.nio.file.Files

/** Frozen budgets P1-P3, P5 and P6 (JVM) for the MNI152NLin2009cAsym -> fsLR 32k route through the pointwise
  * inverse bridge, recorded on native ticket bd-01M3WCQD1MFW1WRTJP6C6A2ZFS before the bridge was built. Measured
  * through the public entry point (`StandardSurfaceRouteFiles.fsLR32kFrom2009c`, frozen policy) on the locked GM
  * volume, against the all-vertex SimpleITK oracle. P4 (consumer fixtures) is measured by `FslrBridgeDisagreement`
  * with specs supplied outside ScalaFIM.
  */
class Fslr32kRouteBudgetSuite extends munit.FunSuite, RealAssetGate:
  import scala.concurrent.duration.*
  override val munitTimeout: Duration = 10.minutes

  private def distance(p: WorldPoint, q: WorldPoint): Double =
    math.sqrt(math.pow(p.x - q.x, 2) + math.pow(p.y - q.y, 2) + math.pow(p.z - q.z, 2))

  private def receipt(name: String, fields: (String, ujson.Value)*): Unit =
    println(s"[pointwise-receipt] ${ujson.write(ujson.Obj("check" -> name, fields*))}")

  test("P1-P3, P5, P6 (timing asserted with SCALAFIM_TIMING_BUDGETS=1): the public route matches the oracle at every vertex and maps GM within budget"):
    requireReal(RealAssets.evidencePresent, s"locked assets under ${RealAssets.root}")
    val policy = InversePolicy.Default
    // P6 times admission and placement only: load and digest-check the locked assets first.
    val (route, gm) = (PublicFslrRoute.route, RealAssets.gm)
    assertEquals(route.policy, StandardRoutePolicy.Frozen)
    assertEquals(route.policy.inverse, policy)
    val start = System.nanoTime()
    val source = VolumeReference.make(Fslr32kFrom2009c.sourceFrame, gm.volume.space).fold(e => fail(e.message), identity)
    val routes = Fslr32kFrom2009c.hemispheres.map: lock =>
      val admitted = route.admit(source, lock.hemisphere).fold(r => fail(r.message), identity)
      (lock, admitted, admitted.map(gm).fold(e => fail(e.message), identity))
    val seconds = (System.nanoTime() - start) / 1e9
    val grids = Vector("res-01" -> (Vector(-96.0, -132.0, -78.0), 1.0), "res-02" -> (Vector(-96.5, -132.5, -78.5), 2.0))
    def voxel(p: WorldPoint, origin: Vector[Double], spacing: Double) =
      Vector(p.x, p.y, p.z).zip(origin).map((c, o) => math.floor((c - o) / spacing + 0.5).toInt)
    for (lock, admitted, mapped) <- routes do
      val label = PublicFslrRoute.label(lock.hemisphere)
      val wall = admitted.anatomy.reference.medialWall
      val placement = admitted.bridgePlacement.getOrElse(fail("no bridge placement"))
      val oracle = RealInverseOracle.solutions(label)
      val outcomes = (0 until mapped.vertexCount).map(v => placement.outcomesAt(v).head)
      val inverted = outcomes.collect { case i: PointMapOutcome.Inverted => i }
      val summary = placement.inverseSummary.getOrElse(fail("no inverse summary"))
      // P1: every vertex converged within tolerance.
      assertEquals(inverted.length, 32492, s"$label: ${summary.unplaced}")
      assert(inverted.forall(_.residualMm <= policy.toleranceMm), label)
      // P2 and P3: placements against the independent oracle.
      val errors = inverted.zip(oracle).map((i, o) => distance(i.point, o.world))
      val disagreements = grids.map: (name, grid) =>
        name -> inverted.zip(oracle).count((i, o) => voxel(i.point, grid._1, grid._2) != voxel(o.world, grid._1, grid._2))
      assert(errors.max <= 1e-6, s"$label: placement vs oracle ${errors.max} mm exceeds the frozen 1e-6 mm")
      assertEquals(disagreements.map(_._2), Vector(0, 0), s"$label: voxel disagreements $disagreements")
      // P5: exact medial wall, nothing unavailable, display identity on the locked inflated surfaces.
      for i <- 0 until mapped.vertexCount do
        val expected = if wall.cortexAt(VertexId(i)).contains(true) then VertexCoverage.Mapped else VertexCoverage.MedialWall
        assertEquals(mapped.coverageAt(VertexId(i)), Some(expected), s"$label vertex $i")
      assertEquals(admitted.disclosure.bridgeExactness, BridgeExactness.Approximate(NumericalBridgeMethod.PointwiseFixedPoint(policy)))
      val locked = Map("L" -> Vector("1672da09e4eb112883fe76fe36e8167340e3dbcb9ba28d1f7b83b55acace635a",
          "8639333cc2823bdbb5b405764699a4344a3752e17ba43f8302370ee163864c6f"),
        "R" -> Vector("8237f4e759e88a64059dddc621104f318abd0346c45c1f7d5a88025862e104c7",
          "57d574b8cb70228564fd18772d7687b5213234648577db71c42482e90c4cb3c7"))
      for ((form, kind), sha) <- Vector("inflated" -> SurfaceKind.Inflated, "veryinflated" -> SurfaceKind.VeryInflated)
          .zip(locked(label)) do
        val path = RealAssets.root.resolve(s"tpl-fsLR/tpl-fsLR_den-32k_hemi-${label}_$form.surf.gii")
        assertEquals(AssetSha256.of(Files.readAllBytes(path)).value, sha, s"locked $form surface differs")
        val geometry = GiftiSurfaceReader.readEither(path, lock.hemisphere.tag, kind).fold(e => fail(e.message), identity)
        val shown = mapped.onDisplay(DisplaySurface.make(admitted.anatomy.reference, geometry).fold(e => fail(e.message), identity))
          .fold(e => fail(e.message), identity)
        assert(shown.mapped eq mapped, s"$form display must carry the same mapped values")
      receipt(s"P1-P5-$label", "route" -> route.identity.token, "vertices" -> mapped.vertexCount,
        "converged" -> summary.converged,
        "unplaced" -> ujson.Obj.from(summary.unplaced.map((k, v) => k.label -> ujson.Num(v))),
        "worstResidualMm" -> summary.worstResidualMm.get, "worstIterations" -> summary.worstIterations.get,
        "placementVsOracleMaxMm" -> errors.max, "voxelDisagreements" -> ujson.Obj.from(disagreements.map((k, v) => k -> ujson.Num(v))),
        "cortical" -> mapped.count(VertexCoverage.Mapped), "medialWall" -> mapped.count(VertexCoverage.MedialWall))
    receipt("P6-JVM", "admitPlaceAndMapBothHemispheresSeconds" -> seconds)
    // Wall-clock budgets are asserted only on request: they are evidence runs, not ordinary unit tests on shared hosts.
    if sys.env.get("SCALAFIM_TIMING_BUDGETS").contains("1") then
      assert(seconds <= 30.0, s"admission + placement + GM mapping took $seconds s; frozen budget 30 s")

  test("the public route is the qualified wiring: same locked anatomy, publisher basis and bridge as the reference fixtures"):
    requireReal(PublicFslrRoute.present, s"locked route assets under ${RealAssets.root}")
    val route = PublicFslrRoute.route
    route.disclosure.anatomyBasis match
      case FrameBasis.PublisherMethods(doi, locator, registration, aggregate, _) =>
        assertEquals(doi, "10.1093/cercor/bhr291")
        assert(locator.contains("p. 2245"))
        assertEquals((registration, aggregate), (PublishedRegistration.Affine, PublishedAggregate.GroupAverage(69)))
      case other => fail(s"expected publisher methods, got $other")
    assertEquals(route.bridge.exactness,
      BridgeExactness.Approximate(NumericalBridgeMethod.PointwiseFixedPoint(InversePolicy.Default)))
    assertEquals(route.pointMap.source.sha256.value, RealAssets.transformSha256)
    assertEquals(route.pointMap.manifest.sha256.value, RealAssets.manifestSha256)
    for lock <- Fslr32kFrom2009c.hemispheres do
      val anatomy = route.anatomy(lock.hemisphere)
      val fixture = RealAssets.hemispheres.find(_.label == PublicFslrRoute.label(lock.hemisphere)).get
      assertEquals(anatomy.declarations, Vector(fixture.surface.declaration))
      assert(anatomy.reference.sameAs(fixture.reference), lock.hemisphere.code)
    // Loading again yields the same digest-bound identity.
    val again = StandardSurfaceRouteFiles.fsLR32kFrom2009c(roots = PublicFslrRoute.roots).fold(r => fail(r.message), identity)
    assertEquals(again.identity, route.identity)

  test("an override is disclosed, changes the identity, and is held to the placement gate"):
    requireReal(RealAssets.evidencePresent, s"locked assets under ${RealAssets.root}")
    val single = InversePolicy.make(1e-8, 1).fold(e => fail(e.message), identity)
    val policy = StandardRoutePolicy.overriding(single, "single-iteration probe").fold(r => fail(r.message), identity)
    val route = StandardSurfaceRouteFiles.fsLR32kFrom2009c(policy, PublicFslrRoute.roots).fold(r => fail(r.message), identity)
    assertNotEquals(route.identity, PublicFslrRoute.route.identity)
    assert(route.disclosure.policy.label.contains("single-iteration probe"))
    val source = VolumeReference.make(Fslr32kFrom2009c.sourceFrame, RealAssets.gm.volume.space).fold(e => fail(e.message), identity)
    val anatomy = route.anatomy(CorticalHemisphere.Left)
    // Evaluate placement independently of the gate, then require the gate to agree with it.
    val ungated = SurfaceRoute.admit(route.request(source, CorticalHemisphere.Left, ValueSemantics.Continuous), anatomy,
      Some(route.bridge)).fold(r => fail(r.message), identity)
    val placement = ungated.bridgePlacement.getOrElse(fail("no bridge placement"))
    val unplaced = (0 until anatomy.reference.vertexCount).count(v =>
      anatomy.reference.medialWall.cortexAt(VertexId(v)).contains(true) && !placement.isAvailable(v))
    route.admit(source, CorticalHemisphere.Left) match
      case Left(StandardRouteRefusal.PlacementGate(_, count, _)) => assertEquals(count, unplaced)
      case Left(other) => fail(other.message)
      case Right(_) => assertEquals(unplaced, 0)
