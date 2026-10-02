package scalafim.surface.reference

import scalafim.image.*
import scalafim.surface.*
import scalafim.surface.io.GiftiSurfaceReader

import java.nio.file.Files

/** Frozen budgets P1-P3, P5 and P6 (JVM) for the MNI152NLin2009cAsym -> fsLR 32k route through the pointwise
  * inverse bridge, recorded on native ticket bd-01M3WCQD1MFW1WRTJP6C6A2ZFS before the bridge was built. Measured
  * through the production route on the locked GM volume, against the all-vertex SimpleITK oracle. P4 (consumer
  * fixtures) is measured by `FslrBridgeDisagreement` with specs supplied outside ScalaFIM.
  */
class RealPointwiseInverseSuite extends munit.FunSuite, RealAssetGate:
  import scala.concurrent.duration.*
  override val munitTimeout: Duration = 10.minutes

  private def distance(p: WorldPoint, q: WorldPoint): Double =
    math.sqrt(math.pow(p.x - q.x, 2) + math.pow(p.y - q.y, 2) + math.pow(p.z - q.z, 2))

  private def receipt(name: String, fields: (String, ujson.Value)*): Unit =
    println(s"[pointwise-receipt] ${ujson.write(ujson.Obj("check" -> name, fields*))}")

  test("P1-P3, P5, P6 (timing asserted with SCALAFIM_TIMING_BUDGETS=1): the pointwise route matches the oracle at every vertex and maps GM within budget"):
    requireReal(RealAssets.evidencePresent, s"locked assets under ${RealAssets.root}")
    val policy = InversePolicy.Default
    // P6 times admission and placement only: load and digest-check the locked assets first.
    val (pointMap, gm, hemispheres) = (RealAssets.pointMap, RealAssets.gm, RealAssets.hemispheres)
    val start = System.nanoTime()
    val bridge = FrameBridge.displacement(pointMap, PointMapUse.Inverse(policy)).fold(e => fail(e.message), identity)
    val source = VolumeReference.make(RealAssets.nlin2009c, gm.volume.space).fold(e => fail(e.message), identity)
    val routes = hemispheres.map: h =>
      val request = RouteRequest(source, StandardCorticalMesh.FsLR32k, h.reference.hemisphere,
        MappingMethod.MidthicknessNearest, ValueSemantics.Continuous)
      val anatomy = SamplingAnatomy.make(h.reference, AnatomicalGeometry.Midthickness(h.surface)).fold(e => fail(e.message), identity)
      val route = SurfaceRoute.admit(request, anatomy, Some(bridge)).fold(r => fail(r.message), identity)
      (h, route, route.map(gm).fold(e => fail(e.message), identity))
    val seconds = (System.nanoTime() - start) / 1e9
    val grids = Vector("res-01" -> (Vector(-96.0, -132.0, -78.0), 1.0), "res-02" -> (Vector(-96.5, -132.5, -78.5), 2.0))
    def voxel(p: WorldPoint, origin: Vector[Double], spacing: Double) =
      Vector(p.x, p.y, p.z).zip(origin).map((c, o) => math.floor((c - o) / spacing + 0.5).toInt)
    for (h, route, mapped) <- routes do
      val placement = route.bridgePlacement.getOrElse(fail("no bridge placement"))
      val oracle = RealInverseOracle.solutions(h.label)
      val outcomes = (0 until mapped.vertexCount).map(v => placement.outcomesAt(v).head)
      val inverted = outcomes.collect { case i: PointMapOutcome.Inverted => i }
      val summary = placement.inverseSummary.getOrElse(fail("no inverse summary"))
      // P1: every vertex converged within tolerance.
      assertEquals(inverted.length, 32492, s"${h.label}: ${summary.unplaced}")
      assert(inverted.forall(_.residualMm <= policy.toleranceMm), h.label)
      // P2 and P3: placements against the independent oracle.
      val errors = inverted.zip(oracle).map((i, o) => distance(i.point, o.world))
      val disagreements = grids.map: (name, grid) =>
        name -> inverted.zip(oracle).count((i, o) => voxel(i.point, grid._1, grid._2) != voxel(o.world, grid._1, grid._2))
      assert(errors.max <= 1e-6, s"${h.label}: placement vs oracle ${errors.max} mm exceeds the frozen 1e-6 mm")
      assertEquals(disagreements.map(_._2), Vector(0, 0), s"${h.label}: voxel disagreements $disagreements")
      // P5: exact medial wall, nothing unavailable, display identity on the locked inflated surfaces.
      for i <- 0 until mapped.vertexCount do
        val expected = if h.cortex(i) then VertexCoverage.Mapped else VertexCoverage.MedialWall
        assertEquals(mapped.coverageAt(VertexId(i)), Some(expected), s"${h.label} vertex $i")
      assertEquals(route.disclosure.bridgeExactness, BridgeExactness.Approximate(NumericalBridgeMethod.PointwiseFixedPoint(policy)))
      val locked = Map("L" -> Vector("1672da09e4eb112883fe76fe36e8167340e3dbcb9ba28d1f7b83b55acace635a",
          "8639333cc2823bdbb5b405764699a4344a3752e17ba43f8302370ee163864c6f"),
        "R" -> Vector("8237f4e759e88a64059dddc621104f318abd0346c45c1f7d5a88025862e104c7",
          "57d574b8cb70228564fd18772d7687b5213234648577db71c42482e90c4cb3c7"))
      for ((form, kind), sha) <- Vector("inflated" -> SurfaceKind.Inflated, "veryinflated" -> SurfaceKind.VeryInflated)
          .zip(locked(h.label)) do
        val path = RealAssets.root.resolve(s"tpl-fsLR/tpl-fsLR_den-32k_hemi-${h.label}_$form.surf.gii")
        assertEquals(AssetSha256.of(Files.readAllBytes(path)).value, sha, s"locked $form surface differs")
        val geometry = GiftiSurfaceReader.readEither(path, h.surface.geometry.hemisphere, kind).fold(e => fail(e.message), identity)
        val shown = mapped.onDisplay(DisplaySurface.make(h.reference, geometry).fold(e => fail(e.message), identity))
          .fold(e => fail(e.message), identity)
        assert(shown.mapped eq mapped, s"$form display must carry the same mapped values")
      receipt(s"P1-P5-${h.label}", "vertices" -> mapped.vertexCount, "converged" -> summary.converged,
        "unplaced" -> ujson.Obj.from(summary.unplaced.map((k, v) => k.label -> ujson.Num(v))),
        "worstResidualMm" -> summary.worstResidualMm.get, "worstIterations" -> summary.worstIterations.get,
        "placementVsOracleMaxMm" -> errors.max, "voxelDisagreements" -> ujson.Obj.from(disagreements.map((k, v) => k -> ujson.Num(v))),
        "cortical" -> mapped.count(VertexCoverage.Mapped), "medialWall" -> mapped.count(VertexCoverage.MedialWall))
    receipt("P6-JVM", "admitPlaceAndMapBothHemispheresSeconds" -> seconds)
    // Wall-clock budgets are asserted only on request: they are evidence runs, not ordinary unit tests on shared hosts.
    if sys.env.get("SCALAFIM_TIMING_BUDGETS").contains("1") then
      assert(seconds <= 30.0, s"admission + placement + GM mapping took $seconds s; frozen budget 30 s")
