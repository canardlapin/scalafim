package scalafim.surface.reference

import scalafim.image.WorldPoint
import scalafim.surface.*

/** Real-asset disconfirmation test for the tpl-fsLR frame declaration: the
  * 2009c res-01 GM probability map, sampled by nearest voxel at the cortical
  * fsLR 32k midthickness vertices, under competing placements. Skipped when
  * the locked assets are absent.
  */
class RealFrameEvidenceSuite extends munit.FunSuite, RealAssetGate:
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")

  private val policy = InversePolicy.make(1e-6, 50).toOption.get

  private final case class Scores(sum: Double, above: Int, scored: Int, excluded: Int):
    def +(o: Scores) = Scores(sum + o.sum, above + o.above, scored + o.scored, excluded + o.excluded)
    def mean: Double = sum / scored
    def fraction: Double = above.toDouble / scored

  /** Nearest-voxel GM at placed cortical vertices, through the production sampling kernel. */
  private def score(h: RealAssets.Hemisphere32k, placed: Array[Option[WorldPoint]]): (Scores, Array[Double]) =
    val g = h.surface.geometry
    val raw = rawWorld(h)
    val coordinates = new Array[Double](3 * placed.length)
    for i <- placed.indices do
      val p = placed(i).getOrElse(raw(i))
      coordinates(3 * i) = p.x
      coordinates(3 * i + 1) = p.y
      coordinates(3 * i + 2) = p.z
    val mesh = SurfaceGeometry(TriangleMesh.fromArrays(coordinates, g.mesh.faceIndices.clone()), g.hemisphere, g.kind)
    val sampler = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(SurfaceGeometryPair(mesh, mesh), SurfaceSamplingPath.White,
      SurfaceSampleAggregation.Nearest))
    val result = sampler.sampleSelected(RealAssets.gm.volume, None, placed.map(_.nonEmpty))
    val values = Array.fill(placed.length)(Double.NaN)
    var s = Scores(0.0, 0, 0, 0)
    for i <- placed.indices if h.cortex(i) do
      if result.sampleCounts.valueAt(VertexId(i)).contains(1) then
        val v = result.values.valueAt(VertexId(i)).get
        values(i) = v
        s = s.copy(sum = s.sum + v, above = s.above + (if v > 0.5 then 1 else 0), scored = s.scored + 1)
      else s = s.copy(excluded = s.excluded + 1)
    (s, values)

  private def rawWorld(h: RealAssets.Hemisphere32k): Array[WorldPoint] =
    val g = h.surface.geometry
    Array.tabulate(g.vertexCount): i =>
      val p = g.mesh.vertex(VertexId(i))
      val w = g.surfaceToWorld(Vector(p.x, p.y, p.z)).toOption.get
      WorldPoint(w(0), w(1), w(2))

  test("tpl-fsLR in MNI152NLin6Asym survives the GM disconfirmation test; the production route matches"):
    requireReal(RealAssets.evidencePresent, s"locked assets under ${RealAssets.root}")
    val map = RealAssets.pointMap.map
    val axes = Vector(EvidenceAxis.X -> 0, EvidenceAxis.Y -> 1, EvidenceAxis.Z -> 2)
    val placements = Vector(Placement.Bridged, Placement.Raw, Placement.Reversed) ++
      (for (axis, _) <- axes; mm <- Vector(-3.0, 3.0) yield Placement.Shifted(axis, mm))
    var totals = placements.map(_ -> Scores(0.0, 0, 0, 0)).toMap
    var residuals = Vector.empty[Double]
    var nonConvergent = 0
    var perHemisphere = Vector.empty[(String, Double)]
    for h <- RealAssets.hemispheres do
      val raw = rawWorld(h)
      val inverse = raw.map(map.inverse(_, policy))
      val cortical = inverse.indices.filter(h.cortex).map(inverse)
      residuals ++= cortical.collect { case PointMapOutcome.Converged(_, r, _) => r }
      nonConvergent += cortical.count(o => !o.isInstanceOf[PointMapOutcome.Converged])
      val bridged = inverse.map(_.placed)
      def shifted(axis: Int, mm: Double) = bridged.map(_.map(p =>
        WorldPoint(p.x + (if axis == 0 then mm else 0.0), p.y + (if axis == 1 then mm else 0.0), p.z + (if axis == 2 then mm else 0.0))))
      val placed: Map[Placement, Array[Option[WorldPoint]]] = Map(
        Placement.Bridged -> bridged,
        Placement.Raw -> raw.map(p => Option(p)),
        Placement.Reversed -> raw.map(p => map.forward(p).placed)) ++
        (for (axis, index) <- axes; mm <- Vector(-3.0, 3.0) yield Placement.Shifted(axis, mm) -> shifted(index, mm))
      val scored = placed.map((p, points) => p -> score(h, points))
      totals = totals.map((p, s) => p -> (s + scored(p)._1))

      perHemisphere = perHemisphere :+ (h.label, scored(Placement.Bridged)._1.mean - scored(Placement.Raw)._1.mean)

      // The production route (inverse point-map bridge) maps exactly the bridged placement's values.
      val source = VolumeReference.make(RealAssets.nlin2009c, RealAssets.gm.volume.space).toOption.get
      val request = RouteRequest(source, StandardCorticalMesh.FsLR32k, h.reference.hemisphere, MappingMethod.MidthicknessNearest,
        ValueSemantics.Continuous)
      val anatomy = SamplingAnatomy.make(h.reference, AnatomicalGeometry.Midthickness(h.surface)).fold(e => fail(e.message), a => a)
      val bridge = FrameBridge.displacement(RealAssets.pointMap, PointMapUse.Inverse(policy)).toOption.get
      val route = SurfaceRoute.admit(request, anatomy, Some(bridge)).fold(r => fail(r.message), r => r)
      val mapped = route.map(RealAssets.gm).fold(e => fail(e.message), m => m)
      val expected = scored(Placement.Bridged)._2
      for i <- 0 until h.surface.geometry.vertexCount if h.cortex(i) do
        mapped.valueAt(VertexId(i)) match
          case Some(v) => assertEquals(v, expected(i), s"${h.label} vertex $i")
          case None => assert(expected(i).isNaN, s"${h.label} vertex $i")
      assertEquals(mapped.count(VertexCoverage.MedialWall), h.cortex.count(!_))

    // The plan's instrument scores all cortical vertices of both hemispheres together.
    val receipt = FrameEvidence.make(RealAssets.hemispheres.map(_.surface.declaration), RealAssets.gmAsset,
      FrameEvidenceThresholds.Default, placements.map { p =>
        val s = totals(p)
        PlacementScore.make(p, s.mean, s.fraction, s.scored, s.excluded).toOption.get
      }).toOption.get
    assertEquals(receipt.verdict, FrameEvidenceVerdict.Pass)

    val b = totals(Placement.Bridged)
    val r = totals(Placement.Raw)
    val rev = totals(Placement.Reversed)
    val sorted = residuals.sorted
    println(f"fsLR 32k GM evidence (cortex ${b.scored + b.excluded}): bridged ${b.mean}%.4f/${b.fraction}%.4f, " +
      f"raw ${r.mean}%.4f/${r.fraction}%.4f, reversed ${rev.mean}%.4f/${rev.fraction}%.4f; " +
      placements.collect { case p: Placement.Shifted => f"${p.name} ${totals(p).mean}%.4f" }.mkString(", ") +
      perHemisphere.map((h, g) => f"; $h bridged-raw ${g}%.4f").mkString +
      f"; inverse residual median ${sorted(sorted.size / 2)}%.2e max ${sorted.last}%.2e mm, not converged $nonConvergent")
    // Plan gates (c): bridged >= 0.69 / 0.78; raw and reversed below; measured by the reference script at
    // 0.703/0.794, 0.670/0.743 and 0.618/0.675.
    assert(b.mean >= 0.69 && b.fraction >= 0.78, s"bridged ${b.mean}/${b.fraction}")
    assert(r.mean < 0.69 && rev.mean < 0.69 && rev.fraction < 0.78, s"raw ${r.mean}, reversed ${rev.mean}/${rev.fraction}")
    for (value, expected) <- Vector(b.mean -> 0.703, b.fraction -> 0.794, r.mean -> 0.670, r.fraction -> 0.743,
        rev.mean -> 0.618, rev.fraction -> 0.675) do
      assertEqualsDouble(value, expected, 0.002)
    // Plan gate (b): every cortical vertex converges inside the field, then the residual budget.
    assertEquals(nonConvergent, 0, "cortical vertices not Converged (NonConvergent, OutsideSupport or NonFinite)")
    assertEquals(residuals.size, b.scored + b.excluded)
    assert(sorted(sorted.size / 2) <= 0.001 && sorted.last <= 0.01)
