package scalafim.surface.reference

import scalafim.image.WorldPoint

import java.nio.charset.StandardCharsets
import java.util.zip.GZIPInputStream

/** The converted TemplateFlow `tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym`
  * point map against SimpleITK 2.5.6: forward on the templateflow4s oracle,
  * and the inverse on 5 000 fsLR 32k vertices. Skipped when the derived cache
  * directory is absent.
  */
class RealPointMapSuite extends munit.FunSuite, RealAssetGate:
  override def munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private def resource(name: String): ujson.Value =
    val in = new GZIPInputStream(getClass.getResourceAsStream(s"/pointmap-real/$name"))
    try ujson.read(new String(in.readAllBytes(), StandardCharsets.UTF_8)) finally in.close()

  private def requireAssets(): DeclaredPointMap =
    requireReal(RealAssets.pointMapPresent, RealAssets.pointMapDirectory.toString)
    RealAssets.pointMap

  test("the real point map is the digest-bound 2009c -> 6Asym composite: displacement then affine"):
    val map = requireAssets()
    assertEquals((map.input.value, map.output.value), ("MNI152NLin2009cAsym", "MNI152NLin6Asym"))
    assertEquals(map.catalogRevision.map(_.value), Some(RealAssets.catalogRevision))
    assertEquals(map.stageFiles.map(_.sha256.value), Vector("4e964918ad63dd1acd8cfbb0eebc2cdb8a33666ef3e58e56335cce67226c7c75"))
    assert(map.map.stages.head.isInstanceOf[PointMapStage.DisplacementStage])
    assert(map.map.stages(1).isInstanceOf[PointMapStage.AffineStage])

  test("forward evaluation agrees with SimpleITK on the templateflow4s oracle to 1e-6 mm"):
    val map = requireAssets()
    val oracle = resource("real_2009c_from_6asym_oracle.json.gz")
    assertEquals(oracle("source")("sha256").str, RealAssets.transformSha256)
    val points = oracle("points").arr.map(_.arr.map(_.num).toVector).toVector
    val mapped = oracle("mapped").arr.map(_.arr.map(_.num).toVector).toVector
    val out = new Array[Double](3)
    val scratch = new Array[Double](3)
    var worst = 0.0
    for (p, i) <- points.zipWithIndex do
      map.map.forwardInto(p(0), p(1), p(2), out, scratch)
      for c <- 0 until 3 do worst = math.max(worst, math.abs(out(c) - mapped(i)(c)))
    assert(worst <= 1e-6, s"max |Δ| = $worst mm over ${points.size} points")

  test("the inverse agrees with SimpleITK's 12-iteration fixed-point iterates on 5 000 fsLR vertices to 1e-6 mm"):
    // ITK has no inverse for DisplacementFieldTransform: the fixture is SimpleITK forward arithmetic under the same
    // fixed-point scheme. It validates forward arithmetic and convergence agreement, not an independent inverse method.
    val map = requireAssets()
    val oracle = resource("fslr-inverse-oracle.json.gz")
    assertEquals(oracle("schema").str, "scalafim.fslr-inverse-oracle/1")
    assertEquals(oracle("transformSha256").str, RealAssets.transformSha256)
    val tolerance = 1e-9
    // SimpleITK ran exactly 12 updates; stopping once the residual is <= 1e-9 mm is indistinguishable at 1e-6 mm.
    val matching = InversePolicy.make(tolerance, 12).toOption.get
    val production = InversePolicy.make(1e-6, 50).toOption.get
    var worst = 0.0
    var count = 0
    var statusDisagreements = 0
    var productionFailures = 0
    for h <- Vector("L", "R"); v <- oracle("hemispheres")(h)("vertices").arr do
      val x = v("input").arr.map(_.num)
      val expected = v("solution").arr.map(_.num)
      val oracleConverged = v("residualMm").num <= tolerance
      val point = WorldPoint(x(0), x(1), x(2))
      map.map.inverse(point, matching) match
        case PointMapOutcome.Converged(p, _, _) =>
          for c <- 0 until 3 do worst = math.max(worst, math.abs(p.toVector(c) - expected(c)))
          if !oracleConverged then statusDisagreements += 1
        case PointMapOutcome.NonConvergent(p, _, _) =>
          for c <- 0 until 3 do worst = math.max(worst, math.abs(p.toVector(c) - expected(c)))
          if oracleConverged then statusDisagreements += 1
        case other => fail(s"$h vertex ${v("vertex").num.toInt}: $other")
      if !map.map.inverse(point, production).isInstanceOf[PointMapOutcome.Converged] then productionFailures += 1
      count += 1
    assertEquals(count, 5000)
    assertEquals(statusDisagreements, 0, s"convergence status at $tolerance mm differs from SimpleITK's residual")
    assertEquals(productionFailures, 0, "production policy (1e-6 mm, 50 iterations) must converge at every vertex")
    assert(worst <= 1e-6, s"max |Δ| = $worst mm over $count vertices")
