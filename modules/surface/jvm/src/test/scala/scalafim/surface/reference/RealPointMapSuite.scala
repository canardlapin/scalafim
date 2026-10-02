package scalafim.surface.reference

import scalafim.image.WorldPoint

import java.nio.charset.StandardCharsets
import java.util.zip.GZIPInputStream

/** The converted TemplateFlow `tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym`
  * point map against SimpleITK 2.5.6: forward on the templateflow4s oracle,
  * and the explicit refusal of the unavailable inverse. Skipped when the derived cache
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
    var worst = 0.0
    for (p, i) <- points.zipWithIndex do
      map.map.forwardInto(p(0), p(1), p(2), out)
      for c <- 0 until 3 do worst = math.max(worst, math.abs(out(c) - mapped(i)(c)))
    assert(worst <= 1e-6, s"max |Δ| = $worst mm over ${points.size} points")

  test("the locked real map admits a pointwise inverse bridge from MNI152NLin6Asym to MNI152NLin2009cAsym"):
    val map = requireAssets()
    val bridge = FrameBridge.displacement(map, PointMapUse.Inverse(InversePolicy.Default)).fold(e => fail(e.message), identity)
    assertEquals((bridge.from.template.value, bridge.to.template.value), ("MNI152NLin6Asym", "MNI152NLin2009cAsym"))
    val origin = map.map.inverter(InversePolicy.Default).fold(e => fail(e.message), identity).place(WorldPoint(0.0, 0.0, 0.0))
    assert(origin.isInstanceOf[PointMapOutcome.Inverted], origin.toString)
