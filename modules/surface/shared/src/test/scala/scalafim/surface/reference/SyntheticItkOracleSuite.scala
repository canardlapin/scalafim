package scalafim.surface.reference

import scalafim.image.WorldPoint

import java.util.Base64

/** The canonical synthetic point map (oblique displacement grid plus affine)
  * against ITK's own `CompositeTransform::TransformPoint` on interior points,
  * voxel centres, the half-voxel border and points outside the field.
  */
class SyntheticItkOracleSuite extends munit.FunSuite:
  private val bytes = Base64.getDecoder.decode(SyntheticItkOracle.stageFile)
  private val declared = DeclaredPointMap.fromManifest(SyntheticItkOracle.manifest, SyntheticItkOracle.sourceSha256,
    name => Option.when(name == "stage-0-displacement.nii")(bytes)).fold(e => fail(e.message), d => d)

  test("the committed synthetic manifest verifies: digests, header and frames"):
    assertEquals((declared.input.value, declared.output.value), ("SynthIn", "SynthOut"))
    assertEquals(declared.stageFiles.size, 1)
    assertEquals(declared.catalogRevision, None)
    assertEquals(declared.map.stages.size, 2)

  test("forward evaluation agrees with SimpleITK to 1e-9 mm on every oracle point"):
    val out = new Array[Double](3)
    val scratch = new Array[Double](3)
    var worst = 0.0
    for (p, i) <- SyntheticItkOracle.points.zipWithIndex do
      declared.map.forwardInto(p(0), p(1), p(2), out, scratch)
      val expected = SyntheticItkOracle.mapped(i)
      for c <- 0 until 3 do worst = math.max(worst, math.abs(out(c) - expected(c)))
    assert(worst <= 1e-9, s"max |Δ| = $worst mm")

  test("each stage alone agrees with SimpleITK to 1e-9 mm"):
    def worst(map: PointMap, expected: Vector[Vector[Double]]): Double =
      val out = new Array[Double](3)
      val scratch = new Array[Double](3)
      SyntheticItkOracle.points.zipWithIndex.map { (p, i) =>
        map.forwardInto(p(0), p(1), p(2), out, scratch)
        (0 until 3).map(c => math.abs(out(c) - expected(i)(c))).max
      }.max
    val Vector(displacement, affine) = declared.map.stages
    val alone = (stage: PointMapStage) => PointMap.make(Vector(stage)).toOption.get
    val d = worst(alone(displacement), SyntheticItkOracle.displacementOnly)
    val a = worst(alone(affine), SyntheticItkOracle.affineOnly)
    assert(d <= 1e-9, s"displacement stage max |Δ| = $d mm")
    assert(a <= 1e-9, s"affine stage max |Δ| = $a mm")

  test("typed forward admits interior points and refuses points outside the field"):
    for (p, i) <- SyntheticItkOracle.points.zipWithIndex do
      val outcome = declared.map.forward(WorldPoint(p(0), p(1), p(2)))
      SyntheticItkOracle.pointClass(i) match
        case "interior" | "voxelCentre" => assert(outcome.placed.nonEmpty, s"point $i should be inside the field")
        // border: 0.02-0.48 voxel beyond an edge centre, still inside the half-voxel border.
        case "border" => assert(outcome.placed.nonEmpty, s"border point $i should be inside the support")
        case "outside" => assertEquals(outcome, PointMapOutcome.OutsideSupport, s"point $i")
        case other => fail(s"unexpected point class $other")
      outcome.placed.foreach: q =>
        for c <- 0 until 3 do assertEqualsDouble(q.toVector(c), SyntheticItkOracle.mapped(i)(c), 1e-9)
