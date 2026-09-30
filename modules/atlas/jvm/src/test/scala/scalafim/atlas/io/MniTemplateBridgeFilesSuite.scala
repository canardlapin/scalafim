package scalafim.atlas.io

import image4s.{BoundaryPolicy, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, D3, Point}
import ravel.NDArray
import scalafim.atlas.*
import scalafim.image.{GridSpec, SpatialDims}
import scalafim.image.world.Spaces as WorldFrames
import scalafim.surface.io.TemplateFlowCache

import java.nio.file.Path
import java.security.MessageDigest
import scala.io.Source

/** The TemplateFlow MNI152NLin6Asym -> MNI152NLin2009cAsym bridge against ITK itself.
  *
  * The composites (about 200 MB each) are never committed. `oracle/templateflow_mni_bridge` holds what
  * `tools/transform/generate_templateflow_mni_bridge_oracle.py` recorded from them with SimpleITK: TransformPoint at
  * 2009c brain-extent landmarks and interior lattice points, its fixed-point inverse at 6Asym points, the files'
  * SHA-256, and the image evidence of each file's direction. Tests that need a composite skip with an explicit
  * "asset missing" message when no local TemplateFlow cache holds it.
  */
class MniTemplateBridgeFilesSuite extends munit.FunSuite:
  // Materializing the composite on its 8.5M-point lattice and inverting it on 7.2M 6Asym points takes over a minute.
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

  private val Oracle = "/scalafim/atlas/oracle/templateflow_mni_bridge"

  /** SimpleITK and ScalaFIM evaluate the same affine and trilinear field in double precision. */
  private val PullTolerance = 1e-8

  /** Fixed before the inverse was first evaluated: the estimate interpolates inverse samples on the 1 mm 6Asym lattice. */
  private val PushTolerance = 0.05

  private final case class Row(key: String, input: Vector[Double], output: Vector[Double])

  private def ok[A](result: Either[AtlasError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def text(name: String): String =
    val stream = getClass.getResourceAsStream(s"$Oracle/$name")
    assert(stream != null, s"missing oracle file $name")
    try Source.fromInputStream(stream, "UTF-8").mkString
    finally stream.close()

  private def bytes(name: String): Array[Byte] =
    val stream = getClass.getResourceAsStream(s"$Oracle/$name")
    assert(stream != null, s"missing oracle file $name")
    try stream.readAllBytes()
    finally stream.close()

  private def table(name: String): Vector[Vector[String]] =
    text(name).linesIterator.drop(1).filter(_.nonEmpty).map(_.split('\t').toVector).toVector

  private def rows(name: String): Vector[Row] =
    table(name).map(cells => Row(cells.head, cells.slice(1, 4).map(_.toDouble), cells.slice(4, 7).map(_.toDouble)))

  private def sha256(content: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(content).map(b => f"${b & 0xff}%02x").mkString

  private def cached(xfm: TemplateFlowXfm): Path =
    val found = TemplateFlowCache.locate(xfm.relativePath)
    assume(found.nonEmpty, s"asset missing: ${xfm.relativePath} is in no TemplateFlow cache (${TemplateFlowCache.roots.mkString(", ")})")
    found.get

  /** Loaded once per suite run: reading, hashing and decoding the composite takes a few seconds. */
  private lazy val bridge: MniTemplateBridge = ok(MniTemplateBridgeFiles.load(cached(TemplateFlowXfm.Mni6ToMni2009c)))

  private lazy val inverted: MniTemplateBridge =
    val started = System.nanoTime()
    val result = ok(bridge.withNumericalInverse())
    println(f"numerical inverse on MNI152NLin6Asym res-01: ${(System.nanoTime() - started) / 1e9}%.1f s; ${result.transform.provenance.describe}")
    result

  private def distance(a: Vector[Double], b: Vector[Double]): Double =
    math.sqrt(a.zip(b).map((x, y) => (x - y) * (x - y)).sum)

  private def pull(on: MniTemplateBridge, ras: Vector[Double]): Vector[Double] =
    val point = Point.in(WorldFrames.MNI152NLin2009cAsym)(ras(0), ras(1), ras(2)).fold(error => fail(error.message), identity)
    on.transform.pullPoint(point).fold(error => fail(error.message), identity).coordinates

  private def push(on: MniTemplateBridge, ras: Vector[Double]): Vector[Double] =
    val point = Point.in(WorldFrames.MNI152NLin6Asym)(ras(0), ras(1), ras(2)).fold(error => fail(error.message), identity)
    on.transform.mapPoint(point).fold(error => fail(error.message), identity).coordinates

  private def point3(values: Vector[Double]): Point3D = Point3D(values(0), values(1), values(2))

  test("the committed oracle matches its manifest and pins the inspected composites"):
    val manifest = text("manifest.json")
    val recorded = "\"([a-z_]+\\.tsv)\":\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(manifest).map(m => m.group(1) -> m.group(2)).toVector
    assertEquals(recorded.map(_._1).sorted, Vector("direction.tsv", "pull_points.tsv", "push_points.tsv", "reverse_points.tsv", "xfm.tsv"))
    recorded.foreach((name, digest) => assertEquals(sha256(bytes(name)), digest, name))
    assert(manifest.contains("\"kind\": \"native-oracle\""))
    val files = table("xfm.tsv").map(cells => cells(0) -> cells).toMap
    TemplateFlowXfm.values.foreach: xfm =>
      val cells = files.getOrElse(xfm.fileName, fail(s"${xfm.fileName} missing from xfm.tsv"))
      assertEquals(cells(1), xfm.sha256, xfm.fileName)
      assertEquals(cells(2).toLong, xfm.bytes, xfm.fileName)
      assertEquals(cells(3), "CompositeTransform_double_3_3,AffineTransform_double_3_3,DisplacementFieldTransform_double_3_3")
      // both displacement lattices are MNI152NLin2009cAsym res-01 (LPS origin (96, 132, -78), axes -x, -y, +z)
      assertEquals(cells(4), "193.0 229.0 193.0 96.0 132.0 -78.0 1.0 1.0 1.0 -1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 1.0")

  test("the image evidence backs each composite's measured direction"):
    val evidence = table("direction.tsv").map(cells => (cells(0), cells(1)) -> (cells(2).toDouble, cells(3).toDouble)).toMap
    def score(transform: String, pull: TemplatePull): (Double, Double) =
      val hypothesis = pull match
        case TemplatePull.PointsFrom2009cTo6Asym => "pulls-2009c-to-6Asym"
        case TemplatePull.PointsFrom6AsymTo2009c => "pulls-6Asym-to-2009c"
      evidence((transform, hypothesis))
    TemplateFlowXfm.values.foreach: xfm =>
      val other = TemplatePull.values.filterNot(_ == xfm.measured).head
      val (measuredR, measuredDice) = score(xfm.fileName, xfm.measured)
      val (otherR, otherDice) = score(xfm.fileName, other)
      // each hypothesis is compared with doing nothing on the same target grid
      val (identityR, identityDice) = score("identity", xfm.measured)
      val (otherIdentityR, otherIdentityDice) = score("identity", other)
      // the measured direction improves on doing nothing; the other one is worse than doing nothing
      assert(measuredR > identityR + 0.01 && measuredDice > identityDice + 0.005, s"${xfm.fileName}: $measuredR, $measuredDice")
      assert(otherR < otherIdentityR - 0.02 && otherDice < otherIdentityDice - 0.02, s"${xfm.fileName}: $otherR, $otherDice")
    assert(TemplateFlowXfm.Mni6ToMni2009c.agreesWithName)
    assert(!TemplateFlowXfm.Mni2009cToMni6.agreesWithName, "TemplateFlow's reverse file pulls the same way as the forward one")

  test("the composite pulls MNI152NLin2009cAsym points to MNI152NLin6Asym exactly as ITK TransformPoint does"):
    val oracle = rows("pull_points.tsv")
    assertEquals(oracle.size, 23)
    assertEquals(bridge.asset.sha256, Some(TemplateFlowXfm.Mni6ToMni2009c.sha256))
    assert(!bridge.hasForwardMap, "a dense warp without an inverse has no forward map")
    oracle.foreach: row =>
      assert(distance(pull(bridge, row.input), row.output) <= PullTolerance, s"${row.key}: ${pull(bridge, row.input)} vs ${row.output}")
    val missing = Point.in(WorldFrames.MNI152NLin6Asym)(0.0, 0.0, 0.0).fold(error => fail(error.message), identity)
    assert(bridge.transform.mapPoint(missing).isLeft, "6Asym points have no 2009c image until an inverse is qualified")
    val outside = Point.in(WorldFrames.MNI152NLin2009cAsym)(0.0, 0.0, 130.0).fold(error => fail(error.message), identity)
    assert(bridge.transform.pullPoint(outside).isLeft, "points beyond the displacement lattice are rejected, not passed through")

  test("resampling 6Asym data onto a 2009c grid samples it through the composite"):
    // x (mm) on the 6Asym res-02 grid: trilinear interpolation of a linear function is exact, so every resampled value
    // is the x coordinate of the target point's pullback.
    val source = TemplateGrids.mni6Res2
    val image =
      Sampled
        .continuous(source.grid, NonSpatialAxes.empty, NDArray.tabulate[Double](91, 109, 91)((i, _, _) => -90.0 + 2.0 * i))
        .fold(error => fail(error.toString), identity)
    val affine = Affine
      .fromRowMajor[D3](Vector(12.0, 0.0, 0.0, -60.0, 0.0, 12.0, 0.0, -90.0, 0.0, 0.0, 12.0, -50.0, 0.0, 0.0, 0.0, 1.0))
      .fold(error => fail(error.message), identity)
    val target = GridSpec.in(WorldFrames.MNI152NLin2009cAsym)(SpatialDims(11, 14, 11), affine).fold(error => fail(error.message), identity)
    val resampled = bridge.transform.resample(image, target.grid, boundary = BoundaryPolicy.Reject).fold(error => fail(error.message), identity)
    for i <- 0 until 11; j <- 0 until 14; k <- 0 until 11 do
      val y = Vector(-60.0 + 12.0 * i, -90.0 + 12.0 * j, -50.0 + 12.0 * k)
      assertEqualsDouble(resampled.image.data.at(IArray(i, j, k)), pull(bridge, y)(0), 1e-9, s"target voxel ($i,$j,$k)")

  test("installed in the manifest, 6Asym -> 2009c is available and pulls 2009c points; it pushes nothing"):
    val registry = bridge.install(SpaceTransforms.manifest)
    assertEquals(registry.length, SpaceTransforms.manifest.length)
    val plan = ok(SpaceTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym, DataKind.Voxel, registry))
    assertEquals(plan.status, TransformStatus.Available)
    assertEquals(plan.steps.map(_.backend), Vector(TransformBackend.TemplateFlowAnts))
    assert(!plan.usedInverses)
    assert(plan.pullbackExecutability.isRight)
    plan.transform(Vector(Point3D.Origin)) match
      case Left(AtlasError.TransformNotExecutable(_, _, reason)) => assert(reason.contains("pullback only"), reason)
      case other                                                 => fail(s"expected a pullback-only refusal, got $other")
    val oracle = rows("pull_points.tsv")
    val pulled = ok(plan.pullPoints(oracle.map(row => point3(row.input))))
    oracle.zip(pulled).foreach: (row, point) =>
      assert(distance(point.toVector, row.output) <= PullTolerance, row.key)
    // no reverse route: TemplateFlow's reverse file is not an inverse, so the planned step stays typed non-executable
    val reverse = ok(SpaceTransforms.plan(SpaceId.MNI152NLin2009cAsym, SpaceId.MNI152NLin6Asym, DataKind.Voxel, registry))
    assertEquals(reverse.status, TransformStatus.Planned)
    assert(reverse.steps.head.notes.exists(_.contains("not an inverse")), reverse.steps.head.notes.toString)
    assert(reverse.pullbackExecutability.isLeft)

  test("TemplateFlow's reverse composite is refused with its measured direction"):
    MniTemplateBridgeFiles.load(cached(TemplateFlowXfm.Mni2009cToMni6)) match
      case Left(AtlasError.TemplateAssetRefused(asset, reason)) =>
        assertEquals(asset, TemplateFlowXfm.Mni2009cToMni6.relativePath)
        assert(reason.contains("measured to pull PointsFrom2009cTo6Asym"), reason)
      case other => fail(s"expected a refusal, got $other")

  test("in ITK's own numbers, TemplateFlow's reverse composite moves 2009c points like the forward one, not back"):
    // Were it the inverse, it would pull 6Asym points to 2009c and, to first order, send x to 2x - F(x). At 22 of the
    // 23 points it lands nearer F(x) (median 0.54 mm) than that (median 4.4 mm); the exception lies outside the brain.
    val reverse = rows("reverse_points.tsv")
    val forward = rows("pull_points.tsv")
    assertEquals(reverse.map(_.key), forward.map(_.key))
    val nearer =
      reverse.zip(forward).count: (r, f) =>
        val firstOrderInverse = f.input.zip(f.output).map((x, fx) => 2.0 * x - fx)
        distance(r.output, f.output) < distance(r.output, firstOrderInverse)
    assert(nearer >= 20, s"only $nearer of ${reverse.size} points move with the forward composite")

  test("a qualified numerical inverse gives the forward map and an executable reverse route"):
    val bridge = inverted
    assert(bridge.hasForwardMap)
    assert(bridge.identity.contains("inverse=numerical"), bridge.identity)
    rows("pull_points.tsv").foreach: row =>
      assert(distance(pull(bridge, row.input), row.output) <= PullTolerance, s"materialized pull at ${row.key}")
    val oracle = rows("push_points.tsv")
    val errors =
      oracle.map: row =>
        val pushed = push(bridge, row.input)
        assert(distance(pushed, row.output) <= PushTolerance, s"${row.key}: $pushed vs ITK inverse ${row.output}")
        assert(distance(pull(bridge, pushed), row.input) <= PushTolerance, s"${row.key}: round trip")
        (distance(pushed, row.output), distance(pull(bridge, pushed), row.input))
    println(f"forward map vs ITK fixed-point inverse: max ${errors.map(_._1).max}%.2e mm; round trip max ${errors.map(_._2).max}%.2e mm")
    val registry = bridge.install(SpaceTransforms.manifest)
    val forward = ok(SpaceTransforms.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym, DataKind.Voxel, registry))
    assert(forward.isExecutable, forward.executability.toString)
    ok(forward.transform(oracle.map(row => point3(row.input)))).zip(oracle).foreach: (point, row) =>
      assert(distance(point.toVector, row.output) <= PushTolerance, row.key)
    val reverse = ok(SpaceTransforms.plan(SpaceId.MNI152NLin2009cAsym, SpaceId.MNI152NLin6Asym, DataKind.Voxel, registry))
    assertEquals(reverse.status, TransformStatus.Available)
    assert(!reverse.usedInverses)
    val pulled = rows("pull_points.tsv")
    // the reverse route's forward map is the composite itself
    ok(reverse.transform(pulled.map(row => point3(row.input)))).zip(pulled).foreach: (point, row) =>
      assert(distance(point.toVector, row.output) <= PullTolerance, row.key)
