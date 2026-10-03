package scalafim.surface.scenarios

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.lie.FramedAffine
import scalafim.image.{GridSpec, SpatialDims}
import scalafim.image.world.*
import scalafim.scenarios.{ScenarioHarness, ScenarioResult, ScenarioStatus, ScenarioTolerance}
import scalafim.surface.*

/** Real ds002748 sub-01 left hemisphere, seven-point segment ribbon and two
  * registered-sphere routes on a frozen connected fsLR patch. Original mesh
  * IDs, ordered contributor faces and volume sample bytes are retained.
  * Native closest-point and public radial interpolation are distinct estimators.
  * The real intensity gap is descriptive; analytic commutativity has a bound
  * derived exclusively from input geometry. This is not a whole-mesh domain,
  * polyhedral ribbon, registration-quality or template-volume qualification.
  */
class SurfaceRealChainScenarioSuite extends munit.FunSuite:
  private val Root = "scalafim/surface/oracle/real_chain"
  private val Id = "surface.real-orig-to-fsaverage-to-fslr32k.v1"
  private val MathBudget = 1e-9
  private val NativeBudget = 1e-3

  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(s"fixture refused: $e"), identity)
  private def bytes(name: String): Array[Byte] = SurfaceRealFixturePlatform.read(s"$Root/$name")
  private def text(name: String): String = String(bytes(name), "UTF-8")
  private def rows(name: String): Array[Array[Double]] =
    text(name).linesIterator.drop(1).filter(_.nonEmpty).map(_.split("\t").map(_.toDouble)).toArray
  private def column(name: String, index: Int): Array[Double] = rows(name).map(_(index))
  private def affine(values: Array[Double]): Affine[D3] = ok(Affine.fromRowMajor[D3](values.toVector))
  private def mesh(name: String): TriangleMesh =
    TriangleMesh.fromArrays(rows(s"$name-vertices.tsv").flatMap(_.drop(1)), rows(s"$name-faces.tsv").flatMap(_.drop(1).map(_.toInt)))

  private lazy val geometry = column("geometry.tsv", 0)
  private val namespace = ok(DatasetNamespace("ds002748-fmriprep"))
  private val subject = ok(SubjectId("01"))
  private lazy val reference = ok(ReferenceAcquisition(Map("acq" -> "orig"), ok(GeometryDigest(geometry.take(3).map(_.toInt).toVector, geometry.slice(3, 15).toVector, 1, 1))))
  private lazy val scanner: Frame[D3] = FrameCatalog.frame(WorldSpace.SubjectNative(namespace, subject, None, reference))
  private lazy val tkRas: Frame[D3] = FrameCatalog.frame(WorldSpace.SubjectTkRas(namespace, subject, reference))
  private lazy val orig = FreeSurferVolumeGeometry(geometry.take(3).map(_.toInt).toVector, affine(geometry.slice(3, 19)))
  private lazy val grid = ok(GridSpec.in(scanner)(SpatialDims(geometry(19).toInt, geometry(20).toInt, geometry(21).toInt), affine(geometry.drop(22))))
  private lazy val volume = SurfaceRealFixturePlatform.gunzip(bytes("volume.raw.gz")).map(b => (b & 0xff).toDouble)
  private lazy val registered = mesh("subject")
  private lazy val average = mesh("fsaverage")
  private lazy val target = mesh("target")
  private lazy val whiteMesh = TriangleMesh.fromArrays(rows("white.tsv").flatten, registered.faceIndices.clone())
  private lazy val pialMesh = TriangleMesh.fromArrays(rows("pial.tsv").flatten, registered.faceIndices.clone())
  private lazy val whiteTk = ok(FramedSurface.in(tkRas)(SurfaceGeometry(whiteMesh, Hemisphere.Left, SurfaceKind.White), SurfacePlacement.StoredCoordinates))
  private lazy val pialTk = ok(FramedSurface.in(tkRas)(SurfaceGeometry(pialMesh, Hemisphere.Left, SurfaceKind.Pial), SurfacePlacement.StoredCoordinates))

  private enum Mutation:
    case Faithful, TkRasAsScanner, ReversePlacement, LpsVolume, PermuteVertices

  private def maxError(a: Array[Double], b: Array[Double]): Double =
    if a.length != b.length || a.isEmpty || a.exists(!_.isFinite) || b.exists(!_.isFinite) then Double.PositiveInfinity
    else a.indices.map(i => math.abs(a(i)-b(i))).max

  private def sample(mutation: Mutation): Either[String, Array[Double]] =
    val surfaces = mutation match
      case Mutation.TkRasAsScanner =>
        for
          w <- FramedSurface.in(scanner)(SurfaceGeometry(whiteMesh, Hemisphere.Left, SurfaceKind.White), SurfacePlacement.StoredCoordinates)
          p <- FramedSurface.in(scanner)(SurfaceGeometry(pialMesh, Hemisphere.Left, SurfaceKind.Pial), SurfacePlacement.StoredCoordinates)
        yield (w, p)
      case Mutation.ReversePlacement =>
        val backwards = FramedAffine.betweenFrames[tkRas.type, scanner.type, D3](tkRas, scanner)(orig.tkrToScanner.inverse)
        for w <- whiteTk.transport(backwards); p <- pialTk.transport(backwards) yield (w, p)
      case _ =>
        for w <- whiteTk.toScanner(scanner, orig); p <- pialTk.toScanner(scanner, orig) yield (w, p)
    val volumeGrid =
      if mutation == Mutation.LpsVolume then
        val m = geometry.drop(22).zipWithIndex.map((v, i) => if i < 8 then -v else v)
        ok(GridSpec.in(scanner)(SpatialDims(grid.dims(0), grid.dims(1), grid.dims(2)), affine(m)))
      else grid
    for
      placed <- surfaces.left.map(_.message)
      op <- RibbonOperator.compile(placed._1, placed._2, volumeGrid, ok(RibbonSteps(6))).left.map(_.message)
      result <- op.sample(volume).left.map(_.message)
    yield if mutation == Mutation.PermuteVertices then result.reverse else result

  private lazy val first = ok(SurfaceResampling.plan(average, registered))
  private lazy val second = ok(SurfaceResampling.plan(target, average))
  private lazy val directPlan = ok(SurfaceResampling.plan(target, registered))

  private def run(mutation: Mutation): ScenarioResult =
    val observations = Vector.newBuilder[scalafim.scenarios.ScenarioObservation]
    def guard(name: String, actual: Array[Double], expected: Array[Double], budget: Double): Unit =
      observations += ScenarioHarness.scalar(name, maxError(actual, expected), 0.0, ScenarioTolerance.absolute(budget))
    sample(mutation) match
      case Left(error) => observations += ScenarioHarness.fact("ribbon.sample", false, error)
      case Right(values) =>
        guard("ribbon.math", values, column("ribbon-math.tsv", 0), MathBudget)
        guard("ribbon.native", values, column("ribbon-native.tsv", 0), NativeBudget)
        val stage1 = ok(SurfaceResampling.apply(first, values))
        val staged = ok(SurfaceResampling.apply(second, stage1))
        val direct = ok(SurfaceResampling.apply(directPlan, values))
        guard("fsaverage.math", stage1, column("fsaverage-math.tsv", 0), MathBudget)
        guard("staged.math", staged, column("staged-math.tsv", 0), MathBudget)
        guard("direct.math", direct, column("direct-math.tsv", 0), MathBudget)
        // Independently derived input-only estimator bounds include closest/radial
        // weight differences and true-header/inferred-model placement differences.
        for (name, result, boundColumn) <- Vector(("direct", direct, 0), ("staged", staged, 1)) do
          val native = column(s"$name-native.tsv", 0)
          val bounds = column("native-bounds.tsv", boundColumn)
          result.indices.foreach: i =>
            observations += ScenarioHarness.scalar(s"$name.native.$i", math.abs(result(i)-native(i)), 0.0, ScenarioTolerance.absolute(bounds(i)))
          guard(s"$name.native-closest-reference", column(s"$name-native.tsv", 0), column(s"$name-closest.tsv", 0), NativeBudget)
        val analytic = column("ribbon-math.tsv", 1)
        val analyticStage1 = ok(SurfaceResampling.apply(first, analytic))
        val analyticStaged = ok(SurfaceResampling.apply(second, analyticStage1))
        val analyticDirect = ok(SurfaceResampling.apply(directPlan, analytic))
        guard("staged.analytic-math", analyticStaged, column("staged-math.tsv", 1), MathBudget)
        guard("direct.analytic-math", analyticDirect, column("direct-math.tsv", 1), MathBudget)
        val bounds = column("analytic-bound.tsv", 0)
        analyticStaged.indices.foreach: i =>
          observations += ScenarioHarness.scalar(s"analytic.commutativity.$i", math.abs(analyticStaged(i)-analyticDirect(i)), 0.0, ScenarioTolerance.absolute(bounds(i)))
        // Validate the native analytic channel separately, including its gauge/order.
        guard("staged.native-analytic-reference", column("staged-native.tsv", 1), column("staged-closest.tsv", 1), NativeBudget)
        guard("direct.native-analytic-reference", column("direct-native.tsv", 1), column("direct-closest.tsv", 1), NativeBudget)
    ScenarioHarness.result(Id, observations.result())

  test("real volume ribbon -> fsaverage -> fsLR32k and direct route: clean bounded pass"):
    val result = run(Mutation.Faithful)
    assertEquals(result.status, ScenarioStatus.Pass, result.render)
    assert(result.ciPass, result.render)

  test("full-source contributor faces and independent radial weights match the public plans"):
    for (plan, name) <- Vector((first, "subject-to-fsaverage"), (second, "fsaverage-to-target"), (directPlan, "subject-to-target")) do
      val expected = rows(s"$name.tsv")
      assertEquals(plan.referenceVertices, expected.length)
      expected.indices.foreach: row =>
        val actual = (0 until plan.nonZeros).filter(k => plan.rows(k) == row).map(k => plan.cols(k) -> plan.vals(k)).toMap
        val wanted = (0 until 3).filter(k => expected(row)(k+3) > 1e-10).map(k => expected(row)(k).toInt -> expected(row)(k+3)).toMap
        assertEquals(actual.keySet, wanted.keySet, s"$name row $row full-source face closure/fallback")
        wanted.foreach((col, weight) => assertEqualsDouble(actual(col), weight, 1e-10, s"$name row $row column $col"))
        assert(actual.values.forall(_ >= 0.0))
        assertEqualsDouble(actual.values.sum, 1.0, 1e-10)

  test("coordinate and vertex-order mutations fail the real scenario"):
    for mutation <- Mutation.values.filter(_ != Mutation.Faithful) do
      val result = run(mutation)
      assertEquals(result.status, ScenarioStatus.Fail, s"$mutation:\n${result.render}")
      assert(result.failures.exists(o => o.render.startsWith("ribbon.")), result.render)

  test("tkRAS surfaces cannot enter a scanner-space ribbon operator without placement"):
    val errors = compileErrors("RibbonOperator.compile(whiteTk, pialTk, grid)")
    assert(errors.contains("Required:") && errors.contains("tkRas") && errors.contains("scanner"), errors)

  test("raw tkRAS placement matches independent affine algebra and actual MGH-header precision"):
    val placed = IArray.genericWrapArray(ok(whiteTk.toScanner(scanner, orig)).coordinates).toArray
    assertEqualsDouble(maxError(placed, rows("white-scanner-math.tsv").flatten), 0.0, MathBudget)
    // Norig column norms infer voxel sizes; actual MGH header delta differs by
    // 1.19e-7 because the stored direction cosines are rounded float32 values.
    assertEqualsDouble(maxError(placed, rows("white-scanner-header.tsv").flatten), 0.0, 5e-5)

  test("shared fixture hashes and frozen target IDs close on both platforms"):
    val entries = "\"([^\"]+)\":\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(text("manifest.json")).filter(_.group(1) != "generator_sha256").toVector
    assert(entries.size >= 25)
    entries.foreach(m => assertEquals(SurfaceRealFixturePlatform.sha256(bytes(m.group(1))), m.group(2), m.group(1)))
    assertEquals(target.vertexCount, 64)
    val ids = rows("target-vertices.tsv").map(_(0).toInt)
    assertEquals(ids.toVector, ids.sorted.distinct.toVector)
    assert(text("contract.json").contains("\"query_count\": 64"))
    assertEquals(volume.length, grid.nVoxels)
