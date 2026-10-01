package scalafim.transform.scenarios

import image4s.{BoundaryPolicy, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, D3, Frame, Grid, Point}
import ravel.DType.given
import ravel.NDArray
import reframe4s.resample.Interpolation
import scalafim.image.world.{FrameCatalog, ToolCoordinates, WorldSpace}
import scalafim.scenarios.{ScenarioHarness, ScenarioObservation, ScenarioResult, ScenarioStatus, ScenarioTolerance}
import scalafim.transform.*
import scalafim.transform.field.{FnirtCoefficientContext, FnirtCoefficientInterpretation, FnirtCoefficientsCodec}
import scalafim.transform.fsl.{FlirtCodec, FlirtInterpretation, FlirtMatrix, FslHeaderGeometry}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}
import ChainScenarioSupport.*

/** Native fits on the real CC0 demo1 BOLDref/T1 pair and licensed MNI2009cAsym.
  * The fixed interior protects FSL scaled coordinates, affine direction and
  * nonlinear composition. This does not establish registration accuracy.
  */
class FslRealChainScenarioSuite extends munit.FunSuite:
  private val Root = "fsl_real_chain"
  private val func: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("demo1 sub-01 task-rest native FSL functional")))
  private val highres: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("demo1 sub-01 native FSL T1")))
  private val standard: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("native FSL MNI152NLin2009cAsym res-02")))
  private val PointBudget = 2e-4

  private def ok[E, A](value: Either[E, A]): A = value.fold(error => fail(s"fixture refused: $error"), identity)
  private def raw(name: String): NiftiRaw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"$Root/$name"))))
  private def asset(name: String): AssetRef = AssetRef(s"$Root/$name", Some(OracleFixtures.sha256Hex(s"$Root/$name")))
  private def indices(shape: Vector[Int]): Vector[Vector[Int]] =
    for i <- (0 until shape(0)).toVector; j <- 0 until shape(1); k <- 0 until shape(2) yield Vector(i, j, k)

  private lazy val funcGeometry = ok(FslHeaderGeometry(raw("example_func_header.nii.gz")))
  private lazy val highresGeometry = ok(FslHeaderGeometry(raw("highres_header.nii.gz")))
  private lazy val standardGeometry = ok(FslHeaderGeometry(raw("standard_header.nii.gz")))
  private lazy val matrix = ok(FlirtCodec.decode(TransformSource.Text(OracleFixtures.text(s"$Root/example_func2highres.mat"))))
  private lazy val coefficients = ok(FnirtCoefficientsCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(s"$Root/highres2standard_coef.nii.gz")))))
  private lazy val fnirt = ok(FnirtCoefficientInterpretation.interpretWith(coefficients,
    FnirtCoefficientContext(FslGrids[highres.type, standard.type](highres, highresGeometry, standard, standardGeometry)),
    asset("highres2standard_coef.nii.gz")))

  private def flirt(invert: Boolean): WorldTransform.Linear[func.type, highres.type] =
    val value = if invert then FlirtMatrix(ok(Affine.fromRowMajor[D3](matrix.rowMajor)).inverse.rowMajor) else matrix
    ok(FlirtInterpretation.interpretWith(value,
      FslGrids[func.type, highres.type](func, funcGeometry, highres, highresGeometry), asset("example_func2highres.mat")))

  private def grid[F <: Frame[D3]](frame: F, file: NiftiRaw): Grid[F, D3] =
    ok(Grid.forFrame[D3, F](frame)(file.spatialShape, ok(FslHeaderGeometry(file)).voxelToWorld))

  private lazy val contract = OracleFixtures.text(s"$Root/contract.json")
  private lazy val intensityBudget = "\"intensity_budget\":\\s*([0-9.eE+-]+)".r.findFirstMatchIn(contract).get.group(1).toDouble
  // FSL raw_affine_transform casts coefficients to float and advances y by
  // repeated additions from y=0. The generator derives this bound from input
  // headers/matrix and frozen queries before inspecting any reference residual.
  private lazy val flirtPointBudget = "\"point_budget_mm\":\\s*([0-9.eE+-]+)".r.findFirstMatchIn(contract.substring(contract.indexOf("\"flirt_float32_precision\""))).get.group(1).toDouble
  private lazy val flirtIntensityBudget = "\"flirt_intensity_budget\":\\s*([0-9.eE+-]+)".r.findFirstMatchIn(contract).get.group(1).toDouble

  private def run(invert: Boolean): ScenarioResult =
    val linear = flirt(invert)
    val chain = linear.andThen(fnirt)
    val target = raw("premat_image.nii.gz")
    val targetGrid = grid[standard.type](standard, target)
    val highresTarget = raw("flirt_image.nii.gz")
    val highresGrid = grid[highres.type](highres, highresTarget)
    val source = raw("func_crop.nii.gz")
    val sourceGrid = grid[func.type](func, source)
    val shape = source.spatialShape
    val image = ok(Sampled.continuous(sourceGrid, NonSpatialAxes.empty,
      NDArray.tabulate[Double](shape(0), shape(1), shape(2))((i, j, k) => source.value(i, j, k))))
    val materialized = chain.materialize(targetGrid)
    val sampled = chain.resample(image, targetGrid, Interpolation.Linear, BoundaryPolicy.Constant(0.0))
    val flirtSampled = linear.resample(image, highresGrid, Interpolation.Linear, BoundaryPolicy.Constant(0.0))

    def errors(name: String, values: Either[TransformError, Vector[Double]], budget: Double): ScenarioObservation =
      values.fold(error => ScenarioHarness.fact(name, false, error.toString),
        values => ScenarioHarness.scalar(name, worst(values), 0.0, ScenarioTolerance.absolute(budget)))

    val queries = indices(target.spatialShape)
    val flirtQueries = indices(highresTarget.spatialShape)
    val mathAndTrace = OracleTable.load(s"$Root/flirt_math_and_trace.tsv")
    val observations = Vector.newBuilder[ScenarioObservation]
    observations += ScenarioHarness.fact("queries.standard-fixed", target.spatialShape == Vector(9, 7, 11) && queries.size == 693, target.spatialShape.toString)
    observations += ScenarioHarness.fact("queries.highres-fixed", highresTarget.spatialShape == Vector(7, 9, 5) && flirtQueries.size == 315, highresTarget.spatialShape.toString)
    observations += ScenarioHarness.fact("chain.full-coverage", materialized.exists(_.coverage.counts.covered == queries.size.toLong), materialized.fold(_.toString, _.coverage.counts.toString))
    observations += ScenarioHarness.fact("flirt.exact-math.all-queries", mathAndTrace.rows.size == 315, s"${mathAndTrace.rows.size} frozen affine queries")
    val exactErrors = mathAndTrace.rows.map: row =>
      linear.pullPoint(ok(Point.fromVector(highres, row.take(3)))).fold(_ => Double.PositiveInfinity,
        point => maxAbsDifference(point.coordinates, row.slice(3, 6)))
    observations += ScenarioHarness.scalar("flirt.exact-math.max-abs-mm", worst(exactErrors), 0.0, ScenarioTolerance.absolute(1e-9))
    for mode <- Vector("premat", "composed", "flirt") do
      val support = raw(s"${mode}_support.nii.gz")
      val selected = if mode == "flirt" then flirtQueries else queries
      observations += ScenarioHarness.fact(s"native.$mode.all-supported", selected.forall(v => support.value(v(0), v(1), v(2)) >= .999), s"all ${selected.size} frozen queries; none excluded")
      val coordinates = Vector.tabulate(3)(c => raw(s"${mode}_coord$c.nii.gz"))
      val expected = raw(s"${mode}_image.nii.gz")
      if mode == "flirt" then
        val traceErrors = selected.zip(mathAndTrace.rows).map: (v, row) =>
          maxAbsDifference(coordinates.map(_.value(v(0), v(1), v(2))), row.slice(6, 9))
        observations += ScenarioHarness.scalar("flirt.native-float32-trace.max-abs-mm", worst(traceErrors), 0.0, ScenarioTolerance.absolute(5e-5))
        val pointErrors = selected.map: v =>
          val point = ok(Point.fromVector(highres, affineAt(highresGrid.indexToFrame, v.map(_.toDouble))))
          linear.pullPoint(point).fold(_ => Double.PositiveInfinity, p => maxAbsDifference(p.coordinates, coordinates.map(_.value(v(0), v(1), v(2)))))
        observations += ScenarioHarness.scalar("flirt.native-coordinates.max-abs-mm", worst(pointErrors), 0.0, ScenarioTolerance.absolute(flirtPointBudget))
        observations += errors("flirt.real-image.max-abs", flirtSampled.map(r => selected.map(v => math.abs(r.image.data.at(IArray(v(0), v(1), v(2))) - expected.value(v(0), v(1), v(2))))), flirtIntensityBudget)
      else
        observations += errors(s"chain.$mode.native-coordinates.max-abs-mm", materialized.map(f => selected.map(v =>
          maxAbsDifference(Vector.tabulate(3)(c => ok(f.field.coordinates.valueAt(v, Vector(c)))), coordinates.map(_.value(v(0), v(1), v(2)))))), PointBudget)
        observations += errors(s"chain.$mode.real-image.max-abs", sampled.map(r => selected.map(v => math.abs(r.image.data.at(IArray(v(0), v(1), v(2))) - expected.value(v(0), v(1), v(2))))), intensityBudget)

    val absolute = raw("chain_abs.nii.gz")
    val scaledToWorld = ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(funcGeometry))
    observations += errors("chain.convertwarp.absolute-field.max-abs-mm", materialized.map(f => queries.map(v =>
      maxAbsDifference(Vector.tabulate(3)(c => ok(f.field.coordinates.valueAt(v, Vector(c)))),
        affineAt(scaledToWorld, Vector.tabulate(3)(c => absolute.value(v(0), v(1), v(2), c)))))), PointBudget)
    observations += ScenarioHarness.fact("chain.forward-unavailable", chain.push.isEmpty, "no inverse registration was supplied")
    val provenance = Vector(
      TransformProvenance.Step.Read(TransformFormat.FslFlirt, asset("example_func2highres.mat")),
      TransformProvenance.Step.Read(TransformFormat.FslFnirtCoefficients, asset("highres2standard_coef.nii.gz")))
    observations += ScenarioHarness.fact("chain.provenance", chain.provenance.steps == provenance, chain.provenance.describe)
    ScenarioHarness.result("transform.fsl-real-demo1-interior.v1", observations.result())

  test("real native FSL example_func -> highres -> standard has one clean interior result"):
    val result = run(false)
    println(result.render)
    assertEquals(result.status, ScenarioStatus.Pass, result.render)
    assert(result.ciPass, result.render)

  test("reversing the real FLIRT matrix fails the native point and composed-field guards"):
    val result = run(true)
    assertEquals(result.status, ScenarioStatus.Fail, result.render)
    assert(result.failures.exists(_.render.startsWith("flirt.native-coordinates")), result.render)
    assert(result.failures.exists(_.render.startsWith("flirt.exact-math.max-abs-mm")), result.render)
    assert(result.failures.exists(_.render.startsWith("chain.convertwarp")), result.render)

  test("fixture closure binds native attempts and independently identified public source bytes"):
    val manifest = OracleFixtures.text(s"$Root/manifest.json")
    val tail = manifest.substring(manifest.indexOf("\"fixture_sha256\""), manifest.indexOf("\"scope\""))
    val hashes = "\"([^\"]+)\":\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(tail).toVector
    assertEquals(hashes.size, 31, "every fixture and native receipt must be bound")
    hashes.foreach(m => assertEquals(OracleFixtures.sha256Hex(s"$Root/${m.group(1)}"), m.group(2), m.group(1)))
    val source = OracleFixtures.text(s"$Root/source-provenance.json")
    assert(source.contains("1d8407e467d1af0ac8933c0539bbbb0168badab6"))
    assert(source.contains("15d7c02160f79f5218d2545b4febebeecc11531d"))
    assert(source.contains("4ef3fb341cd8452223528b5e115e674ec1582e5c3b311a4fbc5e2524472f7e36"))
    assert(source.contains("c3348f1124001ebfdd7c7f9b7b9aa99af106ee86c7839698c7d7ae5e9b467c98"))
    assert(source.contains("\"license\": \"CC0\""))
    assert(OracleFixtures.text(s"$Root/dataset_description.json").contains("\"License\": \"CC0\""))
    assert(OracleFixtures.text(s"$Root/template-LICENSE").contains("Permission to use, copy, modify, and distribute"))
    val commands = OracleFixtures.text(s"$Root/commands.json")
    assertEquals("\"exit_code\": 0".r.findAllMatchIn(commands).size, 19)
    assert(contract.contains("\"retained_float32_bits_exact\": true"))
    assert(intensityBudget.isFinite && intensityBudget > 0.0 && intensityBudget < 1.0)
    assert(flirtPointBudget.isFinite && flirtPointBudget >= PointBudget && flirtPointBudget <= .001)
    assert(flirtIntensityBudget.isFinite && flirtIntensityBudget >= intensityBudget)

  test("the real FSL legs cannot compose in the opposite typed order"):
    val errors = compileErrors("fnirt.andThen(flirt(false))")
    assert(errors.contains("Required:") && errors.contains("standard") && errors.contains("func"), errors)
