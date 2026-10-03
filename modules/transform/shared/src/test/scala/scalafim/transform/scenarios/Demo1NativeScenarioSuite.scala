package scalafim.transform.scenarios

import image4s.{BoundaryPolicy, NonSpatialAxes, Sampled}
import image4s.geometry.{D3, Frame, Grid, Point}
import ravel.DType.given
import ravel.NDArray
import reframe4s.field.CoordinateBoundaryPolicy
import reframe4s.resample.Interpolation
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.scenarios.{ScenarioHarness, ScenarioObservation, ScenarioResult, ScenarioStatus, ScenarioTolerance}
import scalafim.transform.*
import scalafim.transform.field.{DenseContext, LatticeAffine}
import scalafim.transform.itk.{ItkHdf5Dumps, ItkHdf5File, ItkHdf5Interpretation, ItkLinearInterpretation, ItkTextCodec}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}
import ChainScenarioSupport.*

/** A real, source-bound fMRIPrep 21.0.2 chain on a declared interior comparison set.
  * Original ANTs containers produce all references; exact crops keep the offline
  * JVM/JS fixture small. This is not whole-domain or registration-accuracy proof.
  */
class Demo1NativeScenarioSuite extends munit.FunSuite:
  import Demo1NativeContract.*

  test("real demo1 boldref -> T1w -> MNI chain has one clean interior scenario result"):
    val result = run(Mutation.Faithful)
    assertEquals(result.status, ScenarioStatus.Pass, result.render)
    assert(result.ciPass, result.render)

  test("native fixture manifest closes every byte and the exact real source identities"):
    val manifest = OracleFixtures.text(s"$Root/manifest.json")
    assert(manifest.contains("ANTs Version: 2.6.5."), manifest)
    assert(manifest.contains("sha256:ac096b2f75f67866feb6606fde2a549d395675f83baec9fd17610c9a7053fa18"))
    assert(manifest.contains("\"kind\": \"native-oracle\""))
    assert(manifest.contains("antsApplyTransformsToPoints -d 3 -p 1"))
    assert(manifest.contains("--float 0 -u double"))
    val hashes = "\"([^\"]+)\":\\s*\"([0-9a-f]{64})\"".r
      .findAllMatchIn(manifest.substring(manifest.indexOf("\"sha256\"")))
      .map(m => m.group(1) -> m.group(2)).toVector
    assertEquals(hashes.size, 26, "every offline input and native receipt must be recorded")
    hashes.foreach((name, hash) => assertEquals(OracleFixtures.sha256Hex(s"$Root/$name"), hash, name))
    val source = OracleFixtures.text(s"$Root/source-provenance.json")
    assert(source.contains("1d8407e467d1af0ac8933c0539bbbb0168badab6"))
    assert(source.contains(ForwardSha))
    assert(source.contains(InverseSha))
    assert(source.contains(OracleFixtures.sha256Hex(s"$Root/boldref.nii.gz")))
    assert(source.contains(OracleFixtures.sha256Hex(s"$Root/scanner_to_t1.txt")))
    assert(source.contains(OracleFixtures.sha256Hex(s"$Root/t1_to_scanner.txt")))
    assert(OracleFixtures.text(s"$Root/dataset_description.json").contains("\"License\": \"CC0\""))

  test("origin, component, LPS, stage-order and scanner-direction mutations fail their point observations"):
    Mutation.values.filterNot(_ == Mutation.Faithful).foreach: mutation =>
      val result = run(mutation)
      assertEquals(result.status, ScenarioStatus.Fail, s"$mutation\n${result.render}")
      val failedPoint = result.failures.exists:
        case ScenarioObservation.Scalar(name, _, _, _) => name.startsWith("points.")
        case ScenarioObservation.Fact(name, _, _) => name.startsWith("points.")
        case _ => false
      assert(failedPoint, s"$mutation must fail a point observation\n${result.render}")

  test("the compact field refuses points outside its declared support"):
    val transform = t1Transform(file("t1_to_template"))
    assert(transform.pullPoint(checked(Point.in(template)(1000.0, -1000.0, 1000.0))).isLeft)
    assert(transform.push.isEmpty, "an inverse file is evidence supplied separately, not an implicit forward map")

  test("the two registration legs cannot be composed in the opposite typed order"):
    val errors = compileErrors("""
      import scalafim.transform.scenarios.Demo1NativeContract.*
      t1Transform(file("t1_to_template")).andThen(scannerTransform(false))
    """)
    assert(errors.contains("Required:") && errors.contains("bold") && errors.contains("template"), errors)

private[scenarios] object Demo1NativeContract:
  val Root = "demo1_native"
  val ForwardSha = "44fba8c2aec16da828b20bdd53b7c534f4c80bfc7281a1bc2affc10f8b4eb363"
  val InverseSha = "4903d429d8f6dbd30d6b7cdd1b93fac8dd451d34ddcedc4d9d2066bbac4782fe"
  val PointBudget = 1e-6
  val IntensityBudget = 0.01
  val bold: Frame[D3] = FrameCatalog.frame(checked(WorldSpace.declare("ds002748 v1.0.5 sub-01 task-rest scanner/boldref")))
  val t1: Frame[D3] = FrameCatalog.frame(checked(WorldSpace.declare("ds002748 v1.0.5 sub-01 fMRIPrep21.0.2 T1w")))
  val template: Frame[D3] = FrameCatalog.frame(checked(WorldSpace.declare("ds002748 fMRIPrep21.0.2 MNI152NLin6Asym transform reference")))
  lazy val points: OracleTable = OracleTable.load(s"$Root/points.tsv")

  enum Mutation:
    case Faithful, ReverseStages, CropOriginShift, SwapComponents, WrongLps, ScannerInverse

  def checked[E, A](result: Either[E, A]): A =
    result.fold(error => throw new IllegalArgumentException(s"fixture refused: $error"), identity)

  def file(name: String): ItkHdf5File =
    ItkHdf5Dumps.parse(OracleFixtures.text(s"$Root/$name.components.txt"))

  def cropAsset(name: String): AssetRef =
    AssetRef(s"$Root/$name.h5", Some(OracleFixtures.sha256Hex(s"$Root/$name.h5")))

  def t1Transform(value: ItkHdf5File, asset: AssetRef = cropAsset("t1_to_template")): WorldTransform[t1.type, template.type] =
    checked(ItkHdf5Interpretation.interpretWith(value,
      DenseContext(Frames[t1.type, template.type](t1, template), CoordinateBoundaryPolicy.Reject),
      asset)).composed

  def inverseTransform(value: ItkHdf5File, asset: AssetRef = cropAsset("template_to_t1")): WorldTransform[template.type, t1.type] =
    checked(ItkHdf5Interpretation.interpretWith(value,
      DenseContext(Frames[template.type, t1.type](template, t1), CoordinateBoundaryPolicy.Reject),
      asset)).composed

  def scannerTransform(inverted: Boolean): WorldTransform[bold.type, t1.type] =
    val name = if inverted then "t1_to_scanner.txt" else "scanner_to_t1.txt"
    checked(ItkLinearInterpretation.interpretWith(
      checked(ItkTextCodec.decode(TransformSource.Text(OracleFixtures.text(s"$Root/$name")))),
      Frames[bold.type, t1.type](bold, t1),
      AssetRef(name, Some(OracleFixtures.sha256Hex(s"$Root/$name"))), TransformFormat.ItkText)).composed

  def nativePointObservations[S <: Frame[D3]](
      key: String,
      target: Frame[D3]
  )(
      transform: WorldTransform[S, target.type],
      wrongLps: Boolean = false
  ): Vector[ScenarioObservation] =
    points.keyed.filter(_._1 == key).zipWithIndex.map: (entry, index) =>
      val row = entry._2
      val input = if wrongLps then flipLps(row.take(3)) else row.take(3)
      transform.pullPoint(checked(Point.fromVector(target, input))) match
        case Left(error) => ScenarioHarness.fact(s"points.$key[$index]", false, error.toString)
        case Right(actual) => ScenarioHarness.scalar(s"points.$key[$index]", maxAbsDifference(actual.coordinates, row.drop(3)), 0.0, ScenarioTolerance.absolute(PointBudget))

  private def mutated(original: ItkHdf5File, mutation: Mutation): ItkHdf5File =
    if mutation == Mutation.ReverseStages then original.copy(components = original.components.filter(_.isComposite) ++ original.stages.reverse)
    else original.copy(components = original.components.map: component =>
      if !component.isDisplacementField then component
      else mutation match
        case Mutation.CropOriginShift =>
          val fixed = Array.tabulate(component.fixedParameters.length)(component.fixedParameters(_))
          fixed(3) += 1.0
          component.copy(fixedParameters = IArray.unsafeFromArray(fixed))
        case Mutation.SwapComponents =>
          val values = Array.tabulate(component.parameters.length)(i => component.parameters(3 * (i / 3) + (i + 1) % 3))
          component.copy(parameters = IArray.unsafeFromArray(values))
        case _ => component
    )

  def run(mutation: Mutation): ScenarioResult =
    val forward = t1Transform(mutated(file("t1_to_template"), mutation))
    val backward = inverseTransform(file("template_to_t1"))
    val chain = scannerTransform(mutation == Mutation.ScannerInverse).andThen(forward)
    val paired = WorldTransform.Mapped(forward.pull,
      PushAvailability.FromAsset(backward.pull, cropAsset("template_to_t1")), forward.provenance)
    val suppliedInverse = points.keyed.filter(_._1 == "inverse").zipWithIndex.map: (entry, index) =>
      val row = entry._2
      paired.mapPoint(checked(Point.fromVector(t1, row.take(3)))) match
        case Left(error) => ScenarioHarness.fact(s"points.supplied-inverse[$index]", false, error.toString)
        case Right(actual) => ScenarioHarness.scalar(s"points.supplied-inverse[$index]",
          maxAbsDifference(actual.coordinates, row.drop(3)), 0.0, ScenarioTolerance.absolute(PointBudget))
    val pointObservations = nativePointObservations("t1", template)(forward, mutation == Mutation.WrongLps) ++
      nativePointObservations("inverse", t1)(backward) ++
      nativePointObservations("roundtrip", t1)(backward) ++
      nativePointObservations("bold", template)(chain, mutation == Mutation.WrongLps)
    val counts = Vector("t1" -> 347, "bold" -> 347, "inverse" -> 32, "roundtrip" -> 347).map: (key, count) =>
      ScenarioHarness.fact(s"points.$key.count", points.keys.count(_ == key) == count, s"all $count frozen queries, with no exclusions")
    val volumeObservations = if mutation != Mutation.Faithful then Vector.empty else
      val source = checked(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"$Root/boldref.nii.gz"))))
      val reference = checked(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"$Root/target.nii.gz"))))
      val expected = checked(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"$Root/original_bold_warp.nii.gz"))))
      val shape = source.spatialShape
      val sourceAffine = checked(LatticeAffine.of(source, LatticeAffine.Itk))
      val targetAffine = checked(LatticeAffine.of(reference, LatticeAffine.Itk))
      val sourceGrid = checked(Grid.forFrame[D3, bold.type](bold)(shape, sourceAffine))
      val targetGrid = checked(Grid.forFrame[D3, template.type](template)(reference.spatialShape, targetAffine))
      val image = checked(Sampled.continuous(sourceGrid, NonSpatialAxes.empty,
        NDArray.tabulate[Double](shape(0), shape(1), shape(2))((i,j,k) => source.value(i,j,k))))
      val gridIndices = for i <- 0 until 7; j <- 0 until 9; k <- 0 until 5 yield Vector(i,j,k)
      val imageObservations = chain.resample(image, targetGrid, Interpolation.Linear, BoundaryPolicy.Reject) match
        case Left(error) => Vector(ScenarioHarness.fact("volume.full-support", false, error.toString))
        case Right(resampled) =>
          val deltas = gridIndices.map(v => math.abs(resampled.image.data.at(IArray(v(0),v(1),v(2))) - expected.value(v(0),v(1),v(2))))
          Vector(ScenarioHarness.scalar("volume.native-real-bold.maximum", worst(deltas), 0.0, ScenarioTolerance.absolute(IntensityBudget)))
      val coordinates = Vector.tabulate(3)(axis => checked(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"$Root/original_coord_$axis.nii.gz")))))
      val coordinateErrors = gridIndices.map: v =>
        val targetPoint = checked(Point.fromVector(template, affineAt(targetAffine, v.map(_.toDouble))))
        chain.pullPoint(targetPoint).fold(_ => Double.PositiveInfinity,
          actual => maxAbsDifference(actual.coordinates, coordinates.map(_.value(v(0),v(1),v(2)))))
      imageObservations ++ Vector(
        ScenarioHarness.fact("volume.frozen-grid.count", gridIndices.size == 315, "all 315 interior target voxels; rejecting field and image boundaries"),
        ScenarioHarness.scalar("volume.native-coordinate-ramps.maximum", worst(coordinateErrors), 0.0, ScenarioTolerance.absolute(PointBudget))
      )
    ScenarioHarness.result("transform.fmriprep-demo1-interior.v1", counts ++ pointObservations ++ suppliedInverse ++ volumeObservations)
