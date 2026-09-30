package scalafim.transform.fsl

import image4s.geometry.{Affine, D3, Frame, Grid, Point}
import reframe4s.field.{CoordinateBoundaryPolicy, DeterminantValue}
import scalafim.image.world.{FrameCatalog, SpaceError, ToolCoordinates, WorldSpace}
import scalafim.transform.*
import scalafim.transform.field.{FnirtCoefficientInterpretation, FnirtCoefficientContext, FnirtCoefficientsCodec, FnirtContext, FnirtDefinition, FnirtFieldInterpretation, VectorFieldNiftiCodec}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.OracleFixtures

/** Native FSL 6.0 oracle, regenerated in an offline, CPU-2, 1 GiB amd64 container.
  *
  * The frozen FSL 5.0.9 fixtures supply independent numeric controls; this suite checks that the Scala decoders still
  * reproduce FSL6 coordinate ramps rather than merely accepting successful native command receipts.
  */
class Fsl6NativeOracleSuite extends munit.FunSuite:
  private val Root = "fsl6_native"
  private val CoordinateTolerance = 2e-5
  private val FieldTolerance = 2e-5
  private val Image = "sha256:3ffbceee2ab631d765c6e2d1d90eedc9f31a33e8c555ec08e85ef6d65c66c1e2"

  private def ok[E, A](result: Either[E, A]): A = result.fold(error => fail(s"unexpected failure: $error"), identity)
  private def bytes(path: String) = IArray.unsafeFromArray(OracleFixtures.decoded(path))
  private def raw(path: String): NiftiRaw = ok(NiftiRaw.parse(bytes(path)))
  private def affineAt(affine: image4s.geometry.Affine[D3], v: Vector[Double]): Vector[Double] =
    val m = affine.rowMajor
    Vector.tabulate(3)(r => m(4 * r) * v(0) + m(4 * r + 1) * v(1) + m(4 * r + 2) * v(2) + m(4 * r + 3))

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("FSL6 native source")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("FSL6 native reference")))
  private val fsl5 = "neurotransform/fsl_coef_oracle"
  private val coefficientCases = Vector(
    "srcleft_refleft_noaff", "srcleft_refleft_aff", "srcleft_refright_noaff", "srcleft_refright_aff",
    "srcright_refleft_noaff", "srcright_refleft_aff", "srcright_refright_noaff", "srcright_refright_aff",
    "srcleft_refright_aff_quad", "srcright_refleft_noaff_quad"
  )

  private def coefficient(name: String) =
    ok(FnirtCoefficientsCodec.decode(TransformSource.Binary(bytes(s"$fsl5/$name/coef.nii.gz"))))

  test("manifest pins the approved FSL6 image, package lock, every generated fixture, and numerical receipts"):
    val manifest = OracleFixtures.text(s"$Root/manifest.json")
    assert(manifest.contains(s"\"image\": \"$Image\""), manifest)
    assert(manifest.contains(s"\"image_id\": \"$Image\""), manifest)
    assert(manifest.contains("\"timeout_seconds\": 300"), manifest)
    assert(manifest.contains("\"fsl6-package-lock.json\""), manifest)
    val section = manifest.substring(manifest.indexOf("\"fixture_sha256\""), manifest.indexOf("\"commands\""))
    val hashes = "\"([^\"]+)\"\\s*:\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(section).toVector
    assert(hashes.size >= 110, s"expected all generated fixture hashes, got ${hashes.size}")
    hashes.foreach(m => assertEquals(OracleFixtures.sha256Hex(s"$Root/${m.group(1)}"), m.group(2), m.group(1)))
    val inputs = manifest.substring(manifest.indexOf("\"input_sha256\""), manifest.indexOf("\"fixture_sha256\""))
    val inputHashes = "\"([^\"]+)\"\\s*:\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(inputs).toVector
    assertEquals(inputHashes.size, 73)
    inputHashes.foreach(m => assertEquals(OracleFixtures.sha256Hex(m.group(1)), m.group(2), m.group(1)))
    assert(manifest.contains("\"coefficients\": {"), manifest)
    assert(manifest.contains("\"dense\": {"), manifest)
    assert(manifest.contains("img2imgcoord -vox"), manifest)
    assert(!manifest.contains("not found"), manifest)

  test("FSL6 FNIRT coefficient ramps and fields agree with Scala decoders on every frozen asymmetric case"):
    coefficientCases.foreach: name =>
      val sourceGeometry = ok(FslHeaderGeometry(raw(s"$fsl5/$name/source.nii.gz")))
      val referenceGeometry = ok(FslHeaderGeometry(raw(s"$fsl5/$name/target.nii.gz")))
      val file = coefficient(name)
      val warp = ok(FnirtCoefficientInterpretation.interpret(file, FnirtCoefficientContext(FslGrids[source.type, reference.type](source, sourceGeometry, reference, referenceGeometry), CoordinateBoundaryPolicy.Reject)))
      def pull(point: Vector[Double]): Vector[Double] =
        ok(warp.pullPoint(ok(Point.in(reference)(point(0), point(1), point(2))))).coordinates
      val ramps = Vector.tabulate(3)(k => raw(s"$Root/coef_${name}_coord$k.nii.gz"))
      val support = raw(s"$Root/coef_${name}_support.nii.gz")
      val noAff = raw(s"$Root/coef_${name}_field_noaff.nii.gz")
      val withAff = raw(s"$Root/coef_${name}_field_aff.nii.gz")
      val sourceWorldToFsl = ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(sourceGeometry))
      var checked = 0
      for x <- 0 until referenceGeometry.dims(0); y <- 0 until referenceGeometry.dims(1); z <- 0 until referenceGeometry.dims(2) do
        val index = Vector(x.toDouble, y.toDouble, z.toDouble)
        val referenceFsl = affineAt(referenceGeometry.voxelToFsl, index)
        val spline = file.spline.at(ok(image4s.geometry.ContinuousIndex.fromVector[D3](referenceFsl.zip(referenceGeometry.pixdim).map(_ / _))))
        (0 until 3).foreach(k => assertEqualsDouble(spline(k), noAff.value(x, y, z, k), FieldTolerance, s"$name noaff ($x,$y,$z) component $k"))
        val sourceFsl = affineAt(sourceWorldToFsl, pull(affineAt(referenceGeometry.voxelToWorld, index)))
        (0 until 3).foreach(k => assertEqualsDouble(sourceFsl(k) - referenceFsl(k), withAff.value(x, y, z, k), FieldTolerance, s"$name withaff ($x,$y,$z) component $k"))
        if support.value(x, y, z) >= .999 then
          val world = pull(affineAt(referenceGeometry.voxelToWorld, index))
          (0 until 3).foreach(k => assertEqualsDouble(world(k), ramps(k).value(x, y, z), CoordinateTolerance, s"$name ramp ($x,$y,$z) component $k"))
          checked += 1
      assert(checked >= 100, s"$name only checked $checked FSL6-supported voxels")

  test("FSL6 dense coordinate ramps reproduce Scala relative and absolute interpretations in every handedness"):
    for pair <- Vector("left_left", "left_right", "right_left", "right_right"); kind <- Vector("relative", "absolute") do
      val name = s"${pair}_$kind"
      val directory = s"neurotransform/fsl_dense_oracle/$name"
      val sourceGeometry = ok(FslHeaderGeometry(raw(s"$directory/source.nii.gz")))
      val referenceGeometry = ok(FslHeaderGeometry(raw(s"$directory/target.nii.gz")))
      val file = ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(bytes(s"$directory/warp.nii.gz"))))
      val definition = if kind == "relative" then FnirtDefinition.Relative else FnirtDefinition.Absolute
      val warp = ok(FnirtFieldInterpretation.interpret(file, FnirtContext(Frames[source.type, reference.type](source, reference), sourceGeometry, Some(definition), CoordinateBoundaryPolicy.Reject)))
      val support = raw(s"$Root/dense_${name}_support.nii.gz")
      val ramps = Vector.tabulate(3)(k => raw(s"$Root/dense_${name}_coord$k.nii.gz"))
      var checked = 0
      for x <- 1 until referenceGeometry.dims(0) - 1; y <- 1 until referenceGeometry.dims(1) - 1; z <- 1 until referenceGeometry.dims(2) - 1 do
        if support.value(x, y, z) >= .999 then
          val point = affineAt(referenceGeometry.voxelToWorld, Vector(x.toDouble, y.toDouble, z.toDouble))
          val actual = ok(warp.pullPoint(ok(Point.in(reference)(point(0), point(1), point(2))))).coordinates
          (0 until 3).foreach(k => assertEqualsDouble(actual(k), ramps(k).value(x, y, z), CoordinateTolerance, s"$name at ($x,$y,$z)"))
          checked += 1
      assert(checked >= 100, s"$name checked only $checked supported interior points")

  test("ScalaFIM rejects contradictory FLIRT handedness and maps admitted FSL6 -vox coordinates"):
    val cases = Vector("0_to_1", "1_to_0", "2_to_3", "3_to_0", "4_to_2", "fresh")
    cases.foreach: key =>
      val (matrix, sourceName, referenceName) =
        if key == "fresh" then ("flirt_fresh.mat", "flirt_fresh_src.nii", "flirt_fresh_ref.nii")
        else
          val Array(s, r) = key.split("_to_")
          (s"flirt_$key.mat", s"fsl_case$s.nii", s"fsl_case$r.nii")
      val rejectedNames = Set("fsl_case0.nii", "fsl_case1.nii", "flirt_fresh_src.nii")
      val sourceResult = FslHeaderGeometry(raw(s"conventions/$sourceName"))
      val referenceResult = FslHeaderGeometry(raw(s"conventions/$referenceName"))
      def checkAdmission(name: String, result: Either[TransformError, scalafim.image.world.FslVolumeGeometry]): Unit =
        if rejectedNames.contains(name) then
          result match
            case Left(TransformError.Space(SpaceError.FslHandednessConflict(_, qdet, _, sdet))) =>
              assert(qdet * sdet < 0.0, name)
            case other => fail(s"$name expected contradictory handedness rejection, got $other")
        else assert(result.isRight, s"$name: $result")
      checkAdmission(sourceName, sourceResult)
      checkAdmission(referenceName, referenceResult)
      val output = OracleFixtures.text(s"$Root/flirt_$key.tsv").linesIterator.filter(line => line.nonEmpty && !line.startsWith("Coordinates")).toVector.map(_.trim.split("\\s+").toVector.map(_.toDouble))
      assertEquals(output.size, 7, key)
      assert(output.forall(row => row.size == 3 && row.forall(_.isFinite)), key)
      // Native FSL's inconsistent-header fallback is retained as negative
      // evidence. It does not qualify those headers for Scala admission.
      for sourceGeometry <- sourceResult.toOption; referenceGeometry <- referenceResult.toOption do
        checkNativeCoordinates(key, matrix, sourceGeometry, referenceGeometry, output)

  private def checkNativeCoordinates(
      key: String,
      matrix: String,
      sourceGeometry: scalafim.image.world.FslVolumeGeometry,
      referenceGeometry: scalafim.image.world.FslVolumeGeometry,
      output: Vector[Vector[Double]]
  ): Unit =
      val transform = ok(FlirtInterpretation.interpret(ok(FlirtCodec.decode(TransformSource.Text(OracleFixtures.text(s"conventions/$matrix")))), FslGrids[source.type, reference.type](source, sourceGeometry, reference, referenceGeometry)))
      val high = sourceGeometry.dims.map(_.toDouble - 1)
      val probes = Vector(Vector(0.0, 0, 0), high, high.map(_ / 2), Vector(1.0, high(1) - 1, 1), Vector(high(0) - 1, 1, high(2) - 1), Vector(2.0, 3, 4), Vector(high(0) - 2, high(1) - 3, high(2) - 4))
      assertEquals(output.size, probes.size, key)
      probes.zip(output).foreach: (voxel, native) =>
        val world = affineAt(sourceGeometry.voxelToWorld, voxel)
        val mapped = ok(transform.mapPoint(ok(Point.in(source)(world(0), world(1), world(2))))).coordinates
        val expected = affineAt(referenceGeometry.voxelToWorld.inverse, mapped)
        expected.zip(native).foreach((a, e) => assertEqualsDouble(a, e, 2e-4, s"$key voxel $voxel"))

  test("native FLIRT applyxfm preserves the positive-handed stored voxel pullback"):
    val root = "fsl6_header_controls"
    val manifest = OracleFixtures.text(s"$root/manifest.json")
    assert(manifest.contains(Image), manifest)
    val hashes = "\"([^\"]+)\"\\s*:\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(manifest).toVector
    assertEquals(hashes.size, 11)
    hashes.foreach(m => assertEquals(OracleFixtures.sha256Hex(s"$root/${m.group(1)}"), m.group(2), m.group(1)))
    val sourceGeometry = ok(FslHeaderGeometry(raw(s"$root/consistent_positive_source.nii")))
    val referenceGeometry = ok(FslHeaderGeometry(raw(s"$root/consistent_positive_reference.nii")))
    assert(sourceGeometry.neurological && referenceGeometry.neurological)
    val matrix = ok(FlirtCodec.decode(TransformSource.Text(OracleFixtures.text(s"$root/flirt_0_to_1.mat"))))
    val transform = ok(FlirtInterpretation.interpret(matrix, FslGrids[source.type, reference.type](source, sourceGeometry, reference, referenceGeometry)))
    val ramps = Vector("x", "y", "z").map(axis => raw(s"$root/applyxfm_positive_axis_$axis.nii"))
    def trilinear(image: NiftiRaw, voxel: Vector[Double]): Double =
      val lower = voxel.map(math.floor(_).toInt)
      val fraction = voxel.zip(lower).map((v, lo) => v - lo)
      (for dx <- 0 to 1; dy <- 0 to 1; dz <- 0 to 1 yield
        val offsets = Vector(dx, dy, dz)
        val weight = offsets.zip(fraction).map((offset, f) => if offset == 0 then 1.0 - f else f).product
        weight * image.value(lower(0) + dx, lower(1) + dy, lower(2) + dz)
      ).sum
    var checked = 0
    for x <- 1 until sourceGeometry.dims(0) - 1; y <- 1 until sourceGeometry.dims(1) - 1; z <- 1 until sourceGeometry.dims(2) - 1 do
      val sourceVoxel = Vector(x.toDouble, y.toDouble, z.toDouble)
      val world = affineAt(sourceGeometry.voxelToWorld, sourceVoxel)
      val mapped = ok(transform.mapPoint(ok(Point.in(source)(world(0), world(1), world(2))))).coordinates
      val targetVoxel = affineAt(referenceGeometry.voxelToWorld.inverse, mapped)
      // Admission depends only on geometry, before consulting native values.
      if targetVoxel.zip(referenceGeometry.dims).forall((v, size) => v >= 1.0 && v <= size - 2.0) then
        ramps.zip(sourceVoxel).foreach: (ramp, expected) =>
          val actual = trilinear(ramp, targetVoxel)
          assert(actual.isFinite, s"non-finite native ramp at $targetVoxel")
          assertEqualsDouble(actual, expected, 2e-6, s"stored voxel pullback at $sourceVoxel")
        checked += 1
    assertEquals(checked, 20)

  test("FSL6 analytic coefficient Jacobians agree with the warp algebra's interior finite differences"):
    coefficientCases.foreach: name =>
      val sourceGeometry = ok(FslHeaderGeometry(raw(s"$fsl5/$name/source.nii.gz")))
      val referenceGeometry = ok(FslHeaderGeometry(raw(s"$fsl5/$name/target.nii.gz")))
      val warp = ok(FnirtCoefficientInterpretation.interpret(coefficient(name), FnirtCoefficientContext(
        FslGrids[source.type, reference.type](source, sourceGeometry, reference, referenceGeometry), CoordinateBoundaryPolicy.Reject)))
      // Declare the crop before materialization: one voxel per face avoids
      // floating-point admission of physical boundary points; the derivative
      // comparison excludes another voxel to use central differences only.
      val m = referenceGeometry.voxelToWorld.rowMajor
      val cropped = ok(Affine.fromRowMajor[D3](m.updated(3, m(3) + m(0) + m(1) + m(2))
        .updated(7, m(7) + m(4) + m(5) + m(6))
        .updated(11, m(11) + m(8) + m(9) + m(10))))
      val lattice = ok(Grid.forFrame[D3, reference.type](reference)(referenceGeometry.dims.map(_ - 2), cropped))
      val determinant = ok(warp.jacobianDeterminant(lattice))
      val native = raw(s"$Root/coef_${name}_jac.nii.gz")
      val errors = for
        x <- (1 until lattice.shape(0) - 1).toVector
        y <- 1 until lattice.shape(1) - 1
        z <- 1 until lattice.shape(2) - 1
      yield
        val expected = native.value(x + 1, y + 1, z + 1)
        assert(expected.isFinite && expected != 0.0, s"$name native Jacobian at ($x,$y,$z)")
        determinant.at(Vector(x, y, z)) match
          case Some(DeterminantValue.Regular(actual)) =>
            assertEquals(math.signum(actual), math.signum(expected), name)
            math.abs(actual - expected) / math.abs(expected)
          case other => fail(s"$name expected a regular interior determinant, got $other")
      val sorted = errors.sorted
      assert(sorted.size >= 100, s"$name compared only ${sorted.size} determinants")
      // Same predeclared gates as the existing real-FNIRT oracle: native
      // analytic spline derivatives and one-voxel finite differences differ.
      assert(sorted(sorted.size / 2) < 0.01, s"$name median relative error")
      assert(sorted(math.ceil(0.99 * sorted.size).toInt - 1) < 0.05, s"$name p99 relative error")
      assert(sorted.last < 0.08, s"$name max relative error ${sorted.last}")
