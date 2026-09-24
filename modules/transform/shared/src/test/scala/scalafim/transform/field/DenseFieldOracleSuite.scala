package scalafim.transform.field

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.fsl.FslHeaderGeometry
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** Dense displacement fields against ITK's DisplacementFieldTransform and FSL 5.0.9 convertwarp/applywarp. */
class DenseFieldOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def raw(path: String): NiftiRaw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(path))))
  private def field(path: String): VectorFieldNifti =
    ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(path)))))

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("warp source")))
  private val target: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("warp target")))
  private val frames = Frames[source.type, target.type](source, target)

  private def pull(transform: WorldTransform[source.type, target.type], p: Vector[Double]): Vector[Double] =
    ok(transform.pullPoint(ok(Point.fromVector(target, p)).asInstanceOf[Point[target.type, D3]])).coordinates

  test("ANTs/ITK 5D displacement fields reproduce ITK DisplacementFieldTransform"):
    val warp = ok(LpsDisplacementInterpretation.Ants.interpret(field("neurotransform/itk_oracle/warp.nii.gz"), DenseContext(frames)))
    val points = OracleTable.load("itk_field/points.tsv")
    points.rows.foreach: row =>
      val lps = row.take(3)
      val expected = row.slice(3, 6)
      val pulled = pull(warp, Vector(-lps(0), -lps(1), lps(2)))
      Vector(-pulled(0), -pulled(1), pulled(2)).zip(expected).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"at $lps"))
    assert(warp.mapPoint(ok(Point.fromVector(source, Vector(0.0, 0.0, 0.0))).asInstanceOf[Point[source.type, D3]]).isLeft, "a warp without an inverse has no forward map")

  test("outside the lattice the default policy rejects instead of returning identity"):
    val warp = ok(LpsDisplacementInterpretation.Ants.interpret(field("neurotransform/itk_oracle/warp.nii.gz"), DenseContext(frames)))
    assert(warp.pullPoint(ok(Point.fromVector(target, Vector(500.0, 500.0, 500.0))).asInstanceOf[Point[target.type, D3]]).isLeft)

  private val fslCases = Vector("left_left", "left_right", "right_left", "right_right").flatMap(c => Vector(s"${c}_relative", s"${c}_absolute"))

  test("FNIRT relative and absolute fields reproduce FSL applywarp coordinate ramps in every handedness case"):
    fslCases.foreach: name =>
      val dir = s"neurotransform/fsl_dense_oracle/$name"
      val warpField = field(s"$dir/warp.nii.gz")
      val sourceGeometry = ok(FslHeaderGeometry(raw(s"$dir/source.nii.gz")))
      val definition = if name.endsWith("absolute") then FnirtDefinition.Absolute else FnirtDefinition.Relative
      val warp = ok(FnirtFieldInterpretation.interpret(warpField, FnirtContext(frames, sourceGeometry, Some(definition))))
      val ramps = Vector(0, 1, 2).map(i => raw(s"$dir/native_coord$i.nii.gz"))
      val support = raw(s"$dir/native_support.nii.gz")
      val reference = ok(FslHeaderGeometry(warpField.raw))
      val dims = warpField.spatialDims
      assertEquals(ramps.head.spatialShape, dims, name)
      def supported(x: Int, y: Int, z: Int): Boolean =
        (-2 to 2).forall(dx => (-2 to 2).forall(dy => (-2 to 2).forall { dz =>
          val (i, j, k) = (x + dx, y + dy, z + dz)
          i >= 0 && j >= 0 && k >= 0 && i < dims(0) && j < dims(1) && k < dims(2) && support.value(i, j, k) > 0.5
        }))
      var checked = 0
      for x <- 0 until dims(0); y <- 0 until dims(1); z <- 0 until dims(2) if supported(x, y, z) do
        val world = ok(reference.voxelToWorld(Vector(x.toDouble, y.toDouble, z.toDouble)))
        val pulled = pull(warp, world)
        (0 until 3).foreach(c => assertEqualsDouble(pulled(c), ramps(c).value(x, y, z), 3e-5, s"$name voxel ($x,$y,$z) component $c"))
        checked += 1
      assert(checked >= 8, s"$name: only $checked supported voxels")

  test("relative vs absolute FNIRT fields are detected from content in every case"):
    fslCases.foreach: name =>
      val dir = s"neurotransform/fsl_dense_oracle/$name"
      val warpField = field(s"$dir/warp.nii.gz")
      val detected = FnirtDetection.detect(warpField, ok(FslHeaderGeometry(warpField.raw)), ok(FslHeaderGeometry(raw(s"$dir/source.nii.gz"))))
      assertEquals(detected, Right(if name.endsWith("absolute") then FnirtDefinition.Absolute else FnirtDefinition.Relative), name)

  test("vector-field containers round-trip value-exactly"):
    Vector("neurotransform/itk_oracle/warp.nii.gz", "neurotransform/fsl_dense_oracle/left_right_relative/warp.nii.gz").foreach: path =>
      val original = field(path)
      val again = ok(VectorFieldNiftiCodec.decode(ok(VectorFieldNiftiCodec.encode(original))))
      assertEquals(again.raw.shape, original.raw.shape)
      assertEquals(again.raw.qformRowMajor, original.raw.qformRowMajor)
      assertEquals(again.raw.sformRowMajor, original.raw.sformRowMajor)
      (0L until original.raw.voxelCount).foreach(i => assertEquals(again.raw.value(i), original.raw.value(i), s"$path value $i"))

  test("non-vector NIfTI is refused"):
    assert(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded("neurotransform/itk_oracle/source.nii.gz")))).isLeft)
