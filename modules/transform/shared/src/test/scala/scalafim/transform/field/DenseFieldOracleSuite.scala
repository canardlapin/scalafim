package scalafim.transform.field

import image4s.geometry.{D3, Frame, Point}
import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.fsl.FslHeaderGeometry
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}
import scalafim.transform.x5.{X5Dumps, X5Interpretation}

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

  test("ANTs fields whose qform and sform disagree are placed with the affine ITK chooses, off the lattice too"):
    val forms = OracleTable.load("itk_field/forms.tsv")
    val points = OracleTable.load("itk_field/forms_points.tsv")
    assertEquals(forms.column("chosen").map(_.toInt).distinct.sorted, Vector(-1, 0, 1, 2))
    forms.keyed.foreach: (name, row) =>
      val warpField = field(s"itk_field/$name")
      val interpreted = LpsDisplacementInterpretation.Ants.interpret(warpField, DenseContext(frames))
      row(forms.index("chosen")).toInt match
        case -1 => assert(interpreted.isLeft, s"$name: ITK refuses a lone non-orthonormal sform, so must ScalaFIM")
        case chosen =>
          val lattice = ok(LatticeAffine.of(warpField.raw, LatticeAffine.Itk)).rowMajor
          val stored = chosen match
            case 2 => warpField.raw.sformRowMajor
            case 1 => warpField.raw.qformRowMajor
            case _ => Vector.fill(16)(0.0)
          Vector(3, 7, 11).foreach(i => assertEqualsDouble(lattice(i), stored(i), 1e-5, s"$name origin"))
          val warp = ok(interpreted)
          val rows = points.keyed.filter(_._1 == name)
          assertEquals(rows.size, 12, name)
          rows.foreach: (_, point) =>
            val lps = point.take(3)
            val pulled = pull(warp, Vector(-lps(0), -lps(1), lps(2)))
            Vector(-pulled(0), -pulled(1), pulled(2)).zip(point.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"$name at $lps"))

  test("outside the lattice the default policy rejects instead of returning identity"):
    val warp = ok(LpsDisplacementInterpretation.Ants.interpret(field("neurotransform/itk_oracle/warp.nii.gz"), DenseContext(frames)))
    assert(warp.pullPoint(ok(Point.fromVector(target, Vector(500.0, 500.0, 500.0))).asInstanceOf[Point[target.type, D3]]).isLeft)

  test("DenseContext.itk holds the border displacement for half a voxel and then uses the identity, as ITK does"):
    val warpField = field("neurotransform/itk_oracle/warp.nii.gz")
    val warp = ok(LpsDisplacementInterpretation.Ants.interpret(warpField, DenseContext.itk(frames)))
    val m = ok(LatticeAffine.of(warpField.raw, LatticeAffine.Itk)).rowMajor
    def world(u: Vector[Double]) = Vector.tabulate(3)(r => (0 until 3).map(c => m(4 * r + c) * u(c)).sum + m(4 * r + 3))
    // the stored LPS displacement at lattice node (0, 2, 3), in RAS
    val border = Vector.tabulate(3)(c => (if c < 2 then -1.0 else 1.0) * warpField.component(0, 2, 3, c))
    Vector(-0.3, -0.49).foreach: offset =>
      val q = world(Vector(offset, 2.0, 3.0))
      pull(warp, q).zip(q.zip(border).map(_ + _)).foreach((a, e) => assertEqualsDouble(a, e, 1e-12, s"band point at index $offset"))
    Vector(-0.51, -0.7).foreach: offset =>
      val q = world(Vector(offset, 2.0, 3.0))
      pull(warp, q).zip(q).foreach((a, e) => assertEqualsDouble(a, e, 1e-12, s"point beyond the band at index $offset"))
    // the default Reject refuses both
    val strict = ok(LpsDisplacementInterpretation.Ants.interpret(warpField, DenseContext(frames)))
    assert(strict.pullPoint(ok(Point.fromVector(target, world(Vector(-0.3, 2.0, 3.0)))).asInstanceOf[Point[target.type, D3]]).isLeft)

  test("ITK's border hold is refused by formats whose tool does not use it"):
    def refused[A](result: Either[TransformError, A], format: TransformFormat): Unit =
      result match
        case Left(TransformError.UnsupportedBoundary(`format`, CoordinateBoundaryPolicy.HoldBorderDisplacement, _)) => ()
        case other                                                                                             => fail(s"$format must refuse HoldBorderDisplacement, got $other")
    val itk = DenseContext.itk(frames)
    refused(LpsDisplacementInterpretation.AfniQwarp.interpret(field("afni_qwarp/affine_WARP.nii"), itk), TransformFormat.AfniQwarp)
    refused(X5Interpretation.interpret(X5Dumps.parse(OracleFixtures.text("x5/displacements.nodes.txt")), itk), TransformFormat.X5)
    // a linear X5 chain has no lattice, so no boundary to refuse
    assert(X5Interpretation.interpret(X5Dumps.parse(OracleFixtures.text("x5/linear.nodes.txt")), itk).isRight)
    val dir = "neurotransform/fsl_dense_oracle/left_right_relative"
    val sourceGeometry = ok(FslHeaderGeometry(raw(s"$dir/source.nii.gz")))
    refused(
      FnirtFieldInterpretation.interpret(field(s"$dir/warp.nii.gz"), FnirtContext(frames, sourceGeometry, Some(FnirtDefinition.Relative), CoordinateBoundaryPolicy.HoldBorderDisplacement)),
      TransformFormat.FslFnirtField
    )

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
