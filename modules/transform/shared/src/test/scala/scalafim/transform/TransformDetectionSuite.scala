package scalafim.transform

import scalafim.transform.oracle.OracleFixtures

class TransformDetectionSuite extends munit.FunSuite:
  private def file(path: String): TransformSource =
    TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(path)))

  private def name(path: String): Option[String] =
    Some(path.split('/').last.stripSuffix(".gz"))

  private def detected(path: String): Either[TransformIoError, TransformFormat] =
    TransformDetection.detect(file(path), name(path))

  test("vendored oracle files are recognised by content"):
    val expected = Vector(
      "neurotransform/itk_oracle/affine.tfm" -> TransformFormat.ItkText,
      "neurotransform/itk_oracle/affine.mat" -> TransformFormat.ItkMatlab,
      "neurotransform/itk_oracle/affine.h5" -> TransformFormat.ItkHdf5,
      "neurotransform/itk_oracle/warp.nii.gz" -> TransformFormat.AntsDisplacementNifti,
      "neurotransform/afni_oracle/oracle.aff12.1D" -> TransformFormat.AfniAff12,
      "neurotransform/afni_oracle/volreg_series.aff12.1D" -> TransformFormat.AfniAff12,
      "neurotransform/fsl_dense_oracle/left_left_relative/warp.nii.gz" -> TransformFormat.FslFnirtField,
      "neurotransform/fsl_coef_oracle/srcleft_refleft_aff/coef.nii.gz" -> TransformFormat.FslFnirtCoefficients,
      "neurotransform/fsl_coef_oracle/srcleft_refright_aff_quad/coef.nii.gz" -> TransformFormat.FslFnirtCoefficients,
      "neurotransform/fsl_coef_oracle/srcleft_refleft_aff/field_aff.nii.gz" -> TransformFormat.FslFnirtField,
      "conventions/flirt_0_to_1.mat" -> TransformFormat.FslFlirt
    )
    expected.foreach((path, format) => assertEquals(detected(path), Right(format), path))

  test("content wins over a misleading file name"):
    assertEquals(TransformDetection.detect(file("neurotransform/itk_oracle/affine.tfm"), Some("brain.lta")), Right(TransformFormat.ItkText))

  test("a 5D displacement field with no telling name is ambiguous between ANTs and AFNI"):
    TransformDetection.detect(file("neurotransform/itk_oracle/warp.nii.gz"), Some("field.nii")) match
      case Left(TransformIoError.Ambiguous(candidates, _)) =>
        assertEquals(candidates.toSet, Set(TransformFormat.AntsDisplacementNifti, TransformFormat.AfniQwarp))
      case other => fail(s"expected ambiguity, got $other")

  test("an HDF5 container without a telling name is ambiguous between ITK and X5"):
    assert(TransformDetection.detect(file("neurotransform/itk_oracle/affine.h5"), None).left.exists(_.isInstanceOf[TransformIoError.Ambiguous]))

  test("FreeSurfer text formats are recognised"):
    val lta =
      """# transform file brain.lta
        |type      = 1 # LINEAR_RAS_TO_RAS
        |nxforms   = 1
        |mean      = 0.0000 0.0000 0.0000
        |sigma     = 1.0000
        |1 4 4
        |1 0 0 0
        |0 1 0 0
        |0 0 1 0
        |0 0 0 1
        |""".stripMargin
    val xfm =
      """MNI Transform File
        |% avi2talxfm
        |
        |Transform_Type = Linear;
        |Linear_Transform =
        | 1.0 0.0 0.0 0.0
        | 0.0 1.0 0.0 0.0
        | 0.0 0.0 1.0 0.0;
        |""".stripMargin
    val registerDat =
      """bert
        |1.000000
        |1.000000
        |0.150000
        |1 0 0 0
        |0 1 0 0
        |0 0 1 0
        |0 0 0 1
        |round
        |""".stripMargin
    assertEquals(TransformDetection.detect(TransformSource.Text(lta)), Right(TransformFormat.FreeSurferLta))
    assertEquals(TransformDetection.detect(TransformSource.Text(xfm)), Right(TransformFormat.FreeSurferXfm))
    assertEquals(TransformDetection.detect(TransformSource.Text(registerDat)), Right(TransformFormat.FreeSurferRegisterDat))

  test("out-of-scope formats are refused by name, not guessed"):
    val refusals = Vector(
      TransformDetection.detect(TransformSource.Text("x"), Some("warp+tlrc.BRIK")),
      TransformDetection.detect(TransformSource.Text("x"), Some("talairach.m3z")),
      TransformDetection.detect(TransformSource.Text("(Transform \"BSplineTransform\")\n(NumberOfParameters 12)"), None)
    )
    refusals.foreach:
      case Left(TransformIoError.Unsupported(_, _)) => ()
      case other                                    => fail(s"expected an explicit refusal, got $other")

  test("gzip content must be decompressed first"):
    val gz = TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.bytes("neurotransform/itk_oracle/warp.nii.gz")))
    assert(TransformDetection.detect(gz, None).left.exists(_.isInstanceOf[TransformIoError.Undetectable]))
