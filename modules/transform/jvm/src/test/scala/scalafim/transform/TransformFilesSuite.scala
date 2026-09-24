package scalafim.transform

import java.nio.file.{Path, Paths}

/** Every oracle file loads from disk through the single JVM entry point, detected by content. */
class TransformFilesSuite extends munit.FunSuite:
  private def path(relative: String): Path =
    Paths.get(Option(getClass.getResource(s"/scalafim/transform/oracle/$relative")).getOrElse(fail(s"missing $relative")).toURI)

  test("oracle files of every format load with SHA-256 provenance"):
    val expected = Vector(
      "itk_linear/euler.tfm" -> TransformFormat.ItkText,
      "itk_linear/similarity.mat" -> TransformFormat.ItkMatlab,
      "neurotransform/itk_oracle/affine_warp.h5" -> TransformFormat.ItkHdf5,
      "neurotransform/itk_oracle/warp.nii.gz" -> TransformFormat.AntsDisplacementNifti,
      "conventions/flirt_0_to_1.mat" -> TransformFormat.FslFlirt,
      "neurotransform/fsl_dense_oracle/left_right_absolute/warp.nii.gz" -> TransformFormat.FslFnirtField,
      "neurotransform/fsl_coef_oracle/srcleft_refleft_aff/coef.nii.gz" -> TransformFormat.FslFnirtCoefficients,
      "neurotransform/afni_oracle/volreg_series.aff12.1D" -> TransformFormat.AfniAff12,
      "freesurfer_linear/vox2vox.lta" -> TransformFormat.FreeSurferLta,
      "freesurfer_linear/talairach.xfm" -> TransformFormat.FreeSurferXfm,
      "freesurfer_linear/register.dat" -> TransformFormat.FreeSurferRegisterDat,
      "x5/chain.x5" -> TransformFormat.X5
    )
    expected.foreach: (file, format) =>
      val loaded = TransformFiles.load(path(file)).fold(e => fail(s"$file: ${e.message}"), identity)
      assertEquals(loaded.format, format, file)
      assert(loaded.asset.sha256.exists(_.length == 64), file)

  test("an ambiguous file loads once its format is stated"):
    val warp = path("neurotransform/itk_oracle/warp.nii.gz")
    assertEquals(TransformFiles.load(warp, Some(TransformFormat.AfniQwarp)).map(_.format), Right(TransformFormat.AfniQwarp))

  test("missing files are typed failures"):
    assert(TransformFiles.load(Paths.get("/nonexistent/transform.lta")).isLeft)
