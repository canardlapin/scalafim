package scalafim.transform

import java.nio.file.{Path, Paths}

import image4s.geometry.Point
import scalafim.image.world.{FrameCatalog, Spaces, WorldSpace}
import scalafim.transform.Conversion.EncodedTransform
import scalafim.transform.field.{DenseContext, LpsDisplacementInterpretation}
import scalafim.transform.fsl.{FlirtInterpretation, FslHeaderGeometry}
import scalafim.transform.itk.ItkHdf5Interpretation
import scalafim.transform.nifti.NiftiRaw

/** The examples in docs/guides/spatial-transforms.md, compiled and executed. Keep the two in step: each test body is
  * the guide's snippet for the section named in the test.
  */
class GuideExamplesSuite extends munit.FunSuite:
  private def oracle(relative: String): Path =
    Paths.get(getClass.getResource(s"/scalafim/transform/oracle/$relative").toURI)

  test("guide: read a registration file, whatever toolkit wrote it"):
    // A world space for each end of the registration. Templates have static frames; subject spaces are declared
    // (or resolved from NIfTI/GIFTI/BIDS evidence with SpaceResolver).
    val subject = FrameCatalog.frame(WorldSpace.declare("sub-01 T1w").toOption.get)
    val mni = Spaces.MNI152NLin2009cAsym

    // Read and decode by content; the file's own conventions (LPS, FSL scaled voxels, tkRAS, ...) are handled here.
    val loaded = TransformFiles.load(oracle("neurotransform/itk_oracle/affine_warp.h5")).toOption.get
    val transform: WorldTransform[subject.type, mni.type] =
      loaded.native match
        case NativeTransform.ItkHdf5(file) =>
          ItkHdf5Interpretation.interpret(file, DenseContext(Frames[subject.type, mni.type](subject, mni))).toOption.get.composed
        case other => fail(s"expected an ITK composite, got ${other.format}")

    // Every transform can pull a target (MNI) point back to the source (subject) space.
    val peak = Point.fromVector(mni, Vector(-12.3, 3.8, 10.1)).toOption.get
    assert(transform.pullPoint(peak).isRight)
    assertEquals(loaded.asset.sha256.map(_.length), Some(64))

  test("guide: FSL needs each volume's own geometry"):
    val input = FrameCatalog.frame(WorldSpace.declare("example_func").toOption.get)
    val reference = FrameCatalog.frame(WorldSpace.declare("highres").toOption.get)
    def fslGeometry(file: String) =
      FslHeaderGeometry(NiftiRaw.parse(IArray.unsafeFromArray(java.nio.file.Files.readAllBytes(oracle(file)))).toOption.get).toOption.get
    val grids = FslGrids[input.type, reference.type](input, fslGeometry("conventions/fsl_case0.nii"), reference, fslGeometry("conventions/fsl_case1.nii"))
    val flirt = TransformFiles.load(oracle("conventions/flirt_0_to_1.mat")).toOption.get.native match
      case NativeTransform.Flirt(m) => m
      case other                    => fail(s"expected FLIRT, got ${other.format}")
    val registration: WorldTransform.Linear[input.type, reference.type] = FlirtInterpretation.interpret(flirt, grids).toOption.get
    // Affines always have an exact inverse, so both directions are available.
    val back: WorldTransform.Linear[reference.type, input.type] = registration.inverse
    val p = Point.fromVector(input, Vector(1.0, 2.0, 3.0)).toOption.get
    val there = registration.mapPoint(p).toOption.get
    assertEqualsDouble(back.mapPoint(there).toOption.get.coordinates(0), 1.0, 1e-9)

  test("guide: convert between toolkits"):
    val lta = TransformFiles.load(oracle("freesurfer_linear/ras2ras.lta")).toOption.get.native
    def raw(file: String) = NiftiRaw.parse(IArray.unsafeFromArray(java.nio.file.Files.readAllBytes(oracle(file)))).toOption.get
    val (movable, reference) = (raw("freesurfer_linear/movable.nii"), raw("freesurfer_linear/reference.nii"))
    // FLIRT is defined in FSL scaled-voxel coordinates, so writing one needs both volumes' FSL geometry.
    val context = ConversionContext(fsl = Some(ConversionContext.FslPair(FslHeaderGeometry(movable).toOption.get, FslHeaderGeometry(reference).toOption.get)))
    val flirt = Conversion.convert(lta, TransformFormat.FslFlirt, ConversionContext.empty, context)
    val itk = Conversion.convert(lta, TransformFormat.ItkText, ConversionContext.empty, ConversionContext.empty)
    assert(flirt.exists(_.isInstanceOf[EncodedTransform.Source]))
    assert(itk.exists(_.isInstanceOf[EncodedTransform.Source]))
    // Asking for geometry you did not supply is a typed error, never a silent default.
    assert(Conversion.convert(lta, TransformFormat.FslFlirt, ConversionContext.empty, ConversionContext.empty).left.exists(_.isInstanceOf[TransformError.MissingContext]))
    // A dense warp cannot be written as an affine.
    val warp = TransformFiles.load(oracle("neurotransform/itk_oracle/warp.nii.gz")).toOption.get.native
    assert(Conversion.convert(warp, TransformFormat.FslFlirt, ConversionContext.empty, context).left.exists(_.isInstanceOf[TransformError.UnsupportedConversion]))

  test("guide: a dense warp has no forward map unless its inverse is supplied"):
    val subject = FrameCatalog.frame(WorldSpace.declare("sub-01 T1w").toOption.get)
    val mni = Spaces.MNI152NLin2009cAsym
    val warp = TransformFiles.load(oracle("neurotransform/itk_oracle/warp.nii.gz")).toOption.get.native match
      case NativeTransform.AntsField(field) => LpsDisplacementInterpretation.Ants.interpret(field, DenseContext(Frames[subject.type, mni.type](subject, mni))).toOption.get
      case other                            => fail(s"expected an ANTs field, got ${other.format}")
    val point = Point.fromVector(subject, Vector(0.0, 0.0, 0.0)).toOption.get
    // Resampling only needs the pullback, which every warp has. Mapping points forward needs an inverse warp.
    warp.mapPoint(point) match
      case Left(TransformError.NoForwardMap(_)) => ()
      case other                                => fail(s"expected NoForwardMap, got $other")
