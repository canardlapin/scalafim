package scalafim.transform

import java.nio.file.{Path, Paths}

import image4s.{BoundaryPolicy, NonSpatialAxes, Sampled}
import image4s.geometry.{D3, Grid, GridId, Point}
import ravel.DType.given
import ravel.NDArray
import reframe4s.field.CoordinateBoundaryPolicy
import reframe4s.resample.VolumeModulation
import scalafim.image.world.{FrameCatalog, LinkDirection, Spaces, WorldLinkError, WorldSpace}
import scalafim.transform.Conversion.EncodedTransform
import scalafim.transform.field.{DenseContext, FnirtCoefficientContext, FnirtCoefficientInterpretation, LatticeAffine, LpsDisplacementInterpretation}
import scalafim.transform.fsl.{FlirtInterpretation, FslHeaderGeometry}
import scalafim.transform.itk.ItkHdf5Interpretation
import scalafim.transform.nifti.NiftiRaw

/** The examples in docs/spatial-transforms.md, compiled and executed. Keep the two in step: each test body is
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

  test("guide: FNIRT coefficient files need both volumes' geometry"):
    val input = FrameCatalog.frame(WorldSpace.declare("highres").toOption.get)
    val reference = FrameCatalog.frame(WorldSpace.declare("standard").toOption.get)
    val dir = "neurotransform/fsl_coef_oracle/srcleft_refright_aff"
    def fslGeometry(file: String) =
      val gz = new java.util.zip.GZIPInputStream(java.nio.file.Files.newInputStream(oracle(s"$dir/$file")))
      try FslHeaderGeometry(NiftiRaw.parse(IArray.unsafeFromArray(gz.readAllBytes())).toOption.get).toOption.get
      finally gz.close()
    val file = TransformFiles.load(oracle(s"$dir/coef.nii.gz")).toOption.get.native match
      case NativeTransform.FnirtCoefficients(f) => f
      case other                                => fail(s"expected FNIRT coefficients, got ${other.format}")
    val context = FnirtCoefficientContext(FslGrids[input.type, reference.type](input, fslGeometry("source.nii.gz"), reference, fslGeometry("target.nii.gz")))
    val warp: WorldTransform.Mapped[input.type, reference.type] = FnirtCoefficientInterpretation.interpret(file, context).toOption.get
    val centre = fslGeometry("target.nii.gz").voxelToWorld(Vector(5.0, 5.0, 4.0)).toOption.get
    assert(warp.pullPoint(Point.fromVector(reference, centre).toOption.get).isRight)

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

  private def gunzipped(file: String): NiftiRaw =
    val stream = java.util.zip.GZIPInputStream(java.nio.file.Files.newInputStream(oracle(file)))
    try NiftiRaw.parse(IArray.unsafeFromArray(stream.readAllBytes())).toOption.get
    finally stream.close()

  private def antsWarp[S <: image4s.geometry.Frame[D3], T <: image4s.geometry.Frame[D3]](
      subject: S,
      mni: T,
      boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject
  ): WorldTransform.Mapped[S, T] =
    TransformFiles.load(oracle("neurotransform/itk_oracle/warp.nii.gz")).toOption.get.native match
      case NativeTransform.AntsField(field) => LpsDisplacementInterpretation.Ants.interpret(field, DenseContext(Frames[S, T](subject, mni), boundary)).toOption.get
      case other                            => fail(s"expected an ANTs field, got ${other.format}")

  test("guide: a dense warp has no forward map until it is inverted"):
    val subject = FrameCatalog.frame(WorldSpace.declare("sub-01 T1w").toOption.get)
    val mni = Spaces.MNI152NLin2009cAsym
    val warp = antsWarp[subject.type, mni.type](subject, mni)
    // a subject point whose image lies inside the warp's lattice
    val warpRaw = gunzipped("neurotransform/itk_oracle/warp.nii.gz")
    val mniGrid = Grid.forFrame[D3, mni.type](mni)(warpRaw.spatialShape, LatticeAffine.of(warpRaw, LatticeAffine.Itk).toOption.get).toOption.get
    val centre = mniGrid.pointAt(image4s.geometry.LatticeIndex.fromVector[D3](warpRaw.spatialShape.map(_ / 2)).toOption.get).toOption.get
    val pointInSubject = warp.pullPoint(centre).toOption.get
    // Resampling only needs the pullback, which every warp has. Mapping points forward needs an inverse.
    warp.mapPoint(pointInSubject) match
      case Left(TransformError.NoForwardMap(_)) => ()
      case other                                => fail(s"expected NoForwardMap, got $other")

    // The estimate lives on a persistent lattice of the source space: here, the subject image's own grid.
    val subjectRaw = gunzipped("neurotransform/itk_oracle/source.nii.gz")
    val subjectLattice = GridId
      .parse("sub-01-T1w")
      .flatMap(id => Grid.createPersistent[D3, subject.type](id, subject)(subjectRaw.spatialShape, LatticeAffine.of(subjectRaw, LatticeAffine.Itk).toOption.get))
      .toOption
      .get
    val policy = InversionPolicy.create(minimumCoverage = 0.5, maximumResidual = 0.05, p99Residual = 0.01)
    val inverted = policy.flatMap(p => warp.invertNumerically(subjectLattice, p))
    val forward = inverted.flatMap(_.mapPoint(pointInSubject))
    forward.toOption.get.coordinates.zip(centre.coordinates).foreach((a, e) => assertEqualsDouble(a, e, 0.05))
    // an impossible tolerance is refused with the evidence, never returned as a weaker estimate
    val strict = InversionPolicy.create(minimumCoverage = 1.0, maximumResidual = 1e-9, p99Residual = 1e-9)
    assert(strict.flatMap(p => warp.invertNumerically(subjectLattice, p)).left.exists(_.isInstanceOf[TransformError.Inversion]))

  test("guide: linked cursors cross worlds through a transform's link"):
    val subject = FrameCatalog.frame(WorldSpace.declare("sub-01 T1w").toOption.get)
    val mni = Spaces.MNI152NLin2009cAsym
    val warp = antsWarp[subject.type, mni.type](subject, mni)
    val warpRaw = gunzipped("neurotransform/itk_oracle/warp.nii.gz")
    val mniGrid = Grid.forFrame[D3, mni.type](mni)(warpRaw.spatialShape, LatticeAffine.of(warpRaw, LatticeAffine.Itk).toOption.get).toOption.get
    val peak = mniGrid.pointAt(image4s.geometry.LatticeIndex.fromVector[D3](warpRaw.spatialShape.map(_ / 2)).toOption.get).toOption.get

    val link = warp.link                         // WorldLink.Mapped[subject.type, mni.type]
    val inSubject = link.toLeft(peak)                         // MNI cursor -> subject point: the pullback
    assert(inSubject.isRight, clue = inSubject)
    assertEquals(inSubject.toOption.map(_.coordinates), warp.pullPoint(peak).toOption.map(_.coordinates))
    link.toRight(inSubject.toOption.get) match                // subject cursor -> MNI needs the forward map
      case Left(WorldLinkError.DirectionUnavailable(LinkDirection.LeftToRight, _)) => ()
      case other => fail(s"expected the forward direction to be unavailable, got $other")

  test("guide: warp algebra: fields, determinants and modulation"):
    val subject = FrameCatalog.frame(WorldSpace.declare("sub-01 T1w").toOption.get)
    val mni = Spaces.MNI152NLin2009cAsym
    // read with ITK's zero-displacement extension (see the guide: face points of an oblique lattice)
    val warp = antsWarp[subject.type, mni.type](subject, mni, CoordinateBoundaryPolicy.PreserveSource)
    val warpRaw = gunzipped("neurotransform/itk_oracle/warp.nii.gz")
    val mniGrid = Grid.forFrame[D3, mni.type](mni)(warpRaw.spatialShape, LatticeAffine.of(warpRaw, LatticeAffine.Itk).toOption.get).toOption.get
    val subjectRaw = gunzipped("neurotransform/itk_oracle/source.nii.gz")
    val subjectGrid = Grid.forFrame[D3, subject.type](subject)(subjectRaw.spatialShape, LatticeAffine.of(subjectRaw, LatticeAffine.Itk).toOption.get).toOption.get
    val Vector(nx, ny, nz) = subjectRaw.spatialShape: @unchecked
    val density = Sampled.continuous(subjectGrid, NonSpatialAxes.empty, NDArray.tabulate[Double](nx, ny, nz)((i, j, k) => subjectRaw.value(i, j, k))).toOption.get

    val field = warp.materialize(mniGrid)                  // convertwarp: the pullback sampled on a lattice
    assertEquals(field.map(f => f.coverage.counts.covered + f.coverage.counts.sourcePreserved), Right(mniGrid.shape.product.toLong))
    assertEquals(field.map(_.coverage.counts.rejected), Right(0L))
    val jac = warp.jacobianDeterminant(mniGrid)            // det D pull, per target voxel (FSL --jac)
    assertEquals(jac.map(_.foldCount), Right(0L))          // folds (det <= 0) are masked and counted
    assert(warp.logJacobian(mniGrid).isRight)              // NonPositive status at folds, never -Inf
    val modulated = warp.resampleModulated(density, mniGrid, VolumeModulation.Jacobian, boundary = BoundaryPolicy.Constant(0.0))
    assert(modulated.exists(_.diagnostics.orientationReversingPoints == 0L))
