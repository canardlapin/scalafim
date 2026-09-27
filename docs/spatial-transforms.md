# Coordinate spaces and transforms

ScalaFIM reads the registration files that ITK/ANTs, FSL, AFNI and FreeSurfer
write. It knows each toolkit's coordinate conventions, and it can map points,
resample images, compose transforms, and convert between toolkits.

Two ideas carry the design:

- **World spaces are types.** A point in `MNI152NLin2009cAsym` and a point in
  a subject's T1w space cannot be mixed up. Composing A→B with C→D does not
  compile.
- **Direction is a type, not a flag.** A `WorldTransform[S, T]` always carries
  its *pullback* (target point → source point), which is what resampling
  needs. The forward direction exists exactly when it is known to exist.

The examples below are compiled and run in CI by
`modules/transform/jvm/src/test/scala/scalafim/transform/GuideExamplesSuite.scala`;
each section corresponds to one test there. The design rationale is in
[`plans/spatial-transform-parity.md`](plans/spatial-transform-parity.md) and
[`decisions/spatial-transforms.md`](decisions/spatial-transforms.md).

## World spaces and frames

A `WorldSpace` names a continuous RAS-millimetre coordinate system:

- a template
- a subject's native (scanner) space, anchored by a reference acquisition and
  scoped by dataset
- a subject's FreeSurfer tkRAS
- a declared space
- `Unresolved`, when there is no identity evidence

`FrameCatalog` turns a world space into an image4s `Frame[D3]`. Standard
templates have stable frames that can appear in types:

```scala
import scalafim.image.world.{FrameCatalog, Spaces, WorldSpace}

val mni = Spaces.MNI152NLin2009cAsym                      // usable as mni.type
val subject = FrameCatalog.frame(WorldSpace.declare("sub-01 T1w").toOption.get)
```

`SpaceResolver` derives a world space from the evidence a file carries. That
evidence is the xform code of the affine the NIfTI read policy selected, the
GIFTI coordinate system, and the BIDS `space-` entity. It refuses to guess:

- Bare `NIFTI_XFORM_MNI_152` does not say *which* MNI152, so it is a typed
  `AmbiguousTemplate`.
- Contradictory evidence is `ConflictingEvidence`. An explicit assertion
  settles ambiguity but never overrides a contradicting label or code.

`Nifti.readVolumeIn(path, evidence)` (and `readSeriesIn`) is the
identity-bearing read: it adds the header's selected xform code to the
caller's `SpaceEvidence`, resolves it, and returns the volume in that world
space, or a typed error when the evidence is missing or contradictory. The
plain `Nifti.readVolume` still returns volumes in the shared `Unresolved`
world, where every such volume aligns with every other; that is a legacy
convenience, not evidence that two files share a space.

A frame read from a file is a different runtime object from the static
template frame, even when both name the same space. `Placed[V].bindTo(frame)`
retypes a value to the static frame after image4s checks that the two share a
persistent identity.

## Reading a registration file

`TransformFiles.load` (JVM) reads a file, decompresses it if needed, and
detects the format from its content; the file name only breaks ties. It then
decodes the file and records its SHA-256. The format's interpretation turns
the decoded file into a `WorldTransform` between two frames:

```scala
val loaded = TransformFiles.load(path).toOption.get           // e.g. an ANTs/fMRIPrep composite .h5
val transform: WorldTransform[subject.type, mni.type] =
  loaded.native match
    case NativeTransform.ItkHdf5(file) =>
      ItkHdf5Interpretation.interpret(file, DenseContext(Frames[subject.type, mni.type](subject, mni))).toOption.get.composed
    case other => ???

transform.pullPoint(peakInMni)                                 // MNI point -> subject point
```

| Format | Native model | Interpretation context |
|---|---|---|
| ITK/ANTs text, `.mat` (MATLAB v4), `.h5` composite | `ItkTransformFile`, `ItkHdf5File` | `Frames` (composites: `DenseContext`) |
| ANTs / AFNI 3dQwarp displacement NIfTI | `VectorFieldNifti` | `DenseContext` |
| FSL FLIRT | `FlirtMatrix` | `FslGrids`: FSL geometry of both volumes |
| FSL FNIRT field (relative or absolute) | `VectorFieldNifti` | `FnirtContext`: source FSL geometry; relative vs absolute detected when unstated |
| FSL FNIRT coefficients (`--cout`, cubic or quadratic, with or without `--aff`) | `FnirtCoefficientFile` | `FnirtCoefficientContext`: `FslGrids` for both volumes; the reference must match the dimensions and voxel sizes the file records |
| AFNI `.aff12.1D` (one or many rows) | `Aff12Series` | `AfniContext` (oblique correction) |
| FreeSurfer LTA, `talairach.xfm`, `register.dat` | `LtaFile`, `MniXfm`, `RegisterDat` | `Frames`; `TkRegGrids` for register.dat |
| X5 (draft BIDS spec) | `X5File` | `DenseContext` |

Every toolkit convention is decoded once, in `ToolCoordinates`:

- **ITK/ANTs and AFNI** store LPS millimetres. ANTs and AFNI 3dQwarp
  displacement fields share one convention, an LPS displacement from each
  target lattice point to its source point. They differ only in how the
  lattice is placed. ANTs follows ITK's NIfTI reader: an orthonormal sform
  wins when the qform code is unset, the sform code is `SCANNER_ANAT`, or the
  two forms agree; otherwise the qform wins. AFNI reads the sform first but
  places a warp on that form's cardinalised axes. ScalaFIM therefore reads
  3dQwarp fields on cardinal grids only, and refuses an oblique one as
  `UnqualifiedConvention` until AFNI's own tools can pin it.
- **FSL** uses scaled-voxel coordinates, flipped along x when the volume's
  FSL-selected affine is neurological.
- **FreeSurfer tkRAS** is `Norig · inverse(Torig)`, where Torig is
  FreeSurfer's fixed LIA tkregister geometry.

Nothing downstream sees these conventions.

## FSL needs each volume's own geometry

FLIRT matrices live in FSL's scaled-voxel coordinates, so interpreting one
needs the FSL geometry of both volumes. `FslHeaderGeometry` builds that
geometry from each volume's raw NIfTI header, applying FSL's own affine
choice: sform, then qform, then scaling. That choice can differ from the
affine ScalaFIM reads the image with.

```scala
val grids = FslGrids[input.type, reference.type](input, fslGeometry(inputNifti), reference, fslGeometry(referenceNifti))
val registration: WorldTransform.Linear[input.type, reference.type] = FlirtInterpretation.interpret(flirt, grids).toOption.get
val back: WorldTransform.Linear[reference.type, input.type] = registration.inverse   // affines always invert exactly
```

FNIRT coefficient files (`--cout`) need the same pair of geometries. The
header records the spline order (intent 2007 cubic, 2009 quadratic), the knot
spacing (pixdim), the reference dimensions (qform translation) and voxel sizes
(`intent_p1`–`p3`), and the `--aff` matrix (sform). The spline lives on the
reference's FSL voxel lattice, which is mirrored along x for a neurological
reference, and yields displacements in FSL mm. The source FSL point is
`inverse(aff) · r + d`. `FnirtCoefficientInterpretation` evaluates the spline
exactly at every queried point. It matches FSL 5.0.9 `fnirtfileutils` and
`applywarp` in all ten handedness, order and `--aff` cases. DCT coefficients
(intent 2008) and TOPUP files are refused.

```scala
val file = FnirtCoefficientsCodec.decode(TransformSource.Binary(uncompressedCoefBytes)).toOption.get
val context = FnirtCoefficientContext(FslGrids[input.type, reference.type](input, fslGeometry(inputNifti), reference, fslGeometry(referenceNifti)))
val warp: WorldTransform.Mapped[input.type, reference.type] = FnirtCoefficientInterpretation.interpret(file, context).toOption.get
```

## Converting between toolkits

`Conversion.convert` works like `lta_convert` or `c3d_affine_tool`: it
decodes, interprets, expresses the result in the target format, and encodes
it.

```scala
val context = ConversionContext(fsl = Some(ConversionContext.FslPair(movingFsl, referenceFsl)))
Conversion.convert(lta, TransformFormat.FslFlirt, ConversionContext.empty, context)   // LTA -> FLIRT
Conversion.convert(lta, TransformFormat.ItkText, ConversionContext.empty, ConversionContext.empty)
```

Each target has an explicit capability:

| From \ To | ITK text/`.mat`, FLIRT, AFNI, LTA, xfm, register.dat | Dense fields (ANTs, AFNI, FNIRT) | FNIRT coefficients | ITK `.h5` |
|---|---|---|---|---|
| Affine | exact (FLIRT, register.dat and LTA need geometry) | needs a sampling lattice | unsupported | unsupported (read-only) |
| Dense map | unsupported | needs a lattice | unsupported (fitting deferred) | unsupported (read-only) |
| FNIRT coefficients | unsupported | needs FSL geometry and a lattice | unsupported (read-only; fitting deferred) | unsupported (read-only) |

Two rules govern failures:

- **Missing geometry** is `TransformError.MissingContext`, which says what is
  missing.
- **An impossible conversion** is `UnsupportedConversion`, which says why.

ITK `.h5` and X5 are read-only. The JVM HDF5 library cannot write the
variable-length strings both formats require, which writer acceptance
discovered. ANTs and AFNI fields also require orthogonal lattice axes.

## Why a warp has no `mapPoint` until it is inverted

Resampling an image from source to target evaluates the pullback at target
points, and every transform has one. Mapping a *source* point forward
through a dense warp needs the inverse warp. That comes either from a file
(ANTs `InverseWarp`, FSL `invwarp` output) or from a numerical inverse that
passed its qualification gates. Without one, `mapPoint` returns
`TransformError.NoForwardMap` instead of an approximation:

```scala
warp.mapPoint(pointInSubject)   // Left(NoForwardMap(...))

// Qualification gates: coverage fraction, then max and p99 residuals in mm, in both directions.
val policy = InversionPolicy.create(minimumCoverage = 0.5, maximumResidual = 0.05, p99Residual = 0.01)
val inverted = policy.flatMap(p => warp.invertNumerically(subjectLattice, p))
inverted.flatMap(_.mapPoint(pointInSubject))   // Right(point in MNI)
```

`invertNumerically` estimates the forward map on a persistent source lattice
with reframe4s' fixed-point solver. It starts from the identity, so it
converges only for warps that are close to the identity (where `I - D pull`
is a contraction), like the ANTs field in this example. It refuses a FNIRT
field between differently placed volumes, or any field that contains a large
affine. A qualified result is a
`Mapped` warp with `PushAvailability.Estimated`, which carries evidence:

- the reframe4s `InverseEstimate`;
- the evaluation-domain mask and its coverage fraction;
- per-point status counts (`Converged`, `MaxIterations`, `Diverged`,
  `OutsideCoverage`);
- interior residuals in both directions: `|pull(push(x)) - x|` and
  `|push(pull(y)) - y|`.

If any gate fails, the result is
`Left(TransformError.Inversion(GatesFailed(failures, evidence)))`. It is
never a weaker estimate that still counts as success. The estimate also
rejects points outside its evaluation domain rather than extrapolating.
A composite pullback must be materialized on a lattice before it can be
inverted. A forward map read from an inverse asset is never replaced by an
estimate.

Points outside a field's lattice are rejected by default
(`CoordinateBoundaryPolicy.Reject`). `PreserveSource` fills an out-of-lattice
interpolation corner with the query point itself, so beyond the first voxel
outside the lattice the map is the identity, which matches ITK's zero
displacement there. Within that first voxel the two differ: ITK holds the
border displacement for half a voxel and then drops to zero, while
`PreserveSource` blends the absolute coordinate towards the query point. The
fMRIPrep chain scenario declares this as the caveat
`transform.itk-border-band`.

## Warp algebra: fields, determinants and modulation

These operations are available on every `WorldTransform[S, T]`, whether it
is an affine, a composite or a dense warp. Each one hands the numerics to
reframe4s and keeps the result tied to the frame it lives in.

```scala
// here the warp is read with DenseContext(frames, CoordinateBoundaryPolicy.PreserveSource); see the note below
val field = warp.materialize(mniGrid)                  // convertwarp: the pullback sampled on a lattice
field.map(_.coverage.counts)                           // covered / filled / rejected lattice points
val jac = warp.jacobianDeterminant(mniGrid)            // det D pull, per target voxel (FSL --jac)
jac.map(_.foldCount)                                   // folds (det <= 0) are masked and counted
warp.logJacobian(mniGrid)                              // NonPositive status at folds, never -Inf
warp.resampleModulated(density, mniGrid, VolumeModulation.Jacobian, boundary = BoundaryPolicy.Constant(0.0))
```

- **`materialize`** evaluates the pullback at every point of the lattice.
  Each point is attributed to the first stage whose boundary policy supplied
  its value. Rejected points fail with `TransformError.Composition` and the
  coverage report, unless a fill policy is given. The fill never happens
  silently. To compose two transforms into one field, write
  `first.andThen(second).materialize(grid)`. On an oblique lattice, a face
  point of the field's own lattice can round to just outside it, and
  `Reject` then counts it as rejected. Read such fields with `PreserveSource`
  if you need every lattice point. Those points are then reported as
  `SourcePreserved`, and the boundary weight is about 1e-16. The
  materialized field has no forward map of its own, and a field whose
  rejected points were filled cannot be inverted.
- **`jacobianDeterminant`** gives the volume change of the pullback in mm³
  per mm³. It uses central differences of the materialized field, and it
  refuses lattices that the transform does not cover. On a real
  FNIRT registration it matches FSL 5.0.9 `fnirtfileutils --jac`, which
  differentiates the spline analytically, to a median relative error below
  1%.
- **`resample(image, onto, interpolation)`** is plain resampling: one
  reframe4s `ResamplingPlan` evaluates the pullback at every target voxel.
  Affine pullbacks keep the affine kernel. A target voxel whose pullback
  leaves the source image fails the plan unless a `BoundaryPolicy` fills it.
- **`resampleModulated`** scales each resampled value by `|det|`
  (`Jacobian`), which preserves a density's integral. `SqrtJacobian` scales
  by `sqrt|det|` instead and preserves the squared L2 norm of an amplitude.
  The two laws differ, so the two modes are not interchangeable. Folds are
  modulated by `|det|` and counted in `diagnostics.orientationReversingPoints`.
  If you must not resample through a fold, check that count, or check
  `jacobianDeterminant(...).foldCount` first.

## Orientation

`AxisCodes` use nibabel's letters, which name the direction each voxel axis
*points to*: `RAS` means i increases to the Right. neuroim2's
`Orientation3D` names the side each axis starts from, and the two convert
explicitly.

`Reorientation.volume(vol, AxisCodes.RAS)` permutes and flips the data (as
ravel views) and updates the affine to match. Every voxel keeps its world
position and world identity; this matches nibabel's `as_reoriented`.

## Linked cursors between worlds

A viewer that links a cortical surface with a volume often shows data from two
worlds: a surface in the subject's tkRAS or scanner space, and a volume in MNI
reached through an fMRIPrep composite. The link between the two cursors is a
typed `WorldLink[L, R]` (in `scalafim.image.world`):

- `WorldLink.shared(left, right)` links two frames of one world. Coordinates
  carry over unchanged under a checked alignment, and frames of different
  worlds are refused.
- `WorldLink.Mapped` joins different worlds through typed maps. A
  `WorldTransform` supplies one with `link`: `toLeft` is its pullback, and
  `toRight` exists exactly when the forward map does.

```scala
val link = warp.link.toOption.get              // WorldLink.Mapped[subject.type, mni.type]
link.toLeft(peakInMni)                         // Right(point in subject): the pullback
link.toRight(pointInSubject)                   // Left(DirectionUnavailable(LeftToRight, ...)) until the warp is inverted
```

The direction that needs a missing forward map is always a typed error, never
an approximation. The viewers consume the link without depending on this
module:

- `SurfaceVolumeCursor` (surface-view) takes a `FramedSurface[S]` and a
  `WorldLink[S, V]`. It maps a selected vertex to a volume point, and a
  volume point to the nearest vertex within a radius.
- `ViewerCursor` (image-view) reads and sets the orthogonal viewer's cursor as
  a point of the reference frame. It refuses a point from another world.
- A surface camera pose is a pair of typed points in the displayed surface's
  frame, `SurfaceCameraPose[F]`. `focusedOn(hit.vertex)` centres the view on
  a linked vertex.

The surface half is exercised by `SurfaceVolumeCursorSuite` and
`SurfaceCameraPoseSuite`.

## Evidence

The oracle fixtures, and the tools that produced them, are listed in each
fixture directory's `manifest.json` under
`modules/transform/shared/src/test/resources/scalafim/transform/oracle/`.

- **Native tools:** SimpleITK/ITK; AFNI 26.1.04 and FSL 5.0.9, via the
  neurotransform oracles.
- **Reference implementations:** fslpy, nibabel, neurotransform.
- **Cross-implementations:** nitransforms. It covers AFNI 3dQwarp fields,
  FreeSurfer LTAs, a generic oblique AFNI affine with cardinal correction,
  FLIRT and X5.
- **Self-consistency:** the FreeSurfer LTA, `register.dat` and `talairach.xfm`
  files are written by our own generator from FreeSurfer's source semantics.
  They show only that every encoding of one transform reads alike.

Checks that need FreeSurfer, ANTs, AFNI or FSL 6 binaries are recorded as
pending until those tools are available:

- `lta_convert` and `tkregister2` for FreeSurfer
- `3dNwarpXYZ` for 3dQwarp fields, including oblique ones
- AFNI's own handling of oblique datasets in `.aff12.1D` matrices

The read path interpolates dense fields trilinearly. Cubic interpolation and
Jacobian determinants are out of its scope; Jacobians belong to the warp
algebra (P6.03). nitransforms interpolates dense fields with a cubic B-spline,
so its points are compared only where both interpolants agree:

- on lattice nodes
- off the lattice, for fields that are affine in space, at least eight voxels
  from every face

The warp algebra is checked in two ways. Analytic laws cover affine and
radial determinants, the chain rule, folds, the sinusoidal inverse bound, and
the modulation laws. Native outputs cover FSL 5.0.9 `convertwarp` and
`fnirtfileutils --jac`, and ITK `TransformPoint` on composites. Numerical
inverses have not yet been checked against ANTs `InverseWarp` or FSL
`invwarp`. The reframe4s solver starts from the identity, so it refuses
fields that are far from it, such as FNIRT fields between differently placed
volumes. That refusal is itself tested.
