# Spatial Transform Parity

Status: **proposed, revised after fresh-context review** (2026-09-24). Tracked in mote as the STP epic; the mapping from plan to ticket ids is in
[spatial-transform-parity-epic.md](spatial-transform-parity-epic.md). Supersedes the transform sections of
`docs/ecosystem-port.md:123-178` and corrects `docs/module-relations.md:316-356`.

## Goal

ScalaFIM should read, understand, convert, compose, and apply every spatial
transform a working neuroimager meets: ITK/ANTs, FSL, AFNI, FreeSurfer, and
X5. It should reach feature parity with the R packages `neurotransform`
(transform kernel, format IO, warps, Jacobians) and `neurofunctor`
(domains, routing, operator compilation, QC). The result must be a coherent
Scala 3 system, not an R port:

- **Frames are types.** A point in MNI152NLin2009cAsym and a point in subject
  T1w space cannot be confused; composing `A→B` with `C→D` does not compile.
- **Direction is a type, not a flag.** No user ever sets
  `ForwardSourceToTarget | PullbackTargetToSource`. A `WorldTransform[S, T]`
  knows its pullback; inverses exist only when they are known to exist.
- **Toolkit conventions stay at the edge.** LPS, FSL scaled-voxel, AFNI
  DICOM, FreeSurfer tkRAS and ITK centred parameters are decoded once, in one
  place, into canonical RAS-mm frames. Nothing downstream sees them.
- **Every convention is pinned by a native-tool oracle,** not by a round trip
  through our own writer.

## Where we are (survey summary, 2026-09-24)

Detailed inventories were produced by read-only surveys. The main findings:

**Typed foundations already exist upstream, and ScalaFIM discards them.**

- image4s-geometry provides `Frame[D]`, `Point[F, D]`, `Vec[F, D]`, `Grid[F, D]`
  and `Affine[D]`. `Affine[D]` itself has no frame parameters.
- reframe4s provides the transform algebra:
  - `SpatialMap[From, To, D]`, with a frame-checked `andThen`
  - `SmoothIso`, whose `inverse` is total
  - `FramedAffine[From, To, D]`
  - `DenseMap`, which has no inverse by design
  - a frame-typed `TransformGraph`, plus `ResamplingPlan` and the
    interpolation kernels
- ScalaFIM then erases these frames:
  - `SomeSampleSpace` and `opaque GridSpec = SomeSampleSpace` erase them.
  - `type SpatialPullback = SpatialMap[Frame[D3], Frame[D3], D3]`
    (`modules/image/shared/.../ResamplingPlan.scala:37`) erases them.
  - Every RAS-mm space shares the persistent frame id `"scalafim-ras-d3"`
    (`SampleSpaces.scala:74,473`).
- NIfTI `NiftiCoordinateSystem` and GIFTI `GiftiTargetSpace` codes are parsed
  but never reach frame identity.

**Format IO today** (`modules/spatial/jvm/.../io/TransformAssetLoader.scala`,
JVM only, read only):

| Format | Status |
|---|---|
| ITK/ANTs affine text | read; the test oracle is a single x-translation |
| ITK MATLAB v4 binary `.mat` | parser exists in `image/jvm/io/ItkAffine.scala` but is **unused**; the loader rejects real binary `.mat` |
| ITK/ANTs `.h5` composite | read (jHDF), backed by SimpleITK oracles; this is the only native-oracle coverage |
| ANTs displacement NIfTI | read, but only 4D `(x,y,z,3)`; real 5D `(x,y,z,1,3)` intent-1007 files are unverified |
| FSL FLIRT | read (scaled-voxel math present); fixture is hand-written |
| FSL FNIRT | dense absolute field only; coefficient files are misread as fields |
| AFNI `.aff12.1D` | exactly 12 numbers only (multi-row series rejected); direction default unverified |
| AFNI 3dQwarp | generic dense path only; never detected |
| FreeSurfer LTA | enum only, **no parser**; `.xfm` and `register.dat` absent |
| X5 | enum only |

There are no writers for any format.

**Duplication:**

- **Three transform graphs:**
  - reframe4s `TransformGraph`
  - `spatial.SpatialGraph`/`MorphismPath`
  - `atlas.SpaceTransforms`, which has its own route search
- **LPS↔RAS flip:** hardcoded four times (`SpatialCoordinates`, `ItkAffine`, `ItkHdf5TransformReader`, `TransformAssetLoader`).
- **ITK affine code:** two copies.
- **Bounding boxes:** three.
- **Untyped affine records:** two (`archive-zarr.Affine4x4` and
  `AtlasPublication.affineRowMajor`).
- **reframe4s halfflow:** a private copy of ScalaFIM geometry.
- **Frame kernels:** two candidates (image4s-geometry and the unused
  `spatial4s`).

**neurofunctor parity is mostly done.** `modules/spatial` covers domains,
routing, single-interpolation compilation, adjoints, provenance, caching and
lazy views. Missing or partial:

- surface-source barycentric/sphere resampling
- hybrid per-part compile with block-diagonal assembly
- round-trip and commutativity QC
- quality-weighted inverse routing cost and forward-first fallback
- field-level `backproject`
- stock template domains
- mesh smoothing builders
- R-generated triplet fixtures

**neurotransform parity is the larger gap.** Missing on our side:

- ITK binary `.mat` in the loader
- 5D ANTs NIfTI
- FNIRT coefficients (cubic and quadratic B-spline)
- FNIRT relative/absolute detection
- multi-row AFNI `.1D` and the oblique-cardinal correction
- 3dQwarp
- LTA (all types)
- X5
- all writers
- Jacobian and log-Jacobian fields
- Jacobian-modulated resampling
- warp-field composition to a field
- surface resampling plans
- overlap metrics

neurotransform itself lacks several things we should do better:

- `.xfm` / `register.dat` / `.m3z` support
- numerical warp inversion
- a verified X5 direction
- qform/sform policy

It also has defects we must **not** port:

- it treats unknown LTA types as RAS→RAS
- it silently uses identity outside a warp field
- it passes absolute fields as displacements in flattened plans and Jacobians

### Constraints we must honour

- **Authority ledger** (`docs/plans/image-module-unification-hardening.md:21-43`):
  - image4s owns `Affine`/`Grid`/`SampleSpace`.
  - reframe4s owns coordinate transforms and resampling.
  - ScalaFIM adds only neuroimaging policy and adapters.
  - A new ScalaFIM type is admitted only if it enforces an invariant the
    provider type cannot, without copying provider state.
- **`imageAlgebraBoundaryCheck`** (`build.sbt:411-475`) forbids these names in
  production code: `SpatialMorphism`, `Affine3DMorphism`, `IdentityMorphism`,
  `DenseFieldMorphism`, `DenseFieldInverse`, `JacobianField`, `JacobianMode`,
  `DenseCoordinateMap`, `CompositeCoordinateMap`, `DMat`/`Affine3D`
  definitions, and others. This plan uses none of them. Warp inversion and
  Jacobian math go **upstream to reframe4s**.
- **JVM + JS for every phase.** HDF5 (jHDF) and image4s-nifti are JVM-only
  today. Byte-level codecs must therefore live in `shared`, and only file access and
  HDF5/NIfTI container decoding may be JVM-only. Where a format is JVM-only
  its test declares that as a caveat, not a silent skip.

## Design

### D0. Module placement (acyclic, lowest-module-first)

```text
image4s-geometry ─┐        reframe4s (lie, field, resample, core)
                  ▼                        │
image  (+ scalafim.image.world: WorldSpace, FrameCatalog, ToolCoordinates, AxisCodes)
                  │                        │
                  ▼                        ▼
transform  (scalafim.transform: WorldTransform, codecs, interpretations, Transforms.convert)
          ┌───────┴────────┐
          ▼                ▼
       spatial   ──►   atlas         (new edges: atlas → spatial, atlas → transform)
          ▲
       surface ──► (unchanged; surface frames come from image.world)
```

- **Frame identity lives in `image`.** `SampleSpaces.persistentFrameId` must
  derive from it, so it cannot sit in any module above `image`.
- `SubjectId`, `SessionId` and `TemplateName` move **down** from
  `spatial/Ids.scala` into `scalafim.image.world`. `spatial` imports them.
  There are no aliases (rule 1 of the ledger forbids duplicate authorities).
- **Orientation stays in `image`,** as the ledger requires (`AnatomicalAxis`
  and `Orientation3D`). `AxisCodes` is a refinement of `Orientation3D`, not a
  new authority.
- **The only new module is `transform`.** The planned `space` module is
  dropped because it would create cycles.
- **Only format codecs and container readers move to `transform/jvm`.**
  Graph-ingestion adapters stay in `spatial`, as adapters over `transform`,
  because they own descriptors built from `Domain`, `Morphism` and routing
  types. That covers `SpatialIngest`, `TransformDescriptor`,
  `FmriprepSpatialGraph` and `SpatialIngest.toSpatialGraph`. Moving them down
  would invert the dependency.
- The `transform` module gets its own boundary task, `transformBoundaryCheck`,
  wired before `transform/Compile/compile` the same way
  `imageAlgebraBoundaryCheck` is.
- Every new module adds `munit-scalacheck`. It is currently on the classpath
  only for `hrf` and `first-level-laws`.

### D1. World spaces are frames

There are two different identities in play, and they must stay separate:

- **`spatial.SpaceRef`** (`Domain.scala:33`) identifies a *sampled domain*:
  this subject's T1w grid, this subject's lh.white mesh, fsLR32k, a latent
  basis.
- **`image.world.WorldSpace`** identifies a *continuous RAS-mm coordinate
  system*: this subject's scanner RAS, their FreeSurfer tkRAS,
  MNI152NLin2009cAsym.

Many domains share one world space. The T1w grid, the functional grid after
coregistration, and the white and pial surfaces all live in subject scanner
RAS. `SpaceRef` gains `def world: Either[SpaceError, WorldSpace]` (only
templates resolve from the reference alone) and `worldIn(native: NativeContext)`
(subject volumes and anatomical surfaces resolve to that subject's native
world; inflated and spherical surfaces have none, and tkRAS is stated
explicitly);
`WorldSpace` is not a copy of `SpaceRef`.

```scala
package scalafim.image.world

/** Scope for subject-level identifiers. Two datasets that both have sub-01 must not collide. */
opaque type DatasetNamespace = String   // BIDS DatasetDOI, else a content hash of dataset_description.json, else explicit

/** Which acquisition anchors a native space. Distinct boldrefs within one session are distinct spaces. */
final case class ReferenceAcquisition private (entities: Map[String, String], geometry: GeometryDigest)
// ReferenceAcquisition(entities, geometry): Either[SpaceError, _] rejects blank entities
// geometry = lossless (dims, selected vox2world affine, qform/sform codes); entities = task/run/acq/echo/...

enum WorldSpace:
  case Template(name: TemplateName)            // globally unique by definition: MNI152NLin2009cAsym, MNI305, fsaverage, fsLR ...
  case SubjectNative(ns: DatasetNamespace, subject: SubjectId, session: Option[SessionId], reference: ReferenceAcquisition)
  case SubjectTkRas(ns: DatasetNamespace, subject: SubjectId, conformed: ReferenceAcquisition)
      // FreeSurfer surface RAS of one subject's conformed volume (orig.mgz); not per hemisphere
  case Declared(token: DeclaredToken)
      // token is minted fresh by WorldSpace.declare(label). The label rides on the token for display and in the
      // frame's FrameMetadata; equality, hashing and the persistent frame id use the token alone
```

**Identity rules:**

- **Namespacing.** Subject and session ids are always scoped by
  `DatasetNamespace`. Only `Template` spaces are global.
- **Reference acquisition.** Two boldrefs in one session that are not in the
  same world space get different `ReferenceAcquisition`s. They then become the
  same world space only through an explicit, identity-certified coregistration
  edge, never by id.
  - Scanner RAS is physically shared within a session. But the identity we
    need is "coordinates that are comparable without a transform", and
    head motion between runs breaks that.
- **Fresh declarations.** `Declared` spaces are fresh by construction.
  `FrameCatalog.declare("my-space")` twice yields two distinct spaces, and a
  label can never collide with a real space.
- **Restoration.** `WorldSpace` is serialised through its persistent
  `FrameKey`. Reloading it produces a new runtime owner, which is re-bound to
  any live frame with the same key through image4s
  `Frame.alignOwners`/`FrameAlignment`. It is never re-bound by label or by
  string comparison.
- **Tests:**
  - accidental collision: `sub-01` in two datasets, and two boldrefs in one
    session, stay distinct
  - independent representations of the same frame bind: a NIfTI header and a
    GIFTI/X5 reference to the same `FrameKey`, loaded separately
  - `declare` is fresh
  - a persisted key round-trips

**Binding to image4s frames.** `Frame` is a `final class` with a private
constructor. It carries a per-instance runtime owner, and it can have a
persistent `FrameKey` (id, rank, unit, convention) (`Identity.scala:69-75,375-395`).
So:

- `FrameCatalog` is the single place that creates world frames. It calls
  `Frame.createPersistent` with a `FrameId` derived from `WorldSpace`, always
  as RAS and millimetres.
- Well-known spaces are stable vals, so they can appear in types:

  ```scala
  object Spaces:
    val MNI152NLin2009cAsym: Frame[D3] = FrameCatalog.template(TemplateName.MNI152NLin2009cAsym)
    // MNI152NLin6Asym, MNI305, fsaverage, fsLR ...
  type Mni2009c = Spaces.MNI152NLin2009cAsym.type
  ```

- A frame decoded from a file is a **different runtime owner** with the same
  persistent key. Binding it to a static space is an explicit, checked step
  that goes through image4s `FrameAlignment`:

  The frame-erased package is a **dependent pair**. The value's type mentions
  the package's own frame, so an MNI frame cannot be paired with a
  subject-space value:

  ```scala
  /** V is a frame-indexed family: [F] =>> Point[F, D3], GridSpec, NeuroVolume[F] ... */
  sealed trait Placed[V[_ <: Frame[D3]]]:
    type F <: Frame[D3]
    val frame: F
    val world: WorldSpace
    def value: V[F]

  object Placed:
    /** The value must already be typed at exactly this frame. */
    def apply[V[_ <: Frame[D3]]](frame: Frame[D3])(value: V[frame.type]): Either[SpaceError, Placed[V]]
    /** Also accepts a decoder's existential value (GridSpec[?]) by capture; the value's runtime frame owner is
      * checked against `frame`, because a wide F (e.g. Frame[D3]) alone would not prove ownership. */
    def of[F <: Frame[D3], V[_ <: Frame[D3]]](frame: F)(value: V[F])(using FrameOwned[V]): Either[SpaceError, Placed[V]]

  extension [V[_ <: Frame[D3]]](p: Placed[V])
    /** Checked retyping to a static frame, through image4s FrameAlignment on persistent keys. */
    def bindTo(target: Frame[D3])(using Rebind[V]): Either[SpaceError, V[target.type]]
  ```

  - Values enter a frame only through the provider's checked constructors,
    for example `Point.in(frame)`, which returns `Either`, or a grid built in
    that frame.
  - `Rebind[V]` is implemented with `FrameAlignment.pointToRight` and
    `Frame.alignOwners`. It fails when the persistent keys differ.
  - Without a static target, code works inside `p.F`.
  - The frame is a type member rather than `frame.type`, because the
    singleton encoding cannot hold a runtime-decoded `GridSpec[?]`: the
    decoded value's type is a capture of the wildcard, not the singleton type
    of its frame (confirmed by `PlacedProbeSuite`).
  - **Negative tests** (`compileErrors`):
    - `Placed(mniFrame, w)(subjectPoint)` does not compile
    - `p.value` cannot be used as a `Point[Mni2009c, D3]` without `bindTo`
  - **Runtime test:** `bindTo` fails on mismatched keys.
- `SpaceResolver` produces the `WorldSpace` for a file. Evidence comes from:
  - the NIfTI sform/qform code, under image4s `NiftiAffinePolicy`
  - the GIFTI `GiftiTargetSpace`
  - the BIDS `space-` entity (bids4s)
  - an explicit override

  Ambiguity is a typed `Left`. For example, sform code `MNI152` without a
  BIDS entity yields `AmbiguousTemplate`.
- `GridSpec` becomes `GridSpec[F <: Frame[D3]]`, and
  `SpatialPullback[T, S] = SpatialMap[T, S, D3]`. `WorldPoint` becomes neuro
  syntax over `Point[F, D3]`. The `Vector[Double]` overloads get typed
  replacements and a deprecation cycle.

### D2. One convention kernel (`scalafim.image.world`)

```scala
enum ToolCoordinates:
  case RasMm                                        // nibabel, FreeSurfer scanner RAS, fMRIPrep
  case LpsMm                                        // ITK/ANTs, AFNI DICOM order
  case FslScaledVoxel(geometry: FslVolumeGeometry)  // voxel * pixdim, x-flipped iff det(FSL-selected vox2world) > 0
  case TkRas(geometry: FreeSurferVolumeGeometry)    // scanner = Norig · Torig⁻¹ · tkRAS
  case VoxelIndex(grid: GridSpec[?])                // LTA VOX_TO_VOX, register.dat inputs

/** FSL's own affine selection (sform if sform_code != 0, else qform, else scaling), recorded explicitly.
  * This can differ from ScalaFIM's NiftiAffinePolicy choice; both are kept, never silently merged. */
final case class FslVolumeGeometry(dims: Dims3, pixdim: Spacing3, fslVox2World: Affine[D3], selected: NiftiAffineSource)

/** Full FreeSurfer volume geometry: Norig = the scanner vox2ras; Torig = the tkr vox2ras
  * derived from dims and voxel sizes. A c_ras translation is only the non-oblique special case. */
final case class FreeSurferVolumeGeometry(dims: Dims3, voxelSize: Spacing3, norig: Affine[D3]):
  def torig: Affine[D3]
  def tkrToScanner: Affine[D3]                      // Norig · Torig⁻¹

object ToolCoordinates:
  def toRas(c: ToolCoordinates): Affine[D3]         // the only LPS flip constant in the codebase
```

Two references pin these definitions. FreeSurfer's CoordinateSystems page
defines tkRAS, and fslpy's `fsl.transform.flirt` defines the FSL scaled-voxel
convention and which affine it uses.

- **The four existing LPS flips are deleted.**
- **Tests:**
  - property laws: `toRas ∘ fromRas = id`, and FSL handedness in both signs
  - FreeSurfer `mri_info --vox2ras` and `--vox2ras-tkr` oracles on an
    **oblique** conformed volume, where a c_ras translation alone would be
    wrong
  - FSL oracles on files whose qform and sform **conflict** in handedness and
    code. These check that FSL's selection is reproduced, not ours.
- **Orientation:**
  - `AxisCodes` refines `Orientation3D`, replacing
    `reorient(..., Seq[String])`.
  - `reorient` returns `Either`. It produces a **new image4s `Grid`/`SampleSpace`**
    through the provider constructors, plus a zero-copy Ravel axis
    permute/flip view of the data.
  - `findAnatomy` stops throwing.

### D3. `WorldTransform[S, T]`: the one user-facing transform value

It is named to avoid reframe4s's `RegistrationResult` family, where
"registration" means an optimisation outcome.

It is admitted under ledger rule 2. The invariant it adds is a
toolkit-independent pullback between *world spaces*, with explicit inverse
provenance. It wraps reframe4s values without copying their state.

It is a sealed ADT keyed by capability, so the forward and inverse maps
cannot drift apart:

```scala
sealed trait WorldTransform[S <: Frame[D3], T <: Frame[D3]]:
  def source: S
  def target: T
  def pull: SpatialMap[T, S, D3]                     // target point -> source point: what resampling needs
  def provenance: TransformProvenance
  def pullPoint(p: Point[T, D3]): Either[TransformError, Point[S, D3]]

object WorldTransform:
  /** Affine: total inverse and fused composition, both delegated to reframe4s-lie. */
  final case class Linear[S <: Frame[D3], T <: Frame[D3]](framed: FramedAffine[T, S, D3], provenance: TransformProvenance)
      extends WorldTransform[S, T]:
    def inverse: Linear[T, S]                          // = Linear(framed.inverse, ...)
    def mapPoint(p: Point[S, D3]): Either[TransformError, Point[T, D3]]

  /** Smooth analytic iso (e.g. rigid, velocity-integrated with exact inverse). push = pull.inverse, never stored. */
  final case class Smooth[S <: Frame[D3], T <: Frame[D3]](iso: SmoothIso[T, S, D3], provenance: TransformProvenance)
      extends WorldTransform[S, T]

  /** Dense or composite map. The forward map exists only if something provided it. */
  final case class Mapped[S <: Frame[D3], T <: Frame[D3]](
      pull: SpatialMap[T, S, D3],
      push: PushAvailability[S, T],
      provenance: TransformProvenance) extends WorldTransform[S, T]

enum PushAvailability[S <: Frame[D3], T <: Frame[D3]]:
  case FromAsset(push: SpatialMap[S, T, D3], asset: AssetRef)  // ANTs InverseWarp, FSL invwarp output
  case Estimated(estimate: InverseEstimate[S, T, D3])          // reframe4s evidence; added in Phase 6
  case Unavailable
```

- **`andThen`** is defined on the trait with frame-checked types:
  - `Linear ∘ Linear` fuses through the `AffineMap.operator` composition, and
    the result is statically typed as `Linear`, via an overload on `Linear`.
  - Every other combination becomes a lazy reframe4s composition.
- **`mapPoint`** exists only where a push exists. `Linear` and `Smooth` have
  it, returning an `Either`. On `Mapped` it is an extension that requires
  `push != Unavailable` and otherwise returns `Left(NoForwardMap)`. Users
  never pass a direction flag.
- **`resample(image: NeuroVolume, onto: GridSpec[T], interp: Interpolation)`**
  is the user-facing entry point (the `resample_volume` equivalent). It
  expands into a single reframe4s `ResamplingPlan` (ledger rule 1). The
  default boundary policy is `CoordinateBoundaryPolicy.Reject`; there is no
  silent identity outside the field.
- **Affine build/decompose helpers** (`build_affine_matrix`,
  `decompose_affine_matrix`) delegate to reframe4s-lie. If a parameterisation
  is missing there, it lands upstream.

### D4. Codecs: faithful files first, then interpretation

**Inputs.** Codecs take a decoded *container*, not raw bytes:

```scala
enum TransformSource:
  case Text(content: String)                       // ITK .txt/.tfm, FLIRT, aff12, LTA, .xfm, register.dat
  case Binary(bytes: IArray[Byte])                 // ITK MATLAB v4 .mat
  case NiftiVolume(header: NiftiHeader, data: NDArray)  // ANTs/FSL/AFNI fields, FNIRT coefficients
  case Hdf5Tree(root: Hdf5Group)                   // ITK composite .h5, X5
```

- The `Text` and `Binary` codecs are pure and **shared**, so they run on JVM
  and JS.
- `NiftiVolume` and `Hdf5Tree` are produced by JVM container readers
  (image4s-nifti, jHDF). Their codecs are shared code, but only JVM can feed
  them. JS gets the text and binary formats, and that is declared as a
  caveat, not skipped silently.
- AFNI BRIK/HEAD is **out of scope**; 3dQwarp is supported as NIfTI output
  only.

**Codecs and interpretations.** Each format has a codec plus an
interpretation. The interpretation's context is a type constructor over the
two frames, so it cannot erase them:

```scala
trait TransformCodec[N]:
  def format: TransformFormat
  def decode(src: TransformSource): Either[TransformIoError, N]
  def encode(native: N): Either[TransformIoError, TransformSource]

/** N: native model. Ctx: what interpretation needs, indexed by both endpoint frames.
  * Out: the result shape, also indexed by the endpoints. */
trait Interpretation[N, Ctx[_ <: Frame[D3], _ <: Frame[D3]], Out[_ <: Frame[D3], _ <: Frame[D3]]]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](n: N, ctx: Ctx[S, T]): Either[TransformError, Out[S, T]]

/** Conversion into a format is a separate capability, with its own matrix (D4b). */
trait Expression[N, Ctx[_ <: Frame[D3], _ <: Frame[D3]], In[_ <: Frame[D3], _ <: Frame[D3]]]:
  def express[S <: Frame[D3], T <: Frame[D3]](w: In[S, T], ctx: Ctx[S, T]): Either[TransformError, N]
```

**Result shapes are explicit per format:**

- `WorldTransform[S, T]` for single transforms.
- `LinearSeries[S, T]` (`Seq[WorldTransform.Linear[S, T]]` with the volume
  index) for AFNI volreg `.aff12.1D` series and MCFLIRT MAT directories. For
  these, `S` is the per-volume moving frame family, and `T` is the base.
- `TransformChain[S, T]` for multi-node X5 and ITK composites where the
  intermediate frames must stay visible for provenance.

Serialisation back to a file (`express`) is a separate capability. Its
matrix is in the next section.

**Binding contexts to endpoints.** Context grids are typed at the endpoint
frames, for example `FslGrids[S, T](source: GridSpec[S], reference: GridSpec[T])`.
Passing a reference grid from another frame therefore does not compile. When
endpoints are existential (runtime conversion, `ConversionContext` below),
the same checks happen at runtime: each grid's persistent key must match the
declared endpoint, or the result is `Left(ContextFrameMismatch)`. That path
is documented as the dynamic fallback.

**Contexts per native model:**

| Native model | `Ctx[S, T]` |
|---|---|
| `ItkTransform`: text, MATLAB v4, HDF5 composite. Covers AffineTransform, MatrixOffset, Euler3D, VersorRigid3D, Similarity3D, ScaleSkewVersor3D, DisplacementField, any number of affines. BSplineTransform → `Left(Unsupported)` | `Frames[S, T]` (just the two frames) |
| `FlirtMatrix`, `FnirtField(Relative \| Absolute)`, `FnirtCoefficients(Quadratic \| Cubic, aff)` | `FslGrids[S, T](source: GridSpec[S], reference: GridSpec[T])` |
| `Aff12Series` (1..n rows) | `AfniContext[S, T](frames, obliquity: CardinalCorrection)` |
| `QwarpField` | `Frames[S, T]` |
| `Lta(kind: LtaKind, src: VolGeom, dst: VolGeom)` | `Frames[S, T]`; the volgeoms are in the file |
| `MniXfm` (`talairach.xfm`) | `Frames[S, T]` |
| `RegisterDat` | `TkRegGrids[S, T](movable: GridSpec[S], target: GridSpec[T])` |
| `X5File` | `Frames[S, T]` |

**Format rules:**

- `LtaKind` is closed: `VoxToVox`, `RasToRas` and `FslReg` are interpreted.
  Anything else is `Left(UnsupportedLtaType)`, never a guess.
- FNIRT relative/absolute detection keeps neurotransform's rule:
  field-of-view test, then Jacobian-volume test. If both are ambiguous it
  returns `Left(AmbiguousDefinitionType)`.
- A volreg or MCFLIRT series is decoded as a `Seq[WorldTransform.Linear]`.
  The motion-parameter files (`.par`, SPM `rp_*`, AFNI dfile) are **out of
  scope here** and belong to the `motion` module, which consumes these
  affines.
- `TransformFormat.detect` inspects content first: NIfTI intent 1007 and
  2006–2009, ITK headers, LTA `type =`, and HDF5 groups. Filenames only break
  ties. The result is typed as `Ambiguous` when neither resolves it.

**Runtime conversion.** For a format known only at runtime, there is a closed
context ADT, and a missing piece is a typed error:

```scala
enum ConversionContext:
  case None
  case FslGrids(source: GridSpec[?], reference: GridSpec[?])
  case TkRegGrids(movable: GridSpec[?], target: GridSpec[?])
  case Afni(correction: CardinalCorrection)

Transforms.convert(src: TransformSource, from: TransformFormat, to: TransformFormat,
                   decodeCtx: ConversionContext, encodeCtx: ConversionContext)
    : Either[TransformError, TransformSource]      // Left(MissingContext(format, needed)) when incomplete
```

This is the `lta_convert` / `c3d_affine_tool` / `convert_xfm` equivalent.
Statically typed users call `interpret`/`express` directly.

### D4b. Two separate guarantees: serialisation fidelity and conversion capability

**1. Native serialisation fidelity (`decode`/`encode` on the native model
`N`).** It is **value-exact**:

- All numeric values, header fields, volgeoms, parameter kinds and component
  order survive.
- It is **not lexical-exact.** Comments, whitespace and number spelling are
  not kept. We re-emit in canonical formatting.
- Byte-exact reproduction would need a lexical trivia model. That is not a
  goal and is not claimed.

**2. Conversion (`express` from a `WorldTransform` into a target format's
`N`).** Each cell below is one of:

- **Exact:** no information added or lost.
- **Needs policy:** requires a user-supplied sampling or fitting policy,
  typed in the context.
- **Unsupported:** `Left(UnsupportedConversion(from, to, reason))`.

| From \ To | ITK affine / FLIRT / aff12 / LTA / xfm / register.dat | ITK h5 composite | Dense field NIfTI (ANTs / FSL / AFNI) | FNIRT coefficients | X5 |
|---|---|---|---|---|---|
| `Linear` | Exact. FLIRT and register.dat also need both grids; LTA vox2vox needs volgeoms | Exact | Needs policy: an output lattice (`SamplingPolicy(on: GridSpec[T])`) | Unsupported (no parameterisation to recover) | Exact |
| `Smooth` (rigid or other analytic iso) | Exact if the iso is affine; otherwise Unsupported | Exact for ITK rigid, versor or similarity kinds | Needs policy: output lattice | Unsupported | Exact for linear kinds |
| `Mapped`, dense on lattice L | Unsupported | Exact **only** as a DisplacementField on L, plus any affine prefix | Exact on L. On another lattice: Needs policy (resampling) | Needs policy: `FittingPolicy(knotSpacing, order, regularisation)`, returning fit residual evidence; **deferred** | Exact on L |
| `Mapped` composite (affine + dense) | Unsupported | Exact | Needs policy: materialisation lattice | Needs policy (fit); deferred | Exact as a node chain |

Tests:

- **Every Exact cell** round-trips through the oracle files.
- **Every Needs-policy cell** has a test that the call without a policy is a
  typed `Left`.
- **Every Unsupported cell** has a test that it is a typed `Left`.

Loaded values keep SHA-256 provenance, as today. `TransformAssetLoader`,
`SpatialIngest` and `ItkAffine.scala` collapse into `transform/jvm` callers of
these codecs.

### D5. Upstream to reframe4s (generic dense-map math)

Develop these with `-Dscalafim.reframe4s.build=../reframe4s`, then bump
`reframe4sRevision`. Items are ordered by who needs them:

1. **Tensor-product B-spline field evaluation** (quadratic and cubic,
   unnormalised, with offset). **FNIRT coefficient decoding (Phase 3)
   depends on this.**
2. **Field composition:** `DenseMap ∘ DenseMap → DenseField` on a chosen
   lattice (the `convertwarp` equivalent).
3. **Numerical inversion:** fixed-point iteration producing
   `InverseEstimate`/`InverseResidual`. It is never a `SmoothIso`. This
   unlocks `PushAvailability.Estimated`.
4. **Jacobian and log-Jacobian determinant fields,** extending
   `TopologyAssessor`. The pull/push mode enum lives in reframe4s; ScalaFIM
   must not name it `JacobianMode` (boundary check).
5. **Jacobian-modulated resampling** (`none | jacobian | sqrtJacobian`). The
   ScalaFIM `JacobianModulation` becomes a policy adapter over it.

`CoordinateBoundaryPolicy` already exists (`DenseMap.scala:25-28`). Choosing
`Reject` as the ScalaFIM default is Phase 3 policy, not upstream work.

### D6. One routing graph

- `spatial.SpatialGraph` is the single neuroimaging graph.
- `atlas.SpaceTransforms` becomes a manifest (template catalog plus
  TemplateFlow assets) that populates it. Its private route search and the
  throwing `ExecutableCoordinateTransformPlan` are deleted.
- Geometric edges hold `WorldTransform`s. Composition is delegated to
  reframe4s.
- **Routing parity with neurofunctor:**
  - inverse edge cost is `cost + penalty·(1 − quality)`
  - forward-first, then inverse fallback
  - `inspectPath`, `allPaths`
  - `usedInverses` in provenance
- **QC parity:**
  - `roundTrip` and `commutes` helpers that report path-difference norms
  - row-sum and weight-sanity metrics
  - field-level `backproject`
  - hybrid per-part compile with block-diagonal assembly

### D7. Surfaces

- Mesh coordinates are `Placed` in `SubjectScanner`, or, for FreeSurfer-native
  surfaces, in `SubjectTkRas`. The tkRAS↔scanner link comes from `c_ras`
  (GIFTI metadata or FreeSurfer headers) through D2.
- **Owner decision (Phase 0).** The barycentric closest-face kernel and the
  mesh sphere helpers (`isSphere`, `setRadius`) are mesh geometry. They live
  in `surface` unless an upstream mesh library (mesh4s) is already the
  authority.
- Remaining work:
  - `surfaceResamplingPlan` for fsaverage↔fsLR through sphere.reg, with
    normalisation `element | sum | none`
  - surface→volume ribbon fill as a compiled operator, using the vol→surf
    adjoint
  - stock template domains: MNI, fsaverage 5/6/7, fsLR 32k/59k/164k
- Overlap metrics (Dice, Jaccard) are generic set math. They go to locus4s,
  or reuse its region algebra, rather than ScalaFIM.

## Phases

**Every phase ends with:**

- `<modules>JVM/test` and `<modules>JS/test` green, run in **bounded batches**
  (never `scalafimTestAll` in one shot)
- `scalafimCompileAll` warning-clean
- a fresh-context review
- a receipt appended below

**Dependencies:**

- Phase 1 → 2 → 3 → 5 → 7 → 8 are sequential.
- Phase 4 runs alongside 3 and 5.
- The upstream track U (D5) starts with Phase 3. FNIRT coefficients (Phase
  3c) wait on U1; `Estimated` inverses and the Jacobian work (Phase 6) wait on
  U3–U5.

### Phase 0: Contract and authority decisions (short)

**Decisions** (ADR `docs/decisions/spatial-transforms.md`):

1. **Frame kernel authority.** image4s-geometry stays the authority for this
   plan. The spatial4s migration is a separate upstream effort, and nothing
   here waits on it.
2. **Naming.** `WorldTransform`, `WorldSpace` and `FrameCatalog`; these are
   collision-checked against `modules/`. The ADR records why the name is not
   `Registration`.
3. **`Hemisphere`.** Unify surface's `Left/Right/Both/Unknown` with atlas's
   `Left/Right/Bilateral/Midline`, or document why they differ. `WorldSpace`
   no longer needs either.
4. **Owners** for the barycentric kernel and for overlap metrics (D7).
5. **Fixture licensing and tooling:**
   - neurotransform oracles are MIT, same author; copy them with their
     manifests.
   - The FreeSurfer Docker oracle needs a license file on the generating
     machine only, never in the repo or CI.

**Build and docs:**

- Add the `transform` module, the edges `atlas → spatial` and
  `atlas → transform`, `transformBoundaryCheck`, and the aggregates, aliases
  and `README.md` blurbs.
- Correct the stale `docs/module-relations.md:316-356`.

**Exit:** the ADR, and the empty module compiling on JVM and JS.

### Phase 1: World spaces and end-to-end frame typing

- Move `SubjectId`, `SessionId` and `TemplateName` into
  `scalafim.image.world`.
- Implement `WorldSpace`, `FrameCatalog`, `Spaces.*`, `Placed`/`Rebind` and
  `SpaceResolver`.
- Add `SpaceRef.world`.
- Replace the constant persistent frame id.
- Add `GridSpec[F]`, `SpatialPullback[T, S]` and typed `WorldPoint` syntax.
- Add identity-bearing loaders: `Nifti.readVolumeIn`/`readSeriesIn` take
  `SpaceEvidence`, add the header's selected xform code, run
  `SpaceResolver.resolveKnown`, and re-identify the geometry with
  `SampleSpaces.inWorld`. Unresolved, ambiguous or contradictory evidence is a
  typed `NiftiImageReadError.Space`.
- **Migrate:**
  - `SpatialPullbacks`, `ResamplingPlan`
  - `Resample`: remove the `method: String` / `engine: String` overloads in
    favour of reframe4s `Interpolation`
  - `ViewerScene`: per-layer frames with checked alignment
- Consolidate the three bounding-box types into one typed box over
  `Point[F, D3]`. If image4s has one, use it.

**Tests:**

- `compileErrors(...)` negative tests:
  - cross-frame `andThen` is rejected
  - a native point passed where an MNI point is expected is rejected
- `bindTo` succeeds on equal persistent keys and fails otherwise
- resolver tests for every NIfTI and GIFTI code, and for each ambiguity error
- the existing image, spatial and atlas suites stay green without edits to
  their assertions

**Exit:** a grep gate in `transformBoundaryCheck` and
`imageAlgebraBoundaryCheck` proves no public signature outside the IO packages
mentions `Frame[D3]` erased. Both tasks run `project/FrameErasureGate.scala`,
which matches a `Frame[D3]` type argument of `SpatialMap`, `SpatialPullback`,
`GridSpec`, `Point`, `Vec`, `Grid`, `SampleSpace`, `WorldBox` and
`WorldTransform`, and reframe4s `FrameErasedMap[D3]`, across line breaks and
outside comments and strings. Only `private`/`private[this]` definitions,
method-local code, IO packages and a path allowlist are exempt, and the gate
self-tests its matcher on known-bad and known-good snippets before every run.

**Deferred: world identity of legacy reads.** `Nifti.readVolume`/`readSeries`,
`NiftiHeader.space` and `SampleSpaces.make` still place D3 geometry in
`WorldSpace.Unresolved` (frame id `scalafim-ras-d3`), so two subjects' native
volumes, or a native volume and a template, align with each other, including
in the viewer's `LayerAlignment.check`. Giving each legacy read a distinct
frame would break same-subject alignment across the codebase, which relies on
independently read files sharing one frame. Callers that need identity use
`readVolumeIn`/`readSeriesIn` or `SampleSpaces.inWorld`. Migrating the legacy
callers (dataset ingest, viewers, examples) onto evidence-bearing reads is a
follow-up ticket, not part of P1.07.

### Phase 2: Convention kernel and orientation

- Implement `ToolCoordinates`, and delete the four LPS flips.
- Implement `AxisCodes`, and a typed `reorient` on data plus grid.
- Make `findAnatomy`, `Deoblique` and `outputAlignedSpace` return `Either`.

**Tests:**

- property laws for every convention pair
- nibabel `aff2axcodes`/`io_orientation` fixtures
- neuroim2 reorientation fixtures, covering data values *and* affine

These fixtures are text or embedded, so they run on JS too.

### Phase 3: `WorldTransform` and read codecs

- **3a.** `WorldTransform` (Linear, Smooth, Mapped), `PushAvailability`
  (without `Estimated`), `TransformProvenance`, `resample`, and the `Reject`
  boundary default.
- **3b. Text and binary codecs (shared):**
  - ITK text and MATLAB v4, with all the listed parameterisations
  - FLIRT
  - aff12 series, with cardinal correction
  - LTA
  - `.xfm`
  - `register.dat`
- **3c. Container codecs (JVM-fed):**
  - ITK HDF5 composite: generalise the jHDF reader to any number of affines
    and the rigid/similarity types.
  - ANTs displacement NIfTI in 4D and 5D, intent-checked.
  - FNIRT dense (relative and absolute, with detection).
  - FNIRT coefficients: cubic and quadratic, `--aff`, rejecting reflecting
    matrices and DCT files. **Gated on U1.**
  - 3dQwarp.
  - X5: linear and densefield nodes, multi-node chains.
- **3d.** `TransformAssetLoader`, `SpatialIngest`, fMRIPrep ingest and
  `ItkAffine.scala` collapse onto the codecs.

**Exit:** each codec lands **in the same change as its Phase 4 oracle
tests**.

### Phase 4: Native-tool oracle fixtures (alongside Phases 3 and 5)

**Layout:**

- Container fixtures (NIfTI, HDF5):
  `modules/transform/jvm/src/test/resources/scalafim/transform/oracle/<tool>/`
- **Text and binary oracles** are also emitted by the generators as
  **generated shared Scala sources**: string or byte literals plus expected
  points. The shared codec tests then run on JS.
- **Container oracles (NIfTI, HDF5)** additionally get a **decoded shared
  fixture**. The generator writes the already-decoded `TransformSource`:
  - NIfTI header fields and the field `NDArray` values, as a compact generated
    source or a small shared binary resource read through a cross-platform
    loader
  - the same expected points and tolerances as the JVM test

  The interpretation, convention and numerical parity of **every** format is
  then verified on JS. Only container *parsing* (NIfTI bytes →
  `TransformSource`, HDF5 → `Hdf5Tree`) is JVM-only.
- The generators keep fixture grids small enough (on the order of 7×8×9×3) to
  embed. A JVM test asserts that each decoded shared fixture equals what the
  JVM container reader produces from the real file. This ties the two
  together.
- Each directory has a `manifest.json` recording tool version, command lines
  and SHA-256 hashes. Generators live in `tools/transform/`, each running in a
  pinned Docker image.

**Imported from neurotransform:**

| Oracle | Source | Tolerance |
|---|---|---|
| `itk_oracle` | SimpleITK 2.5.6; oblique unequal-spacing grid; `.tfm/.mat/.h5`; both composite orders | points 1e-9 (NIfTI 2e-6), intensity 2e-5 |
| `afni_oracle` | AFNI 26.1.04; forward/inverse `.aff12`, landmarks, vecwarp, volreg series with negative zeros | 1e-9 to 1e-12 |
| `fsl_dense_oracle` | FSL 5.0.9 convertwarp/applywarp; 8 handedness × relative/absolute cases | coordinates 3e-5 mm, intensity 3e-6 |
| `fsl_coef_oracle` | 10 cases: cubic/quadratic × handedness × `--aff` | 1e-4 mm |

**New oracles:**

| Oracle | Generator | Pins |
|---|---|---|
| `freesurfer_oracle` | FreeSurfer 7.x: `lta_convert`, `mri_vol2vol`, `tkregister2 --noedit`, `mri_info --vox2ras-tkr` | LTA types, `.xfm`, `register.dat`, tkRAS |
| `ants_oracle` | ANTs 2.6.x `antsApplyTransformsToPoints`; a real 5D `*Warp.nii.gz`; Euler/Versor/Similarity `.mat` | 5D NIfTI, ITK parameterisations, composites with an inverse warp |
| `fsl6_oracle` | the FSL 6.0.x version of the fsl_coef and fsl_dense cases | records whether FSL 6 FNIRT semantics differ from 5.0.9 |
| `nitransforms_crosscheck` | nitransforms (pinned) | a **cross-implementation consistency check**, not a native oracle. The X5 direction is "consistent with nitransforms"; X5 is a draft spec |

**Writer acceptance (Phase 5).** These cannot be frozen fixtures, because
they test our current output. The protocol:

- Writer outputs are committed as **pinned-bytes goldens**.
- A `tools/transform/writer-acceptance.sh` script runs `flirt -applyxfm`,
  `antsApplyTransforms`, `3dAllineate -1Dmatrix_apply` and
  `mri_vol2vol --lta` on those goldens in Docker, and records a signed
  receipt.
- A CI check fails if a writer's golden bytes change without a fresh receipt.
- The native tools are not required in CI.

**Rules:**

- Oracles must break symmetry: oblique, anisotropic, non-square grids with
  shear.
- Each coordinate test uses at least eight asymmetric points, including
  points off the grid centre.
- Jacobian checks use a median relative error < 0.01 **and** a max interior
  error bound.
- Keep the existing SimpleITK HDF5 fixtures.
- Replace the hand-written temp-file affine fixtures and the
  self-round-trip ANTs warp test.

### Phase 5: Writers and conversion

- `encode` for every read format.
- `Expression` for every **Exact** and **Needs-policy** cell of D4b, except
  FNIRT coefficient fitting, which is deferred.
- `Transforms.convert` with `ConversionContext`.
- Displacement-field writers in the ANTs (5D, LPS), FSL (relative or absolute)
  and AFNI encodings, promoted from the `private[scalafim]` test writer.

**Tests:**

- `decode ∘ encode` is **value-exact** on every oracle file (D4b; not
  lexical-exact)
- every cell of the D4b conversion matrix: Exact cells round-trip; Needs-policy
  and Unsupported cells return typed `Left`s
- the writer-acceptance receipts

### Phase 6: Warp algebra adapters (after U2–U5)

- `WorldTransform` operations:
  - `materialize(on: GridSpec[T])`
  - `invertNumerically(policy)`, which yields `PushAvailability.Estimated`
  - `jacobianDeterminant(on)`, `logJacobian(on)`
  - modulated `resample`

**Qualification contract.** Each `WorldTransform` operation has a stated
gate.

**Composition to a field.**

- Match `convertwarp` and ANTs composite outputs on the fixture lattices.
- Points where any stage leaves its coverage are reported under the
  `Reject` / `Constant` / `PreserveSource` policy, never silently filled.

**Numerical inverse (`Estimated`).** The evidence record has these fields:

- **Evaluation domain:** the lattice points of `S` whose forward image lies
  inside the coverage of the pull map. It is reported as a mask and a
  coverage fraction.
- **Residual in both directions:**
  - `‖pull(push(x)) − x‖` for x in the domain of `S`
  - `‖push(pull(y)) − y‖` for y in the target lattice
- **Residual statistics:** mean, p99 and max per direction.
- **Per-point status:** `Converged | MaxIterations | Diverged | OutsideCoverage`,
  with counts for each.
- **Boundary behaviour:** which policy applied, and at how many points.

Gates:

- **Coverage** is at least the policy threshold.
- **Max and p99 residuals** are within tolerance on the interior.
- **Divergence:** zero diverged points in the interior.
- **Failure:** any of the above not met is a typed failure carrying the
  evidence. It is never a degraded success.

Fixtures:

- **Checked against native tools:** fsl_dense, ANTs (against `InverseWarp`),
  and FSL `invwarp` output.
- **Analytic:** a smooth sinusoidal displacement with a known inverse bound.

**Jacobian determinant and log-Jacobian.**

- **Analytic cases first:**
  - an affine field has a constant determinant equal to `det(A)`
  - radial scaling has a known closed form
  - a composed pair satisfies the chain rule
- **Against FSL `--jac`:** median relative error < 0.01, **p99 and max
  interior error bounds**, and an error map saved as a test artefact on
  failure.
- **Folds.** Points where `det ≤ 0` are reported as a fold mask and count.
  The log-Jacobian there is a typed `NonPositive` status (a masked value), not
  `NaN` or `-Inf`. A fold-injected synthetic field tests this.

**Modulated resampling: two distinct laws, tested separately.**

- `jacobian` modulation **preserves the integral of a density** (the sum of
  intensity times voxel volume), up to discretisation tolerance.
- `sqrtJacobian` modulation **preserves the squared L2 norm**, ∫|f|², the law
  for amplitude-like quantities.
- Each is tested on synthetic compressive and expansive fields. Each test
  also checks that the *other* law is **not** preserved, which proves the
  modes are not interchangeable.

### Phase 7: Graph unification and neurofunctor parity

Implement D6. Also pull forward from D7, because the surface scenario below
needs them:

- the barycentric closest-face kernel
- sphere helpers
- `surfaceResamplingPlan` (fsaverage↔fsLR)
- stock template surface domains

- TemplateFlow MNI152NLin6↔2009c H5 steps move from `Planned` to executable.

**Tests:**

- **neurofunctor law fixtures** exported as triplets (0-based, R commit hash
  recorded), following `docs/plans/neurofunctor-support.md`.
- **Scenario-harness contracts** (`docs/plans/scenario-parity-harness.md`:
  each returns one `ScenarioResult`, clean `Pass` by default):
  - **fMRIPrep chain:** boldref→T1w (LTA or ITK) → MNI (H5 composite), using
    demo1 real outputs.
  - **FSL chain:** example_func→highres (FLIRT) → standard (FNIRT
    coefficients), compared against FSL `applywarp`.
  - **Surface chain:** volume→fsaverage→fsLR32k, including commutativity
    against the direct route.

### Phase 8: Surfaces, viewers, documentation

- **Surfaces:** the remainder of D7:
  - tkRAS/scanner placement of meshes
  - surface→volume ribbon fill operator
- **Viewers:**
  - `image-view` layers carry their own frames, with explicit alignment.
  - The linked surface↔volume cursor uses `WorldTransform`.
  - The surface camera uses typed points.
- **Documentation:** a "Coordinate spaces and transforms" guide with compiled,
  verified examples:
  - reading an fMRIPrep and an FSL registration
  - converting FLIRT↔ITK↔LTA
  - mapping an MNI peak to subject volume and surface
  - explaining why a dense warp has no `mapPoint` without an inverse
- **Housekeeping:** update `README.md` and `docs/module-relations.md`. Ask
  reframe4s to remove halfflow's private geometry copy.

## Scope

**In scope:** every format and feature in the ledger below.

**Explicitly out of scope, each rejected with a typed error where it could be
met:**

- ITK BSplineTransform
- AFNI BRIK/HEAD containers
- FreeSurfer `.m3z` morphs
- motion-parameter files (owned by `motion`)
- elastix parameter files

## Parity ledger (target state)

| Capability | neurotransform | neurofunctor | ScalaFIM phase |
|---|---|---|---|
| RAS/LPS, tkRAS, FSL scaled voxel | ✓ | – | 2 |
| qform/sform policy and template identity | ✗ | partial | 1 (better) |
| Orientation codes, reorient data | ✗ | – | 2 (better) |
| ITK text/mat/h5 read+write; rigid/similarity parameterisations | affine only | – | 3, 5 (better) |
| ANTs 5D displacement NIfTI | ✓ | – | 3, 5 |
| FLIRT, FNIRT dense + coefficients | ✓ | – | 3 (+U1), 5 |
| AFNI aff12 series, oblique correction, 3dQwarp (NIfTI) | ✓ | – | 3, 5 |
| LTA | partial (guesses unknown types) | – | 3, 5 (strict) |
| `.xfm`, `register.dat` | ✗ | – | 3, 5 (better) |
| X5 | ✓ (direction unverified) | – | 3, 5 (cross-checked) |
| Volume resampling through a transform | ✓ | – | 3 |
| Affine build/decompose | ✓ | – | 3 (delegated to reframe4s-lie) |
| Warp composition to a field | dead code | – | U2, 6 |
| Numerical warp inverse | ✗ | – | U3, 6 (better, with evidence) |
| Jacobian / log-Jacobian fields, modulation | ✓ | – | U4–U5, 6 |
| Surface resampling (barycentric sphere) | ✓ | ✓ | 8 |
| Routing with inverse quality, inspect | – | ✓ | 7 |
| Round-trip / commutativity QC | – | tests only | 7 (helpers) |
| Hybrid block assembly, backproject | – | ✓ | 7 |
| Overlap metrics | ✓ | – | 7 (in locus4s) |
| Typed frames, compile-time composition | ✗ | ✗ | 1 (Scala only) |

## Risks

- **Frame typing leaking into every signature.** Mitigation: use
  `Placed`/`bindTo` at IO edges, and static types only for known spaces. Run
  a `scala-type-discipline` review after Phase 1.
- **Moving identifier types down into `image`** touches `spatial` and `atlas`
  imports. Mitigation: do it as one mechanical commit at the start of Phase 1,
  and nothing else in that commit.
- **The JS gap for NIfTI and HDF5 transforms** is declared. The codecs are
  shared, so JS gains those formats once the container readers exist.
- **Upstream latency.** U1 blocks only FNIRT coefficients; U2–U5 block only
  Phase 6. Everything else proceeds.
- **Concurrent writers in this worktree.** Stage with explicit pathspecs only.

## Receipts

_(append per phase: date, commits, test commands run, results)_

### P0: contracts, skeleton, vendored oracles (closed 2026-09-24)

- **Commits:** ecc72f64 (plan, epic map, ADR), 7579ca65 (transform module,
  build edges, transformBoundaryCheck, vendored neurotransform oracles with
  SHA-256 provenance).
- **Verification:** transformJVM/test and transformJS/test; spatial and atlas
  compile on JVM and JS against the new edges.

### P2: convention kernel and orientation (closed 2026-09-24)

- **Commits:**
  - 85354a3d: ToolCoordinates with a single LPS flip; FSL and FreeSurfer
    geometry; oracles from fslpy and nibabel.
  - f028c03f: tkRAS uses FreeSurfer's fixed LIA Torig. This corrects an
    earlier derivation.
  - 23ff65b5: AxisCodes and data-and-affine reorientation, checked against
    nibabel.
  - 6bdd2dd1: typed deoblique and aligned space.
  - ffbeeb45: duplicate LPS flips removed.
- **Verification:** image JVM 353 and JS 333; transform JVM and JS; the
  convention and orientation oracle suites pass on both platforms;
  scalafimCompileAll has 0 warnings and 0 errors.
- **Caveats:**
  - Native mri_info on an oblique volume is pending (P4.02).
  - Native flirt outputs are pending (P4.04).


### P7.07: scenario contracts for the fMRIPrep, FSL and surface chains (2026-09-27)

- **Suites** (registered in `docs/scenarios/manifest.json`; each returns one
  `ScenarioResult` and runs every convention mutation, which must fail):
  - `transform.fmriprep-chain.v1` (`FmriprepChainScenarioSuite`):
    boldref -> T1w (ITK rigid) -> template (ITK HDF5 composite) against
    SimpleITK `TransformPoint` and `Resample`, a gated numerical-inverse round
    trip, and SHA-256 provenance. **PassWithCaveats**, caveat
    `transform.itk-border-band`: ITK holds a displacement field's (and an
    image's) border value for half a voxel outside the lattice, reframe4s
    blends across the first voxel; voxels in that band are not compared.
  - `transform.fsl-chain.v1` (`FslChainScenarioSuite`): FLIRT -> FNIRT
    `--cout --aff` materialized on the standard lattice, against FSL 5.0.9
    `applywarp` ramps and phantom, a FLIRT oracle built from a declared world
    registration, the Jacobian chain rule (a law check), and
    `fnirtfileutils --jac`. **PassWithCaveats**, caveats
    `transform.fsl-jacobian-spline-vs-finite-difference` (statistical
    Jacobian gates) and `transform.fsl-chain-no-native-flirt-convertwarp`
    (no native FLIRT or `convertwarp --premat` output for the pair).
  - `surface.volume-to-template-mesh-chain.v1` (`SurfaceChainScenarioSuite`):
    scanner volume -> tkRAS ribbon (`toScanner`, `RibbonOperator`) ->
    registered sphere -> fsaverage-like -> fsLR-like, commuting with the
    direct route. **PassWithCaveats**, caveats
    `surface.mni152-nlin6-nlin2009c-bridge` (bead
    bd-01M37FQFRRF1TW2REJPWS8BRM8) and `surface.ribbon-overlap-metrics`
    (P7.05).
- **Build:** `transform` and `surface` gain a test-scope edge on
  `scenario-testkit`.
- **Verification:** transformJVM 149 and transformJS 125; surfaceJVM 146 and
  surfaceJS 118; scalafimCompileAll 0 warnings; manifest validator passes.
- **Deviation from the Phase 7 text:** the fMRIPrep chain uses the synthetic
  SimpleITK composites, not demo1 outputs. The surface chain uses synthetic
  icospheres, not fsaverage or fsLR32k meshes.


### P7.04 graph wiring and the P7.07 template leg (2026-09-27)

- **Graph wiring (P7.04):** `TemplateSurfaceSampling` (atlas) gives a
  transform graph's template surface domains the vertices of loaded
  `TemplateSphere`s (JVM: `TemplateSurfaceSamplingFiles.onFsAverage`). A
  sphere-resampling step between two sampled spaces carries its
  `TemplateResamplingPlan` as `CoordinateMap.SphereResampling` (spatial) and
  becomes available. `SpaceTransformGraph.vertexOperator` compiles the route
  with the mixed pullback compiler, which now composes weighted surface rows.
  A space without a sphere stays unsampled, and routes through it remain a
  typed `TransformNotExecutable`. Through the graph, fsaverage <-> fsLR 32k and
  fsaverage -> fsaverage5 match Workbench 2.2.1 within the surface budgets.
- **Template leg (P7.07):** `surface.volume-to-template-mesh-chain.v1` moved to
  atlas, the lowest module that sees both the bridge and the graph. On the JVM
  with a TemplateFlow cache, the leg samples an MNI152NLin2009cAsym volume at
  the fsLR 32k left midthickness, carried to 2009c by `MniTemplateBridge`'s
  forward map. The sampled values equal the exact chain value
  `h(pull(push(p)))` to 1.4e-13. The vertex round trip is 0.081 mm max and
  0.019 mm p99, against the inversion gates of 0.5 and 0.05. The value against
  the 6Asym field is 0.10 max (budget 0.76) and 0.018 p99 (budget 0.076). The
  leg anchors the bridge to SimpleITK: pull 3e-14 mm, push 0.010 mm. It then
  routes a sphere-linear probe fsLR 32k -> fsaverage through the graph, with
  error 2.8e-4 against a gradient-times-sagitta bound of 3.5e-4. Three new
  mutations fail it: the two MNI templates taken as one (5.15), the bridge run
  backwards (9.32), and nearest-vertex sphere resampling (0.027).
- **Caveats:** `surface.mni152-nlin6-nlin2009c-bridge` is gone. Where the
  assets are absent (always on Scala.js), the scenario emits the declared caveat
  `surface.template-leg-assets-absent` instead, and the template mutation test
  is skipped, not passed. A cached asset that is refused fails the scenario,
  even when another asset is missing.
- **Commits:** 6d4691b2 (graph wiring), d4a7f47e (template leg), and a review
  follow-up. The follow-up renormalises volume-root rows over the bridge share
  that hit the volume, adds the exact-chain, round-trip and sphere-probe
  observations, and ties a step's plan to its spaces.
- **Verification (2026-09-27, with a TemplateFlow cache):**
  - `sbt surfaceJVM/test spatialJVM/test atlasJVM/test`: 155, 219 and 128
    passed.
  - `sbt spatialJS/test`: 195 passed.
  - `sbt atlasJS/test surfaceJS/test`: 87 passed with 1 skipped (the template
    mutation test), and 121 passed.
  - `sbt scalafimCompileAll`: 0 warnings. The manifest validator passes.
- **Not done:** `vertexOperator` takes no hemisphere; a graph is sampled for one
  (`sampling.hemisphere`). `TemplateSurfaceSampling` refusals are one string.
  Plans are computed eagerly at graph build.
