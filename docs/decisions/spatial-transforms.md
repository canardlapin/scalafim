# ADR: Spatial transforms

- **Status:** accepted for implementation (2026-09-24).
- **Epic:** STP (`bd-01M39Q2YNKRTJTD8PZ4694DS52`). Packet P0.01.
- **Plan:** [`docs/plans/spatial-transform-parity.md`](../plans/spatial-transform-parity.md).
  Ticket map: [`docs/plans/spatial-transform-parity-epic.md`](../plans/spatial-transform-parity-epic.md).

The user delegated these decisions by setting the goal "implement the epic" and
asking not to be consulted before each step. Each one takes the
recommendation from the plan. Any of them can be reopened by a note on the
epic; reopening one blocks the packets that depend on it.

## 1. Frame kernel authority

**Decision.** image4s-geometry stays the sole authority for `Dim`, `Frame`,
`Point`, `Vec`, `Affine` and `Grid` throughout this epic.

**Why.**
- ScalaFIM already pins image4s `26a74ad`.
- spatial4s is pre-release, has two commits, and has no consumers.
- Moving image4s and reframe4s onto spatial4s is an upstream change that
  would block every phase here.

**Consequence.**
- No STP packet depends on spatial4s.
- If spatial4s later becomes the kernel, image4s supplies the compatibility
  aliases, and ScalaFIM changes imports only.

## 2. Names

| Name | Meaning | Collision check |
|---|---|---|
| `WorldTransform[S, T]` | the user-facing transform value | none in `modules/`, image4s or reframe4s |
| `WorldSpace` | identity of a continuous RAS-mm coordinate system | none |
| `FrameCatalog` | the only creator of world frames | none |
| `Placed[V]` | a frame-erased dependent pair | none |
| `TransformFormat`, `TransformCodec`, `TransformSource` | the format layer | `SpatialIngest.TransformFileFormat` exists and is migrated in P3.11 |

**Not `Registration`.** reframe4s-register defines `RegistrationResult`,
`RegistrationFailure` and `BidirectionalRegistrationResult`. There,
"registration" means the outcome of an optimisation. A `WorldTransform` is a
geometric map between named spaces, however it was obtained. Keeping the
words apart avoids a false reading, such as treating a transform read from an
FSL `.mat` as a registration result carrying convergence evidence.

`scalafim.spatial.SpaceRef` and `scalafim.image.world.WorldSpace` are
deliberately different:

- A `SpaceRef` names a *sampled domain*: a grid, a mesh or a latent basis.
- A `WorldSpace` names the *continuous coordinate system* that domains live
  in. Many domains share one world space.

## 3. `Hemisphere`

**Decision.** Keep both enums, because they mean different things.

- `scalafim.surface.Hemisphere` (`Left | Right | Both | Unknown`) says which
  mesh a surface value belongs to.
- `scalafim.atlas.Hemisphere` (`Left | Right | Bilateral | Midline`) says
  where an anatomical region lies. `Midline` is not a mesh, and `Unknown` is
  not an anatomical location.

`WorldSpace` needs neither: FreeSurfer tkRAS belongs to a subject's conformed
volume, not to a hemisphere. P8.04 adds a scaladoc cross-reference between
the two enums, so readers see the distinction.

## 4. Owners of mesh kernels and overlap metrics

- **Barycentric closest-face lookup and the sphere helpers** (`isSphere`,
  `setRadius`) live in **`scalafim.surface`**.
  - mesh4s is pre-alpha and not a ScalaFIM dependency. Admitting it is its
    own provider-admission decision, outside this epic.
  - If mesh4s is admitted later, these kernels move there together with the
    mesh types.
- **Overlap metrics** (Dice, Jaccard, and volume and label overlap) are
  generic set measures.
  - They belong with the region algebra, which is locus4s. locus4s has no
    overlap metrics today.
  - P7.05 adds them upstream and bumps the pin.
  - If an upstream change cannot land, P7.05 stays open with a declared
    caveat. ScalaFIM does not grow a private copy.

## 5. Out of scope

Each of these is rejected with a typed error at the point it could be met,
never with a guess:

| Item | Where it is rejected |
|---|---|
| ITK `BSplineTransform` (text, `.mat`, `.h5`) | `TransformIoError.UnsupportedItkTransform` |
| AFNI BRIK/HEAD containers | `TransformFormat.detect` → `Unsupported(AfniBrik)` |
| FreeSurfer `.m3z` morphs | `Unsupported(FreeSurferM3z)` |
| Motion-parameter files (`.par`, SPM `rp_*`, AFNI dfile) | owned by `motion`, which consumes affine series from this epic |
| elastix parameter files | `Unsupported(Elastix)` |
| Fitting FNIRT coefficients from a dense field | `Needs policy` cell in the D4b conversion matrix, deferred |
| Writing ITK HDF5 or X5 files | `UnsupportedConversion` for ITK HDF5; X5 converts only to an in-memory model. Writer acceptance (P5.03) showed ITK and nitransforms need variable-length HDF5 strings, which jHDF (through 0.13.0) cannot write. Both formats stay read-only until an HDF5 writer with variable-length strings is available. |

## 6. Oracle tooling and licensing

- **neurotransform fixtures.** neurotransform is MIT, with the same author.
  Its native-tool oracle fixtures are vendored with their manifests and the
  neurotransform commit hash (P0.04).
- **FreeSurfer license.** It is required only on the machine that generates
  FreeSurfer oracles. It never goes into the repository, CI or a fixture
  manifest.
- **Tooling on the implementation machine as of 2026-09-24:**
  - FSL, ANTs, AFNI and FreeSurfer are not installed.
  - The Docker client is present, but the daemon does not answer.
  - `uv` is available, so Python generators run with
    `uv run --with nibabel,SimpleITK,nitransforms,h5py`.
- **Consequences:**
  - SimpleITK, nibabel and nitransforms generators run now.
  - The new native oracle sets stay open until a machine with Docker or the
    native tools is available. They are the FreeSurfer 7 set (P4.02), the
    ANTs 2.6 set (P4.03), the FSL 6 set (P4.04) and writer acceptance
    (P5.03).
  - Codecs whose only native evidence would come from those sets land with
    the imported neurotransform oracles and SimpleITK/nitransforms
    cross-checks. The owning P4 packet records the missing native coverage
    as an explicit caveat and keeps it open. The gate does not close on
    cross-checks alone.

## 7. Placement (restates plan D0)

- Frame identity, conventions and orientation live in `image`, under
  `scalafim.image.world`.
- Formats, `WorldTransform` and conversion live in the new `transform`
  module.
- Graph ingestion stays in `spatial`.
- Edges:
  - `transform` → `image`, reframe4s
  - `spatial` → `transform`
  - `atlas` → `spatial`, `transform`

## 8. World identity of loaded data (STP P1.07 review)

**Decision.** Identity-bearing reads are explicit; the legacy reads keep the
shared unresolved world for now.

- `Nifti.readVolumeIn`/`readSeriesIn` resolve a file's world space from its
  selected xform code plus caller evidence (BIDS `space-`, a `NativeContext`,
  an assertion) and re-identify the geometry in that world. Unresolved or
  contradictory evidence is a typed error.
- `Nifti.readVolume`/`readSeries` and `SampleSpaces.make` keep
  `WorldSpace.Unresolved`. Every file read this way aligns with every other,
  which same-subject pipelines rely on. That alignment is not evidence that two
  spaces coincide, and the scaladoc says so.
- **Deferred:** moving the legacy callers onto evidence-bearing reads. Until
  then, two subjects' natively read volumes still align.

**Assertions do not override evidence.** A caller's asserted world space must
agree with the BIDS label (a native label admits only a subject-native
assertion) and with the xform code, under the rules a BIDS-named space meets.

**A declared space is its token.** The label is presentation metadata carried
by the token and the frame's `FrameMetadata`; it is not encoded in the
persistent frame id and does not take part in equality.
