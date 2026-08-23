# Native image data model

- Status: accepted for implementation
- Date: 2026-08-23
- ScalaFIM epic: `bd-01KYNR99SWCFEW94T4JGD9QPBS`
- Contract issue: `bd-01M0QG00P8A73RG1F6K6SJGH8A`
- Atlas follow-up: `bd-01M0NS3FBVXXG1CRQWSH60EAKC`
- Admitted providers: Ravel `9c5669399ab8e2a11402e71973dd5f1e2f2c13f4`, image4s `6b9016b7aa622df8dec3d887080ee512fa7439c6`, reframe4s `357426b4fd1e35ddead0068375016f55b082c9e2`, and locus4s `58c9739be51345ad9adc4bc9c9e7335023254ec9`

## Context

ScalaFIM already stores dense images in image4s `Sampled` values backed by
Ravel arrays, but its public behavior still preserves an older array model.
A Scala `Array` enters in first-axis-fastest order, is transposed into Ravel,
and `linear(i)` converts the same historical ordinal back to coordinates.
Several ROI, sparse, cluster, and domain types separately encode concepts now
owned by locus4s and image4s-locus. That arrangement pays conversion costs,
weakens semantic types, and leaves two notions of logical order.

There is no compatibility requirement for that API or ordering. This decision
therefore specifies the image layer we would choose from scratch.

## Decision

ScalaFIM does not own a generic image container, dense array hierarchy, or
voxel-domain algebra. It exposes neuroimaging-specific opaque refinements and
algorithms over the provider types that already own those responsibilities.

| Layer | Sole responsibility |
| --- | --- |
| Ravel | Dense values, rank, shape, strides, immutable views, builders, and generic numeric kernels |
| image4s | Grids, sample spaces, non-spatial axes, metadata, value semantics, sampled images, and image views |
| locus4s and image4s-locus | Exact grid domains, indices, regions, ordered selections, fields, sections, and exact maps |
| reframe4s | Generic coordinate transforms and resampling machinery |
| ScalaFIM image | Neuroimaging names, typed scientific policy, algorithms, and format boundaries |

A new ScalaFIM abstraction is permitted only when it adds a neuroimaging
invariant that none of those providers can express. It must not copy or hide
provider storage merely to offer different method names.

## Dense public types

The conceptual signatures are:

```scala
opaque type NeuroVolume[
    S <: SampleSpace[?, D3],
    A,
    Sem
] = Sampled[S, A, Sem, Rank[3]]

opaque type NeuroSeries[
    S <: SampleSpace[?, D3],
    A,
    Sem
] = Sampled[S, A, Sem, Rank[4]]

type ScalarVolume[S <: SampleSpace[?, D3], A] =
  NeuroVolume[S, A, Continuous]
type LabelVolume[S <: SampleSpace[?, D3], A] =
  NeuroVolume[S, A, Categorical]
type MaskVolume[S <: SampleSpace[?, D3]] =
  NeuroVolume[S, Boolean, Mask]
```

The exact aliases may vary with provider syntax, but these properties are
mandatory:

- the opaque value has exactly the representation of its `Sampled` value;
- the public type retains the concrete sample-space owner `S`;
- construction from a known sample space returns a value parameterized by
  `space.type`;
- dynamic I/O returns a named existential package such as
  `SomeNeuroVolume[A, Sem]`, never a fake static owner;
- `NeuroSeries` validates one and only one non-spatial time axis; and
- component, trial, feature, subject, and other axis combinations remain
  ordinary typed `Sampled` values unless a neuroimaging invariant justifies a
  narrower name.

The current `NeuroVol`, `NeuroVec`, universal `ScalaFimValues`, and opaque
`NeuroSpace` compatibility surface are replaced. The deliberate public names
are `NeuroVolume` and `NeuroSeries`. There are no deprecated aliases in the
new core. Image metadata lives once, in image4s `ImageMetadata`.

## Value semantics are part of the type

Continuous intensity, categorical label, and mask data are distinct semantic
types. ScalaFIM uses the image4s `Continuous`, `Categorical`, and `Mask` tags
and their `ValueSemantics` instances directly. It does not supply a universal
instance that makes every operation appear valid for every image.

Interpolation and numerical transforms require the appropriate semantic
capability. Consequently, categorical and mask images cannot enter continuous
linear interpolation by accident. A semantic conversion is a named operation
that returns a newly typed value; `map` does not silently retain an invalid
semantic tag.

## Geometry, axes, and identity

The image4s `SampleSpace` is the geometry owner. A three-dimensional sampled
grid and a four-dimensional stored array with one time axis describe a series;
time is not folded into spatial geometry. Axis kind, coordinates, unit, and
labels are retained rather than reconstructed from an extent.

The path-dependent sample-space value is meaningful identity inside typed
code. At dynamic boundaries, exact compatibility is checked once and the
result is packaged existentially. Shape equality or equal voxel counts never
stand in for domain identity.

Physical layout is not persistent image identity. Shape, grid geometry,
ordered axes, semantic tag, and explicit metadata are. Serialized values do
not record Ravel offsets or strides.

## One logical order

The only logical linear order is Ravel canonical C order,
`row-major-last-axis-fastest/v1`. For a series shaped `(x, y, z, time)`, time
is contiguous within a voxel. The canonical two-dimensional view is therefore
`(voxel, time)`, which can be a zero-copy reshape when the input is a whole
canonical array.

Public image access is by coordinates or by an exact locus4s domain index.
There is no public ambiguous `linear(Int)`. Code requiring logical linear
access first obtains a capability whose meaning is explicit.

Arbitrary immutable strided Ravel views remain valid sampled images. A kernel
must declare which of these input capabilities it needs:

| Capability | Meaning | Allowed behavior |
| --- | --- | --- |
| view-safe | Any valid immutable Ravel layout | Coordinate/stride-aware read, no copy |
| canonical layout | Canonical strides, possibly a subview | Canonical iteration over the visible extent |
| whole canonical | `CanonicalArray.from(data)` succeeds | Bounded `readLinear`, zero-copy reshape, no hidden buffer |
| prepared workload layout | A named algorithm-specific representation | One explicit preparation, then allocation-free hot loop |

A failed capability refinement is data, not an invitation to copy silently.
The caller decides whether to select a view-safe algorithm or request a named
materialization.

## Construction, views, and copies

Every boundary belongs to one of four categories:

| Category | Contract | Representative API vocabulary |
| --- | --- | --- |
| zero-copy retention | Retain the exact immutable provider value | `fromSampled`, `fromRavel` |
| immutable view | Return a `Sampled`/Ravel view sharing storage | `selectTime`, crop, flip, permute, stride, singleton-plane selection |
| explicit materialization | Allocate a canonical destination by request | `materializedCopy`, `copyFromCanonicalArray`, `copyToCanonicalArray` |
| streaming conversion | Decode or encode in bounded chunks at a format boundary | NIfTI reader/writer and archive codecs |

Bare `fromLinear(Array[A], ...)`, `copyLegacyLinear`, and similarly ambiguous
APIs are removed. A mutable Scala `Array` is never adopted as immutable image
storage. If accepted at all, its method name says that it copies and requires
canonical order. Internal algorithm scratch arrays remain appropriate where
profiling justifies them, but they are not an image representation.

`NDArray.build` or the corresponding Ravel builder constructs final output
storage directly. Algorithms do not fill a Scala array and then copy it into
Ravel. Export is an iterator/visitor, a streaming encoder, a retained Ravel
value, or an explicitly named copy.

## Exact voxel domains

`image4s.locus.GridDomain` is the only voxel-domain owner. Its canonical
ordinal is the same last-axis-fastest order as Ravel. `spatialField(image)` and
`seriesField(image)` provide exact locus4s fields over the sampled value. The
current ScalaFIM `VolumeDomain`, ordinal bridges, size-only admission, and
parallel domain fingerprints are removed.

A foreign or reordered domain is rejected even when it has the same size. It
may enter only through an explicit checked `Bijection`. Exact atlas and
surface publications use `PartialSurjection` directly over their true domains.

## ROI and sparse data

A voxel ROI is a locus4s `Region[V]`; an ordered ROI is a
`Selection[V]`. Selection order is semantic and its position domain is the
path-dependent domain owned by that selection.

Compact selected data is represented by:

- the exact `Selection[V]`;
- one Ravel array whose first axis is selection position;
- typed non-spatial axes and value semantics; and
- image metadata and any explicit fill/missing policy.

The canonical shapes are `(selectionPosition)` for scalar data,
`(selectionPosition, time)` for a time series, and
`(selectionPosition, trial, feature)` for trial-feature data. This keeps time
contiguous per selected voxel and aligns compact rows with the selection's
position domain. Duplicate indices are rejected by construction.

Selected storage is a provider-level image/locus concept, not a second
ScalaFIM multidimensional-array hierarchy. If image4s-locus lacks the required
Ravel-backed selected-field type, it is added there and admitted by immutable
revision. ScalaFIM then adds only domain policy and neuroimaging operations.

Dense masks and regions are distinct. `MaskVolume` is sampled Boolean data;
`Region` is a set in an exact domain. Their conversions are explicit.
Gather and scatter require exact domain compatibility and an explicit policy
for missing positions: reject, drop, or fill with a supplied value. A numeric
ring zero is not an implicit missing-data policy.

## Labels, clusters, parcels, and atlases

A dense label image is categorical. A parcellation or clustering is an exact
`PartialSurjection[Voxel, Parcel]` plus parcel metadata and provenance. Parcel
series have shape `(parcel, time)` and share that assignment. They do not also
cache a dense map, mask, cluster vectors, and lookup table as independent
sources of truth.

The exact atlas work from commit `143e6ae7` is rebased onto direct
`GridDomain` and `PartialSurjection` semantics. Atlas identities, metadata,
provenance, stable content hashes, surface topology, and neutral publication
records are retained. `VolumeOrdinalBridge`, legacy layout records, and tests
that certify first-axis-fastest compatibility are discarded.

## Slices and planes

Selecting an axis-aligned plane is normally a zero-copy rank-3 view with a
singleton spatial dimension. ScalaFIM does not pretend that a generic 2D
affine fully describes a plane embedded in 3D. Rendering projection is a
separate `SliceGrid`/plane operation. A true rank-2 sampled plane will be used
only when the provider can encode its embedding without loss.

## NIfTI and encoded values

NIfTI voxel order is a file-format boundary, not a legacy ScalaFIM order. The
reader streams file values into one Ravel-built canonical destination. It does
not stage a full `Array[Double]`. Native stored dtype, slope, and intercept are
retained with image4s `EncodedSampled`; conversion to decoded `Double` is an
explicit materialization.

The writer traverses the sampled image in NIfTI coordinate order and emits
bounded chunks. It does not call `copyLegacyLinear` or build simultaneous full
value and byte buffers. The sform/qform choice, coordinate convention, dtype,
encoding, and loss policy are explicit writer policy.

## Equality

Images do not acquire accidental deep structural equality. Reference identity,
metadata/domain identity, exact-value comparison, and tolerance-based numeric
comparison are separate named operations. Tests use exact comparison only for
discrete values and explicit tolerances for floating point.

## Rejected alternatives

- Preserving the historical ScalaFIM ordinal alongside Ravel canonical order.
  It makes every linear access and I/O boundary ambiguous.
- Reversing public dimensions to make old buffers appear canonical. It corrupts
  geometric and axis meaning.
- Requiring every image to be contiguous. Immutable strided views are a core
  Ravel/image4s feature and often avoid large copies.
- Exposing raw backing arrays for speed. `CanonicalArray` is the bounded
  capability for whole-canonical hot paths.
- Keeping duplicate ROI, sparse, and domain wrappers for familiar names. That
  creates competing identities and order contracts.
- Encoding masks and labels as untyped scalar volumes. It permits scientifically
  invalid interpolation and numerical operations.

## Verification contract

The migration is complete only when all of the following hold on JVM and
Scala.js:

1. Compile-time probes accept continuous interpolation capability and reject
   it for masks and categorical labels.
2. Asymmetric-shape oracle tests prove coordinate access, canonical ordinal,
   exact GridDomain alignment, zero-copy views, and explicit copy boundaries.
3. Sparse/ROI properties prove selection order, duplicate rejection, exact
   gather/scatter, and all missing policies.
4. Atlas tests reject same-sized foreign/reordered domains and prove direct
   volume and surface publication realizations.
5. NIfTI fixtures prove affine, axis, dtype, scaling, coordinate order, and
   roundtrip behavior without whole-file decoded staging.
6. Allocation tests show zero wrappers and zero copies for retained `Sampled`,
   Ravel, view, and canonical-refinement paths.
7. Same-run performance courts compare hot kernels with primitive loop oracles
   and reject material allocation or material throughput regressions.
8. The full `compileAll`, `testAll`, and examples/downstream courts pass with
   immutable provider pins and a warning-clean build.

The detailed removal and migration ledger is
[`../plans/native-image-migration.md`](../plans/native-image-migration.md).
