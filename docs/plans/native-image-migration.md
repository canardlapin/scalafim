# Native image migration

This is the execution ledger for
[`../decisions/native-image-data-model.md`](../decisions/native-image-data-model.md).
It is intentionally breaking: no caller is promised the old type names,
first-axis-fastest ordinals, or implicit copies.

## Baseline diagnosis

The current dense layer is partly native and partly compatibility code:

- `NeuroVol[A]`, `NeuroVec[A]`, and `NeuroSlice[A]` retain image4s `Sampled`
  values backed by Ravel;
- `ScalaFimValues` erases continuous, categorical, and mask distinctions;
- `fromLinear` interprets a mutable Scala array in first-axis-fastest order and
  transposes it into canonical Ravel storage;
- `linear(i)` and `copyLegacyLinear` reintroduce that historical order;
- `SomeSampleSpace` hides the precise sample-space owner and duplicates axis helpers;
- `VolumeDomain` and the ROI/sparse types reproduce locus4s concepts; and
- many kernels fill Scala arrays before constructing their final Ravel value.

The repository-wide baseline has 2,725 references to the principal old image
and domain names across 245 Scala files. This is a coordinated migration, not
a local rename.

## Type disposition ledger

| Current family | Target owner/type | Disposition |
| --- | --- | --- |
| `NeuroVol[A]` | opaque `NeuroVolume[S,A,Sem]` over rank-3 `Sampled` | Replace; precise space and semantic parameters |
| `NeuroVec[A]` | opaque `NeuroSeries[S,A,Sem]` over rank-4 `Sampled` | Replace; require exactly one time axis |
| `NeuroSlice[A]` | singleton-dimension `Sampled` view plus renderer plane | Remove as dense owner |
| `SomeSampleSpace`, `VolumeSpace`, `SeriesSpace` | image4s `SampleSpace`; named existential at dynamic boundaries | Remove duplicate ownership; retain only neuro-specific geometry utilities |
| `ScalaFimValues` | image4s `Continuous`, `Categorical`, `Mask` | Delete |
| `Image4sInterop.Packed*` | direct checked opaque refinements | Delete compatibility layer |
| `VolumeDomain` | image4s-locus `GridDomain` | Delete |
| `VoxelIndexSet` | locus4s `Region[V]` or ordered `Selection[V]` | Delete |
| `VoxelRegion` | locus4s `Region[V]` | Delete |
| `VoxelSelection` | locus4s `Selection[V]` | Delete; dataset request ADT may keep its separate role and must resolve to an exact selection |
| `VoxelRoi`, `ROICoords` | exact `Region[V]`/`Selection[V]` | Delete |
| `RoiValues`, `RoiSeries`, `ROIVol`, `ROIVec`, `ROIVolWindow` | selected field with compact Ravel storage | Consolidate into one provider-backed selected-data family |
| `SparseSupport`, `IndexLookupVol` | `Selection[V]` and its position map | Delete |
| `SparseNeuroVol`, `SparseNeuroVec` | selected scalar/series with shapes `(position)` and `(position,time)` | Replace |
| `ClusteredNeuroVol` | `PartialSurjection[Voxel,Parcel]` plus parcel metadata | Replace; dense labels derived |
| `ClusteredNeuroVec` | assignment plus `(parcel,time)` Ravel data | Replace |
| `NeuroHyperVec` | ordinary `Sampled` axes or selected `(position,trial,feature)` | Delete special container |
| `MorphismFields` packed rank-4 values | typed continuous `Sampled` with direction axis | Retain invariant, remove legacy planar ingress/export |

## Constructor and export classification

This table is the migration rule for every existing constructor/exporter. An
implementation PR must either map an API to one row or remove it; it may not
leave an unclassified overload.

| Existing API pattern | New category | Required result |
| --- | --- | --- |
| `NeuroVol/NeuroVec.fromRavel` | zero-copy retention | Validate shape, sample space, axes, and semantics; retain exact Ravel value |
| construction from existing `Sampled` | zero-copy retention | Validate neuro-specific refinement; retain exact `Sampled` object |
| `fromLinear`, `fromLinearChecked`, array-taking `apply` | explicit materialization or removal | Replace only where needed with `copyFromCanonicalArray`; never infer legacy order or adopt mutable storage |
| `Image4sInterop.*FromLegacyLinear`, `canonicalizeScalarVolume` | removal | Delete transpose and transfer-status machinery |
| `selectTime`, crop, flip, permute, stride, spatial plane | immutable view | Share Ravel storage and preserve exact mapped sample space/domain |
| `slice` that fills `Array` | immutable view | Replace with a singleton-dimension view; renderer projection stays separate |
| `asMatrix` | immutable view when whole canonical | Canonical `(voxel,time)` reshape; otherwise return a capability error or named materialization |
| `map`, `mapValues`, `mapVoxels`, concatenation | Ravel builder/materialization | Allocate the final Ravel output once; require or derive valid output semantics |
| `copyLegacyLinear`, `copyLegacyPlanar` | removal | No legacy-order export |
| required array export | explicit materialization | `copyToCanonicalArray` with canonical-order name and fresh ownership |
| ROI/sparse constructors from indices | exact selection construction | Validate bounds and uniqueness once; preserve declared order |
| dense-to-selected `select`/`asSparse` | builder/materialization | Gather directly into final compact Ravel shape `(position,...)` |
| selected-to-dense `toDense` | builder/materialization | Scatter once under an explicit missing/fill policy |
| cluster constructors | exact map construction | Produce one `PartialSurjection`; derive labels/lookups |
| NIfTI read | streaming conversion | Decode bounded chunks into one Ravel builder or retain `EncodedSampled` |
| NIfTI write | streaming conversion | Traverse source coordinates into bounded byte chunks |
| archive/JSON codecs | logical serialization | Persist domain/geometry/axes/semantics/metadata and logical values, never strides |

## Source migration map

### Image shared core

- Replace `Image4sInterop.scala`, `NeuroVol.scala`, `NeuroVec.scala`,
  `NeuroSlice.scala`, and dense portions of `SampleSpaces.scala` first.
- Replace `VolumeDomain.scala`, `VoxelIndexSet.scala`, `VoxelRegion.scala`,
  `ROI.scala`, `RoiData.scala`, `ROIVec.scala`, `ROIVolWindow.scala`,
  `SparseSupport.scala`, `SparseSelection.scala`, `SparseNeuroVol.scala`, and
  `SparseNeuroVec.scala` with exact provider types.
- Rebuild `Mask.scala`, `ConnComp.scala`, `ClusteredNeuroVol.scala`, and
  `ClusteredNeuroVec.scala` on semantic images and exact maps.
- Retype `NeuroHyperVec.scala` and `MorphismFields.scala` as ordinary sampled or
  selected data.
- Move output allocation in transforms, resampling, filters, statistics,
  searchlights, morphisms, and indexing directly to Ravel builders.

### JVM I/O

- Rebuild `modules/image/jvm/src/main/scala/scalafim/image/io/Nifti.scala` as a
  streaming, encoded-aware boundary.
- Audit atlas readers, dataset readers, motion parameter I/O, spatial transform
  readers, archived-response stores, and Zarr adapters for canonical-order or
  Scala-array assumptions.

### Downstream consumers

The coordinated rename and type migration includes at least `atlas`,
`dataset`, `dataset-zarr`, `fit`, `fmri-workflow`, `latent`, `motion`,
`mvpa-spatial`, `spatial`, `surface`, `surface-view`, all image-view backends,
`threshold`, and archived-response interop. Tests and examples are consumers,
not optional cleanup.

Dataset request enums may continue to describe user intent (`All`, coordinates,
indices), but resolution must produce an exact `GridDomain` selection before
data access. Public scientific APIs must not pass unowned integer ordinals.

## Provider work

Ravel and image4s already provide immutable arrays, builders, canonical
capabilities, sampled views, semantic tags, encoded values, and metadata.
image4s-locus provides direct spatial/series fields over `GridDomain`.

The expected provider gap is compact selected data: one exact
`Selection[V]`, a Ravel array whose leading axis is selection position, and
typed remaining axes/semantics. Before adding a ScalaFIM type, inspect and, if
necessary, implement that generic abstraction in image4s-locus. Admit it by an
immutable revision and prove it independently on JVM and Scala.js.

## Atlas commit rebase ledger

Commit `143e6ae7f038b9e014f399a2b38df9c4480e954a` is design input, not a commit to
merge unchanged.

| Retain and rebase | Discard |
| --- | --- |
| Provider revision adoption and direct locus integration | `VolumeOrdinalBridge` |
| Exact `AtlasQuotient` and `AtlasPublication` metadata/provenance | Structural or size-only volume-domain admission |
| Stable content identities and SHA records | Legacy first-axis-fastest bijections and layout records |
| Exact volume and surface realizations | Tests whose oracle is compatibility ordering |
| Surface topology and neutral Neuropublish records | Duplicate ScalaFIM domain ownership |
| Same-size foreign/reordered-domain rejection | Any compatibility alias that hides a copy or reorder |

Volume realizations use `image4s.locus.GridDomain.spatialField` or
`seriesField` directly. Surface realizations remain exact fields over their
surface domains. Both feed `PartialSurjection` without ordinal translation.

## Mote execution graph

| Order | Issue | Deliverable |
| --- | --- | --- |
| 1 | `bd-01M0QG00P8A73RG1F6K6SJGH8A` | This contract, inventory, and semantic compile probes |
| 2 | `bd-01M0QG2V4EYWQE7G4G4GA6JFFV` | Dense semantic `Sampled` core and canonical access |
| 3 | `bd-01M0QG2VQJZXZ2RQJTSTXC246X` | Direct `GridDomain`; legacy ordinal/domain removal |
| 4 | `bd-01M0QG2W8QX9NHJE59PB9E95H6` | ROI, selection, and compact selected data |
| 5 | `bd-01M0QG2WT1MEQR8HD4P38Y07N9` | Masks, labels, clusters, and parcels |
| 6 | `bd-01M0QG2XC19813ERSFJC21TH1S` | Algorithms, builders, and hot kernels |
| 7 | `bd-01M0QG2XX3JENQEEJV1W4FCJ41` | NIfTI, encoded values, and streaming I/O |
| 8 | `bd-01M0NS3FBVXXG1CRQWSH60EAKC` | Exact atlas realization rebase |
| 9 | `bd-01M0QG2YE6Q3PNDVBHWVVJZ6XC` | Full correctness, allocation, performance, and downstream court |

The ROI/selected-data work also gates the existing locus-data removal epic.
The final court waits for both exact atlas and locus-data closeout.

## Mechanical gates

The final source tree must satisfy searches equivalent to:

```text
no ScalaFimValues
no CanonicalizedLegacy
no copyLegacyLinear or copyLegacyPlanar
no VolumeOrdinalBridge
no public def linear(Int) on image values
no fromLinear image constructor
no ScalaFIM VolumeDomain, VoxelIndexSet, SparseSupport, or IndexLookupVol
no image owner backed by Array
```

A raw `Array` remains acceptable for private, measured kernel scratch or a
clearly named copy boundary. Each surviving occurrence is reviewed rather than
blindly banned.

## Acceptance gates

Each phase must pass focused tests on JVM and Scala.js before its Mote issue is
closed. The final phase additionally requires:

- warning-clean `compileAll`;
- `testAll` and example tests;
- independent asymmetric coordinate/domain oracles, including committed
  nibabel and neuroim2 4D NIfTI fixtures;
- compile-time semantic rejection probes;
- zero-copy identity and allocation tests;
- sparse/selection property tests;
- NIfTI fixture and streaming-memory tests, with coordinate-exact ingress and
  raw-payload writer parity across the distinct file and Ravel orders;
- exact atlas volume/surface publication tests;
- same-run performance comparisons against primitive reference loops; and
- a clean immutable-provider rerun, followed by focused commit, non-force push,
  and local/tracking/live SHA equality.
