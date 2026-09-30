# Neuroimaging estimate sets — V0 specification

**Canonical ScalaFIM scientific design, version 0.2.0, 10 September 2026.** This replaces the
0.1 straw man. MUST, SHOULD and MAY define the implementation target. They do not
claim that a schema, reader, writer or statistical procedure already conforms.
The originating PLS Neuro conformance and performance plan
(`plsneuro/docs/verification/first-level-artifact-conformance.md`)
separates existing evidence from required implementation tests. W5 remains open:
`bd-01M1YVQ52TAVZB2SN37DT96EAD`. This document was moved from PLS Neuro's
reviewed specification (SHA256 `5ac6e5a9a546eaf63f80fa5ade8fab10853af307c58f8d68f7b6807c8f66d9f3`);
ScalaFIM owns subsequent scientific schema changes. The implementation's wire
discriminator is independently versioned and does not inherit conformance from
this design document.

## 1. Purpose and conformance boundary

Represent identified neuroimaging estimates and statistics so independent tools
can discover, validate, inspect and reuse them without the original fitting
process. The initial domain is first-level volumetric output for second-level
GLM, PLS, regional/FIR plotting and interactive inspection. Producer neutrality,
scientific meaning and bounded access are primary. R support is one possible
implementation or backport. Official BIDS adoption is not a project goal.

V0 consists of a structural core plus at least one available numerical product:
**effects or statistics**. A statistic-only artifact is valid without beta or SE
maps. Conformance, information supplied and eligibility for a downstream method
are distinct judgments.

| Profile | V0 requirement |
|---|---|
| Structural core | Identity, shared estimand/hypothesis catalog, unit bindings, domain, typed validity, provenance, requested-product outcomes and immutable publication |
| Core-NIfTI | Required baseline: JSON/TSV metadata and NIfTI volume products; effects-only and statistic-only fixtures both required |
| Joint uncertainty | Optional artifact capability; conforming consumers claiming it support shared-normalized and voxelwise covariance, with explicit scale semantics |
| HDF5 | First-class additional encoding profile, advertised only after physical conformance tests; performance claims require separate measurements |
| Diagnostics and explorer statistics | Optional extensions; reserved kinds and identity rules below, outside mandatory V0 payload coverage |

The first W5 increment can deliver Core-NIfTI before HDF5 is qualified. The full
PLS Neuro interoperability target includes **both HDF5 and NIfTI**. No artifact
must duplicate a product into both encodings. This sequencing does not designate
an optimal storage format or remove HDF5 from the product target.

## 2. Objects, identity and axes

| Object | Authority and identity |
|---|---|
| Dataset | Persistent namespace; participants are `(DatasetId, ParticipantLabel)`, never bare labels |
| Model revision | Immutable model specification, semantic column definitions and the shared estimand/hypothesis catalog |
| Fit unit revision | Exact executed scope, realized design/bindings, input receipts, local observations, products and outcomes |
| Estimand | Scientific target or hypothesis, identified by `(ModelRevisionId, EstimandId)`; values of an estimand are estimates |
| Product | Typed values on explicit ordered axes, one pooling scope and one scientific transformation contract |
| Representation | Physical encoding of a product, with locators, dtype, axes, scaling and digests |
| Collection revision | Immutable intended membership, unit-revision references/digests, outcomes and coverage |
| Discovery pointer | Mutable convenience reference to a collection revision; never an analysis input identity |

IDs are opaque persistent strings, separate from labels and digests. V0-generated
DatasetId, ModelRevisionId, UnitId, UnitRevisionId and CollectionRevisionId use
UUIDs; existing IDs may be preserved in explicit source-identity fields. UUIDs
are allocated before writing and remain stable through a retry of the same
publication. A scientifically changed computation gets a new revision. Estimand
IDs are unique strings within the immutable model catalog. Labels may repeat;
label sanitization must never merge identities.

The **model-level catalog** defines each estimand's kind, units, normalization,
condition/factor meaning, response coordinate and definition. Units reference
catalog IDs in an explicit order and supply realized numeric operators against
their actual columns. Different event timings, retained scans and nuisance
columns make realized designs unit-specific; `design.tsv` belongs to the unit.
The model revision contains a specification and column semantics, not one
purportedly universal numeric design matrix. Unit-local nuisance/readout entries
may be scoped to that unit and cannot participate in an automatic cohort join.

Within one model revision, catalog identity permits an estimand join only after
unit definitions, scaling and provenance pass validation. Across model revisions,
matching labels do not establish equivalence: a consumer needs an explicit
verified mapping or must keep the estimands distinct. A changed basis
normalization changes the estimand definition. Numeric realized operators must
preserve the shared target despite differing unit-specific columns.

Logical scalar products use `observation × estimand × sample`. Local observations
are usually length one; trialwise/beta-series products may have several identified
rows. Each product has one `PoolingScope` (`run`, `joint-runs`, `pooled-runs` or
`trialwise`) and explicit acquisition membership. Runwise and pooled rows never
share a product. Pooled units reference contributing units; joint fits enumerate
acquisitions directly. The collection derives its cohort axis from unit/local-row
references. It never converts runs, trials, conditions or FIR bins into independent
participants or includes pooled and constituent rows as independent evidence.

## 3. Scientific metadata and product outcomes

Every unit manifest records model/catalog references and digests, execution ID,
producer/version, contributing acquisitions, exact input receipts, selected scans,
effective estimator/noise/nuisance/run-combination description and available
estimability evidence. Unknown imported facts are explicit; they restrict
operations rather than making intact numerical maps unreadable.

Each catalog entry specifies:

- A coefficient, linear contrast, basis/readout, or hypothesis definition with
  stable column/basis IDs. Unit bindings carry exact numerical weights/operators.
  A formula string or display name is insufficient to recover those weights.
- Units, input/output scaling and sign convention. A t, z, p or unsigned F product
  identifies its actual kind and hypothesis; it is not an effect estimate.
- Condition/trial and physical response coordinates. FIR bins are `[start,end)`
  intervals in seconds relative to the stated event origin. Sampled curves name
  their time grid. Reconstructed readouts retain the numerical operator over the
  named basis and its normalization, not just the plotted time labels.

The requested-product inventory uses `available`, `not-requested`,
`unsupported` or `failed`, with a reason for unavailable requested products.
An available product names its axes, shape, dtype/precision, validity reference,
scientific definition and representations. Missing SE/covariance means unavailable,
never zero. Supplied reference-distribution and transformation information remains
separate from the producer's assertion that an analysis is appropriate.

Design matrices and full fitting configurations SHOULD be retained for auditing.
They are required for operations reconstructing that design or refitting it.
Raw BOLD and full residual series are optional external resources. Missing sources
can block source re-verification or refitting without blocking inspection of
verified self-contained products.

## 4. New contrasts and uncertainty

Capabilities are information-dependent:

| Operation | Required retained information |
|---|---|
| New linear effect `c'b` | Identified/scaled coefficients or basis estimates, numerical `c`, and evidence that the requested function is estimable |
| Its variance `c'Sigma c` | Above plus the matching joint covariance; marginal variances suffice only when the required cross-covariance is known or irrelevant |
| Its SE | Valid nonnegative variance and the declared uncertainty/scale convention; df is not needed merely to take a square root |
| Its test/p-value | Effect/uncertainty plus applicable df/reference distribution, null/tail semantics and scientific admission |
| Design reconstruction/refit | Realized design/configuration and the inputs needed by that operation |

An estimability certificate is an axis-bound full-rank assertion with numerical
rank/tolerance evidence, the realized design, or a basis/projector for the admitted
estimable subspace. In deficient fits, consumers verify membership of the requested
operator in that subspace. A pseudoinverse covariance, or a computed zero variance,
does not establish estimability. Certificate tolerances belong to the declared
numerical method and are checked by the consumer, not inferred from array shape.

Covariance descriptors bind both estimand axes, observation/sample variation,
validity and one explicit reconstruction equation:

- `absolute`: the stored matrix is `Sigma`; no residual scale is applied.
- `normalized`: `Sigma[o,s] = scale[o,s] * U[o,s]`. `scale` is a **variance**
  multiplier, not an SD; its units and named axes are explicit. A shared U omits
  sample variation only by an explicit invariant-axis declaration.
- `factor`: declare either `U = F F'` or `U = F' F`, or the corresponding equation
  for absolute Sigma. Name factor axes and rank. Any permutation is an explicit
  ordered index mapping applied once to both covariance axes.

V0 joint readers support a shared normalized matrix plus voxelwise scale, and
voxelwise matrices (absolute or normalized). Factor encodings are an advertised
extension with the same reconstruction semantics; they cannot be exposed as
supported until decoded-covariance fixtures pass. Matrix storage can use full
matrices or indexed upper-triangle entries `(i,j)`, `i <= j`, ordered by increasing
`i`, then `j`; manifest indices are zero-based. Symmetry, nonnegative diagonals and
positive-semidefiniteness are validated with declared consumer-checked numerical
tolerances. A failure marks the covariance unavailable; it cannot imply certainty.

Full-rank shared-operator OLS naturally supports `U = L (X' W X)^-1 L'` and voxelwise
residual variance where its assumptions hold; deficient models require their
certified estimable-subspace construction. An OLS writer advertising joint
uncertainty MUST preserve that requested representation or an equivalent one.
**Effects-only and scalar contrast exports remain legal.** No universal rule
forces every producer to retain covariance. Run pooling can produce voxelwise
covariance even when each run had shared U; a pooled writer must retain the true
form rather than force it into one shared matrix times one voxel scale.

Covariance over estimands at a voxel does not describe spatial covariance. ROI
uncertainty requires its own aggregation assumptions or retained information.
Cross-run/observation blocks may be omitted only under explicitly declared
independence or unavailable-capability rules.

Degrees of freedom are separate typed records: `Role` is `residual`, `effective`
or `reference`; `Status` is `available`, `unknown` or `not-applicable`; available
values declare axes and whether the interpretation is exact-under-model or
approximate, with a method reference. Tests separately name their distribution
and parameters (including numerator/denominator df for F), tails, null and any
correction family. Scalar broadcasting is explicit. An inference-df min/max
summary is not a replacement for values required by a test. Normal-reference
statistics need not invent finite t df. Unknown df permits appropriate inspection,
not automatic inference.

Available df values are finite: residual df is nonnegative; reference/effective
df is positive where the named distribution/method requires it. Degenerate or
undefined cases carry unavailable validity/status rather than fabricated values.

Statistic-only products include a hypothesis reference and known/unknown fields
for these semantics, plus explicit absent effect/SE links. Their presence never
authorizes reconstructing beta, SE or a known error model from the statistic.

## 5. Domain, validity and geometry

The V0 domain is a volume grid with dimensions, voxel-to-world affine, world frame,
spatial units, requested support mask and ordered sample identity. The interchange
world convention is RAS+ in millimetres. **RAS world axes do not imply RAS voxel
storage order.** Stored voxel order is bound to the affine; packed indices are
zero-based, x fastest: `i = x + nx * (y + ny * z)`.

Product validity is uint8 on named axes. Core codes are `0 valid`, `1 outside-support`,
`2 missing-input`, `3 non-estimable`, `4 numerical-failure`, `5 not-computed`.
Extensions use a versioned code dictionary. Omitted axes broadcast only through an
explicit invariance declaration. Two estimands with different exclusions require
different validity values; a single 3D mask cannot express that difference.
Valid values must be finite; invalid array cells are ignored regardless of their
stored numeric fill. Legitimate zeros are valid data. Support and product validity
are distinct. Core product statistics must also pass kind-specific domain checks
such as p in [0,1] and nonnegative variance.

A manifest declares `SelectedTransform`, the target frame and the authoritative
voxel-to-world affine. Importers choose a transform that matches that intended
frame: sform when coded and applicable; qform when it is the applicable coded
mapping. If the frame is unspecified and two coded forms imply different frames,
the importer requires an explicit choice rather than silently choosing anatomy.
No usable coded transform requires explicit externally verified geometry or refusal
of spatial admission. Preserve original forms/codes and the import decision.

Different qform/sform mappings may legitimately refer to different frames. Record
that difference as a diagnostic; do not reject it merely for inequality. Unexplained
left/right handedness conflicts, singular transforms or disagreement between the
**selected** transform and the manifest block spatial admission. This qualifies
both the old blanket-rejection rule and a blanket “always use sform” rule.

For manifest/header agreement, compare decoded transforms in mm at all voxel-grid
corners: maximum displacement must be <= 0.001 mm in V0. This is a serialization
agreement tolerance, not a registration or scientific alignment tolerance. Record
measured error; a conversion exceeding it needs a corrected representation or an
explicitly separate geometry-conversion product, not a relaxed silent comparison.

V0 writers set a coded sform for the manifest geometry. A qform may be unset, agree
when representable, or preserve an explicitly described alternative frame. Readers
support any valid stored voxel orientation. A backend needing canonical voxel
orientation may perform a lossless signed permutation/flip, updating data, mask,
indices and affine together and retaining that mapping. It must not interpolate
oblique/sheared images merely to satisfy a storage convention. Resampling is a
separate scientific transformation and product revision.

## 6. Files, references and authority

This is a local BIDS-shaped derivative profile; new suffixes below are not official
BIDS statistical-derivative claims. The BIDS version and this profile version are
separate. Dataset-level `GeneratedBy`, source dataset mappings and BIDS URIs follow
established BIDS conventions. Required products never depend on RDS/JVM objects or
executing embedded code.

```text
derivatives/<pipeline>/
  dataset_description.json
  current.json                           mutable discovery pointer only
  collections/<collection-revision>/estimateset.json
  models/<model-revision>/model.json
  models/<model-revision>/estimands.json  shared catalog + column definitions
  sub-01/ses-01/func/
    <entities>_desc-<unit-revision>_estimates.json
    <entities>_desc-<unit-revision>_observations.tsv
    <entities>_desc-<unit-revision>_design.tsv       when retained
    <entities>_desc-<unit-revision>_bindings.json
    <entities>_desc-<unit-revision>_mask.nii.gz
    <entities>_desc-<unit-revision>_effect.nii
    <entities>_desc-<unit-revision>_validity.nii
    # or statistic/variance/covariance products, or HDF5 representation files
```

A joint/pooled unit omits a misleading single-run filename entity and records all
contributors. `desc` uses an alphanumeric revision token mapped to the full opaque
ID by the manifest. Never use a truncated token without collision detection.
Statistic volumes use this profile's `statistic` suffix and an explicit kind in
metadata; they do not reuse `bold`. Final alignment with a future external naming
standard changes a representation/path mapping, not estimand identity.

Authoritative JSON and TSV metadata are outside numerical containers so discovery
is cheap. TSV column dictionaries define types/nulls. HDF5 duplicates, if present,
are exact or declared subsets and must agree. No editable second authority exists.

All internal references are objects `{Path, SHA256, Bytes}`. SHA256 is lowercase
hex over exact file bytes; Bytes is the exact nonnegative byte count. V0 `Path`
is relative to the **derivative dataset root**, never the containing file. It uses
UTF-8 forward-slash segments, no absolute path, `.`/`..` segment or escape outside
that root after resolution. A standalone unit opener is given its dataset root.
External sources use explicit URI/dataset mappings and digests; missing originals
are distinguishable from missing required bundle content. Display/source paths
are not artifact identities.

The digest graph is acyclic: payloads and leaf metadata -> unit manifest -> immutable
collection manifest -> mutable pointer. Shared model/catalog files are leaves;
payloads cannot embed the digest of a manifest that hashes those payloads. Units
record scientific IDs independently of digests. Unit ID/digest conflicts refuse.
A new representation is published in a new immutable unit-manifest revision referencing
its predecessor; verified unchanged scientific products can retain product IDs.
Precision-changing conversion creates derived product IDs and a conversion receipt.

The pointer is a small JSON object containing profile version, collection ID and
its `{Path,SHA256,Bytes}` reference. An analysis resolves it once and pins the
immutable collection reference. No completed analysis follows a moving pointer.

### Core-NIfTI-1 metadata dispatch

The implemented Core-NIfTI JSON envelope has exactly `Schema`, `WireVersion`,
`ProfileVersion`, `DocumentKind` and `Content`. `Schema` is
`scalafim-estimates-core-nifti-1`; `WireVersion` is `1.0.0`; and
`ProfileVersion` is the independently versioned scientific target `0.2.0`.
`DocumentKind` selects `catalog`, `unit`, `collection` or `pointer`. Unknown
envelope fields or versions refuse. `Content` is authoritative JSON; no TSV or
NIfTI header may silently redefine scientific IDs, ordered axes, pooling or
statistics. Core unit content explicitly includes every scientific field plus
`Catalog`, `ModelRevisionId`, `Representations` and `Tables`; omitted fields do
not acquire implicit scientific defaults. The catalog is an immutable digest
reference. Both TSV tables are digest-pinned projections with zero-based indices,
and their bytes must reproduce the JSON catalog and observation order exactly.

Every Core representation covers one declared product/observation pair and names
`precision` (decoded logical product precision), `storedDatatype` (physical
NIfTI Float32/Float64), `slope`/`intercept`, ordered volume or pair IDs, selected
transform, and value/validity `{Path,SHA256,Bytes}` references. A coded qform may
agree with selected scanner sform, or a same-handed difference can be described
by `qformAlternativeFrame` matching transform code 2/3/4 as
`aligned-anatomical`/`talairach`/`mni-152`. Opposite handedness refuses even
with that declaration. Statistic effect/SE links use structured product IDs and
either `Known` complete hypothesis-to-target mappings or `Unknown` with a reason;
unknown links supply no positional inference capability. The development-1
decoder remains for historical bundles, mapping its bare link IDs to `Unknown`.
Default local store publications use Core-NIfTI-1; this wire freeze is specific to
the Core-NIfTI baseline and does not claim HDF5 or optional capability conformance.

### Core-NIfTI-2 compact normalized covariance

`CovarianceLayout.SharedNormalizedTable()` explicitly selects an additive unit
layout. Its envelope uses `Schema: scalafim-estimates-core-nifti-2`,
`WireVersion: 2.0.0`, `ProfileVersion: 0.2.0`, and `DocumentKind: unit`.
No Core-2 catalog, collection or pointer exists: those documents retain the
Core-1 envelope. Unit scientific fields and digest-pinned Core-1 catalog/TSV
projections retain the preceding semantics. Core-1 and development documents
remain readable and the default `newSink(unit, maximumCells)` remains Core-1.

Each Core-2 `Representations` entry has exactly `Tag` and `Content`. `Tag: Nifti`
wraps the existing record with every field explicit, including `pairOrder`,
`qformAlternativeFrame` and `storedDatatype`. `Tag: SharedNormalizedUpperTriangle`
has exactly `product`, `observation`, `table`, `estimands`, `precision` and
`validityBroadcast`. Its `table` is an exact `{Path,SHA256,Bytes}` leaf reference;
`precision` is `Float64`; `validityBroadcast` is `SupportedSamples`; and
`estimands` exactly matches the named, ordered product axis. Every declared
product/observation pair has exactly one representation across both arms.
Unknown tags, fields, versions, omissions, duplicate coverage or substituted
identities refuse. `EstimateMetadata.allRepresentations` returns the mixed ADT;
the retained NIfTI-only `representations` helper rejects Core-2 explicitly.

The table has exactly `Schema`, `WireVersion`, `Product`, `Observation`,
`Estimands`, `Precision`, `ValidityBroadcast` and `Pairs`. The table schema is
`scalafim-estimates-shared-normalized-upper-triangle-1`, wire `1.0.0`.
Each `Pairs` row has exactly `First`, `Second`, `Value` and `Validity`, with
zero-based indices in lexicographic upper-triangle order over the declared axis.
All K(K+1)/2 pairs appear exactly once. Values are finite Float64; diagonal
values are nonnegative. A pair carries one existing validity code (0 or 2..5);
`OutsideSupport` (1) cannot appear in a shared table. Invalid entries remain
explicit, and selected matrix reconstruction refuses them.

This arm requires a `Normalized(scaleProduct)` covariance descriptor with
`invariantSamples=true`. Raw source reads return U unchanged at supported
samples and `OutsideSupport` elsewhere. The consumer applies the separately
decoded variance scale exactly once. An `invariantObservations=true`
declaration requires equal U/status/axes across observations, including any
NIfTI covariance arms; it is checked before use/publication. The table format
does not itself certify PSD, design estimability or scientific admission.

Default shared limits are 4,096 total pairs and 1 MiB total declared table bytes
per unit, independent of block-cell limits. Readers check cumulative limits
before touching any payload. Writers check cumulative pairs and a conservative
serialization reservation before allocating O(P) pair coverage and check actual
bytes at seal. A strict writer budget can therefore refuse a table that a reader
would accept by its exact size. The 32-pair/64-handle NIfTI cap counts only actual
NIfTI arms; gzip staging remains separately bounded. Compact U has no sample
axis, dense sample-by-pair allocation, payload or coverage ledger. Effect,
residual scale, support, catalog and observation costs remain visible and bounded
by their existing policies. See the
[bounded qualification receipt](verification/estimate-set-compact-covariance-2026-09-30/README.md).

## 7. Encoding rules

Core-NIfTI supports NIfTI-1 single-file `.nii` and `.nii.gz`, IEEE float32/float64
numerical products and uint8 masks/validity. The manifest records stored dtype,
NIfTI scaling and decoded logical precision. Readers apply standard NIfTI scaling
once; writers use slope=1/intercept=0 for float products unless an explicit conversion
profile is declared. No lossy integer quantization is part of Core-NIfTI scientific
working products.

Each NIfTI product file has one local observation, a 3D map or a 4D ordered
estimand axis. Every volume maps to a catalog ID through the manifest; the fourth
axis is not acquisition time. Multi-observation units reference separate files.
Validity uses matching volumes or explicit per-estimand files and broadcasting;
it is not constrained to one shared 3D volume. For covariance, the fourth axis is
an explicitly indexed estimand-pair axis. Shared small matrices/factors are
JSON/TSV references; scale maps are separate products. Hypothesis statistic axes
reference the corresponding catalog hypotheses.

NIfTI-1 dimension overflow must be handled by explicit sharding along allowed
observation/estimand/pair boundaries, a separately advertised NIfTI-2 profile, or
a precise refusal. Never wrap/truncate dimensions. Core readers do not need
NIfTI-2 to conform. Gzip is valid interchange, not random-seek capability: readers
may stage verified `.nii` under a disk budget and must report decompression cost.
Spatially chunked/sharded image encodings require a future profile; do not split
a grid silently to evade limits.

HDF5 representations declare `StoredForm`: `full-grid`, `packed-by-mask` or
`dataset-per-estimand`. Named storage axes, index origin, dataset locators,
chunk dimensions and filter IDs are mandatory. Core HDF5 readers support no
compression and standard deflate; other filters are advertised extensions.
Dataset-per-estimand uses an explicit catalog-ID -> dataset-path mapping, not
labels as identity. A family of datasets is a legal representation. Converting
one-based source indices to the zero-based wire identity occurs exactly once,
with values permuted along with indices.

Every stored form declares its mapping to the logical axes. A full grid names
its x/y/z storage axes and uses the domain's linear sample identity; a packed
form supplies the ordered sample-index vector. Neither language-specific array
flattening nor dataset iteration order defines that mapping implicitly.

Existing fmristore/fmrigds layouts are adapter candidates, not presumed conforming
because they use HDF5. Physical fixtures must establish axis, dtype, scaling,
mask, labels and covariance reconstruction. A versioned layout revision is allowed
only for a demonstrated semantic or access limitation. An adapter must not reorder
samples/estimands without reordering values and validity.

A float32 product is a legitimate scientific product at its declared precision.
Converting already declared float64 values to float32 is not lossless by default:
record a derived product, actual error and permitted use. Lossless re-encoding
requires matching decoded valid values and all identities at declared precision.
Display quantization remains a derived display capability and cannot quietly feed
inference. Geometry and validity equivalence are checked independently of value error.

## 8. Publication, coverage and recovery

Three facts remain separate: (a) a unit was durably published; (b) the requested
collection has complete coverage; (c) a consumer admitted a specific analysis.

For the V0 local-filesystem publisher:

1. Create uniquely owned staging on the destination filesystem. Write payloads
   and leaf metadata; close/flush, fsync files, validate contents/digests, and publish
   immutable names without overwrite. Fsync affected directories.
2. Serialize the complete unit manifest to a temporary file; validate it, fsync it,
   atomically rename to its immutable name, then fsync the parent directory.
3. Write a new immutable collection manifest by the same protocol. It records every
   referenced unit manifest's ID, digest and byte count, including requested unit/
   product outcomes. Its own digest/length is supplied in its publication receipt.
4. Update `current.json` using a serialized compare-and-swap transaction: hold the
   dataset publisher lock, reread the expected pointer digest, merge only compatible
   nonconflicting additions, and commit a new pointer via temp/fsync/rename/directory
   fsync. On a stale base retry; a conflict on one unit's selected revision requires
   explicit resolution. Last-writer-wins replacement is forbidden.

Readers require a valid complete document and verify the referenced manifest digest
and length before accepting a unit. File existence or a `complete` string alone is
not a commit. Detached collection opening obtains a verified reference/publication
receipt; absent a trusted prior digest, it can validate internal consistency but
must not claim external authenticity. Checksums provide integrity, not signatures.
Other filesystems/object stores require an advertised equivalent commit protocol;
unsupported durability semantics must be reported rather than simulated.

Collection revisions are immutable; only discovery pointers and job logs move.
Concurrent jobs may publish different units without a shared HDF5 writer. Staging
or unreferenced immutable payloads can remain after interruption; V0 never deletes
these automatically. Explicit cleanup uses ownership and reachability checks,
respects active writer locks and does not delete sources or referenced products.

A required run failing within a declared joint fit means that unit cannot be
published as the requested successful joint fit. A fit on fewer runs requires a new
explicit model/unit revision with the changed contributors. Independently completed
run units remain reusable. A complete publication may contain typed per-voxel
exclusions; collection coverage separately states required units/products missing
or failed. A partial cohort never becomes complete by silently dropping members.
Retry reuses verified completed revisions and produces new revisions for changed work.

Jobs own queued/running/cancelled state and progress outside the artifacts. Views
subscribe to jobs. CPU, numerical threads, I/O, scratch and memory limits govern
execution; the file profile does not prescribe a scheduler framework.

## 9. Bounded reading and derived caches

The common provider API is `inspect`, `open`, `read` and `close`. `inspect` lists
metadata/axes/capabilities without loading products. `open` verifies manifests and
required payloads under a stated verification/budget policy and owns all handles.
Analysis input must be verified; missing original BOLD is distinct from corrupt
retained products. V0 verifies required payloads in full before analysis reuse;
chunk-authenticated lazy verification is a future extension.

`read(product, observationIDs, estimandIDs, sampleIDs, destination, cancellation)`
returns values, validity and a read receipt. IDs must be unique and known; duplicates
and unknown IDs are errors, not implicit sorting or intersection. Known-but-invalid
cells return their validity. Output follows caller-requested axis order, observation
outermost and sample innermost. Destination dtype and capacity are checked before
writing; no hidden full-cohort expansion is allowed. Blockwise APIs declare a maximum
block size. On cancellation/failure a partially filled destination is unpublished
scratch and the caller discards it; no partial success is returned as a complete block.
Providers check cancellation between bounded I/O/compute units and declare unavoidable
noninterruptible operations. Closing waits for owned reads to terminate.

Derived caches may unpack gzip, remap orientation, transpose panels or assemble
cohort slabs/ROI summaries. Keys bind source product/axis digests, transformations,
precision and cache-format version. Caches are replaceable; their absence cannot
change scientific identity. Additional retained Gram/cross-product/response-square
objects are durable scientific products when reconstruction needs them: they are
not disposable just because an optimized view of them is cached.

Reserve explorer kinds `gram`, `crossproduct`, `responsesquares`, with explicit
basis/metric/selected-row/operator descriptors. Their full payload profile follows
V0 core and must state the exact model family it supports. The current
The originating PLS Neuro explorer contract (`plsneuro/docs/response-explorer-plan.md`)
and `ExplorerT1Store` illustrate
product-versus-mapped-cache distinctions; their existing storage format is not
implicitly this interchange profile. Unknown required extensions block the affected
operation; unknown optional ones can be preserved and skipped.

## 10. Validation, implementation sequence and decisions

Validation checks version, IDs/references/digests, catalog/unit binding, axis lengths,
product/validity domains, geometry, operator dimensions, numerical covariance rules,
precision and publication coverage. It produces separate results for structural
conformance, available information, operation eligibility and performance. A producer
name never substitutes for any of them.

Implementation starts with machine schemas and independent logical/physical golden
bundles, then the native selected-output sink and Core-NIfTI readback, W5 reopen/retry
and downstream access. HDF5 adapters/readback follow the same semantic fixtures;
performance measurements determine their access defaults and derived-cache policy.
Both encodings remain the full product target. A second producer is a useful
interoperability test, not the purpose or prerequisite of the first W5 increment.

ScalaFIM owns the scientific manifest/catalog, typed readers and selected-block
sinks; BIDS4s owns BIDS identities/references; container libraries own physical I/O.
PLS Neuro owns jobs, review/selection/admission and cache policy. General adapters
or improvements to fmrireg/fmristore/fmrigds belong in those projects.

The two reviews are resolved as follows:

- Shared model-level estimands, per-unit realized designs, homogeneous pooling scopes,
  statistic-only products, information-dependent contrasts and typed validity: adopted.
- Covariance equations, df tags, voxelwise pooled forms and immutable collection
  manifests/discovery pointers: adopted. Mandatory covariance for every effects export,
  one shared covariance shape for every pooled fit, and one shared validity volume:
  rejected as general rules.
- NIfTI baseline and HDF5 staged qualification: adopted; both remain the end-product
  encoding target. HDF5 adapter suitability is decided by physical fixtures.
- Explicit geometry selection: adopted. Neither blanket q/s rejection nor universal
  sform precedence overrides the declared scientific frame. RAS world convention is
  distinguished from array orientation; lossless reindexing requires a receipt.
- Robust manifest publication, immutable digests, concurrent pointer updates and
  executable interoperability/performance gates: adopted. Performance budgets live
  in the separate verification plan, not in semantic identity.

Review posts: `post-01M25RK5YKTQ3EJBXMGP4C7M9T` and
`post-01M25RKFDS3KHHCWHMAG8QJXH8`. The coordinator's
independent logical counterexamples (`plsneuro/docs/verification/first-level-artifact-review.md`)
resolve covariance, estimability, validity, axis and publication ambiguities; they
are not physical encoding or durability certification.

References: [BIDS derivative metadata](https://bids-specification.readthedocs.io/en/stable/derivatives/common-data-types.html),
[BEP041](https://bids.neuroimaging.io/extensions/beps/bep_041.html),
[NIfTI coordinate guidance](https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/qsform_brief_usage.html),
[HDF5 chunking](https://portal.hdfgroup.org/documentation/hdf5/latest/hdf5_chunking.html).
The qform/sform definitions allow different coordinate contexts; this profile binds
one explicitly. BIDS statistical derivatives remain an extension effort; this local
profile makes no official suffix/conformance claim. Chunked storage supports partial
I/O but supplies no scientific semantics or automatic speed guarantee.

Local reference implementations/designs:
the `fmrireg/R/bids_export.R` exporter,
`fmristore/raw-data/LabeledVolumeSetSpec.yaml` layout,
`fmrigds/notes/TECHNICAL_SPECIFICATION.md` design, and
`plsneuro/docs/first-level-estimation-contract.md` selective-estimation contract.
