# Ecosystem ownership and deletion map

U0 decision, 2026-10-16. Source baseline: ScalaFIM
`3a2fda43c0a1155832bfb2dc5c4ead1503038190`. This supplements the
[program](ecosystem-unification.md); Mote `eco-unification` tracks execution.
Destinations below are ownership decisions, not claims that extraction is done.

## Shared contracts

| Contract | Authoritative destination | Cutover and deletion |
| --- | --- | --- |
| Domains, indices, selections, maps, neighborhoods, fields, partial quotients | Existing locus4s core/data | Replace `locus-data/package.scala`, `MapEvidence`, `Parcellation`, and generic `Searchlight` machinery. Geometry generates neighborhoods; locus4s owns their representation. Delete the facade after its last caller moves. |
| Domain restoration | Explicit locus4s `DomainRegistry` per load/composition, with checked `DomainAlignment` across loads | Remove `DomainFactory`'s process-wide cache. Pass an explicit scope where canonical live owners are needed; otherwise restore independently and align by full persistent identity. Never use size or display name as identity. Derived supports stay ephemeral. |
| Source lineage | New optional `locus4s-provenance`, package `locus4s.provenance` | Move `response/Provenance.scala`, its ID validation, evidence-reference record and errors. Core/data must not depend on this artifact. Response and MVPA consume the same values; delete response-owned definitions at cutover. |
| Scientific axes and repeated draws | mvpa4s, over locus4s, Multivar and Resample4s | Keep axis keys, role/basis/scale/lineage and occurrence identity. Delete `AxisRef.responseDomain` and `ReindexingLeg.responseSelection`; build response selections in ScalaFIM adapters. A draw such as `[2,0,2]` is not a locus4s injective selection. |
| Image sampling axes | image4s | Delete aliases and forwarding implementations that add no neuroimaging invariant. Do not merge image coordinates with scientific sample identity. |
| Frames, points, units, conventions and alignment | spatial4s | image4s, mesh4s and reframe4s use the same nominal types. Temporary provider aliases must be aliases of those types, not conversion wrappers or new owners. |
| Anatomical identities | `neuroim4s-spaces`, package `neuroim4s.spaces` | Move `WorldSpace`, `DatasetNamespace`, subject/session/template IDs, `DeclaredToken`, `FrameCatalog`, `NativeContext` and declaration helpers. Only spatial4s is needed by the frame-facing API. No image, volume, transform, surface, atlas, MVPA or ScalaFIM runtime dependency. |
| Acquisition identity evidence | Value records in `neuroim4s-spaces`; admission adapters in neuroim4s IO and neurosurf4s IO | `GeometryDigest` and `ReferenceAcquisition` remain pure records with their existing canonical grammar. Reading NIfTI/BIDS/FreeSurfer evidence stays outside spaces. Preserve exact dimensions, affine bits, xform codes and entity escaping; do not replace acquisition identity with a lossy hash. |
| Coordinate maps and generic sampling | reframe4s | Replace generic `WorldLink`, `SpatialPullbacks`, `NativeResampling`, resampling plans and coordinate-map wrappers with provider capabilities. Neuro-specific admission and toolkit conventions remain downstream. |
| Mesh topology and realizations | mesh4s, with spatial4s frames and locus4s cell domains | Delete `TriangleMesh`, local topology/domain ownership and generic traversal/kernel copies after strict/raw admission and ordered-field parity pass. Alternate coordinates share topology where justified. |

Source provenance fits locus4s as an **optional identity/evidence contract**:
its references describe identified domains and external sources, it has no
numeric/storage dependencies, and response and MVPA already share it. It is
not a graph execution engine. Keep the existing topologically ordered node
validation and root semantics. A provenance evidence reference is descriptive;
it must not mint a live domain or certify alignment. `DomainReference` must
become provider-owned without moving response `TimeAxis`, `SampleAxis`, read
selection, receipt, or access policy into the provider. Multivar value identity,
MVPA exposure/independence, atlas citations and physical read receipts keep
their separate meanings.

`SpaceResolver`'s NIfTI xform interpretation belongs in neuroim4s IO;
`ToolCoordinates` and FSL/FreeSurfer volume geometry interpretation belong in
neuroim4s transforms. Surface loader placement consumes the resulting frame/map
capabilities. The small spaces artifact's repository address does not place
neurosurf4s beneath the volume stack.

## Domain libraries and consumers

| Current source family | Destination and removal condition |
| --- | --- |
| `image` neuro volume/series admission and missing-value policy | neuroim4s core, preserving native image4s/Ravel values. Keep no second sampled-image container. |
| `image` indexing, masks, validity, selected views/windows, grid-domain bridges, local statistics, filters, components, clustering, parcel reductions/neighborhoods | Existing image4s/locus4s/Gale capabilities; add missing generic behavior there, then retain only neuro-specific admission in neuroim4s. In particular `KMeans` numerical machinery belongs in Multivar/Gale, not neuroim4s. |
| `image` NIfTI IO | image4s for binary/header/scaling/ordering mechanics; neuroim4s IO for neuro-volume/series and world admission. Preserve bounded reading/writing. |
| `transform` toolkit codecs, detections and anatomical interpretation | neuroim4s transforms and platform IO. Generic affine/warp/solver composition stays reframe4s/Gale; delete wrappers when equivalent provider APIs cover callers. |
| `image-view` and Canvas/Java2D/JavaFX | Optional neuroim4s view/backend artifacts; renderer-neutral primitives remain Intaglio-owned. |
| `surface` neuro geometry/hemisphere/medial-wall/reference metadata, GIFTI/FreeSurfer | neurosurf4s core/IO. Generic topology, geodesic, barycentric, sphere and spatial-index kernels go to mesh4s; remove local copies. |
| `surface` projection/ribbon sampling, `reference/DeclaredVolume`, linked volume behavior | Optional neurosurf4s volume integration over neuroim4s. Basic surface IO and display do not depend on it. |
| `surface-view`, raster, JavaFX, Three.js | neurosurf4s view/backend artifacts. Keep connectivity-result interpretation in ScalaFIM's `surface-view-connectivity`. |
| `atlas` reference, labels, hierarchy, registry, query, provenance and realization assignments | neuroatlas4s core over locus4s and spaces. Split `AtlasRealization.scala`: core assignments/metadata; volume and surface realizations in their respective optional modules. Publication projection is an optional adapter, not required abstract members on every realization. |
| `atlas` volume/template catalogs, volume loaders and template grids | neuroatlas4s volume/IO over neuroim4s. Generic grid and reduction math stays upstream. |
| `atlas` surface catalogs/loaders, `StandardSurfaceRoutes`, `SpaceTransformGraph` | neuroatlas4s surface/routing over neurosurf4s; volume routes additionally consume neuroim4s transforms. Atlas-specific confidence, assets and version policy stay here. |
| `mvpa` scientific plans, evidence, methods and finite-family execution | mvpa4s core. Remove response/facade/pipeline dependencies before extraction. |
| MVPA interpreter in `execution/LocalExecution.scala` | Optional ScalaFIM `mvpa-pipeline` integration. Core retains deterministic addresses, resource/commit contract and local execution; pipeline wraps one admitted unit without duplicating scientific scheduling or resource cleanup. |
| MVPA fit/dataset/group/archive integrations | Keep ScalaFIM-specific readers and policy. Move generic predictive machinery to mvpa4s predictive and generic locus/image/surface/atlas measurement adapters to optional mvpa4s integrations. |

Default rule for a moved family: its tests, fixtures, generators, examples and
tools travel with their behavior. A cross-domain scenario stays at the highest
consumer needed to run it. No provider depends on ScalaFIM test code. The U0
baseline receipt records the concrete corpus and target specimens.

## Complete partition of the mixed spatial module

Rows cover all 30 current production files, including public declarations
within them. A split means replacing the mixed contract and migrating callers,
not moving the old sealed union into a provider. Provider entries below reuse
or extend existing contracts and never depend on the anatomical libraries.

| Files | Partition and canonical owner |
| --- | --- |
| `Domain.scala` | Finite identity/alignment: locus4s. Volume `SpaceRef`/sampling: neuroim4s spatial. Surface: neurosurf4s core. Hybrid and `DomainPart`: neurosurf4s volume integration. Latent: ScalaFIM latent/spatial integration. Unsampled frames: spatial4s plus shared anatomical declarations. Delete mixed `DomainKind`, `TemplateKind`, `SamplingGeometry` and `DomainLocus` facade after typed consumers move. |
| `Ids.scala` | Finite-domain ID: locus4s persistent identity; transform/operator IDs: reframe4s execution records; modality/resolution: neuroim4s metadata; basis: ScalaFIM latent; hybrid part name: neurosurf4s volume integration. Persist old ID strings; do not equate old route labels with a certified domain key. |
| `DiscreteTransport.scala`, `RowSelection.scala` | locus4s total maps, relations and ordered selections; caller-specific row demand remains in the relevant execution adapter. Remove `ExactSpatialMap`/`CrispSpatialRelation` wrappers when they add no proof. |
| `GaleSpatialSupport.scala`, `Backprojection.scala` | Generic numerical composition/backprojection: Gale. Scientific route policy stays in consumers. |
| `Morphism.scala` | Coordinate maps, inverse evidence, composition/fingerprints: reframe4s core/graph. Volume transform binding: neuroim4s spatial. Surface mapping: neurosurf4s spatial; projection/hybrid: neurosurf4s volume. Functional/value plugins: ScalaFIM. Remove mixed `MorphismKind`/path unions in favor of registered capabilities with typed endpoints. |
| `SpatialGraph.scala` | Route algorithms and inverse costs: reframe4s graph, using graph4s algorithms where applicable. Anatomical route declarations: neuroim4s or neurosurf4s adapters; atlas selection policy: neuroatlas4s routing. `PathStep`/`PathInspection` are provider route diagnostics with consumer metadata. |
| `MorphismPlugin.scala` | Generic value-stage linear algebra: Gale. `StageSemantics`, plugin registration and execution over admitted routes: reframe4s flow. Functional/latent plugin implementations stay ScalaFIM. |
| `PullbackCompiler.scala`, `StagedOperatorCompiler.scala` | Generic lowering, program stages, registry, support planning/execution: reframe4s flow/resample. Register volume lowering from neuroim4s, surface/projection/hybrid lowering from neurosurf4s, functional lowering from ScalaFIM. The provider registry cannot import those implementations. |
| `OperatorCompiler.scala` | Generic sparse operator/shape, weights and algebra: Gale/reframe4s resample. Volume grid admission, sampling and compile request: neuroim4s spatial. QC/provenance records describe the selected provider program, not a parallel operator authority. |
| `OperatorSignature.scala` | Canonical operator recipe/signature: reframe4s flow, with consumer route and sampling descriptors. Preserve signature framing and cache invalidation. |
| `VolumeToSurfaceOperator.scala`, `HybridOperator.scala` | neurosurf4s volume integration. Generic row assembly uses provider operators; volume-to-surface requests, coverage policy and hybrid layout remain here. |
| `SurfaceResamplingBinding.scala` | neurosurf4s spatial, over mesh4s/reframe4s; no volume dependency for surface-only resampling. |
| `Demand.scala` | Generic ordered finite support/selection: locus4s. Grid boxes/voxel regions: image4s. Observation/time demand and combined field demand: reframe4s flow's non-anatomical execution contract, adapted to response downstream. No replacement universal geometry union. |
| `FieldSource.scala`, `FieldRuntime.scala`, `LazyFieldRuntime.scala`, `ViewPlan.scala`, `FieldExplanation.scala` | Generic source revisions, scoped blocks, staged views, caches, explanation/QC and materialization lineage: reframe4s flow over provider domains/operators. Volume admission/backend: neuroim4s spatial; surface/hybrid: neurosurf4s integration; response/latent adapters: ScalaFIM. `Field` must compose locus4s field ownership rather than compete with it. |
| `FieldApi.scala`, `SpatialError.scala`, `SpatialQc.scala` | Partition extensions/errors/QC with their owning operation above. Volume voxel syntax/QC: neuroim4s spatial; provider coordinate/operator checks: reframe4s; surface/projection metrics: neurosurf4s. Delete the monolithic error wrapper; preserve refusal reasons in adapters. |
| `io/NiftiFieldSource.scala` | neuroim4s IO adapter over image4s bounded reads and provider source contract. |
| `io/SpatialIngest.scala`, `io/TransformAssetLoader.scala`, `io/SpatialIoError.scala` | neuroim4s transform IO, including fMRIPrep transform asset interpretation. Dataset/functional workflow assembly stays ScalaFIM. |
| `io/SpatialTripletFileCache.scala` | reframe4s JVM execution cache mechanics; neuroim4s supplies domain-aware recipe/provenance. Preserve validation and stale-file behavior. |

The target dependency order is: core numerical/domain/coordinate providers;
generic transform/execution providers; shared anatomical declarations;
independent neuroim4s/neurosurf4s/mvpa4s/neuroatlas4s cores; optional cross-domain
adapters; ScalaFIM integrations/applications. Volume code cannot import the
surface or atlas implementations. Provider execution is parameterized by
admitted domain/map capabilities, not by the current four-case spatial union.
No provider core points back at its optional adapters.

## Mesh input policy

The current `TriangleMesh.fromArrays` checks finite coordinates, bounds and
three distinct indices per face. mesh4s additionally audits duplicate faces,
orientation, edge incidence, unused vertices and vertex links. U3 must preserve
input order and expose this admission difference:

| Representative ordered faces | Policy |
| --- | --- |
| One triangle `(0,1,2)`, or a consistently oriented open patch | Admit boundaries; do not require a closed cortical shell. |
| Two disjoint triangles `(0,1,2),(3,4,5)` | Admit disconnected components when each component is valid. |
| `(0,1,2),(0,1,3)` sharing an edge in the same direction | Return orientation diagnostics; no automatic winding repair. |
| `(0,1,2),(2,1,0)` duplicate unoriented face | Return duplicate-face diagnostics; no dropping faces. |
| `(0,1,2),(1,0,3),(0,1,4)` non-manifold edge, or two fans meeting only at a vertex | Return manifold/link diagnostics; no splitting vertices. |
| Valid face table with additional unused vertices | Preserve raw input and report unused vertices; no renumbering or shrinking fields. |
| Repeated indices, out-of-range indices or nonfinite coordinates | Reject at input admission, as today. |

Raw/diagnostic input is a mesh4s-owned ordered triangle-table/coordinate
representation, separate from admitted manifold topology. It may support
round-trip and explicitly diagnostic display, but must not obtain topology,
geodesic or field-alignment guarantees by casting. Required raw display access
is an explicit U3 capability; a loader must return a typed refusal until that
capability exists. No silent behavior change from accepting soup to claiming
manifold topology is allowed. Zero-area coordinates need explicit geometric
admission where an algorithm requires it; topological admission alone is not
that proof.

## Access, persistence and downstream cutover

Audit found broad `private[scalafim]` access in image `SampleSpaces`,
`SpatialCoordinates`, `SpatialPullbacks`, `NeuroVolume`, `NeuroSeries`,
`NeuroSeriesSeq`, `FieldValidity`, `SeparableVolumeFilters`, `WorldLink`,
`Identifiers` and JVM `Nifti`; spatial `Ids`, `Domain`, `SpatialQc`;
surface `SurfaceFaceField`; locus `DomainFactory`/`package`.
Replace callers of unsafe IDs with existing validated constructors. Replace
sample-space forwarding with provider APIs. Provide validated scoped bulk
read/write capabilities for series, face fields and IO; do not expose raw
constructors or copy full arrays to avoid access errors. Move package-private
MVPA families together; fit/dataset integrations use admitted public evidence
constructors. External-package compile tests must cover each exposed seam.

Keep `scalafim-world:*`, the retired `scalafim-ras-d3` refusal, all
`scalafim.mvpa.*` digest domain separators, `scalafim-measurement*/v1`, atlas
`org.scalafim.atlas/*/v1`, bilateral surface hashes, standard surface route
schemas, and scene/profile tags unchanged during package moves. Preserve byte
order, counts, escaping, enum labels and signed-zero behavior. Schema migration
is a separate explicit task if semantics change.

Maintained downstream consumer: `../plsneuro`, inspected at
`2e0aac8e90300fd5a87d5b0d79d3bb9b78ecd1bf`. Its adapters and desktop directly use
image/spatial/atlas/surface APIs; `project/Providers.scala` forwards source-build
overrides. Include its public-import/build and workflow probes at relevant
cutovers. Preserve its other worktrees; migrate the intended integration
candidate, not historical snapshots. `neuropublish` and `eidolon` are
experimental consumers per the user: record known breakage/migration guidance,
but do not block cutovers on those projects. Persisted atlas publication schemas
remain stable regardless of the experimental status of a renderer consuming them.

Previous Mote work is evidence, not permission to skip a cutover:
native-image epic `bd-01KYNR99SWCFEW94T4JGD9QPBS` is closed (retain the native
representation); unified-MVPA `bd-01M2BMGRP4MSM0RRRKNTHM0H4K` is doing;
spatial-transform `bd-01M39Q2YNKRTJTD8PZ4694DS52` is open; historical mesh epic
`bd-01KYT2FD73TC4X1SC10HB5NCW3` is closed, but the current source still owns
`TriangleMesh` and local topology, so U3's deletion/parity gate remains required.
`surface/warm-update-20260921` remains unmerged with unresolved native correctness
and performance gates. This program neither lands it nor upgrades its evidence.
