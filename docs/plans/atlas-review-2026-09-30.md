# ScalaFIM atlas review and neuroatlas comparison

The atlas module has a sound architectural center: an atlas realization owns an exact spatial-to-parcel assignment, and parcel metadata, networks, regions, and dense images derive from that assignment. Keep that design. The main work now is to make the public API preserve its guarantees and to complete parcel-value workflows, composition, and standard asset acquisition.

The most serious finding is that persistent parcel identity can certify an incorrect Glasser volume/surface alignment. Several smaller public API defects also reproduce. Adding more atlas families before fixing those boundaries would multiply the same problems.

## Scope and evidence

Reviewed the current working trees on 2026-09-30, including existing uncommitted atlas changes. At intake, ScalaFIM HEAD was `9504d6962fa39dbcbda27732e8f8291ab3259fbc`; neuroatlas HEAD was `d65ff97cb9316dbb92e4b9decbd2d7eb9ccf85fd`. ScalaFIM advanced concurrently to `7989608d32ed8283ed11f5ff3df54c7c7761a23e` during the review, absorbing the pre-existing atlas edits. The reviewed atlas source hashes did not change between the post-test snapshot and final verification. No existing source, test, or documentation file was edited by this review.

The comparison covers computational code, atlas datasets, metadata, loaders, queries, reductions, parcel results, graphs, and transform integration. Plotting, colors as a presentation system, interactive viewers, and rendering are excluded. Visual-cortex atlases such as Wang and Julich V1–V5 are anatomical datasets and remain in scope.

Verification:

- JVM: **121 tests passed**, including six temporary counterexample probes; 115 were repository tests.
- Scala.js: **85 tests passed**, including the same six probes; 79 were repository tests.
- `Rscript tools/r-parity/check_neuroatlas_atlas_parity.R`: passed against the local neuroatlas tree. This checks the synthetic reduction and overlap fixture, not the whole R package or every atlas family.
- The JVM run also executed the cached TemplateFlow composite and numerical-inverse tests; these were not skipped for missing assets in this run.
- No Scala compiler warnings appeared in either retained atlas test log. The JVM run emitted SLF4J no-provider notices; R reported that testthat was built under a newer R patch version.

The probes assert the observed defective behavior, so their passing confirms the counterexamples; it does not mean the defects are fixed. They use synthetic data and metadata. The Glasser probe exercises the exact parcel-domain restoration used by realization constructors, with the documented opposite ID conventions; it is not a full-density Glasser asset qualification.

Evidence is in [/private/tmp/scalafim-atlas-review-20260930](/private/tmp/scalafim-atlas-review-20260930): [probe suite](/private/tmp/scalafim-atlas-review-20260930/probes/AtlasReviewProbeSuite.scala), [JVM log](/private/tmp/scalafim-atlas-review-20260930/atlas-jvm-probes.log), [JS log](/private/tmp/scalafim-atlas-review-20260930/atlas-js-probes.log), [R parity log](/private/tmp/scalafim-atlas-review-20260930/neuroatlas-parity.log), and [source hashes](/private/tmp/scalafim-atlas-review-20260930/review-source-hashes.json). Each command log has a JSON sidecar recording argv and actual exit status. These temporary artifacts may be cleared by the operating system. The source hashes were captured after testing as a review snapshot, not as an isolated build receipt.

## Confirmed defects

### P1 Glasser parcel identity ignores opposite label conventions

[AtlasPublication.scala:286](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/AtlasPublication.scala:286) constructs parcel keys from atlas family, model, parcel variant, release, and numeric `RegionId`. The namespace at [line 342](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/AtlasPublication.scala:342) does not include a label convention or a canonical anatomical parcel key. Glasser's volume and surface descriptors both use the default parcel variant.

Glasser volume ID 1 and surface ID 1 can refer to opposite hemispheres. neuroatlas explicitly documents volume `hcp_R_first` and surface `surfatlas_L_first`, and [parcel_data.R:710](/Users/bbuchsbaum/code/neuroatlas/R/parcel_data.R:710) rejects ID joins without a matching convention. Its cross-representation keys are canonical full labels or area plus hemisphere.

The reproduction gives volume ID 1 right-hemisphere V1 and surface ID 1 left-hemisphere V1. Scala produces identical element keys and fingerprints, restores them as the same live parcel owner, and accepts exact domain alignment. A downstream field can therefore align to the wrong anatomy while satisfying the current domain certificate.

**Recommendation:** distinguish anatomical parcel identity from source integer encoding. Use validated canonical parcel keys for each release/variant, retain local label IDs and their convention as encoding metadata, and certify the conversion from source labels to canonical keys. Where a source lacks a known mapping, keep its parcel domain distinct and require an explicit checked bijection. Do not merely put `Volume` or `Surface` into every namespace: genuine cross-representation identity is useful and already has tests.

### P2 Phantom tags are forgeable and can contradict their values

[AtlasRef.scala:22](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/AtlasRef.scala:22) exposes `SpaceIdOf[K]`; [line 171](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/AtlasRef.scala:171) exposes `AtlasRefOf[R]` with an independently supplied runtime representation and unrestricted space fields. Public construction and `copy` permit:

```scala
val forged: VolumeAtlasRef =
  AtlasRefOf[AtlasRepresentationKind.Volume](
    "review", "Forged", AtlasRepresentation.Surface,
    SpaceId.FsLR32k, SpaceId.FsLR32k
  )

val copied: VolumeAtlasRef =
  GlasserHcpMmp1().atlasRef().copy(
    representation = AtlasRepresentation.Surface,
    coordSpace = SpaceId.FsLR32k
  )

val fakeSpace: VolumeSpaceId =
  SpaceIdOf[SpaceKind.Volume]("fsLR_32k")
```

All compile and construct on JVM and JS. Later realization validation rejects some contradictions, but the reference type itself does not supply the promise implied by its name. `SpaceId.volume` is also an unchecked escape hatch for known surface names.

**Recommendation:** make constructors inaccessible to ordinary callers and derive the runtime tag from the representation type, or use a sealed representation ADT with representation-specific fields. Expose checked parsing and safe metadata updates. Treat known volume/surface conflicts differently from genuinely unknown custom spaces. Do not add phantom tags unless their constructors and update methods preserve them.

### P2 ParcelValues permits duplicate and inconsistent records

[Reduce.scala:16](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/Reduce.scala:16) checks coverage using sets. A two-parcel atlas accepts three values with IDs `[1, 1, 2]`, including contradictory values for ID 1. `value(RegionId(1))` silently returns the first. Supplied metadata is also checked only by ID, so a record can carry the wrong label or hemisphere for that ID.

**Recommendation:** store values as `locus4s.data.Field[P, A]` on the realization's exact parcel owner and derive records in display order. At an input-table boundary, reject duplicate, unknown, and missing keys under an explicit completeness policy. Do not use set equality as a uniqueness proof.

### P2 The query entry points disagree about the default coordinate space

[Query.scala:21](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/Query.scala:21) defaults the input space to MNI152, while [syntax.scala:7](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/syntax.scala:7) defaults to `atlas.ref.coordSpace`. On an MNI305 atlas, `AtlasQuery.exact(atlas, point)` transforms the input while `atlas.query(point)` does not. The two calls return different atlas coordinates for the same supplied point.

**Recommendation:** give the native-coordinate path one canonical expansion and use it consistently. Offer an explicit source-frame path for external coordinates, coupled to the existing image geometry/frame model. Keep radius units explicit rather than adding another point representation in atlas.

### P2 Metadata constructors disagree about validity

[Region.scala:75](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/Region.scala:75) permits `labelFull = Some(" ")`, while `AtlasRegionMetadata.checked` rejects it. A supposedly valid instance then throws when reading `typedFullLabel`.

The core fields still store raw labels and attribute maps; their typed getters perform validation after construction. This gives callers two representations and two validation paths to reason about.

**Recommendation:** store `RegionLabel` and `RegionAttributes` as the actual fields, with raw-string convenience constructors that delegate to one checked path. Keep genuinely extensible attributes extensible; mandatory anatomical identity and source label conventions should have explicit fields. Opaque positive IDs can remove boxing without changing the public meaning.

### P2 Subsetting omits the membership derivation

[VolumeAtlas.scala:69](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/VolumeAtlas.scala:69) changes membership and updates the label schema, but returns the original reference and unchanged derivation vector. The resulting provenance does not record which parcels were removed, even though `DerivationStep.FilteredLabels` already exists.

**Recommendation:** record kept/dropped canonical keys and the parent realization identity. Preserve the source atlas's release identity while recording the derived support. Implement selection through the existing assignment/domain algebra where possible, rather than rendering a full label image and reconstructing the realization.

## Capability comparison

| Capability | neuroatlas implementation | Current ScalaFIM position | Recommended direction |
| --- | --- | --- | --- |
| Atlas references, provenance, citations | `atlas_ref.R`, `atlas_provenance.R`, `atlas_catalog.R` | Substantial typed support, including structural publication records | Consolidate compact reference and richer provenance; repair derivation gaps |
| Parcel lookup and ROI membership | `get_roi.atlas`, `get_roi.surfatlas`, `roi_metadata` | Metadata lookup plus exact `realization.region` for both representations | Add a small common facade; preserve exact locus regions |
| Scalar and time-series parcel reduction | `reduce_atlas`, `reduce_atlas_vec` | Volume reductions, custom reducers, masks for series, explicit empty-series policy | Return parcel fields consistently; expose masks and missing-value policy uniformly |
| Network and hemisphere summaries | `reduce_atlas(level=..., by=...)`, `atlas_hierarchy` | Total parcel-to-network quotient when every parcel is annotated; no comparable summary facade or hemisphere grouping | Typed grouping maps and explicit weighting policy |
| Parcel-value alignment | `align_parcel_values`, `parcel_values` | `ParcelData.fromValues` only zips a positional vector | High priority: checked key-based alignment onto the existing parcel domain |
| Parcel metric expansion | `parcel_volume` | Categorical rendering in `VolumeParcellation`; no direct continuous parcel-result expansion facade | Add continuous image expansion in the appropriate image layer, with atlas adapters and explicit background |
| Parcel-result persistence | `write_parcel_data`, `read_parcel_data` | No corresponding metric-table codec; publication projection covers atlas structure, not parcel-result round trips | Versioned codec carrying parcel identity and schema; shared schema, platform IO adapters |
| Atlas composition | `merge_atlases` | No atlas composition API | Exact-grid composition, explicit overlap policy, source-key remapping, both parents retained |
| Parcel growth | `dilate_atlas` | No atlas-level equivalent | Image-layer algorithm, atlas derivation wrapper; explicit voxel/mm radius and tie policy |
| Volume subsetting | `sub_atlas`, `filter_atlas` | Predicate-based subset exists | Keep predicates; add checked empty-result behavior and derivation evidence |
| Surface subset and field reduction | Surface subset explicitly errors; R reducer is volume-oriented | Neither has an atlas facade in Scala | Useful extension beyond R, built on the bilateral assignment |
| Volume overlap | `atlas_overlap` | Dice/Jaccard/counts with explicit exact/nearest alignment | Keep explicit alignment; expose coverage where useful |
| Surface overlap | R overlap is volume-oriented | No atlas facade | A possible extension using exact surface owners; not required R parity |
| Parcel graphs | `atlas_graph`, `atlas_connectivity`, igraph conversion | Volume contact graph, semantic quotient relation, surface contacts/distances | Keep graph4s; distinguish boundary counts from metric boundary length |
| Functional connectivity | `atlas_connectivity.R` | Estimators and workflow types already in `modules/connectivity`; no atlas-to-series adapter found | Adapter preserving canonical parcel/node identity, ordering, and sample timing |
| Batch extraction | `batch_reduce.R` | No named subject/input batch facade | Outer workflow composition over the same reducer; keep scheduling and file loading outside the pure atlas core |
| Spin inference | `spin_test.R` | No spin implementation found in ScalaFIM source | Separate inference work using typed spheres and aligned fields; explicit reassignment and missing-data conventions |
| Template acquisition | `template_flow.R`, `fsaverage.R` | Local TemplateFlow cache lookup, pinned sphere loading, a specific MNI bridge | General explicit asset resolver/query adapter if needed; no implicit core downloads |
| Atlas transform execution | `transform_atlas`, transform manifests/plans, provider routes | Shared `spatial`/`transform` machinery and atlas catalogs; some routes remain planned | Thin atlas transport adapters and named supported routes, not another transform engine |

The R grouping reducer summarizes parcel summaries. A mean over parcels is generally different from a mean over their combined voxels when parcel sizes differ. Scala should name that choice rather than silently treating both as “network mean.” Partial network metadata currently suppresses the entire network assignment ([AtlasRealization.scala:701](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/AtlasRealization.scala:701)); a partial grouping capability would preserve known annotations without pretending they form a total surjection.

Likewise, R's dilation currently works in voxel units and uses neighbor votes, and its merge lets the second atlas overwrite the first. Those are policies to make explicit, not defaults to inherit. R's spin reassignment uses nearest centroids and does not itself prove a bijection; a Scala implementation should not call an arbitrary nearest-neighbor map a permutation.

## Missing atlas datasets and loading workflows

Scala currently has four volume loader families—Schaefer, Glasser, Brainnetome, and ASEG—and pure Schaefer/Glasser surface descriptors plus generic GIFTI loading. The current R catalog also includes:

- **HCPex**, including cortex and subcortical coverage.
- **Harvard–Oxford** cortical/subcortical variants and **Julich-Brain**, with a generic FSL XML loader and the important XML/probability-index to summary-label offset.
- **Olsen MTL** and hippocampal subfields.
- TemplateFlow-backed **CIT168**, **HCP thalamus**, **MDTB10 cerebellum**, and **HCP hippocampus/amygdala**.
- **Wang visual topography**, **visfAtlas**, and the **Julich early-visual subset**.

These are supported by [atlas_registry.R:210](/Users/bbuchsbaum/code/neuroatlas/R/atlas_registry.R:210), [subcortical_atlases.R:297](/Users/bbuchsbaum/code/neuroatlas/R/subcortical_atlases.R:297), and their corresponding loaders. The early-visual subset can be a documented selection over a Julich atlas rather than a new payload implementation.

Wang's probability-volume acquisition is also missing, but be precise about the reference: R returns per-area volumes and metadata; it does not supply a general validated soft-atlas algebra. The FSL probability path explicitly refuses to load a probability image as a discrete atlas. A true probabilistic atlas would be a new Scala scientific capability, not a straightforward port.

For surface assets, the principal missing workflow is named atlas loading from the formats actually distributed upstream. The module has generic GIFTI labels but no standard Schaefer/Glasser acquisition adapter or atlas-level FreeSurfer annotation/CIFTI loader. Reuse the lower surface readers and geometry capabilities; keep atlas-specific IDs, metadata, and source policy here.

`AtlasRegistry` currently discovers descriptions, not executable typed requests. The mismatch between its Glasser default space and the no-argument descriptor's uncertain xcpEngine source is another reason to connect discovery to explicit requests. Prefer typed per-family specifications or a closed request ADT with an honest existential result for dynamic discovery; avoid an R-style function accepting an arbitrary options bag.

## Scala API direction

The low-level types are substantially better than the high-level result containers. `AtlasRealization` already couples membership to `X`, parcels to `P`, metadata to `Field[P, AtlasRegionMetadata]`, display order to `Selection[P]`, and network grouping to `Surjection[P, N]`. Use that information all the way through a workflow.

The key shape is:

```text
source labels + explicit encoding convention
              -> checked canonical parcel assignment
              -> Field[P, A] / parcel time series
              -> checked key alignment, grouping, image expansion, persistence
```

Recommendations:

1. **Keep one membership algebra.** Use existing locus4s fields, regions, selections, partial assignments, and certified bijections. Avoid an atlas-specific duplicate of each generic abstraction.
2. **Make identity independent of storage encoding.** Canonical keys identify anatomy; positive integer labels identify a particular source encoding. Retain both and certify their relationship.
3. **Use one parcel-value model.** `ParcelValues` and `ParcelData` currently provide competing detached-record forms. Make typed fields authoritative, with record/table views at boundaries. Let volume and surface share this model.
4. **Preserve inferred ownership in results.** Offer a typed entry point whose result retains the named realization's parcel type. Existential convenience results are useful, but should not force users to rediscover and align proof already available to the reducer.
5. **Store validated core metadata.** Typed label getters should not be the first validation point. Keep arbitrary optional attributes separate from mandatory identity fields.
6. **Name scientific policy.** Completeness, missing data, empty support, grouping weights, overlap, dilation metric, and transform direction deserve small explicit ADTs or checked values. Avoid a large configuration DSL.
7. **Keep acquisition separate.** Shared descriptors and computation remain pure; JVM file/network interpreters should return structured errors. Throwing convenience methods can delegate to checked methods, but should not be the only entry point for standard loaders.
8. **Make common programs direct.** A native query, keyed metric alignment, grouping, and expansion should compose without callers manually shuttling domain certificates. The facade should delegate to the same core semantics, including ordering and errors.

A soft atlas, if pursued, should represent weighted/overlapping membership separately from a hard `PartialSurjection`. Specify probability scale, overlap/normalization rules, missing coverage, and maximum-probability conversion before implementing it. Generic weighted operators belong in locus4s/Gale as appropriate; atlas owns scientific metadata and conversion policy.

Several surface methods accept the broad `surface.Hemisphere` enum and throw for `Both` or `Unknown`, although the surface module already supplies [CorticalHemisphere](/Users/bbuchsbaum/code/scala/scalafim/modules/surface/shared/src/main/scala/scalafim/surface/SurfaceTags.scala:3) with exactly Left and Right. Use that existing type for one-side access, and let a compatibility adapter perform checked conversion. This is a direct opportunity to remove runtime invalid states without inventing more types.

## Source observations about allocation and layering

These are source observations, not benchmark results:

- Exact overlap calls `labelVolume` merely to validate geometry ([Overlap.scala:53](/Users/bbuchsbaum/code/scala/scalafim/modules/atlas/shared/src/main/scala/scalafim/atlas/Overlap.scala:53)). Each call materializes a fresh dense image even though both exact grids already exist on the realizations. Validate those grids directly.
- Volume subsetting renders the full volume and rebuilds the realization. An assignment restriction should avoid that dense round trip where the provider algebra permits it.
- The standard reducer path is chosen by function-reference equality. Wrapping the same mean computation loses that path. A small reducer algebra can make supported aggregation and missing-data behavior inspectable while retaining an explicit custom escape hatch.
- Publication assignment vectors are eagerly retained alongside the live assignment, despite the stated goal of deriving materializations. Consider deriving publication payloads lazily when exporting, with a clear reuse policy for expensive immutable results.
- The large versioned publication DTO/encoding file is useful interchange infrastructure. Keep its implementation details out of ordinary atlas workflows; do not replace it with a second parcel-identity scheme for result serialization.
- The README/design guide still show removed APIs such as `ClusteredNeuroVol`, `reduceVec`, `quotient`, and boolean resampling arguments. Update and compile the examples after settling the facade. The current documentation overstates how direct some workflows are.

## Recommended order

1. Repair canonical Glasser identity and constructor/update guarantees. Convert the temporary probes into regression tests that expect safe rejection or correct alignment.
2. Complete parcel fields: keyed alignment, continuous expansion, persistence, and consistent metadata. This is the highest-value missing workflow foundation.
3. Add atlas composition, checked selection with derivation, and typed grouping. Build hemisphere grouping and partial-network handling on the same algebra.
4. Add FSL XML/Harvard–Oxford/Julich, HCPex, and the subcortical/MTL families using shared loader components and pinned asset specifications.
5. Add standard surface asset adapters and then surface selection/reduction as an improvement beyond the current R API.
6. Add thin connectivity, batch, and transport adapters in the appropriate outer modules. Treat spin inference and soft atlases as separate scientifically verified work.

The existing asset store supports optional expected SHA-256, but standard Schaefer/Glasser assets currently use mutable branch URLs without expected digests. Hashing the files after reading records what happened; it does not pin acquisition. neuroatlas's HCPex and Glasser annotation acquisition provide stronger examples of immutable source and integrity specifications. Adopt that pattern for supported standard assets without making hidden downloads part of the computational core.

The module does not need a wholesale rewrite. It needs its excellent exact-assignment core to become the public API's center, with fewer detached records and fewer parallel declarations of identity. The six reproduced defects should come before catalog expansion.
