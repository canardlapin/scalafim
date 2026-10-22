# U6 delivered ownership and test custody

This ledger records implementation ownership, not final epic acceptance. U6
build/docs consolidation and the immutable final regression/scenario campaign
remain separate tasks. The [U0 map](../../plans/ecosystem-ownership.md) is the
historical design; this ledger records the delivered boundaries and the reason
functional field execution remains in ScalaFIM.

`baseline-implementation-custody.json` inventories all 352 production Scala
paths in the affected families at U0 `3a2fda43c0a1155832bfb2dc5c4ead1503038190`.
It records their owner family and whether the old path remains. A retained path
does not imply retained implementation: the spatial adapter split is below.
The exact last locus/prototype input source at Root
`0ba3e7e99c2b43b51e0e51beb3233d9daad7dd56` is preserved in
`scalafim-eco-u6-retired-inputs-07.tar.gz`; its JSON inventory binds each file
and the archive SHA-256. Nothing relies on an unversioned temporary file.

## One owner per replaced family

| Retired ScalaFIM family | Live owner and boundary | Behavior/test custody |
| --- | --- | --- |
| `image`: sampled containers, masks, validity, selections and neuro admission | neuroim4s core; native sampled storage/axes and generic operations in image4s, finite support in locus4s, numeric kernels in Gale/Multivar | [Core qualification](../neuroim-core-20261019/README.md) and [affine admission](../neuroim-core-affine-20261019/README.md); provider suites and independent public consumers travel with the behavior. No local image module or container remains. |
| `transform`: generic coordinate maps and neuro toolkit IO | reframe4s map/field/resample; neuroim4s transform and optional JVM transform IO | [Transform cutover](../neuroim-transforms-20261020/README.md); toolkit fixtures and admitted source conventions stay with the owning provider. Experimental HalfFlow is not a ScalaFIM dependency. |
| Shared world/frame declarations | neuroim4s-spaces over spatial4s | One nominal coordinate/frame owner. Existing world grammar, full acquisition evidence and retired-RAS refusal are preserved; no ScalaFIM WorldSpace/FrameCatalog/NativeContext definitions. |
| `surface`: topology, points, fields, geometry, transport and IO | mesh4s topology/point primitives; neurosurf4s anatomy/IO and optional transport/volume/routing | [Mesh kernels](../mesh-kernels-20261020/ownership.md), [geodesic custody](../geodesic-extraction-20261020/ownership.md), [U5 cutover](../eco-u5-20261021/surface-cutover.json). Strict admission, ordered topology and one vertex owner remain provider contracts. Fixtures, examples, tools and persisted topology goldens moved with them. |
| Volume and surface viewers | optional neuroim4s and neurosurf4s view/backends; Intaglio primitives | Owning provider suites and retained native/browser receipts; Root keeps only connectivity-result adaptation. These remain optional platform artifacts. Prior native/affine/performance qualification limits remain in their original receipts. |
| `atlas`: ontology, membership, publication, volume/surface realizations and routes | neuroatlas4s core plus optional volume/surface/routing/IO | [Volume custody](../neuroatlas-volume-20261020/ownership.md) and A1 provider qualification. Core assignment and metadata do not require a volume or renderer. Canonical publication bytes and real surface/volume digest fixtures stay upstream. |
| `mvpa`: axes, reindexing, observations, measurement and scientific execution | mvpa4s core/predictive; functional fit/dataset/group/pipeline/storage integrations stay in ScalaFIM | Native axis/observation/reindexing suites, scientific parity fixtures and public consumers. Repeated draws are occurrence-bearing reindexings, not injective locus selections. |
| `mvpa-spatial` generic locus/surface/atlas adapters | optional mvpa4s-locus/surface/atlas | [U6 adapter receipt](../eco-u6-20261022/README.md); exact surface fingerprints, atlas key/support correspondence, sample-owner bijection and independent JVM/JS public consumers. Root retains functional threshold policy. The atlas example intentionally adopts native complete keys rather than old `region-N` IDs. |
| `response/Provenance` identity DAG and grammar | optional locus4s-provenance | Response and MVPA use the same provider contract. Functional field materialization provenance and fMRI result provenance remain distinct consumer records, not alternative source-lineage DAG owners. |
| `locus-data/DomainFactory` and old package facade | locus4s `DomainRecord`, explicit `DomainRegistry`, `FiniteDomain.ephemeral` | Dataset acquisition, connectivity node/edge admission and latent radial selection call native constructors directly. Each independent restore has an independent live owner; equal persistent keys need checked alignment. No global registry or replacement production forwarding helper. |
| `locus-data/Parcellation` | locus4s `PartialSurjection`, `Surjection`, `PartialMap`, native `Aggregation` | Eight translated public-provider law cases plus two aggregation cases in `provider-spike`: fiber partition/disjoint union, false partition, composition/associativity, relabeling, total lift, partial aggregation fusion and background exclusion. Native typed refusals replace obsolete wrapper error spellings; no tolerance was loosened. |
| `locus-data/Searchlight`, `CenteredSearchlight` | locus4s `NeighborhoodSystem` and separate checked `CenteredNeighborhoodSystem` | Four translated consumer cases retain sparse ambient center lookup, endpoint owner refusals and missing embedded-center refusal. Compact relation row count is checked before construction. Native neuroim4s `ExactVolumeSearchlight.fromRelation` and `DomainValiditySuite` retain ambient nonempty-outside-centers refusal before compaction. |
| Old MVPA foundation admission scaffold and standalone old-pin runner | shipping mvpa4s contracts plus existing unpublished `provider-spike` court | Prototype removed only after the behavior census below. All eight ProviderAdmissionSuite bodies moved; package changed and two unused imports removed under the destination's unchanged strict warnings. The obsolete runner/build are archived, not a current qualification path. |

Current immutable provider sources are those in `build.sbt`: mvpa4s `c2746948`,
neurosurf4s `73a244fb`, neuroatlas4s `4df875d8`, neuroim4s `4f75c2bf`,
locus4s `e32b0aeb`, image4s `855a3a61`, reframe4s `622ab31f` and Gale
`779ed3d8`. The final acceptance task must bind its receipts to full source SHAs.

## Preserved scaffold witnesses

The five old locus suites now live under
`modules/provider-spike/shared/src/test/scala/providercontract/locus/`:
DomainIdentityProbeSuite (7), IndexedFieldSuite (5), ParcellationSuite (8),
ProviderAggregationSuite (2), SearchlightSuite (4). DomainFixtures is private
test scaffolding, not production domain authority. All eight moved provider
admission cases live under `providercontract/admission`. Total: 34 per platform.
They exercise only public provider APIs; no provider imports ScalaFIM test code.

The ten IdentityPrototypeSuite witnesses map to native mvpa4s at `c2746948`:

| Witness | Native suite custody |
| --- | --- |
| Poisoned source admission before reads; complete descriptor; bounded metadata | ObservationsSuite admission/refusal/read-count tests; PatternOperatorSuite bounded-read tests |
| Dense, sparse and matrix-free identity, revisions and delayed reads | ObservationsSuite source constructors |
| Repeated draws, complete keys and occurrence identity | ReindexingLegSuite |
| Typed owner/ordinal and illegal construction compile refusal | AxisRefSuite |
| Reindexing lineage, nested restriction and multi-response | ReindexingLegSuite and ObservationsSuite |
| Heterogeneous measurement spaces | MeasurementLegSuite |
| Malformed axes, shape mismatch and foreign domains | AxisRefSuite, ReindexingLegSuite, PatternOperatorSuite, ObservationsSuite |
| Public compile controls | AxisRefSuite, ObservationsSuite, ReindexingLegSuite |

The exact old Lin API and batch object-identity witnesses are not assumed to be
covered by similar upstream value tests: their original eight-case admission
suite remains intact here. Native `PartialSurjection.andThen` is a typed
operation rather than a defensive checker for forged phantom endpoints. No
old dynamic Parcellation ingress remains that would need that wrapper check.

## The remaining spatial layer

The twelve shared files and JVM source adapter form one functional
sample-by-observation composition layer. They do not define another anatomical
domain, mesh topology, atlas membership, coordinate field or numerical solver.
This matches the already delivered [routing ownership split](../neuroim-routing-20261020/ownership.md).

| Retained files | Root responsibility and delegation |
| --- | --- |
| FunctionalRouting, StagedOperatorCompiler | Functional row projections, observation mixing, pointwise stages and barriers; anatomical compilation stays in provider routing, numerical application in Gale. |
| FieldRuntime, LazyFieldRuntime | Root/source identity and revision, demand-specific execution caches, result lineage and materialization. Uses the provider operator key and graph; no second operator identity. |
| ViewPlan, Demand, FieldApi | Functional intent, ordered spatial/observation selection and terminal-selection rules. Native domain selections enter through checked adapters. Unused VoxelRegion/region forwarding compatibility APIs are removed. |
| FieldSource, VolumeReadErrors, JVM NiftiFieldSource | Functional source admission and error adaptation. Actual file access, freshness and scoped NIfTI reads delegate to neuroim4s `NiftiVolumeSource`. |
| Backprojection | Functional route composition and selected-row adjoint; numerical transpose application delegates to Gale. |
| FieldExplanation, DomainDemands | Execution/QC explanation and checked adaptation from provider-owned selections; no new domain authority. |

The U0 proposal placed generic staged execution in a future reframe flow
contract. Exact-source review found no equivalent runtime in the delivered
providers: reframe DenseField is a displacement/velocity/momentum field;
neurosurf SpatialTripletFileCache persists operators and recipes. Neither
replaces source/graph/observation/demand/result caching. Creating another
provider execution family is not needed for this cleanup.

CachedFieldRuntime executes supplied operators sequentially; LazyFieldRuntime
compiles the root-to-target program. These are distinct semantics: fractional
interpolation can distinguish them, and a supplied operator may not be
reconstructible from a graph. This tranche does not replace either strategy.
The template-resampling test now calls the provider operator directly, preserving
the same independent native result/adjoint checks without unnecessary runtime
scaffolding. Any later runtime consolidation must explicitly choose semantics
and cover fractional chaining and supplied-operator discriminators. Existing
staged literal values, observation order, selected-row adjoints, cache freshness
and recipe/fingerprint witnesses remain required. No external legacy-runtime
caller census or retirement claim is made here.

## Enforced boundaries

`ecosystemBoundaries` resolves 22 actual transitive Compile closures: JVM and JS
for MVPA core/locus/surface/atlas, anatomical cores, spaces, and locus
core/data/provenance. It rejects Root backedges, Test outputs/testkits in Compile,
and forbidden anatomical/volume/render dependencies in those cores/adapters.
Neutral spaces and image geometry remain permitted in surface core; semantic
atlas core is permitted in the optional MVPA atlas adapter. It also rejects
multiple loaded immutable Git revisions of one provider. Explicit local build
overrides are supported; Git-URI checking alone does not certify their Git state.

The source guard scans production Scala roots, ignoring build outputs, comments
and string literals. It rejects retired implementation modules/namespaces and
known duplicated authority declarations (including aliases). This is a named
architecture tripwire, not a semantic proof against arbitrarily renamed code.
Six negative-control tests cover duplicate aliases, nested comments/literals,
retired source custody, transitive source/binary edges, missing platform/empty
closures and split provider revisions. Independent public-package consumers
remain separate executed evidence at each extraction boundary.

No module merge adds a forced fit, storage, graphics or platform edge. Test-only
foundation dependencies remain confined to the existing composition court.
