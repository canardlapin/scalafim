# Routing ownership after eco-neuroim-routing

This slice extracts anatomical volume routing and execution. It does not finish
the eco epic or replace mixed spatial cases with a universal provider hierarchy.

| Responsibility and symbols | Owner / destination |
| --- | --- |
| Directed shortest paths and simple-path enumeration, parallel route choices | graph4s `DirectedRoutes`, `RouteChoice`, existing `Digraph`/`ArcField`/`WeightedDigraph` |
| Native/template volume references, admitted D3 space and congruent mask | neuroim4s routing `VolumeReference`, `VolumeGrid`, `VolumeDomain`; actual geometry stays image4s/spatial4s |
| Checked runtime provider maps and fingerprints | neuroim4s routing `ProviderMapBinding`, `CoordinateMapFingerprint`; evaluation/composition stay reframe4s |
| Anatomical registration cost, inverse fallback and quality, route pullback | neuroim4s routing `VolumeRegistration`, `VolumeGraph`, `VolumeRoute` |
| Ordered voxel demand, nearest/trilinear weights, sparse sampling assembly | neuroim4s routing `VolumeSampling`, `SamplingPolicy`; sparse operators and numerical execution stay Gale |
| Volume read request, compact block, support expansion and standalone execution | neuroim4s routing `VolumeReadRequest`, `VolumeBlock`, `VolumeSource`, `VolumeMaterialization`, `VolumeExecution` |
| File freshness, header admission, byte windows, canonical order, endian/scaling and resource lifetime | optional neuroim4s routing IO `NiftiVolumeSource`; actual NIfTI codec/header stays image4s, anatomical evidence stays neuroim4s IO/spaces |
| Mixed `DomainKind`, `TemplateKind`, `SpaceRef`, `SamplingGeometry`, `Domain`, `DomainPart`, `DomainLocus` and IDs | ScalaFIM integration for now; volume payloads are upstream `VolumeGrid`, surface/hybrid payloads go to neurosurf4s U5, latent/functional IDs and acquisition modality/resolution stay ScalaFIM. No parallel voxel grid or coordinate identity is introduced. |
| `SpatialGraph`, `Morphism`, `MorphismPath`, `RoutingPolicy`, `Inverse`, `RouteTag`, `CoordinateMap` mixed cases, `VolumeRouting`, `SpatialError` | ScalaFIM integration adapter. Pure anatomical volume routes delegate upstream; functional policy stays downstream; surface/hybrid cases go to the optional neurosurf spatial integration in U5. Generic search is graph4s. |
| `SurfaceResamplingBinding`, surface fingerprints, `VolumeToSurfaceOperator`, `HybridOperator`, surface branches of `Backprojection` | neurosurf4s U5 and its explicit neuroim4s integration. They remain downstream in this slice. |
| `Demand`, `RowSelection`, `ViewPlan`, `FieldApi`, `FieldSource`, `FieldRuntime`, `LazyFieldRuntime`, `FieldExplanation` | ScalaFIM functional/latent field layer: observation/value transforms, root revisions, demand, caches, provenance and explanations. `NiftiFieldSource` and `VolumeReadErrors` attach this layer's source identity to the anatomical reader. |
| `PullbackCompiler`, `MorphismPlugin`, `StagedOperatorCompiler`, `OperatorCompiler`, `OperatorSignature`, `SpatialQc`, `GaleSpatialSupport` | Mixed integration and functional/value-stage compilation stay ScalaFIM; volume sampling is upstream, generic linear operators stay Gale, surface/hybrid compilation goes to neurosurf U5. Existing recipe/signature wire strings are retained. |
| `TransformAssetLoader`, `SpatialTripletFileCache` | ScalaFIM persisted asset/catalog integration; toolkit interpretation remains neuroim4s transform. Atlas manifests/catalog routing move in eco-neuroatlas-volume/surface, never into the volume routing core. |

Frame ownership checks, forward-first routing, parallel-edge order, inverse
penalties and minimum path quality are preserved. Sampling retains corner order,
partial-boundary weight normalization, coverage, canonical ordinal order and one
pullback per target row. Scoped execution retains structural sparse support,
requested observation order, freshness before cache hits, and closed resources.
The optional provider modules have external consumer tests without ScalaFIM,
surface, atlas, response or dataset dependencies.
