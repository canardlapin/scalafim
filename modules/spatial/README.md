# scalafim-spatial

`scalafim-spatial` is the typed spatial-functor layer for ScalaFIM. It recasts
the durable core of `neurofunctor` as Scala 3 values: domains, spaces,
geometries, morphisms, sampled linear operators, provenance, QC, and lazy field
views.

The first slices establish the domain/id model, immutable graph routing, and a
portable affine volume-to-volume operator compiler.

Package root:

```scala
import scalafim.spatial.*
```

Shared code stays cross-platform and dependency-light. JVM-only ingest,
fMRIPrep graph descriptors, transform-file metadata, and file-backed triplet
caches live under `scalafim.spatial.io`.

## Current Scope

- `DomainId`, `MorphismId`, and `OperatorId` opaque identifiers.
- Supporting typed labels for subject, session, modality, template,
  resolution, basis, and hybrid part names.
- `SpaceRef` for volume, surface, template, and latent spaces.
- `SamplingGeometry` for volume, surface, and hybrid sampled domains.
- `Domain` and `DomainPart` with explicit sample counts and hybrid offsets.
- `SpatialError` as the shared error ADT.
- `Morphism`, `CoordinateMap`, `SpatialGraph`, and `MorphismPath` for immutable
  route selection with geometric inverse policy.
- `ImageMorphismBridge` for lowering executable identity/affine graph paths
  into `scalafim.image.SpatialMorphism` values without duplicating coordinate
  execution in this module.
- `CompileRequest`, `SamplingPolicy`, `SpatialOperator`, and
  `OperatorCompiler.volumeAffine` for sampled volume affine projectors backed by
  `scalafim.linalg.LinearMap`.
- `CoverageReport`, `OperatorQc`, and `OperatorProvenance` for inspectable
  compiler output.
- `SpatialQc`, `QcTolerance`, `QcCheck`, `QcReport`, and `TripletFixture` for
  identity, composition, adjoint, ROI, coverage, and fixture law checks.
- `VolumeToSurfaceRequest` and `VolumeToSurfaceOperatorCompiler` for sparse
  midpoint/ribbon volume-to-surface projectors with mask-aware coverage and
  adjoint backprojection.
- `Field`, `FieldDataRef`, `FieldRuntime`, `OperatorCacheKey`, and
  `InMemoryOperatorCache` for inspectable lazy field views over compiled
  operators without mutating `SpatialGraph`.
- JVM-only `scalafim.spatial.io` descriptors for fMRIPrep-style ANTs/FSL/AFNI,
  FreeSurfer LTA, and X5 transform assets, explicit inverse-quality state, and
  binary sparse-triplet cache artifacts that roundtrip `OperatorCacheKey`,
  `OperatorProvenance`, and `SparseTriplets`.

## Design Constraints

- Zero-based sample indexing.
- Immutable public values.
- Smart constructors returning `Either[SpatialError, ...]`.
- Shared code has no JVM-only dependencies.
- File-system ingest and cache artifacts stay behind the JVM package boundary.
- Hybrid/grayordinate domains use ordered parts with stable offsets.
- Operator adjoints are separate from geometric inverses.
