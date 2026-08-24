# Response/Representation/Archive Phase 8 Record

Status: complete; repository-wide JVM/Scala.js gates passed  
Issue: `bd-01KYAA8PCPZNAAFKHRNXP5TWGR`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 7 record](response-representation-archive-phase-7.md)

Phase 8 makes the response, representation, archive, and dataset ownership
rules executable. Generic archive contracts, the LNA format profile, pure
representation mathematics, pure dataset semantics, and their compatibility
assembly now have distinct physical artifacts.

## 1. Extracted ownership

The live artifact split is:

```text
archive
  format-neutral revisions, publication, resources, payload execution,
  canonical values, and physical receipts

archive-lna
  LNA schema, manifest codec, validation, quant/delta payload codecs,
  shared-basis artifacts, JVM HDF5 stores, and eager physical driver

latent
  archive-independent representation contracts, encoders, decode plans,
  temporal bases, transport/BOLDZip semantics, and radial mathematics

dataset
  pure scientific descriptions, query lowering, synchronous-reader
  capabilities, and checked response-source attachment

interop-archived-response
  LNA reconstruction, archive-specific latent codecs, LNA dataset
  adapters, representation/archive bindings, registries, and runtime
```

Domain package names remain aligned with the values they describe inside the
new artifacts. Physical artifact ownership, not a package prefix, is the
enforced boundary.

The generic `archive` artifact no longer depends on `image` or jHDF.
`latent` no longer depends on `archive`. `dataset` no longer depends on either
`archive` or `latent`, and `DatasetError` no longer exposes either domain's
error algebra. Adapter failures are attributed to a neutral `OperationId`.

## 2. Recorded secondary edges

Two live edges are intentional and visible in `build.sbt` and
`docs/module-relations.md`:

- `latent -> image + locus-kernel` supplies the established `SomeSampleSpace` and
  ordered-mask geometry used by pure HRBF/radial mathematics;
- `archive-lna -> image` preserves the established owned row-major matrix wire
  values used by the LNA schema and shared-basis artifacts.

Neither edge restores archive knowledge to `latent`, representation knowledge
to `dataset`, or scientific reconstruction to generic `archive`. Removing
either edge would require a separate geometry or wire-value adapter migration;
it is not hidden inside Phase 8.

## 3. Executable boundary guard

`ResponseArchiveBoundaryGuardSuite` checks the production tree and build:

- latent main sources import no archive package and contain no HDF5, Zarr,
  object-key, archive-path, byte-codec, or checksum vocabulary;
- dataset main sources import neither archive nor latent;
- archive main sources contain no DCT, Haar, HRBF, transport, BOLDZip, or
  reconstruction branch;
- required LNA format, reconstruction, latent codec, and dataset compatibility
  files have their new physical owners and no old copy remains;
- build edges exclude `latent -> archive`, `dataset -> archive`,
  `dataset -> latent`, and `archive -> image`;
- `archive-lna` and `interop-archived-response` declare the intended one-way
  dependencies.

The guard is a JVM architecture test because it inspects repository files. All
portable behavior moved with its shared JVM/Scala.js tests.

## 4. Numerical and behavioral continuity

The Phase 0 migration suite remains at its frozen source path and is excluded
from the pure `latent` test artifact. The interop artifact compiles and runs
that unchanged suite because it owns the archive-aware compatibility surface.
The corpus runner now invokes the interop JVM and Scala.js projects.

All 13 frozen reconstruction cases passed without regeneration:

```text
dense
LNA quant, delta, delta-quant, basis/embed, temporal DCT
explicit latent, temporal DCT, temporal Haar, shared basis, HRBF
transport, BOLDZip
```

The focused final state passed:

```text
archive JVM / JS                 4 / 4
archive-lna JVM / JS             9 / 6
latent JVM / JS                 44 / 44
dataset JVM / JS                67 / 54
interop archived response JVM  139
interop archived response JS   102
Phase 8 boundary guard            3
compileAll                    passed
testAll                       passed
Phase 0 numerical corpus       valid
git diff --check               clean
```

The full matrix also verifies downstream model, fit, MVPA, workflow,
dataset-Zarr, archive-Zarr, JVM-only HDF5, and browser targets against the new
acyclic graph.
