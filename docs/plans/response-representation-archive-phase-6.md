# Response/Representation/Archive Phase 6 Record

Status: complete; permanent module edge and repository-wide JVM/Scala.js gates passed  
Issue: `bd-01KYAA8NQH72CY0J7W3EXH04FZ`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 5 record](response-representation-archive-phase-5.md)

Phase 6 separates scientific dataset descriptions from effectful response
access, validates the seam once at attachment, and carries response evidence
through downstream dataset, fit, and MVPA reads.

## 1. Pure dataset and explicit read capabilities

`FmriDataset` now owns only scientific identity and query information:

- dataset identity, shape, voxel domain, metadata, and events;
- sampling frame and typed run-local time axis;
- pure selection resolution and run partitioning.

It has no effect parameter and no backend field. `DatasetRun` and
`DatasetIndex` are likewise pure. Synchronous indexed reads require an
immutable `SynchronousDatasetReaders` registry, so an index cannot hide a
reader in each description.

`DatasetSeriesReader` is the explicit synchronous capability.
`SynchronousFmriDataset` is the named compatibility facade that pairs a checked
description with a real `DatasetBackend`. Compatibility methods on a value
typed as `FmriDataset` succeed only when that value is this synchronous
subtype; a pure or asynchronous description returns
`SynchronousReaderNotFound` immediately. There is no blocking browser facade.

## 2. Checked response attachment

`OpenedDataset[F]` pairs:

```text
FmriDataset
+ ResponseSource[F]
+ AcquisitionContext
```

Attachment accumulates a non-empty chain of typed issues across:

- dataset, run, and response-schema identity;
- time count, units, and raw coordinate bits;
- sample count and volume-versus-surface kind;
- exact volume geometry and mask identity;
- surface identity and topology reference;
- sample ordering and explicit bijective alignment;
- signal units, calibration, and non-finite policy.

`SampleAlignment` permits an explicit reorder only when it is a total
bijection. Attachment never resamples, transforms coordinates, or infers a
topology. Response owns neutral identity, cardinality, ordering, and verifiable
references; image and surface modules remain the concrete geometry
authorities.

Planning resolves `DatasetRunQuery` and each run-local `DataSelection` into
ordered output-domain `ResolvedResponseSelection` values. No dataset path
addresses latent coefficients. Execution returns the original response
`ReadResult` inside each dataset segment, preserving derivation provenance,
logical selection, physical receipt, integrity evidence, and fallback truth.
Conversions to current `FmriSeries` values copy once into the existing image
matrix boundary.

The response value remains `ResponseBlock`, the specialized owned row-major
`Double` carrier. Phase 6 introduces no general tensor, scalar-generic
response, or public mutable alias.

## 3. Downstream effect boundary

Model descriptions, `FmriModel`, `FitPlan`, chunk plans, and numerical kernels
remain effect-free.

- current synchronous fit execution accepts a `DatasetSeriesReader`;
- `OpenedDatasetFitExecutor` reads through `OpenedDataset[F]` and then invokes
  the unchanged pure fit interpreter;
- current synchronous MVPA construction accepts a `DatasetSeriesReader`;
- `OpenedDatasetMvpaExecutor` builds pattern views through the same attached
  response path;
- compatibility overloads resolve only a real `SynchronousFmriDataset`.

Direct public `DatasetError` cases carrying archive- or latent-owned error ADTs
were removed. Legacy LNA adapters now report a named
`DatasetCompatibilityLayer` plus a textual boundary detail, avoiding those
module types in the dataset error surface.

## 4. Runtime ownership

`ScalafimRuntime.openDataset` is the named owner of the complete resource
order:

```text
driver selection and archive acquire
-> structure and publication validation
-> representation-family acquire
-> checked dataset attachment
-> OpenedDataset[F] use
-> representation-family release
-> archive release
```

Its `RuntimeResource` error channel distinguishes `Archive` from
`Attachment(NonEmptyChain[AttachmentIssue])`. Attachment failure releases both
already-acquired resources. Success, user failure, and cancellation preserve
the same nested release order as `openResponse`.

The runtime accepts the dataset and acquisition claim separately. This allows
identity and schema drift to be reported instead of erasing the comparison by
construction.

## 5. Focused executable evidence

The implementation state has passed:

```text
dataset JVM                    96 passed
dataset JS                     69 passed
fit JVM                       154 passed
fit JS                        146 passed
mvpa-dataset JVM                7 passed
mvpa-dataset JS                 7 passed
model JVM / JS                 16 / 16 passed
group JVM / JS                 50 / 50 passed
fmri-workflow JVM / JS         21 / 18 passed
dataset-zarr JVM / JS          15 / 7 passed
interop runtime JVM / JS       15 / 10 passed
compileAll                      passed
testAll                         passed
Phase 0 numerical corpus        valid
git diff --check                clean
```

The permanent `interop-archived-response -> dataset` edge is installed.
Repository-wide `compileAll` and `testAll` verify the resulting module graph
and all aggregated JVM and Scala.js targets.

Static production scans found no general tensor, effect parameter on
`FmriDataset` or fit plans, unchecked cast, untyped `Any` carrier, `null`,
warning suppression, or new deprecation warning. The Phase 0 numerical corpus
remained valid after the permanent edge was installed.
