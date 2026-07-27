# Response/Representation/Archive Phase 10 Record

Status: complete; repository-wide JVM/Scala.js gates passed  
Issue: `bd-01KYAA8Q1Z0HV74WQHFYYTE709`  
Governing plan: [response-representation-archive-architecture.md](response-representation-archive-architecture.md)  
Baseline: [Phase 9 record](response-representation-archive-phase-9.md)

Phase 10 converts the architectural acceptance criteria into reusable laws,
closes the remaining provenance and corruption gaps, and prevents new
representations from extending the staged legacy dispatcher. It does not add a
general tensor abstraction: response values remain owned row-major
`ResponseBlock` values over primitive `Double` storage, while representation
payloads retain their own typed scalar and shape contracts.

## 1. Reusable response laws

The new `response-laws` cross-project is a small test-support artifact that
depends only on `response`. `ResponseLawChecks` returns typed,
framework-neutral `ResponseLawResult` values for:

- requested order and exact block shape;
- selected-versus-whole agreement under the declared `DecodeConsistency`;
- ordered partition assembly;
- raw floating-point bit persistence, including NaN payloads and negative zero;
- axis-keyed locality and receipt conformance;
- provenance derivation that retains every parent node and directly references
  every parent root.

The law artifact owns no fixtures, effects, archive bindings, representation
mathematics, or production execution. Representation, dataset, and interop
suites provide those concrete values and reuse the same checks on JVM and
Scala.js.

## 2. Law matrix

The final executable matrix is:

| Contract | Executable owner |
|---|---|
| Response selection, order, shape, partition, locality, and provenance construction | `ResponseKernelSuite`, `ResponseLawChecksSuite` |
| Scientific reconstruction versus decode-path consistency | `TemporalDctRepresentationSuite` and the frozen Phase 0 corpus |
| Raw-bit persistence and typed LNA binding | `TemporalDctLnaBindingSuite`, `TemporalDctLnaHdf5Suite` |
| Memory/LNA/Zarr interop commutation | `TemporalDctCrossBackendSuite` |
| Single- and multi-acquisition attachment and hierarchy | `OpenedDatasetSuite`, `DatasetIndexSuite`, `DatasetMultiRunFixtureSuite` |
| Archive publication and interrupted-write absence | `CanonicalArchiveManifestSuite`, LNA prefix tests, Zarr publication suites |
| Immutable registry, duplicate rejection, and unknown representation | `ArchivedResponseRuntimeSuite`, `ArchiveResourceSuite` |
| Legacy migration and numerical compatibility | `LegacyLnaManifestTranslatorSuite`, `ResponseArchiveMigrationBaselineSuite` |
| Phase 8 dependency and source boundaries | `ResponseArchiveBoundaryGuardSuite` |

Malformed canonical manifests now have explicit truncated framing, invalid tag,
invalid digest, and bad-prefix fixtures. Unknown representations remain
structurally readable and are tested independently from corruption.
Canonical, LNA, and Zarr publication tests inject interruption before
publication and require the destination to remain absent or incomplete.

## 3. Provenance derivation

`Provenance.derive` copies the complete parent graph and creates one derived
root whose immediate parents are all previous roots. `OpenedDataset.execute`
uses it to add an `opened-dataset-attachment-v1` operation after every source
read. It rebuilds the checked `ReadResult`, preserving physical evidence while
making the dataset adapter visible in the causal graph. Invalid derivations
remain typed as `ReadError.InvalidProvenance`.

The dataset law runs both a single acquisition and a two-run acquisition. Each
bridged segment must agree with the corresponding direct response-source read,
retain exact order, report segmented assembly, and satisfy the provenance law.

## 4. Legacy-dispatch retirement

Historical archive reconstruction remains available through the explicitly
named `LegacyLatentArchiveCodec`. The former `LatentArchiveCodec` symbol is a
deprecated forwarding alias for one compatibility release; repository code
uses the named legacy entry point and therefore stays warning-clean.

The Phase 8 boundary guard freezes the compatibility dispatch callers in main
sources. A new representation must be installed through the immutable
`ArchivedResponseRegistry` as an `ArchivedResponseFamily`; adding another
central match fails the guard.

## 5. Release evidence

The exact final tree passed:

```text
response JVM / JS                  passed
response-laws JVM / JS             passed
latent JVM / JS                    passed
archive JVM / JS                   passed
archive-lna JVM / JS               passed
archive-zarr JVM / JS              passed
dataset JVM / JS                   passed
interop archived response JVM / JS passed
model, fit, MVPA dataset, group,
and fMRI workflow JVM / JS         passed
Phase 8 boundary guard             passed
Phase 0 numerical corpus           valid, 13 cases
compileAll                         passed warning-clean
testAll                            passed
git diff --check                   clean
```

Static production scans found no response-level tensor abstraction, unchecked
cast, warning suppression, scientific reconstruction in archive core,
principal-domain reverse dependency, or newly installed caller of the legacy
central dispatcher.
