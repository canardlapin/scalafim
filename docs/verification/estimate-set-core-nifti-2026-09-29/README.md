# Estimate IO candidate evidence, 29 September 2026

## Scientific and physical boundary

The reviewed V0 scientific design now lives at
[`docs/estimate-set-v0-spec.md`](../../estimate-set-v0-spec.md). The current
ScalaFIM writer still emits `scalafim-estimates-development-1`: this is an
implementation checkpoint, not a stable interchange or full V0 conformance
claim. JSON catalog/unit/collection documents are the sole scientific metadata
authority. New unit manifests pin UTF-8 `estimands.tsv` and `observations.tsv`
projections, whose zero-based ordered IDs must agree exactly with the JSON.
Older development documents without tables remain readable.

`generate_physical_golden.py` writes two tiny NIfTI-1 files using Python
`struct`, without calling image4s or ScalaFIM. Their x-fastest physical order
contains two voxels for each of two ordered estimands. One supported cell has
validity `3` (non-estimable), demonstrating that a numerical fill value is not
evidence of estimability. The JVM golden test publishes those exact bytes under
digest-pinned references and opens them in a new `LocalEstimateStore` instance.
`generate_bundles.py` independently encodes complete development-profile
JSON/TSV/NIfTI bundles for effects-only, statistic-only and deficient-rank
cases. Each bundle includes digest-pinned axis projections; the deficient case
also pins its subspace evidence. The JVM reader tests reopen these files from
a new store and check ordered values, validity and scientific declarations.

| Fixture | Bytes | SHA256 |
|---|---:|---|
| `values.nii` | 384 | `6e52bf5447b7a75c643da98d56a3d4cca3ee97dff1add29d029e0bfd6aa6a594` |
| `validity.nii` | 356 | `846fcfeb5841c41c237456b7b2c7c52af6930ccc2806a1a4fef1784e1c2a95ec` |

The complete-bundle test passed all three cases on the JVM. Its raw log and
actual exit receipt are
`/private/tmp/scalafim-execution-20260929/logs/io-independent-bundles-r1.log`
and `.meta.json`. The other focused estimate suites passed 17 JVM and 4 JS
tests in `io-estimates-r4.log` against local image4s provider candidate
`e0d720f4b968f7d6deeac6978aa986e75dc2f721`.

Additional regression cases cover a validly pinned TSV with reversed IDs,
changed fourth-axis identity, validly pinned surplus payload byte, missing or
altered deficient-rank subspace evidence, exact-byte same-revision retry,
changed-byte no-clobber and two concurrent CAS contenders. The reader requires
an unknown temporal unit, zero `toffset` and unit fourth-axis pixel dimension
for estimate products; it verifies exact file length and the selected scanner
sform against the authoritative manifest geometry.

## Retained gates

The fresh-process crash matrix passed all 12 stages: for each of four kill
points (before seal, after seal, after collection publication, after pointer
CAS), a separate JVM seeded a base collection, a second JVM halted with exit
87, and a third reopened the expected base or new collection and verified
values and validity. The machine-readable receipt is
`/private/tmp/scalafim-execution-20260929/io-crash-probe-20260929-direct/receipt.json`.
This is an API-boundary process crash test, not a power-loss durability test.

Filesystem durability across power loss, compact shared covariance, broader registered-frame/gzip
policy, producer/exporter replacement, workflow/consumer adoption and access
measurements remain open. The retained HDF5 target requires a bounded generic
writer: jhdf 0.12.0 exposes `WritableGroup.putDataset(String,Object)` but no
slice/chunk method on `WritableDataset`; the existing LNA adapter materializes
whole payloads and replaces its output. This limitation was recorded on Mote
`bd-01KX6G9B8R86MRBZ9S8K8F5G7V`.

Warning-clean cross-platform compilation is in progress. Local evidence does not authorize provider publication or
ticket closure.
