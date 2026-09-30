# Estimate IO candidate evidence, 29–30 September 2026

## Scientific and physical boundary

The reviewed V0 scientific design now lives at
[`docs/estimate-set-v0-spec.md`](../../estimate-set-v0-spec.md). The current
ScalaFIM writer now emits the bounded `scalafim-estimates-core-nifti-1` candidate
for new local publications. It remains an implementation checkpoint, not a full
V0/HDF5 conformance claim. The three complete bundles below retain the earlier
`scalafim-estimates-development-1` format and exercise compatibility decoding.
JSON catalog/unit/collection documents are the sole scientific metadata
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

## Core-NIfTI repair and exact evidence

The Core envelope dispatches on `Schema`, `WireVersion`, `ProfileVersion` and
`DocumentKind`; it requires all unit fields, digest-pinned TSV tables, complete
representation coverage and explicit physical stored dtype. Development bare
statistic links decode as unknown correspondence; known links require named
hypothesis mappings, aligned observations/pooling and matching effect/SE target
IDs and units. Known T/F/Z references must match their statistic kind; T/F df
requires positive scalar or correctly typed, axis-aligned product values. The
reader rejects unexplained opposite-handed coded
qform/sform, admits a same-handed alternative only with its matching frame code,
and refuses 33 product/observation pairs before touching missing payloads. The
32-pair boundary opens, reads and closes all 64 handles.

`generate_physical_golden.py` now also writes independent singleton-3D,
big-endian Float32 bytes with nontrivial slope/intercept and a nonidentity affine,
plus deterministic gzip variants. The reader decodes those as logical Float64
values `4.0` and `-3.0`, stages both gzip files under exactly 714 bytes, and
refuses 713 bytes. The source is Python standard library only.

| Added fixture | Bytes | SHA256 |
|---|---:|---|
| `values-3d-be-scaled.nii` | 360 | `7ac6a88c778866ee9f326135a8a847e7185f0b43a7b5609d0f9ae6105bb5b4c4` |
| `validity-3d.nii` | 354 | `8e53240f0d0ca64bba16e9623a1c1e3d92faafb0187341a1d43facc95088c633` |
| `values-3d-be-scaled.nii.gz` | 95 | `a63261362b4f6ddf8db7f098e511ca17c61b0aa2420ffb8ebf1e2e0c7587cd2c` |
| `validity-3d.nii.gz` | 88 | `416d7c4efbafc3518988d10366dc89f7a8247fd07ae560e8c1c87c81bb78c76f` |

Final `estimatesJVM/test` and `estimatesJS/test` each passed 12 tests;
`estimatesIoJVM/test` passed 24 and `estimatesIoJS/test` passed 5.
`scalafimCompileAll` passed 89 additional alias steps. The single combined
invocation exited 0 with no `[warn]` or `[error]` lines: complete
`io-core-final-gates-r1.log` SHA256
`31dbdfe440c48e096282ce11e0196afaa0452d9661b6282753b9c6a42c46c7d9`.
A final test-only known-link roundtrip assertion passed 5/5 JVM and 5/5 JS,
exit 0, in `io-core-known-link-r1.log` SHA256
`4d99bf30b38ba328ff9fcd9989ad0eddbb3ab1384c0e52779f55f22288998307`.
Each log has a `.meta.json` command/exit receipt under the shared execution
root's `logs/` directory. The default image4s staging checkout was verified at
hosted commit `2695f891cbec31a7f565a9b39e2554fe3b6d4b40`.

`run_gzip_budget_probe.py` generated compressed NIfTI-1 content independently,
then started a direct `-Xmx64m` JVM. It staged exactly 75,498,176 decoded bytes
across two owned files, refused a one-byte-smaller cap, read both support states,
deleted staging on close and reported 38,172,664 bytes peak heap (67,108,864
bytes configured maximum). Receipt
`/private/tmp/scalafim-execution-20260929/io-gzip-budget-r1/receipt.json`
SHA256 `e997a4d5924be8293f88c81c1adb63bf4f4882d79ebaa9865a43d5d6d50aa43d`
pins generated gzip hashes, probe source/class hashes, raw direct-JVM log and
exit 0. This is one measured workload, not a general throughput bound.

## Retained gates

The Core-wire fresh-process crash matrix passed all 12 stages: for each of four kill
points (before seal, after seal, after collection publication, after pointer
CAS), a separate JVM seeded a base collection, a second JVM halted with exit
87, and a third reopened the expected base or new collection and verified
values and validity. The machine-readable receipt is
`/private/tmp/scalafim-execution-20260929/io-core-crash-r1/receipt.json`,
SHA256 `0b7344f3847da9f0d35c53ee71ee48d3401ecbbd6aa59a9f58097fa5820dfbfc`.
The receipt pins the direct probe class bytes and hosted provider HEAD; each
stage retains its own raw JVM log. The earlier development-wire matrix remains
historical.
This is an API-boundary process crash test, not a power-loss durability test.

Filesystem durability across power loss, compact shared covariance, broader
registered-frame support, producer/exporter replacement, workflow/consumer
adoption and representative access performance remain open. The retained HDF5
target requires a bounded generic
writer: jhdf 0.12.0 exposes `WritableGroup.putDataset(String,Object)` but no
slice/chunk method on `WritableDataset`; the existing LNA adapter materializes
whole payloads and replaces its output. This limitation was recorded on Mote
`bd-01KX6G9B8R86MRBZ9S8K8F5G7V`.

Local evidence does not authorize publication or ticket closure.
