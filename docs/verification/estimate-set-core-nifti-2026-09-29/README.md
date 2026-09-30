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

## Independent Core-1 review repairs

The Core reader now refuses a *valid* decoded cell declared logical Float32
unless its Double value is exactly representable as Float32, matching sink
admission. Invalid fill cells retain their validity code without imposing a
numeric precision claim. `generate_physical_golden.py` independently emits
physical Float64 fraction bytes (SHA256
`099eb7d8172d4b895ce405246ff090ebb413efd079a5bd6174934f45aebe8183`)
and scaled, big-endian physical Float32 fraction bytes (SHA256
`6edff266b3e8263c27dcb86d53b84bf83fbfe19a51264423f8c01999b4198099`).
The JVM checks logical Float32 refusal, logical Float64 acceptance, an exactly
representable Float32 control and non-estimable fill handling.

Gzip input ownership begins with the raw stream, before constructing the
decoder. Both raw and wrapped streams close on constructor/read/output failures;
owned staging files are removed. Repeated malformed and truncated gzip headers
are checked against process descriptor counts, including failure after an
earlier representation's data and validity channels have opened.

An Unknown statistic correspondence validates the referenced product's
existence, kind and nonempty reason only; it supplies no alignment or inference
capability. Known correspondence still checks observation and pooling alignment
and complete named hypothesis targets. The shared tests cover a legacy bare
link crossing pooling scopes as Unknown, plus Known refusal on the same scope.

`generate_core_bundle.py` is a separate literal Python standard-library source
for a **complete** Core-NIfTI-1 bundle, including Core catalog and unit JSON,
digest-pinned TSV projections, two NIfTI value/validity pairs, explicit stored
datatype, product units and axes, and a named Student-t hypothesis-to-effect
mapping with scalar reference df. It does not import a production encoder or
copy production output. Its complete unit manifest is SHA256
`e2d54370a163f98b44d670e9d87d46af1d1982b818a11190ad613bd53ad0706d`;
the test pins that digest and checks decoded scientific fields and physical
cells against literal expectations. Negative variants change the wire version,
remove required product units, or replace and re-pin the TSV with an axis that
disagrees with JSON. The generator and all eight fixture files are committed
with the source so later decoder drift is visible.

The literal Core generator is SHA256
`eb0269082a2b87096cbd5058644ab5187398d7fbe328884e57d553a3d9b8e154`.
The `core-literal/units/00000000-0000-4000-8000-000000000084/` fixture
files are pinned as follows:

| File | SHA256 |
|---|---|
| `estimands.json` | `4a79d5173e638732ed3d186db66e721082eadb7dac0dc188efa7496578a59445` |
| `estimands.tsv` | `3c7ca58dd9293091f37b93509fe4c692ae4cbe0779f3f6cdf46fa489fe83d28e` |
| `observations.tsv` | `37cfa6ae6a44cd41a40e845b188308ab1214624401059695df1b7504b961be97` |
| `effect-values.nii` | `e1f7c6e89f02b88ab407f155cf7eef962d0a2c764499cdac250aba28ed4fe09e` |
| `effect-validity.nii` | `fbb1f739ff7439f75a2a4138fe8452d2099318145754184c7f82aba4890ca704` |
| `t-values.nii` | `2eca81e10c1efb684448df43a758d2f1436670e4d174e3f688b158156f4f7aef` |
| `t-validity.nii` | `4b7f24a857b5f507b109290e99fee269413fa76a6935dd34ff34698fc6f6bf3b` |
| `estimates.json` | `e2d54370a163f98b44d670e9d87d46af1d1982b818a11190ad613bd53ad0706d` |

The final affected gates passed: `estimatesJVM/test` 12/12,
`estimatesJS/test` 12/12, `estimatesIoJVM/test` 30/30,
`estimatesIoJS/test` 5/5, and `scalafimCompileAll` 89 alias steps.
The complete raw log `io-core-final-repair-gates-r3.log` is SHA256
`54a39a9101184e6892672be7c28d84866592480e209b41026f3289bbddc1537a`;
its `.meta.json` records exit 0. There are no `[warn]` or `[error]` lines in
the complete log. Earlier r1 ended before project load at the sandboxed sbt
boot lock, and r2 found a test-only misspelled pooling enum; neither is test
evidence for this candidate.
