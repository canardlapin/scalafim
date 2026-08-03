# Z8 NeuroArchive profile and extraction receipt

Date: 2026-07-22

Status: final JVM/Scala.js, external-oracle, standalone-consumer, and
repository-wide gates passed against the combined worktree.

## 2026-07-30 ownership update

The generic kernel and optional Blosc/Zstandard provider described in this
historical receipt now live in the sibling `zarr4s` repository. Their public
packages are `zarr4s` and `zarr4s.codec.blosc`; their artifacts are
`zarr4s-core` and `zarr4s-codec-blosc-zstd`. ScalaFIM retains `archive-zarr`,
`dataset-zarr`, and archived-response interop as neuroimaging-specific
consumers, pinned to zarr4s commit
`2a5ba963b151b62c739d1bf5a19d49202bb6ff29`.

The commands and `scalafim.zarr` names below record the 2026-07-22 pre-move
gate. Current generic-kernel verification runs `sbt checkAll` in the zarr4s
checkout; current ScalaFIM verification starts at `archiveZarrJVM/test` and
`archiveZarrJS/test`.

## Boundary under test

The generic `scalafim-zarr` kernel remains runtime-rank and contains no BIDS,
NIfTI, or canonical-BOLD concepts. `archive-zarr` refines an opened generic
array into the strict `neuroarchive-zarr-0.1` profile. `dataset-zarr` supplies
the JVM NIfTI/BIDS adapters and the storage-neutral response-block bridge.

This is the extraction seam intended for a future standalone `scala-zarr`:
the standalone consumer depends only on `scalafim-zarr`, while the two domain
modules depend on the generic artifact in the ordinary direction.

## Portable profile HTTP trace

`AsyncNeuroArchiveZarr` is shared by the JVM and Scala.js.
`BrowserNeuroArchiveZarr` is a compatibility facade that selects browser gzip;
the JVM test supplies the explicitly blocking `JvmGzip` bridge on a dedicated
execution context.

The byte-identical fixture is one complete start-indexed shard:

| Property | Value |
| --- | ---: |
| Canonical shape | `[2,2,2,3]` (`t,z,y,x`) |
| Scalar | `int16`, values 0 through 23 |
| Inner chunk shape | `[1,1,2,2]` |
| Outer shard shape | `[2,2,2,4]` |
| Shard index | 132 bytes at the start |
| Encoded shard | 380 bytes |

Both platform suites require this exact profile-open trace:

```text
GET  publication.json             whole object
GET  zarr.json                    whole object
GET  neuroarchive.json            whole object
GET  canonical/zarr.json          whole object
HEAD canonical/c/0/0/0/0          length
```

The first complete canonical read must then issue exactly:

```text
GET canonical/c/0/0/0/0 bytes=0-131
GET canonical/c/0/0/0/0 bytes=132-379
```

An identical second read must return the same 24 values without another HTTP
request. The shared expected traces prevent the JVM and Scala.js suites from
quietly asserting different behavior.

## Independent Zarr and NIfTI oracle

`tools/verify_zarr_nifti_oracle.py` is pinned to Zarr-Python 3.2.1, nibabel
5.2.1, and NumPy 2.2.6. For each of `uint8`, `int16`, `int32`, `float32`, and
`float64`, it must:

1. open the Scala-published `canonical` array with Zarr-Python;
2. verify the stored dtype, raw values, shape, and explicit `t,z,y,x` order;
3. verify the NeuroArchive manifest affine, timing, shape, and axes;
4. open the exported NIfTI with nibabel and compare raw and calibrated values;
5. verify exported dtype, shape, affine, repetition time, and BIDS joins.

A separate JVM regression requires NIfTI second, millisecond, and microsecond
TR units to lower to canonical seconds and refuses frequency-domain unit codes
for BOLD timing.

This supplements the broader bidirectional Zarr-Python corpus, the independent
`zarrs` Rust fixture, and the pinned official `zarr_implementations` v2 corpus
chunk used by the generic kernel.

`tools/verify_zarr_java_oracle.py` adds an independent JVM implementation
without adding zarr-java to any ScalaFIM module. The tool pins zarr-java 0.1.3
and replaces its non-Central `cdm-core` edge with Unidata's official
`netcdfAll` 5.9.1 GitHub asset, whose required SHA-256 is
`3ba80b2b2125028ebcb4d98034b165dfca5a5dddaeaad5fa41ab211aa378fc72`.
The executable probe opened the pinned Zarr-Python start-indexed shard and
verified all 16 values. Its reciprocal writer produced a direct v3 `int16`
fixture now read by the shared Scala suite on JVM and Scala.js; the metadata
and chunk are pinned separately by SHA-256. A structurally equivalent
Zarr-Python profile matrix also passed zarr-java for all five scalar types,
including start-indexed sharding, inner gzip/CRC32C, axes, shape, and values.
The final extraction run made zarr-java open the actual Scala-published
canonical arrays for all five scalar types and verify their dtype, axes, shape,
and values. The oracle reported `zarr-java 0.1.3 NeuroArchive scalar matrix
oracle: ok`.

## BIDS compatibility gate

NeuroArchive Zarr is **not itself a BIDS-compliant imaging dataset**. Current
BIDS 1.11.1 names NIfTI as the MRI imaging representation; the Zarr revision is
the computational representation tied to its source identity. BIDS compliance
is claimed only for an exported BIDS/NIfTI tree after validation.

`tools/verify_zarr_bids_validator.py` runs the official BIDS validator 3.0.1
over all five scalar exports. The script fails if the official executable is
missing, the version differs, output is not JSON, or any dataset fails. The
internal `BidsProjectLoader` remains a useful integration test but is not a
substitute for this gate.

The final gate validated the actual Scala exports. Every one of the `uint8`,
`int16`, `int32`, `float32`, and `float64` BIDS/NIfTI trees produced zero errors
under validator 3.0.1 (bundled schema 1.2.7). Each minimal tree also produced 36
recommendation warnings for absent source-specific fields such as scanner
metadata, authors, license, and README. The exporter does not fabricate those
values; warnings remain visible while zero validation errors is the gate.

## Epic completion ledger

This ledger separates recorded slice evidence from the final whole-epic audit.
A closed child is not, by itself, proof that the current combined worktree still
passes every gate.

| Slice | Contract | Current evidence | Final-audit state |
| --- | --- | --- | --- |
| Z7 | Optional Blosc/Zstandard provider without a core dependency | JVM and Scala.js provider suites passed in the final whole-tree run; prior reciprocal Zarr-Python reconstruction remains recorded | Passed within its documented non-universal writer boundary |
| Z8.1 | Scalar algebra, fixed-width dtypes, transpose, and v2 chunk keys | Final JVM/Scala.js suites plus fresh bidirectional Zarr-Python 3.2.1 fixtures | Passed |
| Z8.2 | Runtime-rank hierarchy plus bounded v2/consolidated lowering | Final JVM/Scala.js suites plus fresh v2/v3 Zarr-Python differentials | Passed |
| Z8.3 | Factored selections and bounded streaming fragments | Final JVM/Scala.js suites plus fresh NumPy/Zarr-Python factored-selection differential and fMRI receipt | Passed |
| Z8.4 | Store-independent create-only writers | Final JVM/Scala.js suites, reciprocal Python fixtures, and freshly republished standalone consumer | Passed |
| Z8.5 | Revision-scoped cache and portable async reads | Final cache, concurrency, JVM HTTP, Fetch, `zarrs`, `zarr_implementations`, and zarr-java suites; standalone consumer compiled on both platforms | Passed |
| Z8.6 | NeuroArchive profile plus BIDS/NIfTI extraction | Final archive/dataset JVM/Scala.js suites; actual Scala outputs passed pinned Python/nibabel, zarr-java, and official BIDS validation | Passed |

## Final audit receipt

The following all passed on 2026-07-22 against the same combined worktree:

- focused suites: `zarrJVM` 139, `zarrJS` 147, `archiveZarrJVM` 13,
  `archiveZarrJS` 11, `datasetZarrJVM` 9, and `datasetZarrJS` 3 tests, for 322
  passing tests and no failures;
- fresh bidirectional Zarr-Python 3.2.1 interoperability: Python-created data
  read by Scala and Scala-created data read by Python;
- actual five-dtype Scala NeuroArchive generation followed by the pinned
  Zarr-Python 3.2.1, nibabel 5.2.1, and NumPy 2.2.6 oracle;
- zarr-java 0.1.3 opening those same five Scala-produced canonical arrays;
- official BIDS validator 3.0.1: zero errors for all five exports;
- fresh `publishLocal` for JVM and Scala.js followed by isolated
  `consumerJVM/compile` and `consumerJS/compile` using only the published
  `scalafim-zarr` coordinates;
- repository-wide `compileAll testAll`, exit 0.

Any future failure of one of these commands reopens the owning contract rather
than being waived at the epic level.

## Final commands

The final receipt was produced with the following reproducible commands:

```text
sbt zarrJVM/test zarrJS/test
sbt archiveZarrJVM/test archiveZarrJS/test
sbt datasetZarrJVM/test datasetZarrJS/test
npm ci --prefix modules/zarr-codec-blosc-zstd/js
sbt compileAll testAll

uv run --python 3.12 --with 'zarr==3.2.1' --with 'nibabel==5.2.1' --with 'numpy==2.2.6' \
  python tools/verify_zarr_nifti_oracle.py write-inputs <inputs>
sbt "datasetZarrJVM/Test/runMain scalafim.dataset.zarr.NiftiOracleBatchMain <inputs> <outputs>"
uv run --python 3.12 --with 'zarr==3.2.1' --with 'nibabel==5.2.1' --with 'numpy==2.2.6' \
  python tools/verify_zarr_nifti_oracle.py verify-outputs <inputs> <outputs>
python tools/verify_zarr_java_oracle.py verify-neuroarchive <outputs> \
  --java-home <compatible-jdk>
uv run --python 3.12 --with 'bids-validator-deno==3.0.1' \
  python tools/verify_zarr_bids_validator.py <outputs>

sbt zarrJVM/publishLocal zarrJS/publishLocal
cd tools/zarr-standalone-consumer
sbt consumerJVM/compile consumerJS/compile
```

## Explicit non-claims

Z8.6 does not claim full preservation of every NIfTI header field. Complete
qform/qfac/form-code, intent, slice timing, and arbitrary header provenance
remain follow-up profile work. It also does not add S3 credentials, catalogs,
Parquet tables, mutable Zarr, a persistent cache, or NeuroArchive retention
policy. Those omissions do not weaken the generic Zarr mechanics proved here,
but they must not be described as completed NeuroArchive 0.1 behavior.

Primary references: [Zarr v3 specification](https://zarr-specs.readthedocs.io/en/latest/v3/core/v3.0.html),
[BIDS 1.11.1](https://bids-specification.readthedocs.io/en/stable/), and
[official BIDS validator](https://github.com/bids-standard/bids-validator).
