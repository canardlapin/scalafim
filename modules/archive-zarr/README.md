# scalafim-archive-zarr

`archive-zarr` is the strict NeuroArchive Zarr 0.1 profile over the standalone
`zarr4s` kernel. It refines an ordinary runtime-rank descriptor into canonical
BOLD only when the array is exactly `[t,z,y,x]` and agrees with typed signal
calibration, voxel geometry, acquisition timing, BIDS identity, source hashes,
and an immutable publication receipt.

The shared profile, asynchronous profile opener, and SHA-256 implementation
cross-compile to the JVM and Scala.js. `AsyncNeuroArchiveZarr` consumes the
generic `AsyncZarr` capability; `BrowserNeuroArchiveZarr` is only a
source-compatible facade selecting browser gzip. JVM publication is
create-only and stages a complete revision before atomic publication; full
object auditing is explicit. Remote opening requires no object listing, and a
revision-scoped generic cache can reuse metadata, lengths, shard indexes, and
demanded data ranges without profile-specific cache machinery.

`ZarrArchiveDriver[F]` adapts that portable asynchronous path to
`ArchiveDriver[F]`. The caller supplies a resource for `AsyncObjectReader` and
an explicit platform codec runtime. The opened archive exposes a typed
`ZarrPayloadPlan` with either chunk-bounded whole-object or sharded
byte-range-bounded evidence, and records each successful whole-object,
byte-range, and length access in physical-attempt order. Receipt bytes must
equal the generic Zarr execution receipt, and full content validation rechecks
every publication-listed object digest.

The driver projects a checked canonical-BOLD revision into the narrow archive
representation envelope `org.scalafim/dense-bold@1`. That metadata contains
canonical array and calibration facts, response-domain identity and ordering,
payload identity, and the immutable object inventory. It deliberately contains
no response-kernel type; interpretation belongs to
`interop-archived-response`.

The normalized archive revision identifies the physical binding as
`neuroarchive-zarr@1`, the logical object as
`org.scalafim/fmri-response`, and its payload role as the shared typed identity
`org.scalafim.zarr/canonical-response`. Container, object-schema,
representation, and payload-role versions are therefore not inferred from one
another.

Receipt-bearing plans currently require `maxConcurrentRequests = 1`. This is
an explicit Phase 2 correctness boundary: it makes detailed attempt order
deterministic while retaining exact request evidence. A later parallel
implementation must add plan-indexed observation ordering before relaxing
that constraint; it may not sort away retries or reconstruct evidence from a
plan after execution.

`CanonicalChunkProfile.balancedV01` records the measured canonical layout:
inner chunks `[16,24,32,32]` inside start-indexed shards `[64,72,96,96]`.
This choice is profile policy, not a rank-four assumption in `zarr4s`.

NeuroArchive Zarr is a computational profile, not a claim that Zarr is an
accepted BIDS imaging representation. Compatibility means preserving parsed
BIDS source identity and producing an ordinary BIDS/NIfTI export that passes
the official validator. The executable distinction is recorded in
[`zarr-z8-profile-extraction.md`](../../docs/benchmarks/zarr-z8-profile-extraction.md).
