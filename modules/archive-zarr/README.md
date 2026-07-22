# scalafim-archive-zarr

`archive-zarr` is the strict NeuroArchive Zarr 0.1 profile over the generic
`zarr` kernel. It refines an ordinary runtime-rank descriptor into canonical
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

`CanonicalChunkProfile.balancedV01` records the measured canonical layout:
inner chunks `[16,24,32,32]` inside start-indexed shards `[64,72,96,96]`.
This choice is profile policy, not a rank-four assumption in `zarr`.

NeuroArchive Zarr is a computational profile, not a claim that Zarr is an
accepted BIDS imaging representation. Compatibility means preserving parsed
BIDS source identity and producing an ordinary BIDS/NIfTI export that passes
the official validator. The executable distinction is recorded in
[`zarr-z8-profile-extraction.md`](../../docs/benchmarks/zarr-z8-profile-extraction.md).
