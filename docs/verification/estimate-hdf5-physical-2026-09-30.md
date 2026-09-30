# Opt-in physical HDF5 estimate backend, 2026-09-30

This candidate implements physical storage over the existing estimate contracts.
It depends downward only on `estimates`, `estimates-io`, and `archive-hdf5`.
Default stores, root aggregates and global aliases remain provider-free. The
explicit JVM lane uses the retained locked HDFGroup 2.2.0 provider on macOS
ARM64 and actual JDK 25; Python 3.12/h5py is a test oracle only.

## API and resource contract

`Hdf5EstimateBackend.open(rootString, archive, limits)` is the shared reader
capability facade. Scala.js returns `EstimateError.Unsupported` before accessing
its arguments. JVM callers use `Hdf5EstimateStore.open(Path, archive, limits)`
and `newSink(unit, compactProducts, cancelled)` for physical transactions. The
archive must be constructed with `JvmHdf5JniAdapter.open(limits.archive)`;
provider discovery or installation is outside this backend.

Per-sample values and U8 validity have axes `[observation,target,sample]`, where
target follows the scalar estimands or ordered upper triangle. Compact U has
`[observation,pair]`, only accepts normalized sample-invariant Float64
covariance, and uses `writeSharedCovariance`. No variance scale is applied by
this physical reader; consumers retain the existing reconstruction contract.
Unsupported inference evidence is refused before staging or native acquisition.

Every fixed physical cell must be delivered before seal, including explicitly
coded outside-support samples. The private bit ledger is admitted before
allocation under an aggregate `maximumCoverageBytes` cap, independently of the
native metadata cache. One product transaction is active at a time; complete
it before switching products. Both arrays remain caller-owned. Each slab is
bounded by at most 65,536 cells; conversion buffers contain only that slab.
Configured native caps are one file, two attached payload datasets, 1 MiB raw
cache per attached dataset, 4 MiB metadata cache per file, and 24 owned native
IDs. Two payload datasets do not mean two native identifiers. Read buffers
must also fit the caller's staging byte cap.

After completion, staged handles close. Read-only reopen uses
`Hdf5FlatInventory(Vector(values, validity))` and `verifyFlatInventory`, then
separate precise inspection of dtype, fixed extents, deterministic chunks and
filters. The candidate uses uncompressed LE F32/F64 values and U8 validity;
chunk axes are singleton leading axes and `min(lastExtent,256,blockCap)` for
the last axis. Seal rechecks all stages before immutable container publication
and the existing metadata hook. HDF-only collection inspection verifies selected
pinned units before collection publication and compare-and-swap discovery.

Seal on the same successful sink is idempotent. A fresh semantic retry may
have different HDF5 bytes (including provider object metadata); only exact
byte-equivalent containers can reuse an immutable path. Byte mismatch remains
`Conflict`, preserving existing files and pointers. Failed attempts can leave
unreferenced immutable payloads if publication of a later leaf fails, as in the
existing local lifecycle; they do not receive a successful unit receipt.

## Verification specimens

The owned literal JSON contains every value and validity byte for four products
(effect, residual scale, absolute covariance, compact U): 76 stored cells, with
nonlexical axes, distinct observations, outside support and missing/failed
validity. The Python checker independently reads every Scala-written cell,
dtype, rank, extent, chunk and filter. A separate Python writer authors literal
files which Scala reads in reversed named-axis/sample order, including compact
broadcast into support. No Scala-generated expected array is the Python oracle.

Controls cover early Long shape/byte/slab overflow, coverage and buffer refusal,
invalid numeric/validity/support/pair delivery, duplicate cells, one active
product, partial validity failure after a real values write, second-dataset
acquisition failure, close/reinspection failure, cancellation, throwing and
reentrant callbacks, late writes, immutable no-clobber and stale CAS. Independent
external malformed files cover extra datasets/groups, soft links, wrong dtype,
shape, chunks/filter and invalid numeric/validity payload. A 65,539-cell product
uses a 65,536-cell slab and a partial edge under `-Xmx64m`; repeated lifetimes
verify zero owned/provider IDs and descriptor stability. These are bounded
correctness/resource specimens, not sustained throughput measurements.

## Local receipts

Full raw logs, metadata, all fixture files and source/provider/compiled closures
are retained under `/private/tmp/scalafim-execution-20260929/`:
`logs/physical-*.log` and `estimate-hdf5-physical-runtime/resume/`.
The first sandboxed compile failed before project loading on sbt boot-lock
permissions. The authorized JVM/JS compile passed. The first native test run
passed 20/21; its failed test incorrectly required a fresh semantic retry to
have identical container bytes. The final retry control preserves byte-level
refusal. Every attempt is retained, including failed output.

The next physical suite passed 21/21. That batch's subsequent archive suite
failed 5/25 because its external fixtures had not been prepared before it ran;
remaining estimate targets in the batch did not run. The failure is retained.
Archive fixture inputs were then copied with recorded source hashes before the
fresh affected-suite invocation. No archive source was changed.

The complete affected JVM batch passed: physical 22, archive HDF5 25,
estimates 17, estimate IO 68. Provider-free JS passed: physical/shared 6,
archive HDF5 12, estimates 17, estimate IO 14. All compiler output was warning
clean. The JVM test runtime emits the pre-existing Scala LazyVals/JDK25
`sun.misc.Unsafe::objectFieldOffset` warning; it is retained, not suppressed.

The physical resource specimen observed peak 7 owned IDs and zero final
owned/provider IDs; observed metadata cache peak was 38,395 bytes. The literal
suite's sampled heap peak was 55,786,640 bytes under the 67,108,864-byte heap
limit. Twenty alternating-store reader lifetimes retained FD 37 to 37 and zero
final native IDs. These observations describe this bounded specimen only.

A final local cleanup uses explicit zero-tolerance floating-point assertions
and tests typed invalid-path refusal in the JVM facade. Its physical JVM rerun
and the ordinary provider-free `scalafimCompileAll` gate are retained separately
in the final candidate manifest. Independent review remains pending.

## Limits

Inventory checks reachable root links only. They do not examine attributes,
deleted/unreachable bytes, external concurrent writers or every container byte.
Native allocator peak, physical IO bytes and throughput are unmeasured; receipt
selection counts and cache configuration are not those measurements. No
scientific inference, full campaign, native race, power-loss, production
packaging, JDK 21, Linux, release, merge or remote publication claim is made.
Independent SHA-bound review remains required before adoption.
