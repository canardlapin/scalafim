# Official HDF5 JNI bounded provider probe

This standalone Java/Python experiment qualifies a narrow generic IO capability
of HDF Group's public HDF5 2.2.0 archive. It adds no production dependency,
ScalaFIM module, estimate schema, scientific axes, codec or publication protocol.
Mote: `bd-01M3REBACHSKCYRBBPPJZ9HQWZ`; parent review is required before adoption.

Run from any directory with Python 3.12+ for the driver, an already installed
native ARM64 JDK 21+ and an existing h5py interpreter. The recorded lane uses
Homebrew JDK 25.0.1 and Python 3.12.10/h5py 3.14.0 (HDF5 1.14.6).
The Java harness compiles with `--release 21 -Xlint:all -Werror`; JDK 21 runtime
execution was unavailable and is not qualified. Linux and x86_64 are not run.

```sh
python3 tools/validation/hdf5-provider-probe/run_probe.py \
  --cache /private/tmp/scalafim-execution-20260929/hdf5-probe-runtime \
  --java-home /opt/homebrew/opt/openjdk@25/libexec/openjdk.jdk/Contents/Home \
  --python /opt/homebrew/bin/python3.12 \
  --logged-runner /Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py
```

`--loader-only` stops after compilation, runtime closure and filter checks.
Unsupported hosts, archive/closure changes and missing JNI fail before payload
jobs. No credentials, installers, global packages, FFM or alternative bindings
are used. The release metadata supplies the download URL, and both the retained
archive SHA-256 and publisher checksum are verified before extraction/loading.
Safe archive extraction stays inside the caller's task-owned cache. Use a fresh
cache after any interrupted extraction; an incomplete provider tree fails closed.
Python optimized mode is refused because the oracle relies on assertions.

The exclusive-create API uses `H5F_ACC_EXCL`, fixed chunked extents, exact
little-endian Float32/Float64/UInt8 disk types, matching primitive memory types,
none or standard DEFLATE, and explicit H5S hyperslabs/memory selections.
Rank/extents/chunks, checked Long arithmetic, buffer type/capacity and bounds are
validated before entering the native operation region. Zero slabs are no-ops.
Each synchronous slab is at most 65,536 cells. Cancellation runs between slabs;
there is no native interruption guarantee.

All native operations, inspection, initialization, error-stack walking and
closure run under one process-wide owner. Each lifetime scope closes resources
in reverse acquisition order; stores close datasets before files, without global
`H5close`. Predefined datatype constants are never closed. Try-with-resources
preserves the primary exception and suppressed cleanup exceptions. There are
at most two stores and two datasets per store, a 24-identifier policy ceiling,
1 MiB raw chunk caches per dataset and 4 MiB maximum metadata cache per file.
These are explicit cache/handle bounds, not a total native allocator bound.
The recorded workloads reached seven owned identifiers and zero final IDs.

The run checks eight asymmetric 7×11×5 datasets with 3×4×2 chunks, reverse
misaligned/edge writes and every independent h5py value, dtype, byte order,
extent, chunks and filter ID. h5py also authors a distinct 2×3 literal fixture;
Java verifies every value and nonzero edge reads for each primitive type.
For large payloads, shape is `(rows,131072)`, chunks `(1,32768)`, axes are literal
physical row/column, values are `row*1048576 + column*0.125 - 17.25`, and byte
validity is `(row+column)%4`. Those choices precede measurement. The writer
reads back every written block; a relocated fresh process reads every cell, and
h5py independently checks all values and validity. The 128 MiB value lane is
followed by a 256 MiB lane with the same buffers if its first total cost is at
most 60 seconds. Each Java process uses `-Xmx64m`.

Failure checks cover no-clobber bytes/hash, duplicate names, malformed input,
zero selections, capacity/type/overflow refusal, cancellation, six injected
acquisition failures, callback failure, coexistence of two stores, 100 warm
open/read/close cycles with FD/native ID receipts, and a two-process race whose
barrier is immediately before native exclusive creation. The loser must expose
native errno17/EEXIST; arbitrary IO failures are not accepted. An owned child
also aborts after one staging write. This proves process-abort staging behavior,
not power-loss durability, transactional unit publication or estimate completeness.

Every run creates an immutable timestamp/PID directory with raw command logs,
lean-logs exit/timing/argv sidecars and a manifest binding exact source/artifact
hashes and Java PIDs. Source remains frozen while jobs run. `dyld` records the
loader's actual JNI/core/JDK/OS images; `file`/`otool` retain both architectures
and LC dependencies/rpaths. Shared-cache OS images have no standalone hash.
Child `wait4` reports OS peak RSS and block counts; heap is sampled after slabs,
metadata size is sampled through HDF5, and JVM NMT summaries are retained.
Heap samples are lower bounds on instantaneous peak. NMT does not account for
all HDF5 mallocs. OS block counts do not establish physical disk bytes. These
runs occurred amid other builds and support no comparative timing claim.

See `docs/verification/hdf5-provider-probe-2026-09-30.md` for the final retained
run, archive/jar/native lock, measured memory and limitations. Estimate IDs,
validity semantics, df, logical axes/JSON authority, schema, consumers,
transactions, power-loss behavior and production packaging remain future work.
