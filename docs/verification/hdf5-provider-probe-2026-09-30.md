# Official HDF5 JNI generic provider probe — 2026-09-30

**Pass for the bounded local macOS ARM64/JDK25 generic provider experiment.**
Parent review remains required. This is not an estimate backend, production
packaging decision, Core milestone closure, Linux/JDK21 admission or publication.
Mote issue `bd-01M3REBACHSKCYRBBPPJZ9HQWZ`; Fray thread 55.
Base commit: `caf84a216043a2751acaf8791b78cdbafea833d7`.
Exactly five standalone harness files and this report are the owned change.
No Scala source/build/pin/module edits and no sbt invocation occurred.

## Reproduction and immutable evidence

```sh
python3 tools/validation/hdf5-provider-probe/run_probe.py --cache /private/tmp/scalafim-execution-20260929/hdf5-probe-runtime --java-home /opt/homebrew/opt/openjdk@25/libexec/openjdk.jdk/Contents/Home --python /opt/homebrew/bin/python3.12 --logged-runner /Users/bbuchsbaum/.agents/skills/lean-logs/scripts/run_logged.py
```

Final run: `/private/tmp/scalafim-execution-20260929/hdf5-probe-runtime/run-20260930T063253Z-68404`.
Manifest SHA-256: `5af1d1b94cdfca3dd784655222ce2d5c51fd4b36e798b2d77d6b4837d4f664f9`.
Full wrapper log: `/private/tmp/scalafim-execution-20260929/hdf5-probe-runtime/probe-final-02.log`, with actual exit 0 and
`probe-final-02.log.meta.json` (6.643920 seconds wall time for the driver).
`/private/tmp/scalafim-execution-20260929/hdf5-probe-runtime/final-receipt-index.json` records raw/meta hashes, actual Java PIDs,
exits, selected-work counters and memory summaries for every final command.
Each raw log has its own complete argv/cwd/timing/exit sidecar. Sources stayed
frozen from manifest construction through all jobs; the four executable/lock
source SHA-256 values are embedded in the manifest. A separate local commit
binding in the cache identifies the exact six-path commit after it is created.

All expected exits matched: compilation/loader/fixtures/failure checks/payload
writers/readers/oracles and race winner 0; exclusive-create race loser 4;
owned abort 17. Failed attempts are retained, including the original compile
warning, sandbox-denied macOS `time -l` syscall, and an over-specific race
minor-error assumption. The final race captures errno17/EEXIST from the native
error stack before cleanup; it does not accept a generic open-file failure.
The earlier race permitted a filesystem precheck to reject the loser; final
barriers require both processes to reach the point immediately before native
`H5Fcreate`. `probe-attempt-03.log` is historical, not the final source evidence.

## Exact provider closure and host

Official public metadata:
[HDFGroup release](https://github.com/HDFGroup/hdf5/releases/tag/2.2.0).
Downloaded archive: [macOS 2.2.0 archive](https://github.com/HDFGroup/hdf5/releases/download/2.2.0/hdf5-2.2.0-macos15_clang.tar.gz);
34,803,158 bytes, SHA-256
`7402939a854b643022e239dfa544a200c048b798bb0861890010db48a72efc73`.
Both recomputation over retained bytes and the retained
[publisher checksum list](https://github.com/HDFGroup/hdf5/releases/download/2.2.0/hdf5-2.2.0.sha256sums.txt)
matched before extraction/loading. Extraction used safe tar filtering in this
owned cache. No credentials, installer, global install, alternate binding or
FFM was used. Complete `provider-lock.json` is committed alongside the harness.

| Artifact | Bytes | SHA-256 |
| --- | ---: | --- |
| jarhdf5-2.2.0.jar | 86209 | `8a561afd611d95ef2167195eaf89b43f2f96f517690280094d7ffcde306a3895` |
| libhdf5.320.2.0.dylib | 10592048 | `9c681586a5fee10ad7efaf49bab058c38dfc9119aa5d55a78c04e476962ebf8e` |
| libhdf5_java.dylib | 1236544 | `0a67221b6e1617aef522dcf9fc3aad1c27f424fbc4844087efcab560adb3a93c` |
| slf4j-api-2.0.16.jar | 69435 | `a12578dde1ba00bd9b816d388a0b879928d00bab3c83c240f7013bf4196c579a` |
| slf4j-nop-2.0.16.jar | 4982 | `deca6c04ed35515a0a911fa44c0e836bee92c0c59d2e8fa9bab8ffbc464a9ba7` |

Classpath is exactly the HDF JNI jar, SLF4J API and one compatible SLF4J NOP
binding. H5.class major 65 was inspected directly. The retained build settings
say Java compiler 21.0.11 ARM64, Threadsafety OFF, DEFLATE(ZLIB) and LIBAEC.
The actual Java executable is
`/opt/homebrew/Cellar/openjdk/25.0.1/libexec/openjdk.jdk/Contents/Home/bin/java`,
Homebrew OpenJDK 25.0.1, runtime architecture aarch64, macOS 14.3 ARM64.
`/usr/libexec/java_home -V` registered 22, 20, 8 and 7, with no JDK21.
The installed Homebrew JDK25 was explicitly selected, without installation.
Java compilation was warning-clean with `--release 21 -Xlint:all -Werror`.
Runtime flags include `-Xmx64m --enable-native-access=ALL-UNNAMED`, explicit
`-Djava.library.path`, and JVM NMT summary/exit statistics. External HDF5 plugin
preload is disabled (`HDF5_PLUGIN_PRELOAD=::`). No global loader settings changed.

Final `file-*.log` and `loadcommands-*.log` retain universal ARM64/x86_64 Mach-O
and complete LC_LOAD_DYLIB/LC_RPATH records. JNI loads `@rpath/libhdf5.320.dylib`
and `/usr/lib/libSystem.B.dylib`; core lists libSystem only (filters statically
included in core). JNI rpaths include `@loader_path/../lib` and `@loader_path/`.
The final loader's actual dyld images resolve JNI/core to the locked cache
artifacts above. Every actual loader image with a standalone file has a
SHA-256 in the manifest; OS shared-cache images are explicitly unhashable as
standalone files. The actual loaded JDK libraries are also recorded there.
Loader returned HDF5 `[2,2,0]`, DEFLATE encode/decode flags 3 and zero open IDs.

Independent oracle: `/opt/homebrew/opt/python@3.12/bin/python3.12`, Python
3.12.10, h5py 3.14.0, NumPy 2.2.6, independently linked HDF5 1.14.6.
This existing interpreter was inspected and used without package installation.
The Python process and JVM do not load both HDF5 bindings in one process.

## IO and independent checks

Small fixture: physical shape `(7,11,5)`, chunks `(3,4,2)`, Float32/Float64/
UInt8 plus validity, each with none and DEFLATE level 4. Reverse rectangular
writes cover nonzero offsets, misalignment and last-partial edges. h5py checks
all 3,080 cells, dtype, HDF byte order, fixed shape/maxshape, chunks and exact
filter IDs/options. UInt8 crosses signed-byte boundaries and validity includes
all codes 0–3. h5py authors a separate literal `(2,3)` fixture with values
`[-7.5,0,1.25,99.5,-0.125,8192]` and bytes `[0,255,128,1,2,3]`.
Java reads all 18 literal cells plus six nonzero edge cells across the types.

Large fixtures are fixed before measurement: shape `(rows,131072)`, chunks
`(1,32768)`, none filter, literal physical axes row/column, value
`row*1048576 + column*0.125 - 17.25`, validity `(row+column)%4`, and at most
65,536 elements per buffer. Writer allocates one 524,288-byte Double block and
one 65,536-byte validity block and reads back every written block. Fresh
readers reopen moved files after old paths disappear. h5py independently
checks every value and validity cell: 16,777,216 each at rows=128 and
33,554,432 each at rows=256. No whole dataset is materialized by Java.

128-row staging file: 151048027 bytes; SHA-256
`be25d53af847489fc6b7c22f981325c62e7deb1831c7fda2e86058e595cb800c`. Writer/read/oracle total cost before the larger
lane: 1.208459 seconds, below the declared 60-second budget.
256-row staging file: 302090059 bytes; SHA-256
`974fc0af379650990cb65bd7c88b89767c03c0dcf302496460ec5412b7ad998c`. Both old paths are absent; both larger sample
count and same-block-size criteria were actually exercised.

## Memory and selected versus physical work

All values below are bytes. Heap is a sample after synchronous slabs, not an
instantaneous JVM peak. RSS is the OS peak for the exact child from `wait4`;
macOS `time -l` could not access a sandbox-restricted sysctl and its failed
receipt is preserved. Heap maximum reports exactly 67,108,864 in every Java
lane. This is a 64 MiB heap qualification, not a 64 MiB process-memory claim.

| Lane | Java PID | Sampled heap peak | OS peak RSS | Metadata observed peak | Peak owned IDs | JVM NMT committed at exit |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| write-128 | 68470 | 6420272 | 69648384 | 53083 | 6 | 159969096 |
| read-128 | 68478 | 6420272 | 67502080 | 53083 | 7 | 159806196 |
| write-256 | 68484 | 6944560 | 70205440 | 100171 | 6 | 159972224 |
| read-256 | 68487 | 6420272 | 68370432 | 100171 | 7 | 159812636 |

Configured budgets: 1,048,576-byte raw cache and 521 slots per dataset, at most
two datasets/file, maximum metadata cache 4,194,304 bytes per file, at most
two files and a 24-ID policy ceiling. Configured values are read back through
provider properties. Final workloads reach at most seven owned IDs and end
with zero owned/provider IDs. Metadata grows with rows but stays below its
explicit cap. JVM NMT counts JVM committed/reserved memory (including heap
and GC structures), not OS residency or all HDF5 malloc allocations. Exact
HDF5 allocator totals are unavailable. Native RSS is measured only as part
of total child RSS, not separately attributed.

At rows=128, selected value+validity bytes are 150,994,944 per fresh reader,
301,989,888 for writer plus its readback. At rows=256 those double. Aligned
large slabs' theoretical full-chunk upper bytes equal selected bytes. Small
misaligned writes select 10,780 bytes and touch a theoretical upper 59,184
full-chunk bytes. These counters describe selections/chunks, not actual disk
traffic; cache hits, compressed physical work and physical read bytes are
unavailable. OS input/output block counters are retained and do not establish
uncached physical IO. Concurrent builds preclude comparative speed claims.

## Failure/lifetime laws and remaining boundary

Original sentinel bytes and existing small HDF file SHA-256 survive refused
exclusive creation. Duplicate dataset names fail without rewriting. Slab
rank, negative/out-of-range bounds, Long sum/product/byte overflow, null or
wrong primitive type, invalid fixed extent/chunks, insufficient buffer and
>65,536-cell capacity are refused before the native operation gate. Zero
slabs make no JNI operation entry. Dataset/file caps and cancellation are
checked; cancellation is between slabs with no native-interruption claim.

Six injected failures cover both file acquisitions and each dataset acquisition.
A managed callback throws after an actual write; owned resources close and
only the owned staging file remains. Two stores coexist and closing one leaves
the other's edge read operational, with no global `H5close`. Reverse scopes
and Java suppressed-exception semantics preserve primary failure reporting.
One hundred warm open/read/close cycles plus refused creates end with FD
counts 9→9 and zero native/provider IDs. These do not promise recovery from
an arbitrary native close failure or OS crash.

Final race: two owned fresh Java processes pass preflight and acquire their
access properties before the barrier; actual H5F_ACC_EXCL yields one creator
and one captured native errno17/EEXIST refusal (exits 0 and 4), both with zero
final IDs. The owned abort process writes one bounded staging block and halts
with exit 17. Staging remains; the probe has no complete-unit publication API.
No power-loss durability, fsync/CAS/transaction or estimate-unit claim follows.

JDK21, Linux and x86_64 execution are unrun; archives' architecture/header
inspection does not qualify those lanes. Scala.js physical JNI is inapplicable.
Optional module/build admission, reproducible production dependency packaging,
estimate IDs/df/axes/validity/schema/JSON authority, consumer interoperability,
transactional completeness and power-loss semantics remain open for separately
owned future work. No production dependency, estimate adapter, publication,
parent ticket closure or global installation occurred. Parent review must
assess this exact source/runtime evidence before any adoption decision.
