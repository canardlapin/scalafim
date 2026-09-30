# Opt-in generic HDF5 archive adapter — 2026-09-30

Mote `bd-01M3RHJMNCP2SETRFZADRRS8H4`; Fray thread 58.
Base: `1a303565f169f5c4641ebf4a3cecab0378395d53`.
The candidate adds a physical archive-only adapter, with no scientific schema.
The exact ten owned paths include the six module source/test files, build.sbt,
README.md, module map and this document. Parent SHA-bound review is required.

## Explicit capability

The module is absent from the root aggregate and both global compile/test
aliases, and is not published. Only `archiveHdf5JVM` uses the locked JNI jar,
SLF4J API and NOP binding. Its explicit provider preparation task verifies
existing bytes without network access, extraction or installation. An absent
property on this explicit JVM target reports a capability preflight failure;
normal builds and the entire JS target require no provider property.

Retained provider root:
`/private/tmp/scalafim-execution-20260929/hdf5-probe-runtime`.
Lock: `tools/validation/hdf5-provider-probe/provider-lock.json`, SHA-256
`a0f1eda7c29dee1dead84cd7c4d55ca031624721f56953a886fa8e37ee94ad29`.
Official URL: https://github.com/HDFGroup/hdf5/releases/download/2.2.0/hdf5-2.2.0-macos15_clang.tar.gz.
Archive hash: `7402939a854b643022e239dfa544a200c048b798bb0861890010db48a72efc73`.
Inner archive hash: `6df762cc48394584e86fade25e972189d3706a9370923f3ccc968de3b5ee5543`.
Build and runtime gates pin JNI/core/alias/SLF4J/settings bytes. Runtime additionally
checks H5 class origin, exact library path, loader overrides, disabled external
plugin preload, actual host/JDK and HDF version/DEFLATE encode/decode capability.

```sh
python3 /private/tmp/scalafim-execution-20260929/run-sbt.py "$PWD" hdf5-adapter-tests-final.log \
  -Dscalafim.hdf5.provider.dir=/private/tmp/scalafim-execution-20260929/hdf5-probe-runtime \
  -Dscalafim.hdf5.test.dir=/private/tmp/scalafim-execution-20260929/hdf5-adapter-runtime/verified-fixtures \
  archiveHdf5JVM/test archiveHdf5JS/test
# The independent Python fixtures must be authored first; see runtime manifest.
# JS tests also run in a separate invocation with no provider property.
```

## Contracts and resource policy

Flat ASCII dataset names are bounded to 128 characters. Extents have positive
Long dimensions and rank 1–3. Slab count/offset arithmetic, selected products,
dtype capacities and extent/chunk byte products are checked before payload JNI.
Zero selection never acquires a dataspace or enters H5Dread/write. Mutable
primitive arrays are borrowed for a synchronous slab only. Cancellation is
checked before each slab and cannot interrupt an entered HDF call.

One process-wide adapter singleton serializes loading, all JNI work, error stack
capture and closes across every adapter/file/dataset. It does not promise safety
for unrelated direct H5 callers or separately loaded copies of this module.
Files use H5F_ACC_EXCL; there is no TRUNC code path. Only owned IDs are closed,
in reverse order, with primary and cleanup errors retained. Copied dataset types
are owned; predefined types and global H5close are never closed.

Ceilings: 65,536 block elements, 1 MiB raw cache with 521 slots per dataset,
4 MiB metadata cache per file, two files, two open datasets per file, 24 owned
native IDs. Smaller limits within the validated admitted range are accepted.
Receipts retain operation/payload attempts, successful calls, selected elements/
bytes, failures, current/peak IDs, handles, sampled heap and observed metadata.
Cache limits are settings, not allocator or physical IO measurements. An
arbitrary native close failure can leave an ID recorded as outstanding; no
crash or native allocator recovery is promised.

## Verified local result and source binding

**Pass for the explicitly opt-in local macOS ARM64/OpenJDK 25.0.1 lane.**
This is a candidate for parent review, not production distribution or Core
closure. Final source stayed frozen while every queued/running verification
job executed. `tests-final-source.json` and `adapter-checks-manifest.json`
record the tested ten-path hashes. The final report is updated afterward with
measured results; the seven executable/build paths are unchanged. The final
`candidate-manifest.json` binds all ten final paths, commit, compiled classpath,
provider, exact commands, raw/meta receipts and complete retained runtime files.
All files live under:
`/private/tmp/scalafim-execution-20260929/hdf5-adapter-runtime`.
No owned queued/running job remains at handoff.

| Check | Actual result | Raw log and metadata |
| --- | --- | --- |
| Opt-in adapter JVM tests | 20 passed: 9 shared, 11 native | `receipts/hdf5-adapter-tests-final.log` and `.log.meta.json` |
| Opt-in adapter JS tests | 10 passed: 9 shared, 1 platform refusal | same final test log |
| Ordinary `scalafimCompileAll` without provider property | exit 0, zero compiler warnings; sbt eval prints `Option[String] = None` | `receipts/hdf5-adapter-default-compile.log` and sidecar |
| Ordinary archive tests without provider property | JVM 15, JS 10 passed | same ordinary log |
| Adapter JS without provider property | 10 passed | same ordinary log |
| Fresh compiled JVM without provider property | typed MissingCapability, no JNI image loaded, exit 0 | `fresh-missing-capability.log` and sidecar |
| Independent small h5py oracle | every one of 3,080 cells in 8 datasets checked, including dtype/endian/fixed shape/chunks/filter fields | `adapter-oracle-small.log` and sidecar |
| Independent large h5py oracle | all 16,777,216 value cells and all 16,777,216 validity cells checked | `adapter-oracle-128.log` and sidecar |

Both adapter compilations use `-Werror` and are warning-free. The native JVM
runtime logs retain Java 25's deprecation notice for Scala 3.7.4 `LazyVals`
using `sun.misc.Unsafe`; this is not a Scala compiler warning and is not hidden.
Earlier sandbox and compile failures remain in `receipts/` as historical
attempts, with their real nonzero exits. No global test-all result is claimed.
The root aggregate and global compile/test aliases are byte-for-byte unchanged
from the base; neither new target is added to them. Default build/test loading
requires no HDF JNI jar, provider property, native extraction or download.

The small adapter fixture uses shape 7×11×5, chunks 3×4×2, reversed delivery,
nonzero offsets, misaligned and partial-edge slabs, F32/F64/U8 and physical U8
validity codes, each with no filter and DEFLATE level 4. Separate Python-authored
literal data is read at all 18 cells plus six offset edge cells. Another Python
fixture proves seven pre-payload stored-format refusals: contiguous layout,
big-endian floats, unsupported integer dtype, shuffle, multiple filters,
extensible shape and rank 4. The independent interpreter is existing Python
3.12.10 / h5py 3.14.0 / NumPy 2.2.6 / HDF5 1.14.6, in a separate process.

The native suite verifies no clobber of existing sentinel bytes and HDF SHA,
duplicate names, null/dtype/capacity/slab/chunk/filter refusals, zero no-op,
cancellation before IO, two stores/two adapter instances, global file/dataset
caps, six acquisition failures, thrown managed callbacks and typed primary plus
cleanup retention. One hundred warm cycles give descriptors 26→26 and zero
final owned/provider IDs. Concurrent callers on two adapter instances read
100 independent edge slabs and finish with zero handles. The adapter suite
records payload attempts and typed refusals; it does not establish recovery
from arbitrary native close failures. The optional native race was not rerun
through the adapter and is not part of this qualification.

### Fresh bounded heap lane through the adapter

`run_adapter_checks.py` invokes `scalafim.archive.hdf5.Hdf5AdapterCheck`,
compiled from this candidate's JVM test file, using the exact sbt-exported
classpath. It uses `-Xmx64m`, explicit provider/native path,
`--enable-native-access=ALL-UNNAMED`, `HDF5_PLUGIN_PRELOAD=::`, NMT and dyld
image logging. `adapter-checks-manifest.json` retains every full Java/Python
argv and per-command PID/exit/timing/raw/meta hash. `compiled-classpath-closure.json`
hashes all 381 files in the actual classpath. `author_fixtures.py` and its log
retain the Python-authored fixture inputs. Reproduction requires a fresh
fixture output directory: exclusive creation intentionally refuses existing
files. The retained helpers are local qualification artifacts, not production
package tooling.

Physical shape 128×131072, chunks 1×32768, no filter, blocks of at most 65,536
elements. Formula: `row*1048576 + column*0.125 - 17.25`; U8 validity
`(row+column)%4`. One Double buffer (524,288 bytes) and one byte buffer
(65,536 bytes) are reused. The writer reads back every block; a fresh JVM
reopens the moved file after its original pathname is absent and checks all
values again. h5py independently checks every cell. This is actual adapter
128 MiB F64 plus 16 MiB validity evidence, not merely a JNI self-roundtrip.

All sizes below are bytes; heap is sampled after slabs, RSS is the OS peak for
that exact Java child via `wait4`, and NMT is JVM committed memory at exit.

| Lane | Java PID | Sampled heap peak | Heap maximum | OS peak RSS | Metadata observed peak | Peak owned IDs | Final owned/provider IDs | Selected bytes | NMT committed |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | --- | ---: | ---: |
| Write + readback | 16525 | 55,703,344 | 67,108,864 | 126,369,792 | 53,083 | 6 | 0 / 0 | 301,989,888 | 169,398,558 |
| Fresh reopen/read | 16528 | 54,654,768 | 67,108,864 | 124,387,328 | 53,083 | 7 | 0 / 0 | 150,994,944 | 169,309,326 |

Writer/reopen/oracle commands plus orchestration took 2.047381 seconds in this
run; this is cost evidence, not comparative performance evidence. Native
allocator totals, physical IO bytes, uncached reads and native-only peak
residency remain unavailable. A 64 MiB heap limit does not bound total process
memory. Actual dyld images for each fresh JVM resolve the JNI/core to the
locked task-cache artifacts; standalone image files are hashed, while OS
shared-cache images are explicitly unavailable as files.

Retained file:
`hdf5-adapter-runtime/relocated/adapter-128.h5`, 151,048,027 bytes, SHA-256
`93b326be848d5438446cfd5eb86e6ae63b003d704fdd7e22299b98446a6b9813`.
The old accepted provider probe at
`64a14f023013a0c0c3fbccdd1e6e3fd69bcab656` is provider history only. Its
256 MiB lane was not rerun through this adapter and is not adapter evidence.

## Remaining gates

Repository-distributed provider packaging, JDK21, Linux, x86_64, normal native
artifact distribution, unrelated-caller native thread safety, exact HDF native
allocation attribution, uncached physical IO, HDF transactions, power-loss
semantics, estimate scientific identity/axes/df/schema, consumers and estimate
codecs remain separate gates. This local adapter does not close estimate Core.
