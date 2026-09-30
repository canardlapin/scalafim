# Bounded HDF5 flat namespace inventory

Mote prerequisite: `bd-01M3S9P5140KJ5N7HEFTVFK10Z`; base
`16f7e35b5fe4c8ff9ed00f35c860f364138be33b`. This local capability is a
prerequisite for the separate physical estimate backend; it is not its admission.

`Hdf5FlatInventory` has a private constructor and validates an immutable vector
of one or two distinct flat ASCII `Hdf5DatasetName`s. The default
`Hdf5File.verifyFlatInventory` refuses unsupported implementations. The JNI
override runs under the existing process-wide owner and checks a live handle,
non-null descriptor, exact unmounted root link count, expected-name existence,
hard-link type before object lookup, and dataset object type with reference
count one. It acquires no native IDs, attaches no dataset handles, enumerates no
foreign names, follows no soft/external links, and reads no payload. Refusals use
fixed bounded messages and existing attempt/failure counters.

This proves the reachable root namespace only. Dataset shape, dtype, chunk and
filter validation remain `inspect` responsibilities. Deleted/unreachable bytes,
raw-file forensics, attributes, payload correctness, publication, scientific
schema, and the whole physical backend are outside this claim.

The test-only independent `inventory_fixture.py` uses exclusive h5py creation
and literal expected outcomes. Its eighteen fixtures include one/two datasets,
reversed creation order, extra/missing/wrong names, empty root, groups, nested
datasets, group cycle, named datatype, rc=2 hard alias, soft/dangling soft links,
external/dangling external links, a 65,536-character foreign name, and 10,002 root
links. Tests also reverse descriptor order and temporarily hide the existing
external target, exercise the attached-handle cap with acquisition failure
armed, reject null/closed calls, and check repeated success/failure lifetime.

Evidence is retained outside the source tree in
`/private/tmp/scalafim-execution-20260929/hdf5-inventory-runtime`.
Source/provider freezes and all fixture/log/runtime hashes accompany the final
manifest. All six source, 48 provider, and 385 compiled-runtime hashes remained
unchanged through the completed gates. Only this evidence document was updated
after testing; executable sources remain exactly the tested bytes.

Actual results (all final commands exited zero):

- `archiveHdf5JVM/test`: 25/25, including all eighteen fixture outcomes checked
  in both descriptor orders. Native test fork uses `-Xmx64m`.
- `archiveHdf5JS/test` without a provider property: 12/12, including shared
  constructor/fallback checks and explicit JS missing-capability refusal.
- Ordinary `scalafimCompileAll` without a provider property: passed all 89
  alias tasks, with zero compiler warnings. Opt-in HDF5 remains excluded.
- Fresh JVM without a provider property: typed `MissingCapability` before
  native use. Independent h5py readback verified all 3,080 adapter-written cells.
- Inventory lifetime: 100 cycles / 400 files / 300 expected refusals; descriptors
  26 to 26. Inventory contributes zero payload calls; owned files, datasets,
  IDs and provider IDs return to zero after each case. Existing suite payload
  counts are separate: final process receipt has 595 payload calls, unchanged
  throughout the inventory tests. Normal suite peak owned IDs is eight.

Full raw logs and metadata retain every attempt. The default Python lacked
h5py (two fixture attempts exited one); the existing Python 3.12 environment
then generated both fixture families successfully. Three sandboxed sbt attempts
failed at the boot lock before compilation (exit one); authorized cache-access
reruns passed. The Java 25/Scala LazyVals `sun.misc.Unsafe` runtime deprecation
warning is retained; compiler warning count is zero.

The source-controlled fixture generator is test-only. To reproduce, create a
fresh fixture directory with the retained unchanged `author_fixtures.py`, then
run `inventory_fixture.py` with its `inventory` subdirectory. Use ROOT's existing
`run-sbt.py` lock controller with the explicit provider property and
`-Dscalafim.hdf5.test.dir=<fresh fixture directory>` for `archiveHdf5JVM/test`.
Run the JS test and ordinary CompileAll as separate no-provider invocations.
Exact argument vectors, actual exits, versions, limits, fixture bytes and hashes
are in `hdf5-inventory-runtime/candidate-manifest.json` with its SHA-256 file.

The provider remains the existing locked HDFGroup JNI 2.2.0 closure. The only
native lane is actual macOS ARM64/JDK25, with the existing 64 MiB fork heap,
bounded block/cache/handle limits. No JDK21, Linux, x86, native allocator peak,
or new concurrency qualification is claimed. No build, provider, default alias,
dependency, physical backend, or global installation changes are included.
