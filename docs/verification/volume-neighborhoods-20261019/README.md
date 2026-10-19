# Compact volume neighbourhood cutover

ScalaFIM now returns and consumes `locus4s.CenteredNeighborhoodSystem[C, S]`
directly. The private `VolumeNeighborhoods` model, its duplicate centre-inclusion
validation, and the ambient-sized row array are removed. Production code shrinks
by 34 lines. No provider revisions, module edges, or dependencies change.

`C` owns compact centre positions; `S` owns ambient voxels. Membership has one row
per actual centre. Geometry construction traverses only those centres, and target
restriction retains the same centre domain and embedding. Scientific geometry and
window materialization remain ScalaFIM policy.

## Public API and identity

Window preparation and materialization now require an `Index[C]`, rather than an
ambient `Index[S]`. Iterate `neighborhoods.centers.indices` to obtain valid centre
indices; `neighborhoods.center(center)` gives the corresponding voxel. Store a
factory result in a stable value before composing dependent operations.

The compiler rejects using a voxel index in a compact centre position. The former
runtime `CenterUnavailable` branch is therefore removed. Prepared and materialized
windows still report their ambient voxel centre, and MVPA measurement IDs continue
to use ambient voxel ordinals. No persisted identifier or scientific fingerprint
is renamed. This is a source API change; known callers have been migrated without
compatibility aliases or casts.

The `fromRelation` import boundary still accepts the original ambient relation and
centre region. It checks exact live owners, rejects nonempty rows outside the
centre region, and delegates centre inclusion to locus4s. When several rows are
invalid, the earliest ambient-row error is preserved. Empty centre support remains
valid and has no compact rows.

## Verification

Baseline commit `c3f29b158` pins seeded ellipsoid/blobby rows, sparse-centre MVPA
IDs, complete measurement-frame identity, measured values, empty support, and
earliest-error reporting against the original implementation on JVM and JS.

| Final affected suite | JVM | JS |
| --- | ---: | ---: |
| image | 359 | 329 |
| mvpa-spatial | 25 | 25 |

All 738 tests passed with zero skips. `scalafimCompileAll` passed on both platforms
without Scala source warnings. New portable checks cover sparse CSR row counts,
compact centre 0 mapping to voxel 4, selected values and centre positions, retained
centre ownership during restriction, empty support, ambient exclusion errors,
compile-time index separation, and identity under reversed compact centre order.
Existing PyMVPA physical-neighbourhood and local prediction/scatter tests pass.

Geometry, seeded RNG, and prepared native-image execution bodies are unchanged
byte for byte. The existing JVM allocation check for a prepared searchlight passed:
2,050,632 bytes for 64,000 samples against its unchanged 2,244,608-byte limit. This
retains the existing allocation gate; it is not a new performance benchmark.
All qualified source blobs remained unchanged throughout the final gates.

[receipt.json](receipt.json) records exact inputs, commands, exit statuses, raw-log
hashes, and preserved prototype compile failures. Raw evidence lives under the
shared Git directory's `verification/volume-neighborhoods-20261019/`. Test-only
ambient expansion retains the original relation-law assertions; production never
reconstructs that ambient relation.

The cutover is local and unpushed. No hosted CI, complete ScalaFIM test-all run, or
new scientific qualification is claimed. The wider provider task remains open for
remaining duplicate/forwarder cleanup; neuroim2 parity APIs stay intact for their
planned neuroim4s move.
