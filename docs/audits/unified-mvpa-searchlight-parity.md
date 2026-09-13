# Unified MVPA native searchlight parity receipt

Date: 2026-09-13

Packet: M1.E2, `bd-01M0Z69N65WFHE7DCPVZXG4J9P`

Status: qualified within the claim boundary below

## Accepted claim

For the frozen masked volume fixture, ScalaFIM's exact volume searchlight has
the same ordered centers, masked support, closed-radius membership, and local
mean-contrast outputs as PyMVPA's native `Sphere`, `IndexQueryEngine`, and
`sphere_searchlight` when both engines are explicitly bound to the same
integer-valued physical-world coordinates and millimeter radius.

The volume comparison does **not** claim that PyMVPA's default voxel-index
radius is a physical radius. The fixture retains that tempting but wrong route
as a counterfactual, and its neighborhoods differ substantially under the
anisotropic sheared affine.

For the frozen folded mesh, ScalaFIM's surface searchlight has the same ordered
neighborhoods as an independent shortest-path Dijkstra oracle over unique
triangle edges weighted by their three-dimensional lengths. The retained
Euclidean chord counterfactual differs. This is a surface-geodesic
qualification, not a claim that PyMVPA's voxel sphere supplies a surface
metric.

## Frozen reference and construction

The reference is the official PyMVPA repository at commit
[`f699189b5b7e7a1bcaaf6f0a19aa077d8879b422`](https://github.com/PyMVPA/PyMVPA/commit/f699189b5b7e7a1bcaaf6f0a19aa077d8879b422),
installed as `2.6.5.dev1` with `python setup.py --no-libsvm install`. The oracle
environment was Python 3.9.22, NumPy 1.23.5, SciPy 1.10.1,
scikit-learn 1.2.2, nibabel 5.2.1, h5py 3.10.0, setuptools 59.8.0,
and wheel 0.48.0. LibSVM is irrelevant to the neighborhood and scalar-measure
paths used here.

PyMVPA imports the historical `h5py.highlevel` module. The generator aliases
that import to current top-level `h5py` before loading PyMVPA. This is an
oracle-process import repair only: no PyMVPA dependency, compatibility API, or
runtime shim was added to ScalaFIM.

The generator requires the reference checkout path and rejects any Git commit
other than the frozen revision. It runs PyMVPA's native neighborhood and
searchlight code, checks the result against a separately written direct affine
distance oracle, and writes the deterministic fixture. It also executes and
records PyMVPA's `ValueError` for duplicate physical coordinates.

The canonical fixture is
[`mvpa.searchlight-pymvpa.v1.json`](../scenarios/fixtures/mvpa.searchlight-pymvpa.v1.json),
with SHA-256
`07ebd4de37ae675895352c4763643c96b72b929c80c1e6027c8e7aea41645ee0`.
The generator is
[`generate_pymvpa_searchlight_reference.py`](../../tools/mvpa/generate_pymvpa_searchlight_reference.py).

## Geometry and numerical court

### Volume

- Shape: `4 x 3 x 2`, in ScalaFIM domain order
  `(x * dims(1) + y) * dims(2) + z`.
- Complete index-to-world affine, row-major:
  `[2,1,0,10; 0,3,1,-7; 0,0,4,5; 0,0,0,1]`.
- Radius: `sqrt(10)` mm with a closed `distanceSquared <= 10` boundary.
- Mask: 20 of 24 voxel ordinals; centers: `0, 5, 8, 13, 18, 23`.
- Boundary/tie witness: center 8 retains equidistant boundary members 4 and
  10 at exactly 10 squared mm, and all members remain in domain order.
- Local estimand: the mean over every class-one sample-feature cell minus the
  corresponding class-zero mean inside each neighborhood.

The native PyMVPA and ScalaFIM output vector is:

```text
[1.2083333333333335,
 2.2944444444444443,
 1.7944444444444452,
 1.9041666666666677,
 2.220833333333333,
 1.6500000000000021]
```

Absolute and relative tolerances are both `1e-12`. The direct geometry oracle
is evaluated separately in the Scala suite as well as in Python.

### Surface

The mesh is a six-triangle folded strip with eight vertices. Radius 1.0 uses a
closed boundary. The Dijkstra neighborhoods for centers `0, 2, 3, 7` are:

```text
0 -> [0, 1, 4]
2 -> [1, 2, 6]
3 -> [3, 7]
7 -> [3, 7]
```

Vertex 3 is only `sqrt(0.02)` from vertex 0 by chord distance but is outside
the radius-one geodesic neighborhood. Its deliberate presence in the chord
counterfactual prevents accidental substitution of Euclidean point distance
for the mesh metric.

### Failure locality

The shared suite verifies typed rejection of a negative volume radius. It also
forces the local analysis at center 13 to fail, observes exactly one
`RoiOutcome.Failure`, and confirms that later centers 18 and 23 still succeed.
The successful output order remains the original center order with the failed
center omitted.

## Setup and kernel measurement

The JVM-only receipt keeps neighborhood construction outside the local kernel
measurement. After one warm-up of each phase, it takes three observations and
reports the median without imposing a flaky wall-clock assertion.

Observed receipt on this host:

```json
{"fixture":"20x20x12-r3.1","centers":4800,"neighborhood_members":474744,"setup_median_ms":70.263,"kernel_median_ms":2449.374,"checksum":37399.828069647560}
```

The later full affected-module JVM run repeated the same checksum with setup
median 75.584 ms and kernel median 2295.349 ms.

This run used sbt 1.11.7 on the Homebrew Java 25.0.1 runtime reported by the
sbt launcher, on Darwin arm64. The semantic Scala.js gate used Node 26.7.0.
These are exact run versions but are not the JDK 21/Node 22 qualifying profile
frozen by the [resource protocol](../plans/unified-mvpa-resource-comparative-protocol.md).
Accordingly these measurements separate setup from kernel cost and make the
workload reproducible; they do not grant an epic resource-budget or comparative
performance claim.

## Verification

The following commands passed:

```sh
OPENBLAS_NUM_THREADS=1 OMP_NUM_THREADS=1 \
  /path/to/python3.9 tools/mvpa/generate_pymvpa_searchlight_reference.py \
  --check --pymvpa-source /path/to/PyMVPA

sbt -Dsbt.supershell=false \
  "mvpaSpatialJVM/testOnly scalafim.fmri.mvpa.spatial.PyMvpaSearchlightParitySuite"
sbt -Dsbt.supershell=false \
  "mvpaSpatialJS/testOnly scalafim.fmri.mvpa.spatial.PyMvpaSearchlightParitySuite"
sbt -Dsbt.supershell=false \
  "mvpaSpatialJVM/testOnly scalafim.fmri.mvpa.spatial.SearchlightPerformanceReceiptSuite"
```

Targeted semantic results were 3/3 on JVM and 3/3 on Scala.js. The JVM-only
measurement receipt was 1/1. The bounded full affected-module gates also
passed:

- JVM: image 305/305, surface 144/144, mvpa 118/118, mvpa-spatial 16/16.
- Scala.js: image 284/284, surface 118/118, mvpa 118/118, mvpa-spatial 15/15.

## Claim exclusions

- No classifier equivalence is inferred from the scalar local estimand.
- No PyMVPA-default voxel-index-radius equivalence is claimed.
- No PyMVPA surface implementation equivalence is claimed.
- No timing threshold, allocation ceiling, or reference-hardware performance
  admission is claimed.
- No PyMVPA compatibility layer or production dependency was introduced.
