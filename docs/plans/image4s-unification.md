# image4s and Ravel unification

Canonical architecture is owned by reframe4s Mote epic
`bd-01KYNR748JZYEP3A5Q1H9ZB3AR`. ScalaFIM execution is tracked by
`bd-01KYNR99SWCFEW94T4JGD9QPBS`; this document does not fork those contracts.

## Immutable admission route

ScalaFIM consumes `image4s-core` from the exact image4s Git revision declared
in `build.sbt`. Ordinary builds clone that immutable source revision and do not
read a reframe4s or image4s sibling checkout. Coordinated development may
override the source explicitly with:

```text
sbt \
  -Dscalafim.image4s.build=/absolute/path/to/image4s \
  -Dscalafim.ravel.build=/absolute/path/to/ravel \
  imageJVM/test
```

The admitted immutable foundations are:

- image4s revision `497bfd164ad514ff3d1944699550c78caa57e85d`;
- Ravel revision `f804ba51242aae3a1442b3855a20bd896ffa8b64`;
- locus4s revision `4fedc7febf2728f51f6bb008ac7fe41060edc18e`;
- Scala 3.7.4 and JVM plus Scala.js source projects.

The local override is diagnostic only. Final release evidence uses the default
immutable Git coordinates.

## Representation direction

- Ravel owns dense storage, shape, rank, strides, views, and kernels.
- image4s `Sampled` owns the dense image value: one Ravel array plus `Grid`,
  ordered non-spatial axes, and field role.
- ScalaFIM `NeuroVol` and `NeuroVec` are opaque aliases over the corresponding
  ranked `Sampled` values. Their existing APIs are extensions; construction
  and time selection return the exact `Sampled` object without allocating a
  compatibility wrapper. `NeuroSpace`, `VolumeSpace`, and `SeriesSpace` are
  likewise opaque names over image4s sampling geometry.
- Legacy first-axis-fastest buffers cross one checked ingress. Until producers
  emit canonical Ravel storage directly, `Image4sInterop` reports
  `CanonicalizedLegacy` and copies logical values once.
- Sparse time-by-voxel data becomes typed spatial support plus rank-2 Ravel
  storage. It is not a dense sampled image.
- ScalaFIM has no `narr`, `NArray`, `NArrayUtil`, or private multidimensional
  array implementation in source or build definitions. Plain Scala arrays are
  limited to explicit ingress/egress and private mutable kernel scratch.

The admission suite uses asymmetric shape and a permuted affine to prove
logical `(i,j,k)` values, third-axis slicing semantics, affine parity, ranked
access, and explicit materialization on both JVM and Scala.js.

The current semantic and ownership gates pass:

- `imageJVM/test`: 243 tests, zero failures;
- `imageJS/test`: 232 tests, zero failures;
- `registrationJVM/test`: 98 tests, zero failures;
- `registrationJS/test`: 98 tests, zero failures;
- standalone image4s: 157 JVM, Scala.js, and Node tests, zero failures,
  including 22 `image4s-core` tests on each platform;
- the focused admission probe on both platforms;
- independent image4s geometry/core compilation followed by reframe4s and
  ScalaFIM consumers under Scala 3.7.4;
- the complete ScalaFIM `compileAll`, `testAll`, and `examplesTest` aliases,
  including the graph4s/locus4s source-build cutover; and
- reframe4s `compileAll` and `testAll` against the immutable image4s pin.

The source-tree digest is the SHA-256 of the sorted per-file SHA-256 ledger,
excluding `.git`, `.mote`, and generated `target` trees.

The representation verifier scans the full current ScalaFIM source tree and
reports no raw dense-storage escape, parallel ScalaFIM ndarray hierarchy, or
mutable image4s source composite.

Ravel now supplies opaque `CanonicalArray` and `MutableCanonicalArray`
capabilities for whole canonical owned arrays. Refinement retains the exact
Ravel object, exposes bounded logical linear access without a raw-buffer
escape, and passes its JVM and Scala.js correctness suites. The matched JVM
court records identical checksums and 40 bytes of fixed total allocation for
both a primitive canonical array and `CanonicalArray` across 22,440 reads;
median access was 0.717 and 0.719 ns/sample respectively. The gate allows at
most 3x the same-run primitive oracle. This replaces the earlier historical,
cross-run diagnostic in
[`../benchmarks/receipts/image4s-ravel-unification-diagnostic-2026-07-29.json`](../benchmarks/receipts/image4s-ravel-unification-diagnostic-2026-07-29.json).

The matched registration kernel court also compares identical self-composition
semantics in one JMH fork. Setup requires exact validity and component values
at every logical voxel. At side 64, the primitive planar path measured 12.693
ms/op and the canonical Ravel workspace path measured 10.840 ms/op, making the
Ravel path 14.6% faster. Allocation was indistinguishable at 54.38 and 54.29
bytes/op with no observed collection. The complete closeout evidence is in
[`../benchmarks/receipts/image4s-ravel-unification-closeout-2026-07-29.json`](../benchmarks/receipts/image4s-ravel-unification-closeout-2026-07-29.json).

The mutable source-composite blocker is removed. The clean immutable-coordinate
JVM/Scala.js image consumer rerun and the existing allocation/performance court
are green. Restoring a ScalaFIM ndarray or copying into a second owned primitive
image buffer is not an acceptable release solution.

## Sparse series contract

`SparseNeuroVec` is not a second sampled-image or multidimensional-array
hierarchy. Its representation is exactly:

- one `SparseSupport`, containing an ordered, duplicate-free
  `VoxelIndexSet` tied to the spatial volume geometry and a lookup from a
  full-grid linear voxel index to its compact position; and
- one canonical Ravel `NDArray[A, Rank[2]]` with logical shape
  `(time, support-position)`.

Support order is semantic. Constructors from an ordered selection preserve it;
mask-based constructors use the mask's canonical grid order. Time slicing,
dense gathering, sparse arithmetic, and dense roundtrips must keep compact
columns aligned with that order. Duplicate indices are rejected. A missing
full-grid voxel reads as the scalar ring zero; selection APIs require an
explicit `RequireCovered`, `DropMissing`, or `Fill(value)` policy.

Dense-to-sparse gathering and sparse-to-dense scattering operate directly
between ranked Ravel values. Legacy first-axis-fastest or compact ScalaFIM
`NDArray` inputs are compatibility ingress only and always materialize.

A serializer must record the spatial geometry, time-axis extent, label,
ordered support indices, and logical `(time, support-position)` values. It must
not persist or infer raw Ravel offsets or strides. Values are enumerated with
time outermost and support position innermost and reconstructed through ranked
Ravel access, so canonical arrays and future views share one portable format.
