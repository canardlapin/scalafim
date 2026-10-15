# image4s issue draft: one canonical world-to-voxel routine with documented tie-breaking

Drafted 2026-10-10 from scalafim mote `bd-01M37FQGV8ZPT30X37TA4MM8R7` (gap N1).
image4s pin: `20c9515495e43dff9d17bc1283a8ca1d56355c4a`. **Filed 2026-10-10 as [canardlapin/image4s#24](https://github.com/canardlapin/image4s/issues/24).**

---

**Title:** Provide one canonical world→voxel routine with a fixed evaluation order and documented half-voxel tie-breaking

**Problem**

Callers that map world points to voxels currently evaluate `Affine.inverse` in
different ways. A nearest-neighbour lookup at an exact half-voxel tie can then
pick different voxels depending on the route taken, and at the grid edge one
route can admit a point that another rejects.

Downstream, scalafim has two volume-to-surface engines:

- `GridSpec.worldToVoxel` calls `affine.inverse(world)`. That is
  `Affine.applyUnchecked`, which starts the sum from the translation column:
  `t + m0*x + m1*y + m2*z`.
- `SampleSpaces.coordToIndex` takes `grid.indexToFrame.inverse.matrix` and
  multiplies a homogeneous vector itself, adding the translation last:
  `m0*x + m1*y + m2*z + t`.

The two inverse matrices are bitwise identical. Only the summation order
differs, which is enough to move an exact `k + 0.5` result by one ulp, and
`math.round` then flips.

**Minimal reproduction** (JVM, Scala 3, image4s at the pin above)

```scala
val a = Affine.fromRowMajor[D3](Vector(
  2.0, 0.5, 0.0,  10.0,
  0.0, 3.0, 0.25, -7.0,
  0.0, 0.0, 4.0,   5.0,
  0.0, 0.0, 0.0,   1.0)).toOption.get
val world = Vector(11.75, 3.625, 7.0)   // exactly representable; = a(0.0, 3.5, 0.5)
val inv = a.inverse.rowMajor              // y row: 0, 1/3, -1/48, 2.4375
// translation first (Affine.inverse.apply):
inv(7) + inv(4)*world(0) + inv(5)*world(1) + inv(6)*world(2)   // 3.4999999999999996
// translation last (hand-rolled matrix product):
inv(4)*world(0) + inv(5)*world(1) + inv(6)*world(2) + inv(7)   // 3.5
```

The true voxel coordinate is `(0, 3.5, 0.5)`. Rounding half up gives y = 3 on
the first route and y = 4 on the second. On a `3 × 4 × 5` grid, y = 4 is outside
the volume, so one engine samples voxel `(0, 3, 1)` and the other reports the
point as outside the grid. In a sweep of half-integer voxel targets
(`6 × 8 × 10` points), 46 of 480 differed for this affine. For two
non-dyadic oblique affines, 169 and 154 of 480 differed. The independent
scalafim review measured 486 of 3000.

**Proposed fix**

1. Make one routine canonical, for example `Affine.inverse.apply` or a new
   grid-level world→index method built on it, and expose it so that no consumer
   needs the raw inverse matrix to map points. Document its evaluation order as part of the contract.
2. Document the tie-breaking rule for nearest lookup on top of it: round half
   up (`floor(v + 0.5)`) on the computed double. A tie is decided by the
   canonical routine's result, not by the true real value. Optionally, provide a
   compensated evaluation (FMA or a residual-corrected solve), so that
   exactly representable ties on well-conditioned affines come back exact.
3. Add a parity test: the reproduction above, together with a seeded
   half-integer sweep over oblique affines that checks bitwise equality between
   the canonical routine and any convenience wrappers.

After that lands, scalafim will route `SampleSpaces.coordToIndex` (eager and
GPU engines) and `GridSpec.worldToVoxel` (spatial engine) through the
canonical routine and remove its declared caveat
`volume-surface.cross-engine-inverse-ties`.
