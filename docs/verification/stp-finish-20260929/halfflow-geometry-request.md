# DRAFT upstream request: HalfFlow geometry ownership

> **Status: DRAFT, NOT SUBMITTED.** Prepared for the reframe4s owner under
> STP-P8.04 (`bd-01M39Q5EKTWVNNJ3ZHF3SSRV20`). It has not been filed on any
> tracker, sent, or published. Submission needs explicit owner authorization
> and a chosen route, such as a `canardlapin/reframe4s` issue. Until then no
> ScalaFIM document may describe this request as made.

Refreshed 2026-10-10 against ScalaFIM `origin/main` `8e85ef61`.

## Pins

- ScalaFIM `reframe4sRevision`: `9662317912185c51e82dd04b2c87006ed2af82f2`
  (`build.sbt:133`; adopted by `7b51e4c3`, PR #35).
- ScalaFIM `image4sRevision`: `20c9515495e43dff9d17bc1283a8ca1d56355c4a`
  (`build.sbt:108`). reframe4s `9662317` pins the same image4s revision
  (its `build.sbt:51`).
- The earlier draft cited provider `9a4508351d74567147b8ea3221d82db89e5892b0`
  and development candidate `842ec9a752d76793f936d7024681d50de89117db`. The
  published pin above supersedes both. The HalfFlow `internal` sources are
  byte-identical between `9a45083` and `9662317`.
- ScalaFIM depends only on reframe4s `lie`, `field` and `resample`
  (`build.sbt:143-148`), not on `reframe4s-halfflow`. The request is
  housekeeping for the provider, not a consumer blocker.

## Evidence at reframe4s `9662317`

`modules/reframe4s-halfflow/shared/src/main/scala/reframe4s/halfflow/internal/`
still holds private geometry and field implementations:

| File | Blob | Private geometry it owns |
| --- | --- | --- |
| `Affine.scala` | `e6ad0e1b5d38` | `Affine3D` (4x4 `DMat` plus inverse) and `Affine` helpers |
| `Geometry.scala` | `b2971e02860a` | `SpatialAxis`, `SpatialDims`, `VoxelCoord`, `SpatialPoint` |
| `DenseFieldMorphism.scala` | `c8d080c12a28` | `DenseFieldMorphism`, `DenseFieldInterpolationPlan`, `Resample` |
| `DMat.scala` | `6dd9f38e441e` | private dense matrix backing `Affine3D` |
| `CanonicalImageCompat.scala` | `5d0e1b4fd6fe` | shims between those types and canonical images |

As of 2026-10-10, no issue mentioning HalfFlow exists in
`canardlapin/reframe4s`. A read-only
`gh issue list --state all --search halfflow` returned nothing.

## Requested change (text to submit once authorized)

> HalfFlow keeps private copies of volume geometry under
> `reframe4s/halfflow/internal/`: `Affine.scala`, `Geometry.scala` and
> `DenseFieldMorphism.scala`, supported by `DMat.scala` and
> `CanonicalImageCompat.scala`. Please replace these geometry owners with the
> image4s `Frame`/`Point`/`Affine`/`Grid` types and the reframe4s `lie`/`field`
> maps and dense fields. Keep only HalfFlow's scientific policy and execution
> adapters in `halfflow`. Bind the migration's numerical and ownership checks
> to the exact provider candidate. No consumer pin should claim the cleanup
> until that evidence exists.

## Bounds

This request is limited to the existing housekeeping criterion. It creates no
new ScalaFIM abstraction, dependency or ticket. The reframe4s owner decides
whether `DMat` moves to Gale or is removed.
