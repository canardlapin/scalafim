# Atlas INT8 follow-up receipt, 2026-10-04

This receipt binds the signed-INT8 change from `263e6a91` ("fix(atlas): pin
native INT8 codec and record qualification") to commits. It also records the
fixes for the Opus review of that commit. The evidence directory
[`atlas-epic-2026-10-03/`](atlas-epic-2026-10-03/) was measured on a dirty tree
and is working-tree evidence, not SHA-bound; see
[its note](atlas-epic-2026-10-03/WORKING-TREE-EVIDENCE.md).

## Subject

- ScalaFIM branch `fix/atlas-int8-followup-20261004`, cut from `main` at
  `7e1f989f0b1894c8161ec65e34e73eb1ef918ad8`, which contains `263e6a91`.
  The gates below ran on that tree with this commit's changes applied, and on
  nothing else.
- image4s pin `2c0638fb0767058ccd59b37f93754e95fc27df28` (unchanged). It is
  reachable from image4s `main` through the PR #15 merge commit
  `c9fcab27f21bf60301260e0c084c3a20d279c5e6` (parents `26a74ad9`, `2c0638fb`).
- reframe4s pin `9a4508351d74567147b8ea3221d82db89e5892b0` (unchanged).
- The INT8 change on `main` is the image4s pin bump in `build.sbt` and the
  `NiftiSuite` test "signed INT8 storage and volume reads preserve boundaries and
  scaling". This commit adds an atlas-level test,
  `AtlasIoSuite` "AtlasLabelMaps reads signed INT8 NIfTI labels and rejects
  negative region ids".
- The image4s pin adds `NiftiDatatype.Int8` as the first enum case, so the
  ordinals of `UInt8`, `Int16`, `Int32`, `Float32` and `Float64` each shift by
  one. No ScalaFIM code uses `NiftiDatatype.ordinal`, `values` or `fromOrdinal`.

## Gates

All gates used `tools/build/sbt-warm` on the default pinned-URI build.

| Gate | Result |
| --- | ---: |
| `imageJVM/test` | 385 passed, 0 failed |
| `imageJS/test` | 355 passed, 0 failed |
| `atlasJVM/test` (full module, every suite) | 116 passed, 0 failed |
| `atlasJS/test` | 79 passed, 0 failed |
| `examplesCompile` | passed |
| `scalafimCompileAll` | passed, no compiler warnings |
| `transformJVM/test` | 177 passed, 0 failed |
| `transformJS/test` | 147 passed, 0 failed |
| `providerSpikeJVM/test`, `providerSpikeJS/test` | passed (compile-only module, no tests) |

`image`, `transform` and `providerSpike` are the projects that depend on
reframe4s directly. The only build warning was the existing linops4s
build-definition warning (`build.sbt:82`, `streams` inside `if`).

The 2026-10-03 working-tree counts (image 389/359, focused atlas 209/155)
included uncommitted C1 suites and are not comparable to these counts. The full
`atlasJVM/test` on this tree completed in 380 s with no timeout.

## Mutation check

Against a local image4s copy of `2c0638fb`, selected with
`-Dscalafim.image4s.build`, the INT8 branch of `readRawValue` in
`modules/image4s-nifti/shared/src/main/scala/image4s/nifti/Nifti.scala` was
changed from `buffer.get(offset).toDouble` to
`(buffer.get(offset) & 0xff).toDouble`. The new `AtlasIoSuite` test then failed
("expected exception of type 'java.lang.IllegalArgumentException' but body
evaluated successfully"): the stored `-1` decoded as `255`. The file was then
restored. Its SHA-256
(`de98682a1b539f7472a3d5eae593a3fab898b3cadc4ba5334fa8994fed7f9318`) and git
blob (`6d627e2627b364ff7870c7d317fe0ac8dd6a44bf`, equal to `2c0638fb`'s blob)
match the pre-mutation values, and the suite passed again (17/17).

## image4s on the classpath

`build.sbt` now forwards `scalafim.image4s.build` to `reframe4s.image4s.build`,
following the existing `image4s.locus4s.build` pattern. Measured with
`export atlasJVM/Test/fullClasspath`:

- With `-Dscalafim.image4s.build=<local 2c0638fb copy>`: one `image4s-core` and
  one `image4s-geometry` classes directory, both from the local copy.
- On the default pinned-URI build: two of each. One comes from `2c0638fb`,
  loaded by ScalaFIM; the other from `26a74ad9`, loaded by reframe4s. reframe4s reads `reframe4s.image4s.build` as a local
  path (`file(path).getCanonicalFile.toURI`), so it cannot take ScalaFIM's git
  URI. Removing the duplicate in the default build needs a reframe4s pin whose
  image4s pin is `2c0638fb` or later. reframe4s was not bumped here.

## Documentation moved to the C1 branch

These files describe the typed-atlas rework (cluster C1), which is not on
`main`. They were removed from `main`. Each blob is identical on `main`, on
`263e6a91`, and on `wip/atlas-typed-20261004` (`7b32f6e0`):

| Path | Blob |
| --- | --- |
| `docs/plans/atlas-typed-plan-2026-09-30.md` | `9ed8b8a4b7e6bb444e264c11ed367e5321a5405f` |
| `docs/verification/atlas-epic-2026-10-03.md` | `2f08134fe97a900a2d210796e19e4a929053be01` |
| `docs/verification/atlas-epic-2026-10-03/native-source-inventory.json` | `5cf090530514050ce41da865b4d57805961ccdae` |
| `docs/verification/atlas-epic-2026-10-03/parity-fixture-integrity.json` | `cc46cf4cb974cc30d7ca1df8ec60ce2827fbccf3` |

The C1 version of `modules/atlas/README.md` (blob
`3ae1e55ef2fd9d6963522a3d086ef5c5e0774cf0`) is also on the wip branch. The `main` README now documents
only APIs that are on `main`, plus a short INT8 note.
