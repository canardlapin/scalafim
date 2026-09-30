# fsLR VeryInflated display kind: typed slice (2026-09-30)

Mote: `bd-01M37DV2BCSYH88EWF5M835JC5`, acceptance item 3 only, plus the item 6
fixtures for serialization and round trips. Branch `surface/very-inflated-20260930`,
base `9504d696`. The lost candidate `surface/fslr-court-20260924` was not reused.

## mesh4s reconciliation

The unmerged `codex/mesh4s-surfaces` branch moves topology and realizations to
mesh4s. It does not change `SurfaceTags.scala`, and mesh4s itself has no surface
kind or display-kind type. `SurfaceKind` therefore stays owned by ScalaFIM, and
adding a case does not create a parallel identity to anything in mesh4s. If that
branch is merged later, the only overlap is textual, in `FreeSurferSurfaceReader`
if it touches that file. This slice does not touch the uncommitted GIFTI placement
work in the shared tree (`GiftiModel`, `GiftiPayloadDecoder`, `GiftiSurfaceCodec`,
`GiftiSurfaceReader`) and does not depend on it.

## What is admitted

- `SurfaceKind.VeryInflated` is a named case. Its canonical saved label is
  `veryinflated`, the TemplateFlow/BIDS suffix spelling.
- `SurfaceKind.named` is the single table of spellings. `fromString` uses it.
  `VeryInflated` (GIFTI `GeometricType`), `veryinflated`, `very_inflated` (HCP)
  and `very-inflated` all parse to `VeryInflated`, ignoring case and surrounding
  whitespace. A legacy `Custom("veryinflated")` label is decoded as the named
  case, so it does not become a second identity.
- `SurfaceKind.fromFileName` (shared, so it runs on JVM and JS) now does the
  file-name inference that was JVM-private in `FreeSurferSurfaceReader.inferKind`,
  which now delegates to it. **This fixes a bug that existed before this change:**
  the HCP name `*.L.very_inflated.32k_fs_LR.surf.gii` splits into the tokens
  `very` and `inflated`, and was inferred as `Inflated`. It is now `VeryInflated`.
  The TemplateFlow name `..._veryinflated.surf.gii` was previously inferred as
  `Custom("veryinflated")`.
- `SpaceRef.Surface(..., VeryInflated).worldIn` returns `NoWorldSpace`, the same
  as `Inflated`. A display realization is not in scanner coordinates.
- Surface scene documents save `"kind":"veryinflated"`. The display kind is part
  of the saved surface identity, so an inflated document is refused against a
  very-inflated model, and the reverse, with `identity differs`.

Exhaustive matches: `SurfaceKind.label` and `SpaceRef.worldIn` have no wildcard.
The test oracle `SurfaceKindSuite.canonicalSpelling` also has none, so under
`-Werror` a new case that is missing from any of the three fails compilation.
`scalafimCompileAll` found no other exhaustive `SurfaceKind` match.

## Evidence

Every run used `/private/tmp/scalafim-execution-20260929/run-sbt.py`. Logs are in
`/private/tmp/scalafim-execution-20260929/logs/`.

| Run | Log | Exit | Result | Log SHA-256 |
| --- | --- | --- | --- | --- |
| Targeted JVM | `very-inflated-20260930-jvm-1.log` | 0 | surface 18, spatial 5, surface-view 8 | `fa8619538ce4255b406a8818a20585cc29eeaed032a90c6cf7bd5b6f36a6f9ba` |
| JVM gate | `very-inflated-20260930-gate-jvm.log` | 0 | surface 174, spatial 212, surfaceView 62, atlas 114 | `1498c7340331b4d2641b8d4520de5d59db2aa4d9e4ceb147b8ab1a632c4ceddd` |
| JS gate | `very-inflated-20260930-gate-js.log` | 0 | surface 139, spatial 188, surfaceView 62, atlas 78 | `ac75f3c2ae7fb9041bec17628e23ba8ea633ae0b8827bc551a2d8572d7e4513d` |
| Mutation M1 | `very-inflated-20260930-mutation.log` | 1 | 2 of 18 failed (expected) | `3f2b509607778abcd4a709b9ca6ac6184b76ba09a0496c07a614b97377168634` |
| Mutation M2 | `very-inflated-20260930-mutation-spatial.log` | 1 | 1 of 5 failed (expected) | `56151b09e8398b9addb741f94dbc5d3ed0c2808d777ee41e91b66d905094b3b8` |
| `scalafimCompileAll`, then targeted re-test | `very-inflated-20260930-compileall.log` | 0 | warning-clean (0 `warn` lines); 18 and 5 passed | `197dd8c3e7dc7372ac9469862e509ed35dd7a30b5c6ed2e7125be007af68afb4` |

Every gate had failures 0 and errors 0.

Mutations:

- **M1** removes the `very` + `inflated` token-pair rule from `fromFileName`,
  which restores the old HCP behavior. It fails
  `SurfaceKindSuite` "file-name inference reads HCP very_inflated as VeryInflated,
  never Inflated" and `FreeSurferSurfaceReaderSuite` "read infers VeryInflated
  from HCP and TemplateFlow very-inflated file names".
- **M2** moves `VeryInflated` into the scanner-native group in `Domain.scala`. It
  fails `SpaceRefWorldSuite` "only anatomical surfaces are in the subject's
  scanner-native world".

After each mutation, both files were restored from copies and checked with
`shasum -c` against the hashes taken before the mutation:
`SurfaceTags.scala` `a580ed9f...ead45ea7` and `Domain.scala` `8a611fac...0354d`.

## Explicitly not claimed

- No real fsLR 32k assets were read. There is no TemplateFlow4s receipt, no
  source provenance, no check of vertex or triangle counts, no winding or
  correspondence qualification, and no check of midthickness, inflated and
  veryinflated as one family (acceptance items 1 and 2).
- No medial-wall label or mask interpretation (item 4). No display-switch
  invariance or rendering (item 5). No visual court.
- The GIFTI codec does not yet read `GeometricType` metadata. The kind still comes
  from the caller or from the file name. Only the spelling `VeryInflated` is
  covered by `fromString`.
- `SurfaceKind.Custom` can still be constructed directly, for example as
  `Custom("veryinflated")`. That value is not equal to `VeryInflated`. Only the
  decode paths (`fromString`, `fromFileName`, the scene codec) canonicalize.
- Scenes that were saved as `Custom("veryinflated")` now decode as
  `VeryInflated`. They restore against a model built with `VeryInflated`, and not
  against a model that still uses the `Custom` value.
