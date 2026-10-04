# Reconcile origin/main into local main (2026-10-04)

Phase C of the owner-delegated consolidation. Branch
`reconcile/origin-main-20261004`, a `--no-ff` merge (no rebase) of `origin/main`
into canonical local `main`. Not landed on `main` and not pushed.

## Inputs

| Ref | SHA |
| --- | --- |
| local `main` (first parent) | `10d59c82d9b0a597a3eb71538b60f6c03302c911` |
| `origin/main` (second parent, PR #18) | `568ce1082b7f3878e0c6ff4607124aac71b9b63b` |
| merge base | `c05a0811e96295c105633bbad0325790e22377c6` |

`git fetch origin` on 2026-10-04 left `origin/main` at `568ce108`; origin had not
moved. The merge brings in 86 origin-only commits over 66 local-only commits.

## Conflicts and resolutions

Git reported two textual conflicts.

### 1. `modules/mvpa/shared/src/main/scala/scalafim/fmri/mvpa/Rsa.scala` (import / `RdmRows` hunk)

The prior audit (against local `53097f3f`) predicted this conflict.

- Local side: `import gale.linalg.{DMat, Matrix, QROptions, QRPivoting}` plus the
  `RdmRows` enum.
- Origin side: `import gale.linalg.DMat`, and `RdmRows` is deleted as part of the
  UMVPA retirement of legacy RSA orchestration.
- Resolution: take the import union `gale.linalg.{DMat, Matrix, QROptions,
  QRPivoting}` and drop `RdmRows`. The merged file still uses `Matrix`,
  `QROptions` and `QRPivoting` in the Gale-QR partial-Pearson residualization
  that local main landed, so all four names are needed. `git grep RdmRows`
  over the merged tree finds no code reference. The only mention outside this
  receipt is in `docs/plans/mvpa-engine.md`, covered below.

### 2. `build.sbt` (reframe4s pin hunk)

This conflict was not in the prior audit. Local main gained it after the audit
through the atlas INT8 follow-up.

- Local side: reframe4s pin `9a450835…`, with `reframe4sBuild` as a block that
  forwards `scalafim.image4s.build` to `reframe4s.image4s.build` before resolving
  reframe4s. The comment documents that forwarding.
- Origin side: reframe4s pin bumped to `5f7152aa…` ("pointwise displacement
  inverse with query evidence"), with the original single-expression
  `reframe4sBuild` and a shortened comment.
- Resolution: keep local's forwarding block and its comment, and take origin's
  revision `5f7152aada60335935843ddc968162f615337415`. Rationale:
  - `9a450835` is an ancestor of `5f7152aa` in reframe4s, so origin's bump is a
    fast-forward.
  - `5f7152aa`'s `build.sbt` still reads `reframe4s.image4s.build`, so local's
    override forwarding keeps working against the newer pin.
  - Both sides' reviewed intent is preserved: origin's new reframe4s
    capability and local's coherent image4s override.

The rest of `build.sbt` merged cleanly. That includes origin's alder, multivar,
mvpa, mvpaDataset, mvpaArtifacts, surface and workflowExamplesJS changes, and
local's `-release:17` JDK guard and image4s pin.

## Aggregates and aliases

The merged `scalafimCompileAll`, `scalafimTestAll` and `examplesCompile`
aliases are byte-identical to origin's. Local main added no new ScalaFIM
module since the merge base, so origin's lists form a superset. They include:

- `mvpaArtifactsJVM` and `mvpaArtifactsJS` in `scalafimCompileAll`,
  `scalafimTestAll` and the root aggregate;
- `workflowExamplesJS` in `examplesCompile`, `examplesTest` and the root
  aggregate.

The Alder subprojects (`modelsLinear`, `ridgeGale`, `tune`, `metrics`) are
upstream `ProjectRef`s. Neither side lists them in the ScalaFIM aliases. They
compile transitively through `mvpaDataset`.

## Dependency pins after the merge

| Dependency | Local main | origin/main | Merged | Source |
| --- | --- | --- | --- | --- |
| ravel | `9c566939` | `9c566939` | `9c566939` | unchanged |
| gale | `da38f8c4` | `18d24dbb` | `da38f8c4` | local (reachable from gale main `a0f0c5c`) |
| alder | `c56a6b17` | `e555bad9` | `e555bad9` | origin |
| locus4s | `58c9739b` | `58c9739b` | `58c9739b` | unchanged |
| image4s | `2c0638fb` | `26a74ad9` | `2c0638fb` | local (reachable from image4s main `c9fcab2`) |
| reframe4s | `9a450835` | `5f7152aa` | `5f7152aa` | origin (conflict 2) |
| graph4s | `b585e594` | `b585e594` | `b585e594` | unchanged |
| multivar | `f74d6317` | `ab811e25` | `ab811e25` | origin |
| intaglio | `596b398a` | `596b398a` | `596b398a` | unchanged |
| bids4s | `a3367839` | `a3367839` | `a3367839` | unchanged |
| zarr4s | `2a5ba963` | `2a5ba963` | `2a5ba963` | unchanged |

Every changed pin is a fast-forward of the other side's pin. Checked with `git
merge-base --is-ancestor` in the sibling checkouts: gale `18d24dbb` to `da38f8c4`,
image4s `26a74ad9` to `2c0638fb`, alder `c56a6b17` to `e555bad9`, multivar
`f74d6317` to `ab811e25`, and reframe4s `9a450835` to `5f7152aa`.

Caveats:

- Origin pins alder `e555bad9`. That commit is reachable only from the upstream
  branch `origin/umvpa/native-fixed-roles-20260930`, not from alder `main`.
  This merge inherits that pin unchanged.
- The image4s diamond through reframe4s is known. reframe4s `5f7152aa` pins its
  own image4s `26a74ad9`, while ScalaFIM pins `2c0638fb`. Origin itself had no
  diamond, because it pinned image4s `26a74ad9`. Local's image4s bump
  introduces the diamond. It is tracked in bead `bd-01M43MQB7S7X8TK76CE391CJPJ`.
- Informational, and not introduced by this merge: there is also a gale
  diamond. ScalaFIM pins gale `da38f8c4`. reframe4s `5f7152aa` pins gale
  `099832ff`, and so does multivar `ab811e25`. `099832ff` is an ancestor of
  `da38f8c4`. Local main already had this diamond: its reframe4s `9a450835`
  and multivar `f74d6317` also pin gale `099832ff`.

## Historical plans that name retired MVPA APIs

Three origin-owned plan documents still name the retired legacy RSA surface.
None of them describes current code as current, so this merge leaves all
three unchanged.

| Document | Names | Status |
| --- | --- | --- |
| `docs/plans/mvpa-engine.md` | `RdmRows`, `RdmAnalysis`, `RsaAnalysis`, `SamplewiseRsaAnalysis` | Header marks it as a historical design record superseded by the unified MVPA cutover, with a link to the immutable pre-retirement revision `528c302e`. |
| `docs/plans/beta-free-rsa.md` | `OperatorCrossnobisRsaAnalysis` | Same historical-record header and pre-retirement link. |
| `docs/plans/unified-mvpa-migration-ledger.md` | `RsaAnalysis`, `SamplewiseRsaAnalysis`, `OperatorCrossnobisRsaAnalysis` and related names | This is the migration ledger. It names these types because it records their retirement. |

## Gates

Gate tree: the merge commit `7180694a`. Every run used
`tools/build/sbt-warm`, one batch at a time, with at least 30% free memory
(`memory_pressure`) before each batch. The resident server was shut down
between heavy batches and before every JS batch. `scalafimTestAll` was never
run in one shot.

### Compile

- `scalafimCompileAll`: exit 0, with no warnings from ScalaFIM sources and no
  `-release:17` violations. The only warning in the log comes from the
  upstream linops4s `build.sbt` (value lookup of `streams` inside an `if`), not
  from ScalaFIM code.
- `examplesCompile`: exit 0. This includes the new `workflowExamplesJS`.

### JVM tests

All modules in `scalafimTestAll`, plus the JVM examples. Counts are passed
over total, with 0 failed and 0 errors unless noted.

| Module | Passed |
| --- | --- |
| locusData | 25/25 |
| pipeline | 38/38 |
| response | 16/16 |
| responseLaws | 2/2 |
| latent | 44/44 |
| ar | 160/160 |
| hrf | 275/275 |
| hrfLaws | 82/82 |
| scenarioTestkit | 5/5 |
| design | 440/440 |
| image | 385/385 |
| providerSpike | no test sources |
| imageView | 42/42 |
| imageViewJava2d | 2/2 |
| imageViewJavafx | 2/2 |
| threshold | 90/90 |
| motion | 105/105 |
| surface | 280/280 |
| surfaceView | 68/68 |
| surfaceViewRaster | 13/13 |
| surfaceViewJavafx | 15/15 |
| surfaceViewConnectivity | 2/2 |
| surfaceViewExamples | 12/12 |
| transform | 177/177 |
| spatial | 227/227 |
| atlas | 116/116 on rerun (see below) |
| archive | 15/15 |
| estimates | 11/11 |
| estimatesIo | 10/10 |
| fitEstimates | 16/16 |
| archiveLna | 11/11 |
| archivedResponseInterop | 142/142 |
| dataset | 76/76 |
| model | 53/53 |
| fit | 565/565 |
| firstLevelLaws | 83/83, including `ConditionMilestoneSuite` |
| mvpa | 401/401, including `RsaProjectionSuite` and `ComponentConfirmationSuite` |
| mvpaFit | 45/45 |
| connectivity | 64/64 |
| mvpaDataset | 108/108 |
| mvpaArtifacts | 13/13 |
| mvpaSpatial | 20/20 |
| mvpaFoundationAdmission | 18/18 |
| group | 75/75 |
| fmriWorkflow | 23/23 |
| archiveZarr | 18/18 |
| datasetZarr | 15/15 |
| surfaceExamples | 3/3 |
| atlasExamples | 5/5 |
| workflowExamples | 3/3 |

Atlas OOM, classified as environmental:

- The first `atlasJVM/test` ran on a resident sbt server that had just
  finished the full `scalafimCompileAll`. It failed one test,
  `MniTemplateBridgeFilesSuite` ("the composite pulls MNI152NLin2009cAsym
  points to MNI152NLin6Asym exactly as ITK TransformPoint does"), with
  `java.lang.OutOfMemoryError: Java heap space`. That run reported 110 passed,
  1 failed and 5 skipped.
- After `sbt-warm --shutdown`, a fresh server ran `atlasJVM/test` at 116/116,
  with 0 skipped.
- The failure is heap accumulation in the server's 6 GiB JVM, not a merge
  regression. No code or test was changed.

### JS tests (partial)

| Module | Passed |
| --- | --- |
| locusData | 25/25 |
| pipeline | 38/38 |
| response | 15/15 |
| responseLaws | 2/2 |
| latent | 44/44 |
| ar | 158/158 |
| hrf | 275/275 |
| hrfLaws | 82/82 |
| scenarioTestkit | 5/5 |
| design | 439/439 |
| image | 355/355 |
| providerSpike | no test sources |
| imageView | 42/42 |
| imageViewCanvas | 8/8 |
| threshold | 90/90 |
| motion | 92/92 |
| surface | 223 passed, 2 skipped, 0 failed (225 total) |
| surfaceView | 68/68 |
| surfaceViewRaster | 13/13 |
| surfaceViewThree | 18/18 |
| surfaceViewConnectivity | 2/2 |
| surfaceViewExamples | 11/11 |
| transform | 147/147 |
| spatial | 202/202 |
| atlas | 79/79 |
| archive | 10/10 |
| estimates | 11/11 |
| estimatesIo | 4/4 |
| fitEstimates | 12/12 |
| archiveLna | 8/8 |
| archivedResponseInterop | 104/104 |
| dataset | 62/62 |
| model | 53/53 |
| fit | 512/512 |
| firstLevelLaws | 83/83 (1307 s) |
| mvpa | 401/401 |
| mvpaFit | 45/45 |

Several JS counts are lower than the JVM counts, for example response, ar,
design, image, motion, transform, atlas, archive, estimatesIo and dataset.
Those modules keep JVM-only IO and platform suites under `jvm/src/test`.

Still pending on JS: connectivity, mvpaDataset, mvpaArtifacts, mvpaSpatial,
mvpaFoundationAdmission, group, fmriWorkflow, archiveZarr, datasetZarr and
`workflowExamplesJS`.

### Pending re-gate after the `e275bffc` merge

Local main has since moved to `e275bffc`, the model-authoring seam fixes in
hrf `Basis.scala` and design `ConvolvedBasisOrthogonalization.scala`. Once
the owner merges it into this branch, re-run `scalafimCompileAll` and the hrf,
design and fit tests on both platforms, then finish the pending JS batches
above.

## Review evidence

- Merge `7180694a`: an independent Opus review returned APPROVE-WITH-NITS. It
  found the code and build resolutions correct, and its run of `*Rsa*` and
  `*ComponentConfirmation*` passed 31/31. Its nits are addressed in this
  receipt.
- The coordinator also ran an independent Opus batch review of the origin
  commits this merge brings in. On GitHub, PRs #16 and #18 carry only a
  Cursor-bot approval, so this batch is the independent review of record for
  them:
  - PR #18 `785726b9` (ComponentConfirmation): APPROVE-WITH-NITS, with the R
    oracle re-run OK. Nits:
    - the confirmation fixture reuses the training X;
    - the error ADT lacks a `message`;
    - design typing is existential;
    - holdout exposure is not consumed;
    - one package does not match its path.
  - PR #16 `7d399f87`: APPROVE.
  - FIR `adcccf0f` and `d10c6c21`: APPROVE. Their range-diff is identical to
    the reviewed `9a343712`.
  - `fa6bd6a9`: APPROVE-WITH-NITS.
