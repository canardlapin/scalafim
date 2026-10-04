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
  over the merged tree finds no code reference. The only mentions are in
  `docs/plans/mvpa-engine.md`, covered below.

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
diamond, because it pinned image4s `26a74ad9`. Local's image4s bump introduces
the diamond. It is tracked in bead
  `bd-01M43MQB7S7X8TK76CE391CJPJ`.

## `docs/plans/mvpa-engine.md`

The file still names `RdmRows` and `RsaAnalysis`. Its header already marks it
as a historical design record superseded by the unified MVPA cutover, and links
the immutable pre-retirement revision. It does not describe current code as
current, so it is left unchanged.

## Gates

See the gate section appended below.
