# Toolkit transform extraction into neuroim4s

This locally qualified candidate removes ScalaFIM's `transform` module. Portable toolkit codecs,
registration interpretation, conversion and warp policy move into optional
`neuroim4s-transform` on JVM and Scala.js; file/HDF5 containers move into optional
JVM `neuroim4s-transform-io`. Generic raw NIfTI-1 container IO moves into image4s.
Generic transform algebra, inverse solvers and resampling remain in reframe4s/Gale.
No surface, atlas, MVPA or ScalaFIM dependency enters neuroim4s.

## Local source candidates

These source commits have **not** been published. Publication approval and a fresh
ScalaFIM qualification against the published default pins remain outstanding.

| Repository | Candidate | Published parent |
| --- | --- | --- |
| image4s | `aef5e223169f0831c942694c4b109ab20d152217` | `7885f67798242412800a448eda68acfbfff4f751` |
| reframe4s | `7ec7feff96d4057a86196b5e86d592688b7c7f84` | `e4df73969e1fccb7aa0153a9203927c6dd8b0e3d` |
| neuroim4s | `018a9f2e80db4f2d2945c7eba867835ab9144135` | `0ca4d07fc021d21a7802929069396d4df5bf4cc3` |

The prepared consumer is `59db6fbde6a2f6225f591998d2e686eee1617724` on `eco/transforms-20261020`, based on
`d9d1c0113bc1e48944f1b92dd1829ab8cbabf680`. Verified Git bundles and a binary cutover patch preserve all four
candidates. The active eco branch retains published provider pins until the separate
publication step; ScalaFIM remains local and unpushed.

## Ownership and compatibility

- The migration inventory accounts for all 642 original transform paths: 28 production
  Scala files, 46 test/support Scala files and 568 resource files. One trivial link-only
  test is deleted. Seven ScenarioHarness courts/support files stay downstream in the
  existing unpublished `providerSpike` test scope, with its strict compiler flags.
- All 568 resources (8,414,192 bytes), including native manifests, golden files and
  receipt whitespace, are byte-identical. Existing resource addresses and persisted
  provenance text are retained. Namespace changes in compiler-negative snippets preserve
  their intended frame-mismatch failure rather than relying on missing imports.
- Twenty-three production files differ only by namespace/type-name/EOF formatting.
  Five have deliberate boundary changes: the raw NIfTI reader/writer, two typed
  domain codec adapters and conversion admission. Numerical formulae, tolerances and
  budgets are retained. Public composition is tested outside the library namespace.
- image4s owns `Nifti1Container` and `Nifti1Writer`; its existing `NiftiRaw` semantic tag
  is unchanged. Parsing checks supported datatype/bit width, finite integral offset,
  rank/positive extents, payload size and flat-index bounds. Writer counts use `BigInt`,
  and stored extents and output size must be representable.
- Conversion returns a typed refusal for invalid D3 dimensions or unrepresentable dense
  output before allocating or sampling. NIfTI-1 extent limits apply only to NIfTI output;
  a real dense X5 conversion with extent 32,768 succeeds. Both field codecs check float64
  expansion capacity before allocation. Representability does not guarantee available heap.
- The existing checked StageChain owner restoration remains private, with the same narrow
  frame-erasure allowlist at its new provider path. JVM jHDF/file IO is excluded from the
  portable module. No compatibility aliases, local module shell or duplicate kernels remain.
- Native writer-acceptance runners move with their provider. Retained ScalaFIM authoring
  tools require explicit `NEUROIM4S_TRANSFORM_ORACLE_ROOT` for the new corpus; surface and
  atlas authoring paths keep their owners. No guessed sibling path or duplicate corpus is used.

## Verification

**3,455 unique test cases passed across 41 projects**, with zero failures/errors and two existing optional JavaScript skips (real-field pointwise placement and synthetic route timing). Exact skipped names and per-project counts are in the receipt. Producer totals are 123 image4s, 24 reframe4s and 1,055 neuroim4s cases; ScalaFIM contributes 2,253. The final transform suites pass 137 JVM, 137 JS and 28 native IO tests.

The full ScalaFIM modules/examples compile passes on JVM and Scala.js with no Scala
source diagnostics. Existing Intaglio build-definition `lintUnused` warnings are retained.
The ten bounded final-02 gates cover image4s NIfTI, reframe4s resampling, all neuroim4s
modules, and affected ScalaFIM surface/view, spatial/atlas, dataset/MVPA/workflow and
example tests. After the admission repair, final-03 reruns all transform JVM/JS/native IO,
full consumer compile/examples and both-platform providerSpike/spatial/atlas courts.
Unchanged earlier suites are counted once, not added again after reruns.

Independent read-only review accepted the exact final source candidates after finding and
resolving one P2 error-boundary defect. The original escaping-exception regression and
writer count-overflow failure remain in the raw evidence. The initial X5 boundary test
used an oblique oracle and exposed floating-point roundoff in an unjustified exact-zero
assertion; its final specimen is a literal identity field and retains exact-zero checking.
No existing numerical tolerance was loosened.

This is source-migration qualification against explicit local provider checkouts. It is
not hosted CI, an artifact release, a fresh native-tool oracle regeneration, graphical
runtime acceptance, a fresh full-repository test run or a new scientific/performance claim.
Previous full core qualification remains separately documented in
[the core receipt](../neuroim-core-20261019/README.md).

## Evidence and next action

[receipt.json](receipt.json) binds candidates, gate commands/results, unique XML counts,
review, source/resource inventories and recovery artifacts. Raw logs, exit-status sidecars,
input SHA-256 manifests, XML archive, adverse runs and verified bundles live under the shared
Git directory: `verification/neuroim-transforms-20261020/`.

After approval, publish the three provider candidates in dependency order to
`canardlapin/*` using non-force fast-forwards, verify exact live-remote SHAs, apply the
preserved consumer candidate, then qualify fresh default pins before closing
`eco-neuroim-transforms`. Do not push ScalaFIM as part of that provider approval.
