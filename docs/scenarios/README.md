# Scenario Manifest

`manifest.json` is the active registry for executable ScalaFIM scenario tests.
It records which scenarios are release-relevant, where they live, what reference
strategy they use, which platforms they run on, and which focused commands
verify them.

The manifest is a registry, not a replacement for the typed scenario result
or an external numerical receipt. Each fixture-backed reference declares one
`evidence_scope`: `hrf_evaluation`, `design_construction`, `fit_inference`, or
`independent_end_to_end`. The focused gate validates that boundary, every
referenced suite and fixture path, and the receipt contract.

Validate the manifest JSON with:

```sh
python -m json.tool docs/scenarios/manifest.json >/tmp/scalafim-scenario-manifest.json
```

The same stdlib-only validator used by CI is:

```sh
python -S tools/scenarios/validate_manifest.py docs/scenarios/manifest.json
```

Check every generated R or Python receipt and its checked-in Scala fixture
without starting R, importing Nilearn, or rewriting either artifact:

```sh
python -S tools/r-parity/check_receipts.py
```

Each R finalizer also accepts `--check`. Input and output hashes detect stale
JSON independently, while `generated_scala_sha256` detects a stale portable
Scala fixture. The same check verifies the generator hash, the environment-lock
hash, and the declared truth boundary.

Every external receipt also records one comparison policy. Comparisons use the
declared row and column identity; they never search for a better lag after seeing
the output. Signed correlation is diagnostic and can pass only alongside the
absolute-error, relative-error, and norm-ratio bounds. Each scenario declares
its own tolerance profile for its evidence layer. There is no global tolerance
override, so a disagreement in HRF evaluation cannot be hidden by loosening a
fit or end-to-end threshold.

Reproduce the pull-request first-level gate locally with:

```sh
bash tools/ci/first-level-gate.sh
```

This uses Python's standard library for manifest and checked-in fixture
coherence, then runs the shared `scenario-testkit`, `hrf`, `hrf-laws`,
`design`, `model`, and `fit` tests on both the JVM and Scala.js. It does not
install or execute R, Nilearn, or fmrimod.

## Live regeneration

[`tools/r-parity/reference-lock.json`](../../tools/r-parity/reference-lock.json)
pins R and Python versions, package versions, and exact source revisions. The
generators reject a source checkout or installed Python package that does not
match this lock.

Regenerate every R and Python receipt in the locked environments with:

```sh
bash tools/r-parity/regenerate_receipts.sh
```

This command expects the locked R and Python dependencies to be installed. Use
`--r-only` or `--python-only` when working in one reference environment. The
source checkout locations default to the sibling repositories and can be set
with `FMRIDESIGN_R`, `FMRIHRF_R`, `FMRIREG_R`, and `FMRIMOD_ROOT`.

The scheduled and manually dispatched
[`scenario-receipts.yml`](../../.github/workflows/scenario-receipts.yml)
workflow recreates those environments on Ubuntu 24.04, checks out each source
at the locked revision, regenerates the receipts, and fails if the checked-in
JSON or Scala files differ. Manual dispatch distinguishes `nightly` and
`release` evidence runs. Pull requests do not install external statistical
stacks; `first-level.yml` consumes the checked-in portable fixtures instead.

The following command remains useful when only the checked-in fmrimod/Nilearn
pair needs a dependency-free coherence check:

```sh
python -S tools/scenarios/export_fit_public_f_contrast_fixture.py --check
```

The local check rebuilds the Scala source text from checked-in JSON without
importing external packages. `--check-external` additionally recomputes the
Nilearn result and therefore belongs in the locked live lane.

## Important receipt boundaries

The S18 receipt uses base R `stats::lm.wfit` and independently validates fixed
and DVARS-derived volume weights. The S13 receipt uses R `fmridesign`, runwise
base-R QR fits, and full-covariance fixed effects. These are fit/inference and
end-to-end receipts respectively; neither substitutes for a structural schema
test.

The delayed-match-to-sample receipt receives the same trial table as ScalaFIM.
R formula compilation checks its structure, while numerical coordinates are
constructed per run with `fmrihrf::regressor_design`. This avoids importing the
known multi-condition value/name and repeated-run matrix defects in
`fmridesign` 0.6.0. The receipt records that accepted difference explicitly.

The paired S12 scenario does not use that numerical receipt. It checks mixed
block, impulse, and variable-epoch construction through ScalaFIM's structural
schema. S15 separately checks the planted task fit against `fmrihrf` rendering
and base-R `lm.fit`. Keeping these checks separate prevents a passing external
fit from standing in for design-provenance evidence.

The two-run AR(1)/GLS receipt fixes `rho = 0.42` so the oracle isolates row deletion, segment
boundaries, exact-first whitening, and direct GLS inference. The shared
scenario separately exercises ScalaFIM's declared run-pooled AR estimation
policy; it does not treat a different estimator as fixed-coefficient parity.
