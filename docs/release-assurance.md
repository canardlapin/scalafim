# Release assurance

ScalaFIM is `0.1-development` research software. These courts protect the
current scientific behavior and public examples; they do not imply binary or
source compatibility across releases.

## Enforced local courts

Run the focused first-level court with:

```sh
bash tools/ci/first-level-gate.sh
```

It validates the scenario manifest and checked-in parity receipts without
external Python packages, then runs the scenario testkit, AR, HRF, HRF laws,
design, model, fit, and generated first-level laws on both the JVM and
Scala.js. It also checks the formatting of the non-published
`first-level-laws` assurance module.

Run the JVM scientific coverage court separately:

```sh
bash tools/ci/first-level-coverage.sh
```

Coverage is assessed per scientific module rather than aggregated with IO,
viewer, or adapter code. The floors leave a small amount of ordinary source
movement below the measured 2026-08-12 baseline while still rejecting a
material loss of exercised decisions. The `fitJVM` measurement includes the
generated `firstLevelLawsJVM` tests because that non-published assurance module
owns the law coverage for the production `fit/profile` package.

The focused court also proves that the public first-level guide still contains
the exact compiled DMS acceptance construction and that the checked-in
benchmark receipt matches every benchmark and admission-policy source hash.

| Module | Statement baseline | Branch baseline | Enforced floor |
| --- | ---: | ---: | ---: |
| `arJVM` | 78.84% | 65.29% | 75% / 62% |
| `hrfJVM` | 72.77% | 63.21% | 70% / 60% |
| `designJVM` | 72.84% | 61.20% | 70% / 58% |
| `modelJVM` | 61.53% | 56.00% | 58% / 52% |
| `fitJVM` | 77.31% | 64.86% | 75% / 62% |

## Compiler and source policy

Production and test sources in `ar`, `hrf`, `design`, `model`, `fit`, and
`first-level-laws` compile with deprecation, feature, unchecked, unused-symbol,
and value-discard diagnostics promoted to errors. There are no warning
exceptions in this court. `scalafimCompileAll` includes these strict projects, but the
remaining modules retain the repository baseline while they are migrated. The
strict policy must expand to every published module before any stable or
compatibility-bearing release.

Scalafmt 3.11.5 supplies the deterministic source check. Its sbt integration is
pinned to 2.5.6 because the repository's declared sbt 1.11.7 baseline predates
the sbt 1.12.9 requirement of the 2.6.x plugin line. Formatting is initially
enforced only for the newly introduced `first-level-laws` module so adoption
does not rewrite unrelated work. It must expand to all published source before
a compatibility-bearing release.

Scalafix is not installed merely as a ceremonial gate. Add it when a named,
reviewable semantic rewrite invariant cannot be represented by the compiler,
domain types, or formatter.

Compile-negative tests use `scala.compiletime.testing` for distinctions the
type system owns: an HRF accepts `Lag`, not an absolute `Seconds` clock reading,
and coefficients constructed by one `ResponseBasis` cannot be reconstructed by
another. Data-dependent rank, shape, provenance, and estimability remain typed
runtime tests.

## CI lanes

| Lane | Trigger | Policy status | Evidence |
| --- | --- | --- | --- |
| Full repository | pull request and push to the default branch | required by project policy | `scalafimCompileAll` plus bounded tests across every headless JVM and Scala.js module; JavaFX hosts remain compile-gated |
| Focused first-level | pull request and push to the default branch | required by project policy | portable receipts plus JVM/Scala.js tests and generated-law PR profile |
| Scientific coverage | pull request and push to the default branch | required by project policy | per-module JVM statement and branch floors |
| Generated-law calibration | weekly schedule or manual dispatch | advisory | 300 cases per property for three fixed, reproducible seeds on JVM and Scala.js |
| Scenario receipts | weekly schedule | advisory freshness signal | regeneration in locked R and Python/Nilearn environments |
| Scenario receipts | manual `release` dispatch | required release evidence | clean regeneration diff in both reference environments |
| First-level performance | weekly schedule or manual dispatch | required release evidence | raw HRF/AR/fit JMH reports plus a budgeted time/allocation receipt |

The workflow files configure these lanes, but a local checkout cannot prove
that GitHub branch protection marks a check as required or that an unpublished
workflow has run remotely. The final court therefore consumes captured GitHub
API payloads and verifies them offline against the exact candidate commit.

All third-party workflow actions are pinned to immutable commit SHAs. Ordinary
sbt builds resolve external Scala source dependencies from immutable Git
revisions in `build.sbt`; sibling checkouts are used only when a developer
supplies an explicit `-Dscalafim.<project>.build=...` override.

## Deliberately deferred gates

MiMa is not applicable while the project makes no stable binary-compatibility
promise. Enable it, record the first public artifact baseline, and define the
compatibility policy before the first compatibility-bearing release.

A scheduled, deterministic mutation pilot now seeds 22 AR-estimation,
PACF/Yule-Walker, censor/reset, whitening, missing-response grouping/identity,
weight-alignment/status, and fixed-effects variance defects. Run it with
`python3 tools/mutation/ar_na_pilot.py`; every seeded mutant must be killed and
compile-invalid mutants fail the pilot rather than counting as test evidence.
This focused JVM pilot complements, but does not replace, portable JVM and
Scala.js laws. Broader mutation testing of general OLS rank decisions and
design policy lowering remains a release decision before a
compatibility-bearing release.

## Performance and final release court

Performance evidence is a separate lane because JMH is JVM-only, machine
sensitive, and materially slower than the portable correctness court. Run:

```sh
bash tools/ci/first-level-benchmark.sh
```

The court distinguishes basis response, convolution, integration, AR
estimation, fit planning, reusable factorization, multiresponse fitting, and
chunk assembly.
It enforces both time and allocation budgets without changing scientific
tolerances. See [`benchmarks/first-level.md`](benchmarks/first-level.md) for the
workloads, required comparisons, current receipt, and interpretation policy.

The final release court combines the portable focused gate, JVM scientific
coverage, the full aggregate compile gate and bounded repository test batches,
executable documentation, and
a release-eligible performance receipt. It emits a machine-readable report and
fails closed when the source checkout or benchmark receipt is dirty, a required
gate was skipped, or the report cannot be tied to one commit and toolchain.
Remote workflow success, locked regeneration, and required branch protection
remain external evidence. The local finalizer accepts them only through hashed
payload captures tied to the canonical repository and candidate commit.

At startup the court writes one run manifest containing a fresh run ID, the
candidate commit and clean-source flag, provider and toolchain pins, the
scenario-manifest hash, reference-lock hashes, and a hash of the gate inventory.
Every gate receipt repeats that run ID, candidate commit, and input hash, and
records its exact logical command, exit status, timezone-qualified start and end
times, and log SHA-256. Finalization rejects stale or duplicate receipts,
receipts from another run or commit, changed inputs, missing or modified logs,
and any skipped or failed gate. Reports and logs belong under `target/` or an
external output directory so collecting evidence does not dirty the candidate.

Run it only from the candidate commit:

```sh
bash tools/ci/first-level-release.sh
```

The court writes logs, the release JMH receipt, and `report.json` under
`target/first-level-release/` by default. The run manifest and six structured
gate receipts are retained beside those logs so the report can be checked
offline. A supplied performance receipt is accepted only when its candidate
commit, benchmark workload hash, benchmark and policy source hashes, and raw
report hashes match the current candidate and available artifacts. The checked-in
[`release-report.json`](release-report.json) is a transparent historical
snapshot and is never release evidence for a later commit. The exact-candidate
report is generated and checked from the external evidence directory. Its
`release_eligible` field remains false whenever the snapshot was captured from
a dirty checkout, local gates were not run, the benchmark receipt is blocked,
or any external evidence record is less than `verified`.

The checked-in performance receipt is likewise a retained measured baseline.
Its file and workload hashes remain auditable, while release admission always
uses a fresh receipt whose source commit equals the exact candidate.

### External evidence bundles

Set `SCALAFIM_RELEASE_EVIDENCE_BUNDLE` to a
`scalafim-first-level-external-evidence/v1` JSON file, or pass the same file to
`finalize_first_level_release.py --external-evidence`. Each of its three
records contains `kind`, `repository`, `candidate_commit`, `observed_at`,
`source_url`, `payload_path`, and `payload_sha256`. Payload paths are relative
to the bundle and may not escape it.

The remote-CI record captures the run plus its jobs and must show successful
completion of `full-repository`, `focused-first-level`, and
`scientific-coverage`. The record maps those workflow job ids to the display
names returned by the jobs API; those captured names become the required
branch-protection contexts. The regeneration record applies the same checks to
both `r-receipts` and `python-receipts`, requires their evidence artifacts, and
binds every generated fixture and selected reference lock to the candidate.
The branch record binds a hashed protection snapshot for the inspected branch
and must contain every context resolved from the candidate CI run.

Collect workflow evidence only with explicit run IDs after both candidate runs
have reached a terminal state:

```sh
python3 -S tools/ci/collect_first_level_run_evidence.py \
  --candidate <full-commit-sha> \
  --ci-run-id <first-level-run-id> \
  --regeneration-run-id <release-regeneration-run-id> \
  --output-dir <external-evidence-directory>
```

The collector uses the repository-local GitHub wrapper, requests the exact
rerun attempt, and refuses to select a run by recency or display title. It
retains the raw run, job, and artifact API responses plus both downloaded
regeneration manifests. The offline validator checks every capture hash,
requires the `release` regeneration lane, and compares the manifests' package
locks and generated-file hashes with the candidate checkout.

Add the branch-policy snapshot to the same directory after the CI record has
resolved the exact check names:

```sh
python3 -S tools/ci/collect_first_level_branch_evidence.py \
  --candidate <full-commit-sha> \
  --branch main \
  --output-dir <external-evidence-directory>
```

This read-only collector captures the branch reference, classic protection,
and active repository rulesets. A classic `404` can be satisfied by an active
ruleset that applies to the branch. An absent policy, inaccessible API, stale
branch reference, or effective policy missing any candidate CI check remains
unverified or failed; the collector never changes GitHub settings.

External evidence states are fail-closed:

- `unverified`: absent, malformed, foreign, stale, hash-mismatched, or
  structurally incomplete evidence;
- `provided`: a legacy URL without a payload, or a valid run that has not yet
  completed;
- `failed`: a completed workflow or valid branch-policy snapshot that does not
  meet the release contract;
- `verified`: a hashed payload whose parsed facts satisfy the contract for the
  requested repository, branch, and commit.

The legacy `SCALAFIM_RELEASE_CI_URL`,
`SCALAFIM_RELEASE_RECEIPTS_URL`, and
`SCALAFIM_RELEASE_BRANCH_PROTECTION_URL` variables remain readable for old
automation, but a URL alone is only `provided` and cannot make a release
eligible. The validator performs no network calls and never treats a
record-level `status: verified` assertion as evidence.

Maintainers may set `SCALAFIM_RELEASE_ALLOW_DIRTY_DIAGNOSTICS=1` to collect all
local gate logs while developing. Such a report remains blocked by construction.
`SCALAFIM_RELEASE_BENCHMARK_RECEIPT` may point that diagnostic run at an already
validated full-profile receipt; the report records the actual validation
command. Neither option relaxes the clean-checkout rule for release eligibility.

Until that clean-checkout court and its external checks pass, the project
remains pre-release research software regardless of individual local green
tests.
