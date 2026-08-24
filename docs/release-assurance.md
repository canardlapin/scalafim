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
material loss of exercised decisions.

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
workflow has run remotely. Verify both facts in the canonical repository before
release.

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
Remote workflow success and required branch protection remain owner-verified
external evidence; local scripts cannot manufacture either fact.

Run it only from the candidate commit:

```sh
bash tools/ci/first-level-release.sh
```

The court writes logs, the release JMH receipt, and `report.json` under
`target/first-level-release/` by default. The checked-in
[`release-report.json`](release-report.json) is the current transparent
snapshot. Its `release_eligible` field remains false whenever the snapshot was
captured from a dirty checkout, local gates were not run, the benchmark receipt
is blocked, or URLs for remote first-level CI, locked reference regeneration,
and branch-protection verification were not supplied.

Maintainers may set `SCALAFIM_RELEASE_ALLOW_DIRTY_DIAGNOSTICS=1` to collect all
local gate logs while developing. Such a report remains blocked by construction.
`SCALAFIM_RELEASE_BENCHMARK_RECEIPT` may point that diagnostic run at an already
validated full-profile receipt; the report records the actual validation
command. Neither option relaxes the clean-checkout rule for release eligibility.

Until that clean-checkout court and its external checks pass, the project
remains pre-release research software regardless of individual local green
tests.
