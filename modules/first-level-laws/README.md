# First-level generated laws

`first-level-laws` is the non-published property court for ScalaFIM's public
first-level stack. It depends on `hrf-laws` and `fit`, so generated examples can
cross the real HRF, design, model, and fit boundaries without adding a testing
dependency to any production artifact.

The ordinary JVM and Scala.js suites use a deterministic, bounded PR profile.
Set `SCALAFIM_LAW_PROFILE=calibration` to exercise larger examples and more
successful trials. Set `SCALAFIM_LAW_SEED` to a ScalaCheck seed printed by a
failure to replay the same generated case.

The corrected-GLS simulator checks analytic noise covariance and noiseless public
OLS recovery in ordinary tests. Its six-cell statistical qualification campaign
is explicit: use `SCALAFIM_GLS_STUDY_PROFILE=pilot|screen|confirmation`, or run
`tools/scenarios/corrected-gls/run_study.py --profile confirmation --output <dir>`
from the repository root. The runner records both JVM and Scala.js even when a
scientific gate fails and restarts an idle sbt server to bind the requested
environment; a resident server retains its startup environment. Set
`SCALAFIM_GLS_STUDY_LOG` to a file prefix for separate
`-jvm.jsonl` / `-js.jsonl` replicate records. Seed overrides use
`SCALAFIM_GLS_STUDY_SEED` or `SCALAFIM_LAW_SEED_LONG`.

The [frozen 2026-10-08 campaign](../../docs/plans/corrected-gls-statistical-qualification.md)
has an overall negative result: the censored, high-nuisance, voxelwise AR(2) cell
fails recovery RMSE, joint-F rejection and interval coverage. The explicit
qualification gate retains that failure. A passing routine test suite or pilot
does not confer statistical admission.

Generators construct domain values through their public validated APIs.
Shrinkers rebuild smaller valid values rather than deleting fields or emitting
scientifically impossible schedules. Numerical comparisons derive their bound
from the operation count, observed scale, machine epsilon, and condition
evidence attached to each property.
