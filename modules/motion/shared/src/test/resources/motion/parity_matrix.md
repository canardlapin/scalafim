# Motion Parity Fixture Matrix

Reference checkout: `~/code/volregger`

The generator for checked-in core fixtures is:

```sh
LC_ALL=C Rscript tools/motion/generate_volregger_fixtures.R \
  ~/code/volregger \
  modules/motion/shared/src/test/resources/motion/volregger_core.fixture
```

## Covered In `volregger_core.fixture`

| Family | Reference files | Scala test |
|---|---|---|
| Pose vector to 4x4 matrix | `R/transform_metrics.R` | `VolreggerParitySuite` |
| Inverse homogeneous transform | `R/transform_metrics.R` | `VolreggerParitySuite` |
| Radius-50 transform displacement | `R/transform_metrics.R` | `VolreggerParitySuite` |
| Framewise displacement | `R/fd_dvars.R` | `VolreggerParitySuite` |
| Raw DVARS | `R/fd_dvars.R` | `VolreggerParitySuite` |
| Robust DVARS clip rule | `R/fd_dvars.R` | `VolreggerParitySuite` |
| Masked translation displacement summary | `R/transform_metrics.R` | `VolreggerParitySuite` |
| Linear final resampling and pad modes | `R/apply_motion.R`, `src/api_apply.cpp` | `VolreggerParitySuite` |
| Estimator synthetic identity run | `R/estimate_motion.R`, `src/api_estimate.cpp` | `VolreggerParitySuite` |
| Estimator synthetic translation run | `R/estimate_motion.R`, `src/api_estimate.cpp`, `tests/testthat/test-volreg-basic.R` | `VolreggerParitySuite` |
| Resource payload parity | checked-in fixture | `VolreggerResourceSuite` |

## Pending Generator Expansion

These families require loading `volregger` native registration or later Scala
features before they can become executable shared parity tests.

| Family | Reference files | Target tracker slice |
|---|---|---|
| Spline and packet timing | `src/api_spline.cpp`, `tests/testthat/test-synthetic-ablation-modes.R` | `bd-01KWX6QE4NYEWX7CESR04WFYSB` |
| IC stencil and whitening scenarios | `tests/testthat/test-ic-efficacy.R`, `R/profiles.R` | `bd-01KWX6QSTFHZ99M93YR9V7JTGC` |
| CLI and report bundle metadata | `R/cli.R`, `R/reporting.R`, `tests/testthat/test-reporting-cli.R` | `bd-01KWX6RF9Z5K67HMPX0N3MG898` |
| Real-data differential benchmarks | `tests/testthat/test-rniftyreg-differential.R`, `tests/testthat/test-external-benchmark-guardrails.R` | `bd-01KWX6RR3PQYN1H48791VYD161` |
