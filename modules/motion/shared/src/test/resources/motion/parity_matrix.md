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
| Spline smoother and packet offsets | `src/api_spline.cpp`, `tests/testthat/test-synthetic-ablation-modes.R` | `VolreggerParitySuite` |
| IC, whitening, spline, and parallel profile contracts | `R/control.R`, `R/profiles.R`, `tests/testthat/test-ic-efficacy.R` | `VolreggerParitySuite` |
| Shared IC stencil and frame-mean whitening behavior | `R/control.R`, `tests/testthat/test-ic-efficacy.R` | `MotionControlSuite`, `MotionEstimatorSuite` |
| CLI run/report artifact metadata | `R/cli.R`, `R/reporting.R`, `tests/testthat/test-reporting-cli.R` | `VolreggerParitySuite` |
| Real-data benchmark gate metadata | `tests/testthat/test-external-benchmark-guardrails.R` | `VolreggerParitySuite` |
| Resource payload parity | checked-in fixture | `VolreggerResourceSuite` |

## Pending Generator Expansion

The checked-in fixture now records the remaining family contracts. These later
slices still need feature-specific Scala implementations or opt-in JVM
benchmark inputs before they can become executable semantic parity tests.

| Family | Reference files | Target tracker slice |
|---|---|---|
| Packet-aware application semantics | `src/api_apply.cpp`, `src/api_spline.cpp`, `tests/testthat/test-synthetic-ablation-modes.R` | `bd-01KWX6QE4NYEWX7CESR04WFYSB` |
| Full template-mode IC whitening numeric recovery | `tests/testthat/test-ic-efficacy.R`, `R/profiles.R` | `bd-01KWX6RR3PQYN1H48791VYD161` |
| JVM CLI/report roundtrip outputs | `R/cli.R`, `R/reporting.R`, `tests/testthat/test-reporting-cli.R` | `bd-01KWX6RF9Z5K67HMPX0N3MG898` |
| Real-data differential benchmark data | `tests/testthat/test-rniftyreg-differential.R`, `tests/testthat/test-external-benchmark-guardrails.R` | `bd-01KWX6RR3PQYN1H48791VYD161` |
