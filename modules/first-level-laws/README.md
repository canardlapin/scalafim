# First-level generated laws

`first-level-laws` is the non-published property court for ScalaFIM's public
first-level stack. It depends on `hrf-laws` and `fit`, so generated examples can
cross the real HRF, design, model, and fit boundaries without adding a testing
dependency to any production artifact.

The ordinary JVM and Scala.js suites use a deterministic, bounded PR profile.
Set `SCALAFIM_LAW_PROFILE=calibration` to exercise larger examples and more
successful trials. Set `SCALAFIM_LAW_SEED` to a ScalaCheck seed printed by a
failure to replay the same generated case.

Generators construct domain values through their public validated APIs.
Shrinkers rebuild smaller valid values rather than deleting fields or emitting
scientifically impossible schedules. Numerical comparisons derive their bound
from the operation count, observed scale, machine epsilon, and condition
evidence attached to each property.
