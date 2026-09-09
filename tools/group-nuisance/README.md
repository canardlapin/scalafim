# Nuisance inference qualification and group-review diagnostics

These tools compare two specified experimental scalar group tests. Both fail
calibration in retained settings and are **not admitted as general defaults**.
The production addition is `GroupContrastDiagnostics.review`, which supplies
subject-bound design information without p-values.

- `protocol.txt`: initial Mote protocol, lodged before the pilot.
- `calibrate.py`: fixed-design independent-error pilot and confirmation, with
  null, alternative and nonzero-null coverage evaluated on matched studies.
- `reference.R` / `reference.json`: independent clubSandwich 0.7.0 fixtures.
- `bootstrap-reference.R` / `.json`: R's fitted models and CR2 covariance over
  256 explicitly supplied sign actions, not a shared-seed parity claim.
- `verify.py`: R/NumPy/native-row parity, full coverage, finite-p checks and
  simultaneous calibration-failure regression checks. An expected rejection of
  inference admission is a passing scientific regression.
- `GroupContrastReviewProbe.scala`: native fixture export and review-only timing.
- `native-review-20-0.json`: retained native 20-subject fixture for the plots.
- `plot.py`, `qualification.png`, `visual-review.json`: inspected scientific
  figure. This is not a screenshot or independent UX approval of the app.

## Reproduce the scientific comparison

Use a scratch copy so results do not overwrite the retained qualification:

```sh
cp -R tools/group-nuisance /private/tmp/group-nuisance-reproduction
cd /private/tmp/group-nuisance-reproduction
python3 -m venv .venv
.venv/bin/pip install -r requirements.txt
.venv/bin/python calibrate.py
.venv/bin/python calibrate.py --selected confirmation-cells.json --trials 20000 --draws 1999 --root 2026091401 --action-root 2026091402 --out confirmation.jsonl
.venv/bin/python verify.py
.venv/bin/python plot.py
```

The initial run has 108 cells × 2,000 studies × 499 draws. Confirmation has
8 cells × 20,000 fresh studies × 1,999 draws. Every study draws its own actions;
plans are not shared across studies. `SeedSequence([root, cellIndex])` separates
outcome and action streams. Repeated coverage checks use the same actions and
add no RNG consumption. Fresh root values are required for an additional
confirmation after method changes; this retained grid alone cannot qualify them.

R is optional for routine fixture checks; to regenerate the independent oracle:

```sh
mkdir -p rlib
Rscript --vanilla -e 'install.packages("clubSandwich", lib="rlib", repos="https://cloud.r-project.org")'
GROUP_NUISANCE_RLIB="$PWD/rlib" Rscript --vanilla reference.R .
GROUP_NUISANCE_RLIB="$PWD/rlib" Rscript --vanilla bootstrap-reference.R .
.venv/bin/python verify.py
```

R also needs `jsonlite`; install it in that library if absent. The qualification
used R 4.5.1, clubSandwich 0.7.0 and jsonlite 2.0.0. Package build-version warnings
are retained in the receipt. The independent formulas agree to numerical
precision; that agreement does not repair false-positive inflation.

## Native regression and review timing

From the ScalaFIM root, using its qualified provider dependencies:

```sh
sbt 'all groupJVM/test groupJS/test fmriWorkflowJVM/test fmriWorkflowJS/test'
sbt 'set groupJVM / Test / unmanagedSourceDirectories += file("tools/group-nuisance")' 'groupJVM/Test/runMain scalafim.fmri.group.GroupContrastReviewProbe 20'
```

The probe emits one native review JSON object and a timing object. The recorded
benchmark used standalone JVM processes to separate stdout/stderr and avoid
sbt startup. Exact commands, classpaths, nine fork outputs, source equality,
host data and test logs are in the compressed qualification receipt. This is
review performance (one design and contrast), not imagewide GLM execution.

The report is `docs/verification/group-nuisance-2026-09-08.md`. Its receipt retains
all pilot/confirmation records, original two-outcome replay comparisons,
source snapshots, rejected and corrected plots, and scientific limitations.
