# Voxelwise FGLS through FitPlan

Mote `bd-01M4K6KGB95E8Z5MYNWMMSMF2X` integrates the existing voxelwise
estimator into the public plan route. `ArOptions.fgls` explicitly declares
the coefficient scope and covariance uncertainty; `ArCensorTreatment`
declares continuity through censored observations. Portable model documents
use version 2 for this policy, and response-preparation identities include
the policy in `ar_options/v4`. Legacy plans continue to emit version 1.

The result is `VoxelwiseArFmriFitResult`, with contrast-specific KR scale and
denominator degrees of freedom available through each voxel's `test` method.
The dense classical-inference adapter refuses this result. Whole-selection
and chunked execution preserve the policy, preparation provenance, axes,
and excluded voxels.

Unsupported strategies, omitted rows, incomplete time axes, and nuisance
projection are refused. Continuous-missing covariance requires stationary
initialization; restart covariance requires the legacy exact-AR(1)/truncated
initialization. These policies cannot silently substitute for one another.

## Validation

- Full model suites: 100 tests on each platform before the final policy
  refinement; the 10-test codec suite passes on both platforms afterward.
- Full fit suites: 758 JVM and 700 Scala.js tests before the final policy
  refinement; the five-test FitPlan suite passes on both platforms afterward.
- Golden codec, downgrade refusal, identity separation, standalone/plan and
  whole/chunk agreement, exclusions, and unsupported-policy refusal are covered.
- Gale is pinned to `6f7a1f9509a07c2b3f5b956332d39a1bc5b64c3c`; the study's
  private F-tail implementation is replaced by `gale.numeric.FDistribution`.

The three-test study harness suite and its formatting gate pass on JVM and
Scala.js after the replay adapter correction. The initial pilot in `pilot/`
is retained as failed engineering evidence: the adapter had inadvertently
added two constant baseline columns to an already complete design. The
corrected adapter explicitly supplies a zero-column baseline; a regression
compares its column count, coefficients, AR estimates, and covariance to the
standalone route. No production estimator or scientific margin changed.

The corrected pilot in `pilot-corrected/` passes on both platforms: 80
records each, no refusals, unchanged committed source, identical decisions,
and maximum scaled numerical difference 1.54e-12.

## Full replay result

The replay passes on JVM and Scala.js: 2,048 datasets per cell, five cells,
and two engines produce **20,480 records on each platform**, with no refusals
or missing/duplicate slots. Source was committed at
`a5b865e5c217e5da003b3e8ea63cc2c6e267ca90` and remained unchanged throughout.
Both independent R analyses pass every original criterion in all four
admission cells. The run-varying scope boundary still fails AR RMSE
(0.1348 versus 0.10) and whiteness (0.1075 versus 0.10).

[`replay-comparison.json`](replay-comparison.json) audits every retained trial
against the original standalone run. All decisions and per-cell criteria
match exactly. Maximum scaled numerical differences are 3.12e-11 against
the original and 3.50e-12 between platforms, below the 1e-9 comparison
tolerance. Both platforms retain the same boundary failure.

These are already observed confirmation streams replayed through FitPlan:
they establish integration equivalence, not fresh scientific confirmation.
Qualification remains restricted to the original AR(2), continuous-missing,
across-run coefficient pooling, KR policy and its four admitted cells.

Each phase retains its source hashes, execution receipt, independent
analysis, and gzip-compressed original records/logs. `archives.json` binds
compressed and uncompressed hashes. Reproduce the final audit with:

```sh
python3 docs/verification/fgls-fitplan-20261011/compare_replay.py --original docs/verification/fgls-voxelwise-20261010/confirmation --replay docs/verification/fgls-fitplan-20261011/replay --protocol tools/scenarios/fgls-voxelwise/protocol.json --output /tmp/fgls-replay-comparison.json
```
