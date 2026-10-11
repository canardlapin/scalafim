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

The full integration replay is pending. These
replay the already observed confirmation streams through FitPlan: they are
integration evidence, not fresh scientific confirmation. The original four
admission cells and the run-varying failure boundary remain unchanged.
