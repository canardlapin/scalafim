# fit-estimates

See [estimate sets](../../docs/estimate-sets.md) for module boundaries, executable examples, supported operations and remaining qualification.

`FitGroupAdapter.groupData` retains the eager in-memory `TContrastResult`
convenience formerly owned by `group.FirstLevel`. Its tests moved with it;
`group` no longer imports or depends on the first-level fitter. Durable consumers
use the group's pinned, bounded `EstimateGroup` API.

The eager bridge now retains subject/contrast/sample-bound residual df and the
native engine, coefficient scope, response-preparation presence, and AR
whitening policy carried by `TContrastResult`. Manually constructed or legacy
results remain explicit unknowns. Voxel identity is used for reordering, but it
is not presented as cross-subject registration evidence.

`FitEstimateProducer` binds each published OLS standard-error product to its
effect and nominal residual df. Multi-run shared fits remain identified as
`JointRuns`; the receipt does not reinterpret nominal residual df as effective
df or claim calibration.
