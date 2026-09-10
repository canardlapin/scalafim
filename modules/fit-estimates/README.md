# fit-estimates

See [estimate sets](../../docs/estimate-sets.md) for module boundaries, executable examples, supported operations and remaining qualification.

`FitGroupAdapter.groupData` retains the eager in-memory `TContrastResult`
convenience formerly owned by `group.FirstLevel`. Its tests moved with it;
`group` no longer imports or depends on the first-level fitter. Durable consumers
use the group's pinned, bounded `EstimateGroup` API.
