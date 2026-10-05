# Atlas workflows

The outer `atlas-workflows` module depends on `atlas` and `connectivity`. Atlas
core has no dependency on connectivity estimation or scheduling.

Both platforms are included in the root aggregate and standard compile/test
aliases in `build.sbt`.

`ParcelWorkflows.batch(realization)(namedFields, ...)` applies the existing
`AtlasReduce` reducer and missing/empty policies. Names must be non-empty and
unique; spatial fields and masks must carry the exact realization owner.

`ParcelWorkflows.connectivity(realization)(sampleFields, timeAxis)` takes one
parcel field per timepoint. It checks the live parcel owner, uses canonical
parcel order, and retains the explicit timing axis. Connectivity node IDs encode
the complete persistent parcel key, independent of display order and local
integer labels. The returned input retains the source realization identity and
can call the existing weighted correlation estimator.

`AtlasTransport.prepare(atlas, targetGrid, targetSpace, route)` supports fused
affine nearest-label transport through the existing reframe4s categorical
kernel. The route runs **target to source**, and actual grid worlds must match
its endpoints. Planned, mapless, nonlinear and wrong-world routes are refused.
`execute()` returns the transported atlas plus a completed receipt identifying
the original assignment, result assignment, pull direction and dropped labels.
Background is zero. No successful receipt is created while merely planning.

The shared workflow suite provides an end-to-end extraction/correlation example
and a directed transport example. Run both platforms:

```sh
python3 tools/build/sbt-warm atlasWorkflowsJVM/test atlasWorkflowsJS/test
```
