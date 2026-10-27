# scalafim-fmri-mvpa-spatial

Functional threshold policy and fMRI scenario checks over the portable MVPA providers.

Generic adapters have standalone owners:

```scala
import mvpa4s.locus.*
import mvpa4s.surface.SurfaceSupportGraphs
import mvpa4s.atlas.AtlasMeasurementFrames
import scalafim.fmri.mvpa.spatial.FamilyThreshold
```

`mvpa4s-locus` owns region/selection frames, centered volume and surface neighborhoods,
scatter and exact integer-grid support graphs. `SpatialMeasurementFrames.surfacePatches`
preserves ambient center identities. `mvpa4s-surface` owns anatomical mesh adjacency.
`mvpa4s-atlas` adapts atlas assignment fibers through an explicit certified sample
bijection and retains display order separately from ID-sorted scientific traversal.
These optional adapters have no ScalaFIM dependency.

Only `FamilyThreshold` remains here as production policy. Its candidate omnibus
bridge preserves the existing refusal of unqualified strong-FWER/FDR admission.
The module's scenario tests compose native providers and frozen PyMVPA fixtures;
surface and locus dependencies are test-scoped.

Phase D retains this reduced integration boundary: combining MVPA evidence
with threshold policy here keeps MVPA out of the general `threshold` module and
keeps thresholding out of portable MVPA core. No generic spatial adapter or
second spatial authority remains here. Candidate admission refusals stay intact.

The executable [atlas workflow](../../examples/workflows/shared/src/main/scala/scalafim/examples/workflows/AtlasMvpaWorkflow.scala)
uses the public atlas adapter and exact realized parcel keys, and is tested on both platforms.

```sh
python3 tools/build/sbt-warm mvpaSpatialJVM/test mvpaSpatialJS/test
python3 tools/build/sbt-warm workflowExamplesJVM/test workflowExamplesJS/test
```
