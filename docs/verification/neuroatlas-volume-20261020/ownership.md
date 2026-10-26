# Volume atlas ownership

| Owner | Concrete responsibility | Remaining consumer |
| --- | --- | --- |
| neuroatlas4s-core | Atlas references/releases/labels/catalog; exact parcel identity; runtime-neutral Neuropublish v1 records, codecs and validation; published realization projection | Both volume and surface realizations |
| neuroatlas4s-volume | Exact provider-grid volume realizations; query/tie/radius policy; volume and series reductions; explicit overlap/alignment; adjacency; template frames and volume manifest; inspected MNI bridge admission | ScalaFIM atlas examples, atlas-to-MVPA workflows and native bridge oracle |
| neuroatlas4s-volume-io (JVM) | Schaefer, Glasser, Brainnetome and ASEG assets/loaders; local file digest/provenance; label admission; inspected composite file loading | Explicit local-path and cache/store entry points |
| neuroim4s / image4s / reframe4s | Provider grids, parcellations, scoped/native image reading, datatype/scaling/affine mechanics, coordinate bindings, interpolation and inverse qualification | Volume realization/loading and template policy |
| neuroim4s-routing / graph4s | Anatomical routing over actual provider frame owners; forward-first inverse fallback; generic ordered weighted route search | VolumeTransforms |
| ScalaFIM atlas (pending eco-neuroatlas-surface) | SurfaceAtlas / SurfaceAtlasRealizations, surface GIfTI loaders, concrete surface topology publication inputs, standard surface routes | Existing surface consumers and parity witnesses |
| ScalaFIM atlas mixed routes (pending surface/integration cutover) | TemplateDomains, SpaceTransformGraph, TransformPlan and mixed SpaceTransforms manifest; shared volume step declarations imported from upstream | Surface/mixed route descriptions and existing workflow contracts |

No migrated volume implementation or compatibility alias remains in ScalaFIM.
`Point3D` is the pre-existing alias of provider `SpatialPoint`, moved with its
actual atlas API. Volume and surface membership remain locus4s assignments;
there is no second parcel or support owner. The runtime-independent core has
no image, volume, surface, transform, IO or display dependency.

The surface builder supplies ordered topology primitives to the common
publication codec. The scalar spellings and byte preimages remain unchanged,
including every persisted `org.scalafim` / `scalafim:` identifier. Cache roots
for MNI lookup are explicit; the existing atlas download-cache location is
retained to preserve user data and cache behavior.

Shared hard-label validation helpers currently exported by `AtlasRealizations`
are used by both the volume builder and the remaining surface builder. Their
runtime-independent portions belong with core admission when
`eco-neuroatlas-surface` separates the volume-specific error cases; this is a
remaining intermediate coupling, not a final surface-core dependency design.
