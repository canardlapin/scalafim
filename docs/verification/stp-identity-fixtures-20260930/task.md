Integration peer Fray #89 reproduced a LnaDatasetSuite failure after scoped unknown-world identity. LnaManifestCodec.parseRun stores and restores dimensions only, so opening an archive twice creates two independent unresolved frames. Its BIDS-like space label does not establish a precise world identity.

Preserve the path-resolution contract: both opens load the same dimensions, timepoints and selected data. Require raw exact congruence to fail without evidence, then require strict congruence after the caller explicitly admits both spaces into one declared world. Do not change production code, restore a global unknown frame, infer identity from filename/geometry, or weaken a scientific tolerance.

Verify all archivedResponseInterop JVM and JS tests. Reuse the already-passed full compile at local 0aa969a2 because production/build sources are unchanged. Respect the separately owned mvpa-spatial test; its requested migration is on Fray #94.
