# MVPA artifacts

`mvpa-artifacts` persists the bounded M3.11 `PatternArtifact` profile without a
fitter dependency. Version 1 supports categorical and continuous target geometry,
both centering policies, covariance declarations, provenance, diagnostics, and the
always-explicit `ExperimentalFitOnly` interpretation.

The JVM adapter writes each Float64 matrix as a separate immutable, little-endian
object through `LocalObjectStore`. JSON metadata carries only axes, policy, complete
scientific declaration, and hash-pinned object references; it is published after
all leaves. Readers check metadata and every payload digest and byte length before
allocating a matrix. The caller supplies a training `AxisRef`, which must match the
persisted training binding. Checkpoint resumption and unsupported profile variants
return an explicit `Unsupported` result.
