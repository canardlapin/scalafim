# scalafim-fmri-ar

Typed AR/ARMA whitening primitives for fMRI GLM workflows.

This module is the ScalaFIM landing zone for the computational core of
`fmriAR`: immutable whitening plans, run/censor-aware segment construction, and
pure design/data whitening. Estimation, diagnostics, and GLS integration build
on this layer in later slices.
