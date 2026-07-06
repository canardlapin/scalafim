# ScalaFIM Vision

ScalaFIM is a Scala 3 system for functional imaging computation. Its purpose is
to turn the fMRI analysis stack into a coherent, typed, modular engine rather
than a translation of legacy R APIs.

The source R packages are essential references, not straitjackets:
`~/code/neuroim2`, `~/code/fmrihrf`, `~/code/fmridesign`, `~/code/fmrireg`, and
`~/code/fmridataset` define much of the statistical behavior, terminology, and
test corpus ScalaFIM should respect. They should guide parity fixtures and
domain semantics, but they should not force ScalaFIM to inherit R's S3 surfaces,
list-shaped configuration, or incidental implementation boundaries.

The first goal is a beautiful computational core: HRFs, design matrices, image
data, datasets, models, fitting, and inference should compose through small
algebraic types with explicit invariants. A model should be inspectable before
it runs, a fit should return typed statistical results, and failure modes should
be represented directly rather than hidden in ad hoc option lists.

Every core module should cross-compile to the JVM and Scala.js unless there is a
clear platform boundary. Shared numerical code should avoid JVM-only
dependencies such as Breeze in hot paths. Performance-critical code should use
primitive arrays and careful allocation discipline where that matters, while
keeping public APIs idiomatic and readable.

The near-term target is a working `fmrireg`-class engine: dataset plus HRF plus
design plus image data into OLS, runwise OLS, contrasts, diagnostics, and result
containers. Advanced engines such as GLS/AR, robust fitting, nuisance
projection, low-rank methods, and latent/sketch paths should be added only after
the dense path is tested and coherent.

Distributed analysis is an endgame, not the foundation. ScalaFIM should be
embeddable later in systems such as Spark, but Spark should arrive as a JVM-only
orchestration adapter over the same computational core, not as a dependency of
the core itself.

The standard is not a bare port. A feature belongs in ScalaFIM when it has a
clear Scala 3 API, executable JVM and Scala.js tests where applicable, numerical
fixtures for behavior that must match the R ecosystem, and a module boundary
that will still make sense as the system grows.
