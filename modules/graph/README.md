# scalafim-graph

Dependency-free, cross-built graph topology for ScalaFIM.

The module separates stable domain keys from graph-local dense coordinates.
`VertexBasis[K, V]` owns ordered keyed metadata, while `VertexIx` is valid only
as a coordinate in one basis. Version one graphs are immutable, simple,
loopless, and either directed or undirected. Edge identity is determined by
endpoints; input construction order is not observable.

The module owns topology, basis alignment, induced/reindexed graph values, and
their laws. It also provides payload-independent components, validated
nonnegative-cost shortest paths, directed cycle witnesses, deterministic
topological layers, and a checked `Dag` refinement. Numerical matrices,
Laplacians, spectra, connectome measurements, spatial-domain validation, and
pipeline execution remain in higher modules.

Run it directly with:

```sh
sbt graphJVM/test
sbt graphJS/test
```
