# scalafim-graph-linalg

Basis-carrying numerical operators for `scalafim-graph`.

This cross-built module depends only on `graph` and `linalg`. It projects
explicit topology and interpreted edge payloads into the existing
`SparseTriplets`, `CsrMatrix`, and `LinearMap` contracts. Public adjacency,
incidence, degree/strength, and Laplacian results retain their vertex or edge
bases so row and column meaning is never detached from numerical storage.

Topology adjacency is distinct from weighted adjacency, and degree is distinct
from strength. Normalized Laplacians require an explicit zero-strength policy.
The module does not own connectivity measurement semantics or eigensolver
implementations.

`VertexSpectrum` and `SpectralEmbedding` wrap linalg's operator-oriented partial
eigensolver results with the original `VertexBasis`. Spectral methods accept
only nonnegative adjacency evidence and symmetric combinatorial or normalized
Laplacians; random-walk Laplacians are not admitted to this symmetric solver
path. Every result also carries `WeightedSupport`: strictly-positive support
components, explicit zero-weight edge count, and zero-strength vertices.

Embedding nullspace removal is policy-aware. Combinatorial and normalized
zero-row operators drop one eigenvector per positive-support component;
identity-on-zero-strength normalization does not treat isolated identity rows as
zero eigenvectors. Reindexing tests compare repeated eigenspaces through
pairwise embedding geometry rather than byte-identical eigenvectors.

Graph comparison is feature-first. `SpectralDiagonalFeature` produces aligned
vertex-by-time heat-kernel or diffusion-energy summaries from a
`VertexSpectrum`. `VertexFeatureSet` requires identical ordered keys for direct
comparison and offers explicit key-based row alignment; connectivity callers
must still establish atlas/provenance compatibility before using that numerical
alignment. Linear and RBF feature similarities are tagged `KnownPsd`; negative
Euclidean similarity is retained as a similarity with `NotEstablished` PSD
status and is not presented as a kernel.

There is no production dependency from graph-linalg to multivar. Shared tests
stack known-PSD feature vectors and prove exact agreement with
`multivar.Kernel.linear`, demonstrating the adapter path without merging the
two abstractions or making every graph similarity a kernel.

Run it directly with:

```sh
sbt graphLinalgJVM/test
sbt graphLinalgJS/test
```
