# response

`response` is the dependency-light boundary for decoded acquisition responses.
It owns response identities, time and sample domains, ordered axis-safe
selections, an owned row-major `Double` block, source planning, provenance, and
physical-read evidence.

The module intentionally does not define a generic tensor. Stored scalar
formats, compression, latent representations, archive mechanics, concrete
image or surface geometry, dataset hierarchy, and fitting semantics remain in
their owning modules.

Public block construction copies mutable input. Trusted code inside
`scalafim.response` may adopt an owned primitive buffer after the caller has
relinquished it. `ResponseBlock` never exposes its mutable storage and is not a
case class.

