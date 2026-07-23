# scalafim-frame

An immutable, typed local dataframe plan for ScalaFIM, cross-built for the JVM
and Scala.js.

`Frame[Schema]` is a pure logical program whose schema is a Scala 3 named tuple.
`Expr[A]` is a typed column expression, while `DynamicFrame` is the explicit
runtime-schema boundary. Query construction does not perform IO or materialize
data. The dependency-free core owns schema binding, expressions, plans,
normalization, the semantic reference interpreter, and immutable
Arrow-compatible `RecordBatch`/`Table[Schema]` storage. Primitive arrays use
little-endian buffers and LSB-first validity bitmaps; explicit owned/borrowed
leases keep retained slices valid until deterministic close. Effectful streaming
and resource ownership belong in a separate `scalafim-frame-fs2` adapter.

The module deliberately does not implement a production columnar engine.
Arrow-compatible storage and external-engine adapters remain replaceable
execution boundaries.

The extraction and ecosystem-readiness contract is documented in
[`../../docs/frame/ecosystem-readiness.md`](../../docs/frame/ecosystem-readiness.md).

Run it directly with:

```sh
sbt frameJVM/test
sbt frameJS/test
```
