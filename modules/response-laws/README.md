# scalafim-response-laws

Reusable, cross-platform law checks for storage-neutral response sources and
blocks. This is a test-support artifact, not a runtime execution layer.

The artifact provides structured checks for:

- requested ordering and shape;
- selected-versus-whole decode consistency;
- ordered partition assembly;
- exact raw-bit persistence;
- axis-keyed receipt conformance;
- preservation of every parent provenance node and root.

Checks return typed `ResponseLawResult` values instead of throwing test
framework assertions. Representation, archive-interoperability, and dataset
suites can therefore reuse the same laws on the JVM and Scala.js while
retaining their own fixture and effect setup.

Run it directly with:

```sh
sbt responseLawsJVM/test
sbt responseLawsJS/test
```
