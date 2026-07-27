# scalafim-archive-lna

Typed LNA schema and physical HDF5 support over `scalafim-archive`.

Shared code owns the legacy LNA wire model:

- paths, run shapes, payloads, transform descriptors, and manifest codecs;
- quant and delta payload codecs;
- explicit-latent descriptor recognition and structural validation;
- shared-basis artifacts, masks, registries, and deterministic digests;
- typed whole-payload plans for portable logical bindings.

JVM code owns the jHDF stores, the eager LNA archive driver, and standalone
shared-basis persistence. The driver closes jHDF before returning its
`OpenArchive`, so it truthfully advertises whole-payload locality.

`LegacyLnaManifestTranslator` is the pure compatibility boundary from the
existing LNA wire manifest to the normalized archive manifest. It declares
`lna-hdf5@2`, `org.scalafim/fmri-response`, first-class representation
descriptors, and namespaced `org.scalafim.lna/*` payload roles. Existing LNA
fixtures remain readable; the generic canonical writer accepts only the
normalized model. The existing jHDF writer remains an explicitly LNA-specific
compatibility writer rather than the canonical manifest writer.

This artifact describes and transports LNA values but does not execute
scientific reconstruction. Legacy reconstruction, representation-specific
encoding, shared-basis resolution, dataset discovery, and runtime family
assembly live in `interop-archived-response`.

Run it directly with:

```sh
sbt archiveLnaJVM/test
sbt archiveLnaJS/test
```
