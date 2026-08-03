# scalafim-archive-lna

Typed LNA schema and physical HDF5 support over `scalafim-archive`.

Shared code owns the versioned LNA 2 wire model:

- paths, run shapes, payloads, transform descriptors, and manifest codecs;
- quant and delta payload codecs;
- explicit-latent descriptor recognition and structural validation;
- shared-basis artifacts, masks, registries, and deterministic digests;
- typed whole-payload plans for portable logical bindings.

JVM code owns the jHDF stores, the eager LNA archive driver, and standalone
shared-basis persistence. The driver closes jHDF before returning its
`OpenArchive`, so it truthfully advertises whole-payload locality.

`LnaArchiveManifestAdapter` is the pure boundary from the container-specific
LNA manifest to the normalized archive manifest. It declares
`lna-hdf5@2`, `org.scalafim/fmri-response`, first-class representation
descriptors, and namespaced `org.scalafim.lna/*` payload roles. LNA
fixtures remain readable; the generic canonical writer accepts only the
normalized model. The jHDF writer is an explicitly LNA-specific profile
writer rather than the canonical manifest writer.

This artifact describes and transports LNA values but does not execute
scientific reconstruction. LNA pipeline reconstruction, representation-specific
encoding, shared-basis resolution, dataset discovery, and runtime family
assembly live in `interop-archived-response`.

Run it directly with:

```sh
sbt archiveLnaJVM/test
sbt archiveLnaJS/test
```
