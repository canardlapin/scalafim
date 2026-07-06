# scalafim-archive

Cross-compiled JVM/Scala.js archive contract module for `scalafim`.

Package roots:

```scala
import scalafim.archive.*
import scalafim.archive.lna.*
```

This module captures the core Latent NeuroArchive idea from `neuroarchive` as
typed Scala 3 values: archive manifests, transform descriptors, payload
references, validation layers, and executable transform plans. It is not an
R6/S3 registry port.

Shared code contains the pure contract and portable transforms. Platform IO
adapters, including HDF5, live under JVM sources.

Supported shared transform constructs now include:

- `quant`: global or per-voxel range quantization with typed scale/offset
  payloads
- `delta`: first-order time-axis deltas with verbatim first-value references
- `basis` plus `embed`: explicit basis storage and coefficient projection
- external shared-basis `embed`: coefficient-only archives that reference a
  content-addressed shared basis artifact by alias/checksum, with locator and
  registry-backed JVM resolution
- `temporal` DCT descriptors for explicit latent responses with persisted
  temporal bases, loadings, optional sample offsets, and typed DCT params
- shared basis artifacts: content-addressed dense loadings plus masks,
  deterministic SHA-256 checksums, and alias registries for reusable group
  bases
- composed `delta -> quant` archives reconstructed by walking typed transform
  descriptors in reverse order

The current delta core is intentionally lossless and simple: no feature-axis
deltas, run-length coding, entropy coding, or external chunk/filter policy is
implied by the descriptor yet.

The JVM archive module includes a first jHDF-backed `LnaHdf5Store` for basic
LNA files and a `JhdfSharedBasisStore` for standalone `.lna_basis.h5` basis
artifacts. `LnaSharedBasisResolver` materializes coefficient-only shared-basis
archives by resolving the basis from an archive-relative locator, a
`bases/registry.json` alias, or a direct content-addressed filename lookup. The
archive store writes:

- root metadata attributes
- `/__lna__/manifest_json`
- `/__lna__/transforms/<index>/descriptor_json`
- `/__lna__/payloads/...` datasets mirroring archive payload paths

The backend is intentionally restricted to the current `Payload` variants. It
roundtrips ScalaFIM archives, but jHDF currently writes integer arrays as signed
fixed-point datasets. ScalaFIM preserves `UInt8`/`UInt16` semantics from the LNA
manifest on read, while exact unsigned HDF5 datatypes remain a likely jHDF
fork/upstream target if external tools require them.

Run it directly with:

```sh
sbt archiveJVM/test
sbt archiveJS/test
```
