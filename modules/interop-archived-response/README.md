# scalafim-interop-archived-response

Typed composition between response representations and archive containers.

The first vertical is temporal-DCT over interchangeable archive bindings:

- `TemporalDctLnaWritePlan` converts a checked materialized representation
  into ordered staging, payload, manifest, and publication steps;
- `TemporalDctLnaHdf5Writer` publishes through the existing atomic jHDF store
  and returns a typed write receipt;
- `ArchivedTemporalDctSource[F]` compiles and evaluates the representation's
  typed basis, loading, and offset read program;
- the LNA binding lowers those reads to typed LNA payload plans;
- `TemporalDctZarrSource` lowers the same unchanged requests to checked
  float64 Zarr arrays, with independent time- and sample-axis locality;
- `ArchivedResponseRegistry[F]` and `ArchiveDrivers[F]` are explicit,
  immutable, duplicate-rejecting application registries;
- `ScalafimRuntime[F].openResponse` selects an installed physical driver,
  validates structure and publication, projects a narrow representation
  envelope, resolves an installed family, and keeps both resources scoped;
- `ScalafimRuntime[F].openDataset` reuses that exact response-opening path,
  validates a pure `FmriDataset` against an `AcquisitionContext`, and scopes the
  resulting `OpenedDataset[F]` inside both the response-family and archive
  resources;
- archive-opening and attachment failures remain distinct cases of
  `RuntimeOpenError`; attachment reports the complete non-empty chain of
  timing, schema, geometry, mask, ordering, topology, and signal issues;
- families receive no full manifest: descriptor parsing uses the envelope and
  execution receives only a capability-limited payload/content-validation
  handle;
- runtime envelope projection reads the manifest's first-class persisted
  representation descriptor and output schema; it does not scrape
  representation metadata from untyped attributes;
- `TemporalDctLnaRepresentationFamily` reconstructs the typed model from its
  persisted descriptor; `LnaPipelineRepresentationFamily` delegates generic,
  single-run LNA files to the eager reconstruction
  pipeline;
- the source retains native archive receipts while returning a conforming
  response `ReadResult`;
- `DenseBoldZarrRepresentationFamily` admits the canonical dense-BOLD envelope,
  lowers ordered response selections to `[t,z,y,x]` points, applies primitive
  calibration at the response boundary, and returns exact dense blocks;
- Zarr receipt adaptation preserves exact chunks, shard ranges, bytes, cache
  hits, and axis-specific locality without inventing fallback evidence;
- the stored, length-framed model and response-schema descriptors cover exact
  domain identities, raw time coordinates, signal policy, DCT configuration,
  reconstruction contract, and decode-path consistency;
- logical float64 payloads round-trip by raw scalar bits.

The mathematical temporal-DCT implementation remains in `latent` and imports
no LNA, HDF5, or Zarr type. HDF5 resource and publication code is JVM-only;
logical bindings, descriptors, Zarr execution, registries, runtime, and
write-plan laws are shared with Scala.js. One shared law compiles a temporal
DCT selection once and runs it through memory, LNA, and portable Zarr under
the representation's declared decode consistency. A manifest selects only
among family values already installed by the application; it cannot load
code. The framework-neutral checks in `response-laws` are also reused here for
cross-backend consistency, receipt conformance, and provenance rather than
being restated as backend-specific assertions.

This artifact is also the physical owner of cross-domain LNA integration:

- LNA reconstruction and explicit-latent archive construction;
- archive-specific latent encoders and payload codecs for temporal, shared
  basis, radial/HRBF, transport, and BOLDZip representations;
- archive-to-response receipt adaptation;
- latent-response and LNA dataset backends;
- JVM LNA directory discovery and shared-basis resolution.

The package names reflect the domain values being integrated:
`scalafim.archive.lna`, `scalafim.latent`, and `scalafim.dataset`. These
sources compile only into the interop artifact, so `archive`, `latent`, and
`dataset` have no reverse dependency on this module. The frozen Phase 0
numerical regression corpus is compiled and run here on JVM and Scala.js.

Archive construction is representation-specific through
`ExplicitLatentArchiveCodec`, `SharedBasisLatentArchiveCodec`,
`RadialBasisArchiveCodec`, `TransportLatentArchiveCodec`, and
`BoldZipLatentArchiveCodec`. Reading uses an explicitly supplied,
immutable `LatentArchiveRegistry` assembled from independent binding values;
construction rejects invalid or duplicate binding ownership and opening
rejects ambiguous matches. There is no central codec facade or deprecated
forwarding alias.

The runtime owns effects, while the scientific dataset and downstream model
and fit plans remain pure:

```scala
runtime
  .openDataset(location, dataset, acquisition)
  .use: opened =>
    opened.read(DatasetRunQuery.All)
```

There is deliberately no synchronous adapter for this path. Existing
`DatasetBackend` integrations use the explicit
`SynchronousFmriDataset` capability.
