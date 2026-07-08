# scalafim-latent

Typed latent-response core for ScalaFIM.

This module is the Scala home for the useful computational contract behind
`fmrilatent`: a response can be represented as coefficient time series plus a
decoder into samples, rather than as an eagerly materialized dense run.

The current shared slice is intentionally small:

- checked domain identifiers
- typed shape counts, metadata wrappers, and validated time/sample selections
- `LatentResponse` as the shared contract
- `ExplicitLatentResponse` for `basis * loadings' + offset` representations
- `DctSpec`, `RidgePenalty`, and `DctBasis` for fmrilatent-compatible DCT-II
  temporal bases
- `HaarSpec` and `HaarBasis` for power-of-two, orthonormal temporal Haar
  bases in scaling/coarse-to-fine wavelet order
- `TemporalBasisEncoder` for provided bases, temporal DCT encodings, and
  temporal Haar encodings
- `LatentArchiveCodec.toTemporalDctArchive` for first-class LNA temporal DCT
  archives backed by the shared encoder
- `SharedBasisEncoder` and `LatentArchiveCodec.toSharedBasisArchive` for
  Gram-solved shared spatial dictionaries persisted as LNA shared-basis archives
- `SharedBasisLatentArchive.materialize` and `sampleMask` for resolving
  shared-basis archive coefficients against an external basis artifact
- `LatentEncodingSpec` and `LatentEncoder` as the typed factory layer over
  provided temporal bases, temporal DCT, temporal Haar, and shared spatial
  dictionaries
- `TransportLatentResponse` for operator-backed coefficient handoff with typed
  analysis/raw coefficient blocks and native-only/template-capable decoders
- `LatentArchiveCodec.toTransportArchive` for LNA transport latent archives
  backed by dense operator payloads
- `BoldZipPayload` for decoder-only BOLDZip-SR carrier, texture, residual
  event, finite amplitude, checked lag, and explicit coarse/detail
  spatial-basis variants
- `LatentArchiveCodec.toBoldZipArchive` for LNA BOLDZip-SR archives backed by
  temporal, carrier, texture, residual-event, and spatial-basis payloads
- `LatentArchiveCodec.fromArchive` for tagged explicit, temporal-DCT,
  shared-basis, transport, and BOLDZip archive decode variants

Prefer the typed boundary helpers in new code: `LatentShape.checked`,
`TimepointIndex`, `SampleIndex`, `TypedLatentSelection.checked`,
`LatentMetadata`, `BoldZipAmplitude`, and the checked BOLDZip entry/event
constructors. They preserve the older plain-`Int`/`Double` archive wire format
while making invalid state explicit at construction sites.

Later slices should add richer dataset adapters, fmrilatent parity fixtures for
the typed encoder factory, first-class LNA descriptors for Haar archives if the
schema needs to distinguish them from explicit latent responses, and BOLDZip
encoder variants.

The DCT fixture constants in `DctBasisSuite` can be regenerated with
`tools/r-parity/fmrilatent-dct-fixtures.R`.
