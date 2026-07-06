# scalafim-latent

Typed latent-response core for ScalaFIM.

This module is the Scala home for the useful computational contract behind
`fmrilatent`: a response can be represented as coefficient time series plus a
decoder into samples, rather than as an eagerly materialized dense run.

The current shared slice is intentionally small:

- checked domain identifiers
- validated time/sample selections
- `LatentResponse` as the shared contract
- `ExplicitLatentResponse` for `basis * loadings' + offset` representations
- `DctBasis` for fmrilatent-compatible DCT-II temporal bases
- `TemporalBasisEncoder` for provided bases and temporal DCT encodings
- `LatentArchiveCodec.toTemporalDctArchive` for first-class LNA temporal DCT
  archives backed by the shared encoder
- `SharedBasisEncoder` and `LatentArchiveCodec.toSharedBasisArchive` for
  Gram-solved shared spatial dictionaries persisted as LNA shared-basis archives
- `TransportLatentResponse` for operator-backed coefficient handoff with
  analysis/raw transforms and native/template decoder projections
- `BoldZipPayload` for decoder-only BOLDZip-SR carrier, texture, residual
  event, and spatial-basis payloads

Later slices should add encoder factories, richer dataset adapters, shared-basis
archive materialization helpers, and transport archive persistence.

The DCT fixture constants in `DctBasisSuite` can be regenerated with
`tools/r-parity/fmrilatent-dct-fixtures.R`.
