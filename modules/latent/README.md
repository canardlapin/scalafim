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
- `RadialKernel`, checked radial scalar/coordinate types, and `RadialBasis`
  for pure shared-code radial/HRBF atom loadings shaped as active voxels by
  atoms, including `NeuroSpace` plus checked active-index coordinate derivation
- `RadialMaskOrder` for explicit active-row to full-grid/mask-order semantics
- `RadialBasisSpec` for deterministic ScalaFIM-native atom generation,
  including hierarchical sigma levels, radius-factor Poisson-style spacing,
  extra fine levels, normalized atoms, and an identity-like tiny-mask policy
- `RadialDecodeSelection` and `RadialVoxelSelection` for partial radial
  coefficient decode by typed timepoints plus active or full-grid voxel indices
- `RadialBasis.toSharedBasisArtifact` and `RadialBasisEncoder` for emitting
  `kind = hrbf` shared-basis artifacts and delegating radial projection through
  the shared-basis Gram solver
- `LatentEncodingSpec` and `LatentEncoder` as the typed factory layer over
  provided temporal bases, temporal DCT, temporal Haar, shared spatial
  dictionaries, and radial/HRBF spatial dictionaries
- `LatentArchiveCodec.toRadialBasisArchive` for emitting radial/HRBF archives
  through the standard LNA shared-basis descriptor
- `TransportLatentResponse` for operator-backed coefficient handoff with typed
  analysis/raw coefficient blocks and native-only/template-capable decoders
- `LatentArchiveCodec.toTransportArchive` for LNA transport latent archives
  backed by dense operator payloads
- `BoldZipPayload` for BOLDZip-SR carrier, texture, residual event, finite
  amplitude, checked lag, and explicit coarse/detail spatial-basis variants
- `BoldZipEncoder` for the exact identity-detail BOLDZip baseline encoder with
  optional sample offsets and reconstruction-quality metrics
- `LatentArchiveCodec.toBoldZipArchive` for LNA BOLDZip-SR archives backed by
  temporal, carrier, texture, residual-event, and spatial-basis payloads
- `LatentArchiveCodec.fromArchive` for tagged explicit, temporal-DCT,
  temporal-Haar, shared-basis, transport, and BOLDZip archive decode variants

Prefer the typed boundary helpers in new code: `LatentShape.checked`,
`TimepointIndex`, `SampleIndex`, `TypedLatentSelection.checked`,
`LatentMetadata`, `BoldZipAmplitude`, and the checked BOLDZip entry/event
constructors. They preserve the older plain-`Int`/`Double` archive wire format
while making invalid state explicit at construction sites.

Later slices should add richer dataset adapters, fmrilatent parity fixtures for
the typed encoder factory, richer Haar archive descriptor metadata if the schema
needs it, and compressed BOLDZip encoder variants.

The DCT fixture constants in `DctBasisSuite` can be regenerated with
`tools/r-parity/fmrilatent-dct-fixtures.R`.
The HRBF fixture constants in `RadialBasisSuite` can be regenerated with
`tools/r-parity/fmrilatent-hrbf-fixtures.R`.
`LatentSyntheticRoundtripSuite` exercises deterministic synthetic encode/decode
roundtrips for provided temporal, DCT, Haar, shared-basis, and radial encoders
on both JVM and Scala.js.
