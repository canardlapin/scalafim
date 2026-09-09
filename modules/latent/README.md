# scalafim-latent

Pure typed response-representation mathematics for ScalaFIM.

The shared module owns:

- checked representation identities, shapes, annotations, and selections;
- `LatentResponse` and `ExplicitLatentResponse`;
- DCT-II and Haar temporal bases and projection encoders;
- typed, inspectable `DecodePlan[A]` programs and logical payload requests;
- `TemporalDctRepresentation`, with reconstruction error separated from
  selected-versus-whole decode consistency;
- transport and BOLDZip scientific carriers and reconstruction semantics;
- radial/HRBF atoms, active/full-grid order, basis generation, and partial
  decode;
- allocation-conscious portable numerical kernels.

The module does not import archive code and contains no LNA/HDF5, Zarr, object
key, byte-codec, checksum, or publication policy. Archive-specific encoders,
payload descriptors, codecs, and shared-basis artifact adapters live in
`interop-archived-response`. The LNA schema itself lives in `archive-lna`.

This split keeps representation plans executable through memory, LNA, Zarr, or
another binding without teaching the mathematical model about physical
storage.

`DctBasis` preserves the latent specification and normalization API while
delegating explicit DCT-II column construction to Gale. Temporal representation
and projection policy remain in this module.

The DCT fixture constants in `DctBasisSuite` can be regenerated with
`tools/r-parity/fmrilatent-dct-fixtures.R`.

Run it directly with:

```sh
sbt latentJVM/test
sbt latentJS/test
```
