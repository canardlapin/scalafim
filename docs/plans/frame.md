# Frame extraction

The immutable typed dataframe work that incubated in ScalaFIM was extracted on
2026-07-23 to the standalone
[`frame4s`](https://github.com/canardlapin/frame4s) repository.

The standalone repository owns the source, architecture, semantic contract,
benchmarks, ecosystem-readiness notes, and future backend work. Its
`PROVENANCE.md` records ScalaFIM commit
`c26d43b2e28b26a88a41bda854fd8479140e3fe4` as the extraction source.

ScalaFIM does not depend on `frame4s`. Any future integration must be introduced
as an explicit, justified dependency rather than restoring copied modules.
