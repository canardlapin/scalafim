# Intaglio repin to edcdfd5 — 2026-10-08

Mote: `bd-01M4DNVJ9CAA4GYG1G016JCYNR`. Base: ScalaFIM `main` at `c59c21d8`.

## Change

- `build.sbt` `intaglioRevision`: `596b398af380079e4b251535230d0bc03cd88c51` →
  `edcdfd5ffaf010e8205da04a85a7f8b6b2fd0d92` (Intaglio `main` on 2026-10-08, 247 commits later).
- `SurfaceSceneCodec` encodes and decodes every `DisplayThreshold` case. Intaglio `cdf1562`
  added `Below`, `Above` and `TwoSided`; the old two-case match would have thrown
  `MatchError` when encoding them. New wire forms (scene `revision` stays 1; old documents
  decode unchanged):
  - `{"kind":"below","cutoff":x}` and `{"kind":"above","cutoff":x}`
  - `{"kind":"two-sided","inner":[lo,hi],"outer":[lo,hi] | null}`
  Each kind checks its own field set under the strict read policy; cutoffs must be finite and
  `outer` must contain `inner` (Intaglio's `twoSided` constructor). Accept/reject outcomes for
  existing `disabled` and `transparent-band` documents are unchanged; one error precedence
  shifts: under the strict policy, an object with a missing or unknown `kind` *and* a foreign
  field now reports the `kind` problem rather than `UnknownField`.
- API break absorbed: `DiscreteDomain` is now `DiscreteDomain[A]` (Intaglio `55cbbc5`,
  "Retain typed scale categories"); `DesignGraphics.regressorDomain` returns
  `DiscreteDomain[String]`.

## Evidence

- `sbt scalafimCompileAll` (JVM + JS): success, no compiler warnings. The only `[warn]` lines
  are sbt's unused-key lint for `Compile / doc / tags` in Intaglio's own `build.sbt:93`.
- Tests, all passing:

  | Module | JVM | JS |
  |---|---|---|
  | design | 478 | 477 |
  | surfaceView | 70 | 70 |
  | imageView | 42 | 42 |
  | surfaceViewRaster | 13 | 13 |
  | imageViewJava2d | 2 | — |
  | imageViewCanvas | — | 8 |
  | surfaceViewThree | — | 18 |
  | surfaceViewJavafx | 35 | — |
  | imageViewJavafx | 2 | — |

  New `SurfaceSceneDocumentSuite` tests: every-kind round-trip (an exhaustive match over the
  enum, so a future Intaglio case raises a non-exhaustive-match warning in the suite, which
  breaks the warning-clean gate; surface-view is not built with `-Werror`) and
  rejection of non-finite cutoffs, missing fields, foreign fields (and their acceptance under
  the `Ignore` policy), malformed bands and non-nested two-sided bands.

## Direct consumer: PLS Neuro

PLS Neuro (`canardlapin/plsneuro`) does not consume this pin: `project/Providers.scala` sets
`-Dscalafim.intaglio.build` to its own prepared Intaglio checkout from `providers.properties`.
After this repin, ScalaFIM's ordinary pin and a PLS Neuro provider at or after `edcdfd5`
agree, so that override no longer needs to be a migration exception, and PLS Neuro scenes
using `Below`/`Above`/`TwoSided` thresholds now encode instead of throwing. PLS Neuro's own
build was not run here; its provider bump should be qualified in that repository.

Observed in passing (not changed): the scene codec renders numbers with
`java.lang.Double.toString`, so integral values serialize as `1.0` on the JVM and `1` on JS.
Both decode identically, but encoded bytes are platform-dependent.

Out of scope, per the mote: adopting new Intaglio features in ScalaFIM views.
