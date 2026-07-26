# Graphics hardening — type-discipline follow-ups

Status: remediation of the type-discipline review of the `graphics-hardening`
branch (merge-base `01fd8fb` … `1d20197`). The review found no blockers; the
branch adds guarantees on net (erased `AesEnv` map replaced by exhaustive GADT
matches, guide-layout tuples replaced by an ADT, magic-constant guide placement
replaced by a measured solver). These are the localized regressions against that
bar.

Behavior must not change. Plotting, layout, unit, guide, and renderer semantics
stay byte-identical; every item below is a type-shape fix. The renderer
conformance suite is the backstop.

## Items

| # | Sev | Item | Anchor |
|---|-----|------|--------|
| F1 | Major | Phantom `Row` on `TrainedPlot` / `ResolvedFacetPanel` | `PlotCompiler.scala` |
| F2 | Major | `LegendRequest` carries two representations of one fact | `PlotLayout.scala` |
| F3 | Minor | Guide spec↔placement paired positionally; mismatch stuffed into `LayoutOverflow` | `CompilerPhases.scala`, `PlotLayout.scala` |
| F4 | Minor | `GuidePlacement` skips the finiteness `require` its siblings enforce | `PlotLayout.scala` |
| F5 | Minor | Extraction guard scans only `import`/`export` lines | `GraphicsExtractionGuardSuite.scala` |
| N6 | Note | `Independent.effectiveData` hides an always-`Some` invariant | `Grammar.scala` |
| N7 | Minor | `Colorizer` capability query and builder can desync | `Display.scala`, consumed by `surface-view` |
| N8 | Note | Unused import; triple-match hoist | `CanvasTextMetrics.scala`, `PlotLayout.scala` |

## F1 — resolve the phantom type parameter

`ResolvedFacetPanel[Row]` uses `Row` in no field; `TrainedPlot[Row]`'s only use
is the phantom `facetPanels`. `TrainedPlot.droppedRows` returns existential
`Vector[TrainedDroppedRow]` even for homogeneous plots.

Decision: **drop the parameter** from both. They are genuinely heterogeneous
containers once independent layers exist, so the aggregate `droppedRows` must be
existential regardless; keeping `[Row]` advertises a relation the structure does
not enforce. Typed diagnostics remain available per layer via
`TrainedLayer.droppedRows: Vector[DroppedRow[Row]]` at the layer's own row type.

The alternative — parametrizing `TrainedLayer[PlotRow]` so `Inherited` binds the
plot row — was rejected: the aggregate stays existential whenever an independent
layer is present, so it is substantial plumbing for a partial win.

Blast radius is in-module only (`FacetCompiler`, `PlotDsl`, and the
`PositionSuite` / `FacetSuite` / `GeomSuite` fixtures). No module outside
`graphics` names `TrainedPlot`.

## F2 — collapse `LegendRequest`

Drop `title` and `labels`; keep `items: Vector[GuideLayoutRequest]` and
`extraKeyWidthPt`. Remove `normalizedItems` (it silently preferred `items`, so
the flattened pair was already vestigial — one call site passed both halves
redundantly, the other passed the sentinel `(None, Vector.empty)`). Keep a
convenience `apply(title, labels, extraKeyWidthPt)` that lifts a single legend
into `items`.

## F3 — fix the guide spec↔placement correspondence

`GuideStackSolver.plan` emits one placement per request item, but
`GuideStackPlan` erased the pairing, so `GuidePhase.lower` re-zipped by a
running counter and a kind mismatch fell to
`Left(GraphicsError.LayoutOverflow("guide stack plan"))` — an "overflow" error
standing in for a pairing bug. Unreachable today, but the wrong error was
encoded in the type, and the counter would silently misalign every later guide
if a new `GuideSpec` case were added that `specs.collect` did not gather.

Fix: key each request by the index of the spec it came from and look the
placement up by that index (`Map[Int, GuidePlacement]`). The correspondence is
then established at construction rather than by a positional walk, an
unmeasured spec cannot shift the others, and the mismatch branch becomes
genuinely unreachable — so it returns the authored origin instead of a
misleading error.

A first attempt introduced a `PlacedGuide` sum pairing each request with its
placement. Review found its `request` payload was never read — only the variant
tag, which `GuidePlacement` already carries — so it was dropped as a
write-only field in favour of the index keying above, which is where the
guarantee actually lives.

## F4 — validate `GuidePlacement`

Every neighbouring layout record (`LayoutPolicy`, `LegendRequest`, `TextStyle`,
`PanelGridRequest`) `require`s finiteness at construction; `GuidePlacement` does
not, and its inputs come from the open `TextMetrics` trait. Convert the `enum`
to a `sealed trait` plus two `final case class`es (an enum case cannot carry a
`require` body) and add the finiteness clauses.

## F5 — broaden the extraction guard

The regex inspects only `import`/`export` statements, so a production file can
reference another domain by inline fully-qualified name and the Tier-X
extraction guarantee is violated while the test stays green. Flag any
`scalafim.` token outside `scalafim.graphics` in production sources. Comment and
string mentions are controllable in this codebase; a semanticdb-symbol check is
a stretch goal, not required.

Broadening must not narrow the guard elsewhere: the trailing alternation has to
keep matching brace and wildcard selectors (`import scalafim.{linalg, image}`,
`import scalafim.{dataset => D}`, `import scalafim.*`), which a bare
identifier class silently drops. Final form:

```scala
"""\bscalafim\.(?!graphics\b)(?:[A-Za-z_][A-Za-z0-9_]*|\{|\*)""".r
```

## N6 — store independent layer data in exactly one place

`PlotLayer.Independent.effectiveData` read `layer.data.getOrElse(Vector.empty)`
though `independentWithData` guaranteed `Some`. Give the case a real
`data: Vector[Row]` field.

The inner `Layer`'s own `data` must be **cleared** at the same time
(`selfContained` does `copy(data = None, inheritMapping = false)`). A first
attempt left it untouched, which meant
`plot.addIndependentLayer(rows, Layer.point(..., data = Some(other)), policy)`
— both entry points are public — produced a layer whose `layer.data` said
`other` while `effectiveData` said `rows`. That reintroduced the very
two-representations defect this item exists to remove, one level up.

## N7 — unify the `Colorizer` capability

`Colorizer.supportsWindow: Boolean` and `withWindow: Option[Colorizer[A]]` are
two sources of truth for one fact, and an implementation can override them
inconsistently.

The flags are live, in `image-view`: `Layer.supportsWindow` delegates to
`colorizer.supportsWindow`, and `Interaction` uses it to raise a typed
`ImageViewError.WindowUnsupported` before it holds a `DisplayWindow` to try.
So the nullary predicate cannot simply be deleted. (Note that
`SurfaceViewer.requireCapability` takes a `SurfaceLayer => Boolean` and reads
`SurfaceLayer.supportsWindow` — a different method that happens to share the
name.)

Make one primitive and derive both: `windowing: Option[DisplayWindow =>
Colorizer[A]]` with `final def supportsWindow = windowing.isDefined` and
`final def withWindow(w) = windowing.map(_(w))`; likewise `thresholding`. Both
call patterns survive and desync becomes unrepresentable. Verify `image-view`
and `surface-view` on both platforms.

## Known residue

Two accepted items, recorded rather than fixed:

- `GuideStackSolver.plan` is now partial where it was total: the new
  `GuidePlacement` `require`s can throw inside the `Either`-returning
  `PlotLayoutSolver.solve` if a `TextMetrics` implementation returns a
  non-finite measurement. No legal `LayoutPolicy` can reach it — every input is
  a policy field already required finite and `>= 0` — so the only vector is a
  pathological custom `TextMetrics`. This matches the `require`-at-construction
  convention every sibling record in the file already uses.
- `Colorizer.supportsWindow`/`supportsThreshold` allocate a closure per query
  because they derive from `windowing`/`thresholding`. Call sites are
  per-interaction, not per-pixel.

Pre-existing and out of scope: `PlotLayoutSuite`'s "colorbar requests reserve
their wider swatch and tick-label offset" builds two **legends**, not
colorbars. The behavior is unchanged by this work; the test name is misleading.

## Verification

Each phase gates on:

```sh
sbt graphicsJVM/test graphicsJS/test
```

plus `surfaceViewJVM/test surfaceViewJS/test` for N7, and a final `compileAll`.
Baseline at the start of this work: graphics core 216 tests, 0 failures, both
platforms.
