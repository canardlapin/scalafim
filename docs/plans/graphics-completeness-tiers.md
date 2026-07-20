# Graphics: general plotting library — soundness + three-tier completeness

**Mote epic:** `bd-01KX94HQ7CA0NF2ZCH5V3PF3B3`
(reframes the earlier target-10 epic `bd-01KX2HRDJB08TM7GVCFR8E2PKD`; the hardening
and first-backends phases of roadmap epic `bd-01KX30QRM91PC04H73T4XX40H5` are done.)

Mote is the source of truth for status; this doc is the readable overview.

## Where we are

The hardened core is a rigorous kernel: a renderer-neutral
`Plot -> TrainedPlot -> Scene -> DeviceScene -> backend` pipeline with typed
aesthetics/scales, scale-derived guides, a layout solver, numeric device
resolution, a renderer conformance contract, and **four passing backends**
(SVG, Canvas, Java2D, JavaFX). Graded honestly: *all of grid plus ~15% of
ggplot2*. The foundations are unusually good; the gaps are above the
foundations, which is where you want them.

**This is a general grammar-of-graphics library. Neuroimaging is one consumer,
not the design center.** It is already verified extractable (see Tier X).

### Identity to protect (do not regress)

- **Single-point resolution.** All unit/orientation/handedness semantics resolve
  once in `DeviceScene.fromScene`; backends are dumb y-down interpreters. The
  y-flip exists in exactly one place.
- **Determinism as a contract.** Byte-identical output on JVM and JS
  (`Breaks.pretty`/`Labeler` avoid `log10` and `Double.toString`); the
  conformance harness double-renders to check it.
- **The renderer conformance contract.** Portable, backend-neutral, four
  backends pass it. The most original artifact in the module.
- **Typed diagnostics.** `DroppedRow` + `PlotDropReason`; `TrainedPlot` is
  inspectable before rendering.

Every issue below must preserve the renderer-neutral pipeline, keep the
conformance contract green, and pass JVM+JS tests.

## Tier 0 — Soundness (one real hole + two contract holes)

| Issue | Bead | Pri |
|---|---|---|
| **Plot-global scale training** — the one coherence hole | `bd-01KX94HR2WWDNNNN7CS7RDPCSD` | p1 |
| Stroke line-cap/join in `GraphicParams` (contract) | `bd-01KX94HR7FS99STJXB3S4ABRQT` | p2 |
| Resolve dead `Grob.Group.gp` field (inherit or remove) | `bd-01KX94HRCENFWHEW56YN922X0G` | p2 |

The headline: scale training is **per-layer**, not per-plot
(`ScalePhase.registry` builds from each layer env; `AesEnv.bind` only dedupes
within one env). Two layers with different continuous x-scales each rescale into
their own `[0,1]`; the panel unions them; the derived axis comes from
`firstScale` — so layer two is positioned on an axis that lies about it.
`MixedPositionScaling` catches scaled+unscaled but not scaled+differently-scaled.
Fix: one scale per aesthetic per plot, trained over the union of all layer data
(the ggplot2 invariant). This is the first thing to fix.

## Tier 1 — Blocks real use today

| Issue | Bead | Pri |
|---|---|---|
| Axis titles + plot title/subtitle/labels | `bd-01KX94HRH8EQ6XC0KHR0JC1WR2` | p1 |
| Theme (panel background, grid lines, palettes, styling) | `bd-01KX2HRE8QPEVEEH5TY0G4K1JD` | p1 |
| Continuous color guide (colorbar) + derivation | `bd-01KX94HRNA1DCE4HMX6E0YKWD0` | p1 |
| Science geoms: rect, ribbon/area, errorbar, segment, hline/vline, tile/heatmap; public polygon grob | `bd-01KX2HRE53V3A055JNJRMK37EP` | p1 |
| Coordinate systems: fixed aspect (`coord_equal`) + flip | `bd-01KX2HRDTNY48VW7HZRGTC27A8` | p1 |

Today you cannot name an axis (`GuideSpec.Axis` has no title), there is no
theme/panel background/grid, and a continuous color scale has no guide (a
colorbar) — so you cannot legend a t-map. `Grob.Image` is already
device-plumbed, so a tile/heatmap geom (connectivity/design matrices) is cheap.

## Tier 2 — Structural

| Issue | Bead | Pri |
|---|---|---|
| Facets / small multiples | `bd-01KX2HRDY4EHTK0ZQT1J8DY8DF` | p1 |
| Typed stats: count, bin/histogram, summary (mean±CI), density | `bd-01KX2HRDQ43M0EGVQX1VZJ9GME` | p1 |
| Discrete/band position scales (bars, boxplots by condition) | `bd-01KX94HRSHK0BFMFXBX8RFHTP0` | p2 |
| Platform text metrics (Java2D/Canvas font measurement) | `bd-01KX2HRECB8X0W27ZXM0HP5QRA` | p2 |
| Heterogeneous layer data (existentially-typed layers) | `bd-01KX94HRXWPMN3C6QNXWA9H9CF` | p2 |
| Position adjustments (dodge, stack, jitter) | `bd-01KX2HRE1KFRKTKTG0B50025EQ` | p2 |

Facets are the single highest-value structural capability; the layout solver is
single-panel by design, so this is the biggest lift and payoff. `Plot[Row]`
forces every layer to share one row type — annotation-over-different-data needs
existential layers.

## Tier 3 — Polish / creed

| Issue | Bead | Pri |
|---|---|---|
| Reconcile declared-but-rejected enum members (`Geom.Rect`, `Stat.Count`) | `bd-01KX94K2462BJHKK2Q3FJ4M9EE` | p3 |
| Unify `AesSpec` / `AesEnv` into one canonical form | `bd-01KX94K2WBRF31T9WJFRX46D1K` | p3 |
| Legend/guide placement via `TextMetrics`, not magic constants | `bd-01KX94K30NTDE7QME3MF8S2V1Y` | p3 |
| Private `traverse` helper (collapses 21 while-loop Either sites) | `bd-01KX94K34ZDGN1SGDE851JHCP3` | p3 |
| Decide + document `Geom.Line` ordering (path vs sort-by-x) | `bd-01KX94K39CDJ626081ZD0VAB49` | p3 |

The module's own creed ("invalid states unrepresentable") is violated by
`Geom.Rect`/`Stat.Count` — declared enum members that phases reject at runtime.
Implement (Rect lowers to the existing `Grob.Rect`; Count via typed stats) then
remove the blacklist.

## Tier X — Extraction readiness (cross-cutting)

| Issue | Bead | Pri |
|---|---|---|
| Package the graphics stack as a standalone library | `bd-01KX94K3DN5PXZWQN81GCKNV8S` | p2 |

Already clean: the core and all four backends import nothing outside
`scalafim.graphics`; the `graphics` crossProject has no `dependsOn`; the only
inbound edge is domain modules depending **on** graphics. Keep it that way with
a guard test, plan neutral naming, and a cross-publish story so the split stays
a lift-and-shift.

## Capstone + gate

| Issue | Bead | Pri |
|---|---|---|
| User-facing plotting DSL | `bd-01KX2HREFZ2J9EKPNNE8EDXFPV` | p1 |
| 10/10 re-grade gate | `bd-01KX30QS4595N63A5N712E81ES` | p2 |

## Ordering

Sequence: **fix scale-coherence first**, then climb Tier 1
(titles/theme, colorbar, science geoms, fixed aspect), then Tier 2 (facets and
the rest), with Tier 3 polish folded in opportunistically. Hard dependency edges
encoded in mote: facets and heterogeneous layers depend on plot-global scales;
legend-metrics depends on platform text metrics; the enum-creed reconciliation
depends on typed stats and geoms; band scales depend on typed stats; the
re-grade gate depends on the soundness fix plus the Tier-1 set and
extraction-readiness.
