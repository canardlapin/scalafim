# scalafim-fmri-design

Cross-compiled JVM/Scala.js fMRI design module for `scalafim`.

Package root:

```scala
import scalafim.fmri.design.*
```

This module contains event models, formula parsing, condition bases, baseline
models, contrast definitions, design-matrix metadata, and design export helpers.
It depends on `scalafim-fmri-hrf` for sampling/convolution, standalone Gale for
shared QR/rank primitives, and Intaglio core for renderer-neutral plot and scene
exports.

Preferred typed entry points:

- `EventModelBuilder.EventDesignRequest` collects formula, data, schedule,
  build options, and extension registries into one inspectable request value.
- `FormulaParser.parseEither` and `EventModelBuilder.buildEither` return
  `DesignError` values instead of throwing at API boundaries.
- `BaselinePlan`, `NuisanceInput`, and `NuisancePolicy` provide a validated
  baseline/nuisance model request surface over the legacy parameter list.
- `HrfSelection` distinguishes shared HRFs from per-event HRFs without
  union casts.
- `DesignColumnDescriptor` and typed export indices (`DesignColumnIndex`,
  `ScanIndex`, `RunIndex`, `BasisIndex`, `TermIndex`) sit beside the legacy
  `DesignColumnMeta` fields for safer downstream plotting/reporting.
- `DesignGraphics.eventPlot` and `DesignGraphics.eventScene` adapt
  `EventPlotData` into graphics `Plot` and `Scene` values without depending on
  SVG or any platform renderer.
- `ContrastExpr`, `CellSelector`, `BasisSelection`, and `CompiledContrast`
  provide a typed, total contrast path via `compileEither` while legacy
  `ContrastSpec`/`ContrastWeights` constructors remain available.

Renderer-neutral event plot export:

```scala
import scalafim.fmri.design.*
import intaglio.svg.*

val scene = DesignGraphics.eventModelScene(model, termName = Some("task")).toOption.get
val svg = SvgRenderer.render(scene).toOption.get
```

`DesignGraphics` itself is implemented with the public plotting DSL. A design
adapter can use the same surface directly:

```scala
import intaglio.*

val trained = plot(eventPlotData.points)
  .aes(_.time, _.response)
  .group(_.regressor)
  .scaleColorDiscrete(_.regressor, levels = eventPlotData.regressors, name = "regressor")
  .geomLine()
  .resolve
```

The dependency direction is `design -> Intaglio core`; applications select an
Intaglio renderer separately. `design` produces plot specs/scenes, while SVG is
used only by the JVM integration gallery.

Run it directly with:

```sh
sbt designJVM/test
sbt designJS/test
tools/render_design_gallery.sh
```

## Cutoff-period DCT drift

Choose a cutoff explicitly; constant and polynomial defaults are unchanged.
The same basis is accepted by `ModelBuildSpec.baselineBasis`:

```scala
import scalafim.fmri.design.baseline.*
import scalafim.fmri.hrf.design.SamplingFrame

val period = DctCutoffPeriod.fromSeconds(120.0)
  .fold(error => throw new IllegalArgumentException(error.message), identity)
val frame = SamplingFrame(blockLens = Seq(100, 80), tr = Seq(2.0, 1.5))
val model = BaselineModel.buildEither(BaselinePlan(
  frame, basis = BaselineBasis.Dct(period), intercept = Intercept.Runwise
))
```

This example contributes three drift columns in the first run, two in the
second, and two separate run intercepts. For N original scans with repetition
time TR, the drift components are k = 1 through floor(2 N TR / cutoff). A
component exactly at the cutoff is included. Columns use the orthonormal
DCT-II convention, cos(pi (i + 0.5) k / N) sqrt(2/N). This follows the basis and
cutoff convention in [SPM's filter](https://github.com/spm/spm/blob/main/spm_filter.m)
and [DCT matrix](https://github.com/spm/spm/blob/main/spm_dctmtx.m); it does not
claim whole-pipeline SPM equivalence. Gale constructs only these columns.

Cutoffs must be positive and finite. A cutoff at or below twice the run's TR
requests unavailable finite-grid components and is rejected, not truncated or
aliased. A short run can contribute zero drift columns. Component zero is
excluded; `Intercept.Runwise`, `Global`, and `None` control the intercept
separately. Adding the parameterized `BaselineBasis.Dct` case means the enum no
longer has compiler-generated `values`/`valueOf`; existing named cases and
string parsing remain available, and DCT requires its explicit period.

The basis uses each run's complete original scan indices, independently of its
acquisition-start offset. Acquisition times and the offset remain in the schema.
Censoring selects those original rows; it does not rebuild a compressed DCT
grid. Schema policy receipts report cutoff seconds, run-specific TR, original
scan count, component count, and the inclusive endpoint convention. Cutoff is
also part of structural basis identity, encoded from its exact numeric bits so
platform-specific decimal display cannot change a column identifier.

Drift enters the joint native model. The selected-estimate operator adjusts the
task readout for those columns without a separate full-data filtering pass.
If residualizing explicitly, transform both task design and data on the same
retained rows. Do not assume full-grid orthonormality after censoring or that
filtering commutes with GLS whitening. Supplied cosine confounds can duplicate
the drift basis: use the existing `NuisancePolicy` to report, reject or explicitly
drop aliases, and inspect the resulting rank and estimability diagnostics.
