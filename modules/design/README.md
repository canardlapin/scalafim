# scalafim-fmri-design

Cross-compiled JVM/Scala.js fMRI design module for `scalafim`.

Package root:

```scala
import scalafim.fmri.design.*
```

This module contains event models, formula parsing, condition bases, baseline
models, contrast definitions, design-matrix metadata, and design export helpers.
It depends on `scalafim-fmri-hrf` for sampling/convolution and
`scalafim-linalg` for shared QR/rank primitives.

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
- `ContrastExpr`, `CellSelector`, `BasisSelection`, and `CompiledContrast`
  provide a typed, total contrast path via `compileEither` while legacy
  `ContrastSpec`/`ContrastWeights` constructors remain available.

Run it directly with:

```sh
sbt designJVM/test
sbt designJS/test
```
