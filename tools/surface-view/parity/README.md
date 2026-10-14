# Surface-view parity baseline

This is the bounded P0/P1 harness for C01--C03. It builds the pinned SurfViewJS source and captures its public viewer
and the ScalaFIM browser renderer into task-owned Playwright state. It never uses a
user Chrome profile, private renderer fields, image registration, auto-fitting,
or recolouring. C04--C12 are required manifest rows and are emitted as explicit
unimplemented failures until adapters exist.

Build Scala.js first, then point the runner at the generated module (the module
must expose `mountScalafimSurfaceParityBaseline`):

```sh
python3 tools/build/sbt-warm surfaceViewExamplesJS/fastLinkJS
node tools/surface-view/parity/run-parity.mjs \
  --angle metal --native-probes \
  --scalafim-module /absolute/path/to/examples-surface-view-fastopt/main.js \
  --out /private/tmp/surfview-parity-artifacts
```

The runner uses the installed `playwright`, Vite, Three, and SurfViewJS
lockfile under `/Users/bbuchsbaum/code/jscode/surfviewjs`; it installs nothing.
It runs the repository browser ownership audit before it opens a task-owned
Chromium context and after it closes the browser and Vite server. The source,
lockfile, dependency versions, browser, OS, GPU string, semantic receipts,
framebuffer PNGs, side-by-side plates, overlays, pixel error heatmaps, and baseline
comparison receipts are saved under `--out`. Existing JavaFX and raster PNGs in
each case directory are included in the same-scale plate. Local PNG decoding uses
the pinned installed Playwright PNG utility; no new dependency is installed.

`--angle metal` selects the installed browser's hardware profile on this Mac;
the receipt records the requested profile and the actual renderer. The default
headless SwiftShader profile fails the recovered native depth admission probe
with four samples (`RGB(192,64,0)` where opaque red must cover the pixel).
That refusal is retained as a failed platform result. Metal passes the same
check; it does not qualify SwiftShader. `--native-probes` additionally records
the actual face, nearest, scalar-composition, threshold-edge, depth, and resource
update probes. Those probes establish bounded native behavior, not SurfViewJS
display parity.

The runner always returns a failing qualification verdict while family coverage
and material calibration are incomplete. Run `node --test
tools/surface-view/parity/compare.test.mjs` for the comparator mutation and positive
controls.

This baseline does not qualify JVM snapshots or the unimplemented C04--C12
families. A missing module, missing public entry point, capture failure, or
comparator mutation that is accepted causes a nonzero exit.

`matrix.mjs` separately validates one receipt per required row and checkpoint,
with frozen source, fixture, calibration, artifact, and required-element
identities. Product route and concrete renderer are checked separately. Run
`node --test tools/surface-view/parity/*.test.mjs` for both comparator and matrix
mutation controls. Empty contracts and matching missing identities cannot pass.

`fixtures.mjs` and `gallery.mjs` define the numeric inputs for all 151 required
checkpoints. `freeze-inputs.mjs` verifies the external cortex checksums and writes
`frozen-inputs.json`, including source hashes. This is an input contract, not a
qualification receipt. Review changes before regenerating it.

`--reference-calibration-only --angle metal` captures independent constant-color
and curvature specimens twice using the public reference adapter. It measures
repeatability and interior channel errors without loading a candidate module.
`--reference-calibration` includes these probes with a normal candidate run.
Calibration of these specimens does not qualify folded anatomy, other lighting
profiles, or another GPU environment.

## Required matrix

`manifest.json` retains the twelve family entries, with C01--C03 smoke adapters used by the current
runner. Its `requiredMatrix` is the prospective admission matrix: 28 uniquely
identified rows cover C01--C12 across browser, JVM, and publication routes.
Each row names one fixture, actual route/renderer, camera/material profile,
viewport, checkpoints, independent semantic/element oracle family, and its
tolerance-calibration status. Rows are required even while their status is
pending; no smoke capture upgrades a full family row to implemented or passed.

The matrix is deliberately bounded: named checkpoints bundle closely related
state changes in one deterministic sequence rather than making every Cartesian
product a distinct row. Each checkpoint must still receive a unique artifact
receipt. A full family is complete only when every one of its required rows has
calibrated tolerances, all required route artifacts, independent masks and
semantic evidence, and a clean verdict.

The bilateral fsaverage5 corpus was verified locally at
`/Users/bbuchsbaum/code/jscode/surfviewjs/tests/data`: left and right SHA-256
values match the manifest. It remains external and not vendored. Repository
evidence identifies it as FreeSurfer fsaverage5 GIFTI input but does not
establish redistribution terms, so qualification may use the local corpus while
vendoring or redistribution remains out of scope until a documented decision.

## Capturing the JVM baseline

Build and capture through the production JavaFX host before the browser run:

```sh
python3 tools/build/sbt-warm \
  'surfaceViewExamplesJVM/Test/runMain scalafim.examples.surfaceview.SurfaceParityJvm /private/tmp/surfview-parity-artifacts'
```

The adapter opens a temporary JavaFX stage, captures its actual SubScene and a
portable raster, and closes the stage/backend. An unattached native snapshot can
be blank, so the stage is part of the capture contract. `--candidate-root` selects
the checkout whose linked module is being compared; this is how an unchanged
production baseline can use the new test-only adapter.
