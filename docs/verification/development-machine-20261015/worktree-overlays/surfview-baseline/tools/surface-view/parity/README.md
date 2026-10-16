# Surface-view parity baseline

This is the bounded P0/P1 harness for C01--C03. It captures public SurfViewJS
and ScalaFIM browser renderers into task-owned Playwright state. It never uses a
user Chrome profile, private renderer fields, image registration, auto-fitting,
or recolouring. C04--C12 are required manifest rows and are emitted as explicit
unimplemented failures until adapters exist.

Build Scala.js first, then point the runner at the generated module (the module
must expose `mountScalafimSurfaceParityBaseline`):

```sh
python3 tools/build/sbt-warm examplesSurfaceViewJS/fastLinkJS
node tools/surface-view/parity/run-parity.mjs \
  --scalafim-module /absolute/path/to/examples-surface-view-fastopt/main.js \
  --out /private/tmp/surfview-parity-artifacts
```

The runner uses the installed `@playwright/test`, Vite, Three, and SurfViewJS
lockfile under `/Users/bbuchsbaum/code/jscode/surfviewjs`; it installs nothing.
It runs the repository browser ownership audit before it opens a task-owned
Chromium context and after it closes the browser and Vite child. The source,
lockfile, dependency versions, browser, OS, GPU string, semantic receipts,
framebuffer PNGs, side-by-side plates, pixel error heatmaps, and mutation-test
receipts are saved under `--out`.

This baseline does not qualify JVM snapshots, explicit ScalaFIM camera-up, or
the unimplemented C04--C12 cases. A missing module, missing public entry point,
capture failure, or comparator mutation that is accepted causes a nonzero exit.
