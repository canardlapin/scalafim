# surface-view-three

Scala.js interpreter for the shared `SurfaceRenderPlan`. A host injects its
Three.js module namespace and canvas, so the library does not impose an npm
bundler or leak mutable Three.js types through its public API.

The production project uses the repository-wide CommonJS setting. The
standalone real-browser acceptance probe links an ES module so its top-level
exports can be imported explicitly:

```text
sbt 'set surfaceViewThreeJS / scalaJSLinkerConfig ~= (_.withModuleKind(org.scalajs.linker.interface.ModuleKind.ESModule))' surfaceViewThreeJS/fastLinkJS
```

The checked-in harness owns an exact Three.js development dependency. Install
it and run the automated real-WebGL smoke from a clean checkout with:

```text
npm --prefix modules/surface-view-three/browser ci
npm --prefix modules/surface-view-three/browser exec -- playwright install chromium
bash tools/ci/surface-browser-smoke.sh
```

The smoke links the backend as an ES module, starts a repository-local HTTP
server, launches Playwright's Chromium, and asserts geometry/color upload,
draw submission, native picking, ScalaFIM resource release, and native Three.js
geometry/material/renderer disposal. The browser, context, page, and server are
closed in a `finally` block.

For the larger interactive matrix, serve the repository root and open
`/modules/surface-view-three/browser/index.html` after linking and `npm ci`.

See [`docs/surface-viewer.md`](../../docs/surface-viewer.md) for the shared
scientific contract, host lifecycle, feature gates, and example application.
