# surface-view-three

Scala.js interpreter for the shared `SurfaceRenderPlan`. A host injects its
Three.js module namespace and canvas, so the library does not impose an npm
bundler or leak mutable Three.js types through its public API.

The production project uses the repository-wide CommonJS setting. To build the
standalone real-browser acceptance probe as a classic script:

```text
sbt 'set surfaceViewThreeJS / scalaJSLinkerConfig ~= (_.withModuleKind(org.scalajs.linker.interface.ModuleKind.NoModule))' surfaceViewThreeJS/fastLinkJS
```

The checked-in harness resolves the linked output and the local SurfViewJS
Three.js installation relative to `~/code`; serve `~/code` and open
`/scala/scalafim/modules/surface-view-three/browser/index.html`.

See [`docs/surface-viewer.md`](../../docs/surface-viewer.md) for the shared
scientific contract, host lifecycle, feature gates, and example application.
