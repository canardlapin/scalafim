# surface-view-three

Scala.js interpreter for the shared `SurfaceRenderPlan`. A host injects its
Three.js module namespace and canvas, so the library does not impose an npm
bundler or leak mutable Three.js types through its public API.

Inspectable scalar maps and mixed face/color/nearest/scalar layers use generated
fragment shaders. The adapter preserves original scientific picks, reports layer
and geometry uploads separately, and rejects mapping boundaries that collapse at
GPU float precision. Runtime creation checks native depth ordering before surface
uploads and returns a typed error for a failing graphics context. Color/scalar/normal
varyings use centroid interpolation; generated nearest corners preserve the
reference vertex-lighting contract. Fractional fitted viewports stay aligned
with logical display coordinates and original-face picking.
The renderer conditions the occupied depth interval across all draw slots to
improve separation of nearby faces, preserving reached clipping planes and the
scientific camera. Geometry bounds refresh with morphs and include network tubes.

See the [capability guide](../../docs/surface-viewer.md) and
[requirement audit](../../docs/plans/surface-map-final-audit.md) for tested
combinations, numerical budgets, native evidence, and device limitations.

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
