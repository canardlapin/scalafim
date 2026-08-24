# Cross-platform surface viewer example

This example uses one checked GIFTI fixture and one shared viewer construction
on JVM and Scala.js. It exercises bilateral layout, a thresholded dynamic scalar
layer, a dynamic label layer, world-transformed geometry, selection, reference
raster picking, JavaFX Scene3D, and Three.js/WebGL.

The portable semantic receipt is pinned by the shared test and therefore runs
on both platforms:

```sh
sbt surfaceViewExamplesJVM/test surfaceViewExamplesJS/test
sbt "surfaceViewExamplesJVM/runMain scalafim.examples.surfaceview.SurfaceViewerReceiptExample"
```

Launch the JVM application with:

```sh
sbt "surfaceViewExamplesJVM/runMain scalafim.examples.surfaceview.JavaFxSurfaceViewerExample"
```

For the browser example, link the classic script, serve `~/code`, and open
`http://127.0.0.1:8000/scala/scalafim/examples/surface-view/browser/`:

```sh
sbt surfaceViewExamplesJS/fastLinkJS
cd ~/code && python -m http.server 8000 --bind 127.0.0.1
```

The HTML host loads the local SurfViewJS Three.js distribution only as the
renderer runtime. All surface, layer, threshold, layout, camera, and receipt
semantics come from ScalaFIM. A visible `PASS` means more than successful
mounting: the page reads the native WebGL framebuffer and compares coverage,
orientation, position, and interior colors with the shared CPU raster, then
requires exact surface, face, and nearest-vertex parity for an interior pick.

Run the matching JavaFX fixture gate with:

```sh
sbt surfaceViewVisualQaJVM
```

The JavaFX GIFTI probe and browser page use the same 960 x 540 dimensions,
native-backend policy, reference raster, bilateral landmark coordinates, and
expected typed pick. Orthographic plans use JavaFX `ParallelCamera`; perspective
plans retain `PerspectiveCamera`.

## Production cortical acceptance

The tiny checked fixture remains the fast exact semantic gate. A second opt-in
gate exercises real bilateral fsaverage5 pial surfaces (10,242 vertices and
20,480 faces per hemisphere) without vendoring data whose redistribution terms
have not yet been established. By default it reads the checksum-pinned files in
`~/code/jscode/surfviewjs/tests/data`; set `SCALAFIM_SURFACE_CORPUS` or pass a
directory argument to use an equivalent corpus.

```sh
sbt "surfaceViewExamplesJVM/Test/runMain scalafim.surface.view.javafx.JavaFxCorticalAcceptanceProbe"
sbt surfaceViewExamplesJS/fastLinkJS
cd ~/code && python -m http.server 8000 --bind 127.0.0.1
# open http://127.0.0.1:8000/scala/scalafim/examples/surface-view/browser/cortical.html
```

Both files are verified by SHA-256 before decoding. The shared Scala fixture
then constructs the same real bilateral, layered, thresholded, dorsal,
orthographic plan on JVM and Scala.js. The native probes compare pixels with
the CPU oracle, exercise a stable pick, require flip sentinels, and prove that
a camera-only update uploads neither geometry nor layer data.

The browser decodes external GIFTI bytes with ScalaFIM's Scala.js reader. The
local SurfViewJS checkout supplies only the Three.js renderer runtime and the
external checksum-pinned fixture corpus. The browser host chooses a local
typed-array rendition; ScalaFIM does not introduce a second portable mesh byte
format. That rendition carries the source surface-to-world affine beside the
vertices and faces so compiled positions and picks retain RAS+ semantics.
