# Graphics extraction

Status: complete.

The renderer-neutral grammar-of-graphics core and its SVG, Canvas, Java2D, and
JavaFX backends are developed in
[`canardlapin/intaglio`](https://github.com/canardlapin/intaglio). ScalaFIM is a
downstream neuroimaging consumer and no longer owns graphics implementations.

ScalaFIM pins one exact public Intaglio source revision in `build.sbt`. An
optional `scalafim.intaglio.build` system property may point sbt at an Intaglio
checkout for coordinated local development; ordinary builds never depend on a
sibling directory or `publishLocal`.

The dependency boundary is:

| ScalaFIM consumer | Intaglio project |
|---|---|
| `design`, `image-view`, `surface-view`, raster/Three.js surface adapters | `coreJVM` / `coreJS` |
| `image-view-canvas` | `canvasJS` |
| `image-view-java2d` | `java2dJVM` |
| `image-view-javafx` | `javafxJVM` |
| JVM design gallery tests | `svgJVM` |

Generic plotting galleries, renderer conformance, and paired ggplot2 visual QA
belong to Intaglio. ScalaFIM retains only
`tools/render_design_gallery.sh`, which verifies that `DesignGraphics` emits a
portable scene accepted by Intaglio's SVG renderer.

The extraction handoff gate is:

```sh
# In Intaglio
sbt compileAll testAll
tools/render_position_adjustment_qa.sh

# In ScalaFIM
sbt compileAll testAll
sbt examplesCompile examplesTest
tools/render_design_gallery.sh
```
