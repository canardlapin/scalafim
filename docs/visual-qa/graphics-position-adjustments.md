# Plotting visual QA

The comparison gallery is generated from small fixtures rendered independently
by ScalaFIM/Java2D and ggplot2. Run:

```sh
tools/render_position_adjustment_qa.sh
```

Open `target/graphics-position-qa/index.html`. The page places seven 640 x 480
comparisons side by side without enlarging either renderer's raster:

- filled continuous scatter points;
- grouped continuous lines;
- categorical `stat_count` bars;
- a fixed-scale two-panel facet wrap;
- categorical bars divided into equal dodge slots;
- positive stacked bars using reverse group order;
- bounded jitter around two categorical centers.

The ggplot2 runner writes `*-layer.tsv` data for every example. Scatter, line,
count, and facet data give direct structural references. Dodge and stack are
numeric oracles: ScalaFIM uses zero-based categorical centers, so subtracting
one from ggplot2's x coordinates gives the ScalaFIM centers and bounds. Stack
`ymin` and `ymax` should agree directly. Jitter is a semantic comparison only:
both use the requested half-spread, but ScalaFIM intentionally uses a pure
SplitMix64 generator instead of R's RNG so its offsets are identical on JVM
and Scala.js. Its circles bind the same typed color to stroke and fill
explicitly; no renderer infers plotting semantics.

Pixel identity is not expected. Text metrics, theme spacing, and rasterization
belong to their respective renderers; the visual gate checks geometry,
ordering, clipping, legibility, and absence of collisions or truncation.
