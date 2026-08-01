# scalafim-connectivity

Typed structural connectivity core for ScalaFIM.

This module owns shared, portable connectivity algebra and the first
cross-platform analysis kernels: validated node, time, and run axes; parcel
time-series inputs; square and rectangular edge spaces; static and dynamic
connectivity containers; estimator and preprocessing plans; deterministic
workflow receipts; ETS/event-weighted correlation; ridge and PC partial
correlation; connectivity-set inference; and low-risk dynamic stacks.

Connectivity matrices are measurements over complete `EdgeSpace` coordinate
systems, not graph values. `ConnectivityGraphProjection` is the explicit bridge
to simple loopless topology. Its policy keeps eligibility, selection scoring,
selection, output transformation, diagonal treatment, and post-transform zero
handling independent. Density selection uses a deterministic EdgeSpace-order
tie break (or explicitly includes all boundary ties), and distance ordering is
available only for a distance measure.

Every projection returns the canonical graph together with a versioned receipt
and one `EdgeSpaceIx` per graph edge. The correspondence follows canonical graph
edge order even when the source uses Ariadne-compatible vectorization, so graph
statistics can be mapped exactly back to connectivity vectors. Receipts retain
the source measure, ordered node keys, scientific basis provenance, selection
details, realized density, and vertices without incident nonzero transformed
weight. Rectangular connectivity is deliberately rejected by this v1 bridge.

`NodeAxis` owns ordered scientific node metadata; standalone graph4s owns the
projected topology. The module deliberately excludes dataset backends, atlas
registries, BIDS parsing, plotting, JVM IO, multivariate execution adapters,
TVGL/SRLC, phase/HMM internals, and scheduler/runtime execution. Those belong
in higher adapter modules once the structural contracts are stable.

At that application boundary, ordinary connectivity projections can use the
shared plotting DSL without moving renderer types into this module:

```scala
import intaglio.*

final case class EdgeSummary(distance: Double, weight: Double, network: String)

val edgePlot = plot(edgeSummaries)
  .aes(_.distance, _.weight)
  .scaleColorDiscrete(_.network, name = "network")
  .geomPoint()
  .build
```
