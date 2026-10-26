package scalafim.spatial

import neurosurf4s.routing.*

// Frozen before extraction; legacy provider-affine-v1 numeric hashes differ by platform.
private[spatial] object StagedRoutingGolden:
  val program: String = "pullback-v1|root=component-root|target=component-target|path=component-root-to-component-sampled-affine:Affine3D:false:affine-pullback-v1:coordinate:provider-affine-v1:bb8ad63b,component-mixing:Functional:false:functional-value-plugin-v1:ValueTransform:plugin-v1:component-observations:ValueTransform:observation-linear-v1:19d8e79f,component-offset:Filter:false:filter-value-plugin-v1:FusionBarrier:plugin-v1:component-affine-values:FusionBarrier:pointwise-affine-v1:0x1.0p1:0x1.8p1|routing=Shortest|sampling=Trilinear|rows=0,1|inverse=false|compilers=affine-pullback-v1,functional-value-plugin-v1,filter-value-plugin-v1|stages=component-root-to-component-sampled-affine:CoordinatePullback:affine-pullback-v1:coordinate:provider-affine-v1:bb8ad63b,component-mixing:ValueTransform:functional-value-plugin-v1:ValueTransform:plugin-v1:component-observations:ValueTransform:observation-linear-v1:19d8e79f,component-offset:FusionBarrier:filter-value-plugin-v1:FusionBarrier:plugin-v1:component-affine-values:FusionBarrier:pointwise-affine-v1:0x1.0p1:0x1.8p1"
