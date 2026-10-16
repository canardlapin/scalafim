package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Uint8Array
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

/** Native framebuffer evidence; independent CPU mapping/composition at recovered
  * original-face coordinates. Exact mapping boundary rules remain shared tests.
  */
object ThreeFragmentProbe:
  @JSExportTopLevel("runScalafimThreeFragmentProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic): js.Dynamic =
    val surface = SurfaceScalarFixture.Surface
    val geometry = SurfaceFaceFixture.geometry
    val size = ThreeCanvasSize.unsafe(128, 128)
    val runtime = ThreeJsRuntime.create(three, canvas).toOption.get
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val rows = new js.Array[js.Dynamic]()
    var finalImage = ""
    try
      for mode <- Vector("scalar", "color", "face", "nearest"); blend <- DisplayBlendMode.values; lit <- Vector(false, true); perspective <- Vector(false, true) do
        val extra = mode match
          case "scalar" => Vector.empty
          case "face" => Vector(SurfaceLayer.facePackedRgba(SurfaceLayerId.unsafe("under"), surface,
            SurfaceFaceField.make(geometry, Array(Rgba32.unsafe(40, 70, 90), Rgba32.unsafe(170, 80, 20))).toOption.get))
          case _ => Vector(SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("under"), surface, geometry,
            Vector(Rgba32.unsafe(20, 40, 160), Rgba32.unsafe(160, 80, 40), Rgba32.unsafe(60, 160, 40), Rgba32.unsafe(100, 40, 80)),
            interpolation = if mode == "nearest" then SurfaceVertexInterpolation.NearestSample else SurfaceVertexInterpolation.Color).toOption.get)
        val overlay = SurfaceLayer.interpolatedScalar(SurfaceScalarFixture.Layer, surface, geometry,
          SurfaceScalarFixture.Values.toArray, SurfaceScalarFixture.thresholded, opacity = DisplayOpacity.unsafe(0.7), blendMode = blend).toOption.get
        val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), extra :+ overlay).toOption.get
        val initial = SurfaceFaceFixture.state(model)
        val state = if !perspective then initial else
          val projected = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetProjection(
            CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
          SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
        val compiled = SurfaceCompiler.compile(model, state).toOption.get
        val plan = if !lit then compiled else compiled.copy(lighting = SurfaceLighting.directional(0.2, 0.8, 0.4, 0.3, 0.85).toOption.get)
        val receipt = backend.render(plan, size, forceDraw = true).fold(e => throw new IllegalArgumentException(e.message), identity)
        val gl = canvas.applyDynamic("getContext")("webgl2")
        val pixels = new Uint8Array(128 * 128 * 4)
        gl.applyDynamic("readPixels")(0, 0, 128, 128, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
        val evaluator = new SurfaceFragmentEvaluator(plan.meshes.head, plan.layers, plan.lighting, Rgba32.unsafe(184, 184, 184))
        var checked = 0
        var boundaryPixels = 0
        var maximumBoundaryError = 0
        var maxError = 0
        val boundaries = SurfaceMappingPartition.boundaries(SurfaceScalarFixture.thresholded)
        for y <- 5 until 123 by 2; x <- 5 until 123 by 2; pick <- backend.pick(x + 0.5, y + 0.5).toOption.flatten do
          val (wa, wb, wc) = pick.barycentric.get
          val weights = Vector(wa, wb, wc).sorted
          val scalar = 1 + 3 * pick.worldX
          val neighbors = Vector(backend.pick(x + 1.5, y + 0.5), backend.pick(x + 0.5, y + 1.5))
          val variation = neighbors.flatMap(_.toOption.flatten).map(p => math.abs(3 * (p.worldX - pick.worldX))).sum
          if weights.head > 0.08 && boundaries.forall(b => math.abs(scalar - b) > 2 * variation) &&
              (mode != "nearest" || weights(2) - weights(1) > 0.06) then
            val renderFace = (0 until plan.meshes.head.indices.length / 3).find(f => plan.meshes.head.sourceFace(f) == pick.face).get
            val (a, b, c) = plan.meshes.head.sourceFaceVertices(renderFace)
            val expected = evaluator.color(pick.face, a, b, c, wa, wb, wc)
            require(Set(a, b, c)(pick.vertex), s"generated vertex escaped scientific picking: ${pick.vertex}")
            val offset = ((127 - y) * 128 + x) * 4
            val error = Vector(math.abs(pixels(offset).toInt - expected.red), math.abs(pixels(offset + 1).toInt - expected.green),
              math.abs(pixels(offset + 2).toInt - expected.blue)).max
            val mesh = plan.meshes.head
            val cells = if mesh.nearestPartition.nonEmpty then renderFace until renderFace + 6 else renderFace until renderFace + 1
            if cells.exists(face => containsPixel(plan, mesh, face, x, y, size)) then
              maxError = math.max(maxError, error)
              checked += 1
            else
              boundaryPixels += 1
              maximumBoundaryError = math.max(maximumBoundaryError, error)
        require(checked > 100, s"insufficient $mode coverage: $checked")
        rows.push(js.Dynamic.literal(mode = mode, blend = blend.toString, lit = lit, perspective = perspective,
          checked = checked, maximumError = maxError, boundaryPixels = boundaryPixels,
          maximumBoundaryError = maximumBoundaryError, passed = (maxError <= 2), uploadedBytes = receipt.uploadedBytes.toDouble))
        finalImage = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      val updates = verifyUpdates(backend, size, canvas)
      val edges = verifyScalarEdges(backend, size, canvas)
      val clipping = verifyDepthClipping(backend, size, canvas)
      js.Dynamic.literal(cases = rows, edgeCases = edges, clippingCases = clipping, updates = updates, image = finalImage, resourcesBeforeDispose = backend.resourceKeys.size)
    finally backend.dispose()

  private def verifyScalarEdges(backend: ThreeSurfaceBackend, size: ThreeCanvasSize, canvas: js.Dynamic): js.Array[js.Dynamic] =
    val surface = SurfaceScalarFixture.Surface
    val geometry = SurfaceFaceFixture.geometry
    val blue = Rgba32.unsafe(0, 0, 200)
    val white = Rgba32.unsafe(200, 200, 200)
    val red = Rgba32.unsafe(200, 0, 0)
    val lower = ScalarRamp.linear(blue, white)
    val upper = ScalarRamp.linear(white, red)
    val window = DisplayWindow.unsafe(-1, 2)
    val split = ScalarMapping(ScalarScale.split(window, 0, -0.25, 0.5, lower, upper).toOption.get)
    val diverging = ScalarMapping(ScalarScale.diverging(window, 0.25, lower, upper).toOption.get)
    val invalid = SurfaceScalarFixture.mapping.copy(invalid = Rgba32.unsafe(255, 0, 255))
    val highOffset = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(1e8 - 1, 1e8 + 1), lower))
    val fixtures = Vector(
      ("split", split, SurfaceScalarFixture.Values.toArray),
      ("diverging", diverging, SurfaceScalarFixture.Values.toArray),
      ("hide-outside", SurfaceScalarFixture.mapping.copy(outOfRange = ScalarOutOfRange.Hide), SurfaceScalarFixture.Values.toArray),
      ("invalid-nan", invalid, Array(Double.NaN, 4, 4, -2)),
      ("invalid-positive-infinity", invalid, Array(Double.PositiveInfinity, 4, 4, -2)),
      ("invalid-negative-infinity", invalid, Array(Double.NegativeInfinity, 4, 4, -2)),
      ("network-high-offset", highOffset, Array(1e8 - 2, 1e8 + 2, 1e8 + 2, 1e8 - 2))
    )
    val rows = new js.Array[js.Dynamic]()
    for (name, mapping, samples) <- fixtures; lit <- Vector(false, true); perspective <- Vector(false, true) do
      val model = SurfaceScalarFixture.model(mapping, samples)
      val initial = SurfaceFaceFixture.state(model)
      val state = if !perspective then initial else
        val projected = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetProjection(
          CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
        SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
      val compiled = SurfaceCompiler.compile(model, state).toOption.get
      val normals = new FloatBufferView(Array[Float](0, 0, 1, 0.6f, 0, 0.8f, 0, 0.8f, 0.6f, -0.6f, 0, 0.8f))
      val mesh = compiled.meshes.head.copy(normals = normals, sampleNormals = Some(normals),
        geometryRevision = Some(SurfaceResourceKey("varying-normals")))
      val varying = compiled.copy(meshes = Vector(mesh))
      val attached = if name != "network-high-offset" then varying else
        val nodes = Vector(0, 2).map(v => SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe(s"edge-v$v"),
          surface, VertexId(v), geometry).toOption.get)
        val network = SurfaceNetwork.make(nodes, Vector(SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(1).id, 1))).toOption.get
        val display = SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All,
          SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.1), sides = 8).toOption.get).toOption.get
        SurfaceNetworkCompiler.attach(varying, surface, SurfaceLayerId.unsafe("edge-network"), display).toOption.get.plan
      val plan = if !lit then attached else attached.copy(lighting = SurfaceLighting.directional(0.2, 0.8, 0.4, 0.3, 0.85).toOption.get)
      backend.render(plan, size, forceDraw = true).fold(e => throw new IllegalArgumentException(s"$name: ${e.message}"), identity)
      val gl = canvas.applyDynamic("getContext")("webgl2")
      val pixels = new Uint8Array(size.width * size.height * 4)
      gl.applyDynamic("readPixels")(0, 0, size.width, size.height, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
      val packedMesh = plan.meshes.head
      val evaluator = new SurfaceFragmentEvaluator(packedMesh, plan.layers, plan.lighting, Rgba32.unsafe(184, 184, 184))
      val boundaries = SurfaceMappingPartition.boundaries(mapping)
      def scalar(pick: ThreePick): Double =
        val face = (0 until packedMesh.indices.length / 3).find(f => packedMesh.sourceFace(f) == pick.face).get
        val (a, b, c) = packedMesh.sourceFaceVertices(face)
        val (wa, wb, wc) = pick.barycentric.get
        val field = plan.layers.head.scalarField.get
        SurfaceScalarInterpolation.value(field.samples(a), field.samples(b), field.samples(c), wa, wb, wc)
      var checked = 0
      var networkPixels = 0
      var boundaryPixels = 0
      var maximumBoundaryError = 0
      var maximumError = 0
      var worst = ""
      for y <- 5 until 123; x <- 5 until 123; pick <- backend.pick(x + 0.5, y + 0.5).toOption.flatten do
        val (wa, wb, wc) = pick.barycentric.get
        val network = pick.face >= geometry.faceCount
        val value = if network then Double.NaN else scalar(pick)
        val neighbors = if !value.isFinite then Vector.empty else
          Vector(backend.pick(x + 1.5, y + 0.5), backend.pick(x + 0.5, y + 1.5)).flatMap(_.toOption.flatten).filter(_.face < geometry.faceCount)
        val variation = neighbors.map(p => math.abs(scalar(p) - value)).filter(_.isFinite).sum
        // A tube triangle can be narrower than one MSAA pixel even when the
        // center has large barycentric weights. Exclude the two-pixel domain
        // silhouette band, where the framebuffer blends tube and base samples.
        val sameDomain = name != "network-high-offset" || Vector((-2, 0), (2, 0), (0, -2), (0, 2)).forall: (dx, dy) =>
          backend.pick(x + dx + 0.5, y + dy + 0.5).toOption.flatten.exists(p => (p.face >= geometry.faceCount) == network)
        if math.min(wa, math.min(wb, wc)) > 0.1 &&
            sameDomain && (!value.isFinite || boundaries.forall(b => math.abs(value - b) > 2 * variation)) then
          val face = (0 until packedMesh.indices.length / 3).find(f => packedMesh.sourceFace(f) == pick.face).get
          val (a, b, c) = packedMesh.sourceFaceVertices(face)
          val expected = evaluator.color(pick.face, a, b, c, wa, wb, wc)
          require(Set(a, b, c)(pick.vertex), s"$name lost original vertex identity")
          val offset = ((size.height - 1 - y) * size.width + x) * 4
          val error = Vector(math.abs(pixels(offset).toInt - expected.red),
            math.abs(pixels(offset + 1).toInt - expected.green), math.abs(pixels(offset + 2).toInt - expected.blue)).max
          if containsPixel(plan, packedMesh, face, x, y, size) then
            if error > maximumError then
              maximumError = error
              worst = s"pixel=$x,$y face=${pick.face} weights=$wa,$wb,$wc scalar=$value expected=${expected.toPackedInt} actual=${pixels(offset)},${pixels(offset + 1)},${pixels(offset + 2)}"
            checked += 1
            if network then networkPixels += 1
          else
            // A barycentric margin alone does not bound distance in pixels on
            // thin tube triangles. Centroid interpolation can move the sample
            // within a partially covered pixel; retain that band separately.
            boundaryPixels += 1
            maximumBoundaryError = math.max(maximumBoundaryError, error)
      require(checked > 1000, s"$name insufficient coverage: $checked")
      require(name != "network-high-offset" || networkPixels > 20, s"network insufficient coverage: $networkPixels")
      require(maximumError <= 2, s"$name lit=$lit perspective=$perspective error=$maximumError $worst")
      rows.push(js.Dynamic.literal(mode = name, lit = lit, perspective = perspective, checked = checked,
        networkPixels = networkPixels, maximumError = maximumError, boundaryPixels = boundaryPixels,
        maximumBoundaryError = maximumBoundaryError, passed = true))
    rows

  private def containsPixel(plan: SurfaceRenderPlan, mesh: SurfaceMeshPacket, face: Int,
      x: Int, y: Int, size: ThreeCanvasSize): Boolean =
    val slot = plan.viewportFit.resolve(plan.slots, size.width.toDouble, size.height.toDouble).head
    def transform(matrix: FloatBufferView, p: Vector[Double]): Vector[Double] =
      Vector.tabulate(4)(row => (0 until 4).map(column => matrix(row * 4 + column) * p(column)).sum)
    val points = Vector.tabulate(3): corner =>
      val offset = mesh.indices(face * 3 + corner) * 3
      val world = Vector(mesh.positions(offset) + slot.worldOffsetX, mesh.positions(offset + 1) + slot.worldOffsetY,
        mesh.positions(offset + 2) + slot.worldOffsetZ, 1.0)
      val clip = transform(plan.camera.projectionMatrix, transform(plan.camera.viewMatrix, world))
      ((slot.viewport.x + (clip(0) / clip(3) + 1) * 0.5 * slot.viewport.width) * size.width,
        (slot.viewport.y + (1 - clip(1) / clip(3)) * 0.5 * slot.viewport.height) * size.height)
    def cross(a: (Double, Double), b: (Double, Double), p: (Double, Double)): Double =
      (b._1 - a._1) * (p._2 - a._2) - (b._2 - a._2) * (p._1 - a._1)
    val area = cross(points(0), points(1), points(2))
    area != 0 && Vector((x.toDouble, y.toDouble), (x + 1.0, y.toDouble),
      (x.toDouble, y + 1.0), (x + 1.0, y + 1.0)).forall: p =>
      (0 until 3).forall(i => cross(points(i), points((i + 1) % 3), p) / area > 1e-6)

  private def verifyDepthClipping(backend: ThreeSurfaceBackend, size: ThreeCanvasSize, canvas: js.Dynamic): js.Array[js.Dynamic] =
    val model = SurfaceScalarFixture.model()
    val rows = new js.Array[js.Dynamic]()
    for perspective <- Vector(false, true); nearPlane <- Vector(false, true) do
      val initial = SurfaceFaceFixture.state(model)
      val projected = if !perspective then initial else SurfaceViewer.reduce(model, initial,
        SurfaceViewerAction.SetProjection(CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
      val state = SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
      val full = SurfaceCompiler.compile(model, state).toOption.get
      backend.render(full, size, forceDraw = true).toOption.get
      val candidates = for
        y <- 7 until 121 by 2
        x <- 7 until 121 by 2
        pick <- backend.pick(x + 0.5, y + 0.5).toOption.flatten
        if math.min(pick.barycentric.get._1, math.min(pick.barycentric.get._2, pick.barycentric.get._3)) > 0.1
      yield (x, y, pick)
      val view = full.camera.viewMatrix
      val middle = -view(11).toDouble
      val clipping = if nearPlane then SurfaceClipping.NearFar(middle, 1000) else SurfaceClipping.NearFar(0.01, middle)
      val clipped = SurfaceCompiler.compile(model, state.copy(clipping = clipping)).toOption.get
      backend.render(clipped, size, forceDraw = true).toOption.get
      val evaluator = new SurfaceFragmentEvaluator(clipped.meshes.head, clipped.layers, clipped.lighting, Rgba32.unsafe(184, 184, 184))
      val gl = canvas.applyDynamic("getContext")("webgl2")
      val pixels = new Uint8Array(size.width * size.height * 4)
      gl.applyDynamic("readPixels")(0, 0, size.width, size.height, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
      var kept = 0
      var removed = 0
      var maximumError = 0
      candidates.foreach: (x, y, original) =>
        val depth = -(view(8) * original.worldX + view(9) * original.worldY + view(10) * original.worldZ + view(11))
        // Fixed world-space margin exceeds two pixels for this analytic fixture.
        if math.abs(depth - middle) > 0.08 then
          val keep = if nearPlane then depth > middle else depth < middle
          val picked = backend.pick(x + 0.5, y + 0.5).toOption.flatten
          val offset = ((size.height - 1 - y) * size.width + x) * 4
          require(picked.nonEmpty == keep, s"depth clipping pick disagrees: $perspective/$nearPlane depth=$depth")
          if !keep then
            require((0 until 4).forall(channel => pixels(offset + channel) == 255), s"clipped pixel retained color at $x,$y")
            removed += 1
          else
            val p = picked.get
            val (a, b, c) = clipped.meshes.head.sourceFaceVertices(p.face)
            val (wa, wb, wc) = p.barycentric.get
            val expected = evaluator.color(p.face, a, b, c, wa, wb, wc)
            val error = Vector(math.abs(pixels(offset).toInt - expected.red), math.abs(pixels(offset + 1).toInt - expected.green),
              math.abs(pixels(offset + 2).toInt - expected.blue)).max
            maximumError = math.max(maximumError, error)
            kept += 1
      require(kept > 100 && removed > 100, s"depth clipping lacks both regions: $kept/$removed")
      require(maximumError <= 2, s"depth clipping changed scalar interpolation: $maximumError")
      rows.push(js.Dynamic.literal(perspective = perspective, plane = (if nearPlane then "near" else "far"),
        kept = kept, removed = removed, maximumError = maximumError, passed = true))
    rows

  private def verifyUpdates(backend: ThreeSurfaceBackend, size: ThreeCanvasSize, canvas: js.Dynamic): js.Array[js.Dynamic] =
    val surface = SurfaceScalarFixture.Surface
    val original = SurfaceFaceFixture.geometry
    val white = SurfaceGeometry(original.mesh, original.hemisphere, SurfaceKind.White)
    val pial = SurfaceGeometry(TriangleMesh.fromRows(
      original.mesh.vertices.map(p => Vector(p.x * 0.9, p.y * 0.9, p.z + 0.2)),
      Vector((0, 1, 2), (0, 2, 3))), original.hemisphere, SurfaceKind.Pial)
    val asset = SurfaceAsset.make(surface, SurfaceSet.of(SurfaceKind.White, white, SurfaceKind.Pial -> pial)).toOption.get
    val scalar = SurfaceLayer.interpolatedScalar(SurfaceScalarFixture.Layer, surface, white,
      Array(-2, 4, 4, -2, -1, 3, 3, -1).map(_.toDouble), SurfaceScalarFixture.thresholded, frameCount = 2).toOption.get
    val nearest = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("nearest"), surface, white,
      Vector.fill(4)(Rgba32.unsafe(30, 60, 90)), interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    val model = SurfaceViewerModel.make(Vector(asset), Vector(nearest, scalar)).toOption.get
    var state = SurfaceFaceFixture.state(model)
    backend.render(SurfaceCompiler.compile(model, state).toOption.get, size).toOption.get
    val rows = new js.Array[js.Dynamic]()
    val actions = Vector(
      ("timepoint", SurfaceAdmissionPath.TimepointUpdate, Vector(SurfaceViewerAction.SetTimepoint(1))),
      ("threshold", SurfaceAdmissionPath.StyleUpdate, Vector(SurfaceViewerAction.SetLayerThreshold(SurfaceScalarFixture.Layer,
        DisplayThreshold.transparentBand(-0.5, 0.5).toOption.get))),
      ("camera", SurfaceAdmissionPath.CameraOnly, Vector(SurfaceViewerAction.OrbitBy(5, 5))),
      ("morph", SurfaceAdmissionPath.DerivedGeometryUpdate, Vector(SurfaceViewerAction.BeginGeometryMorph(surface, SurfaceKind.Pial),
        SurfaceViewerAction.SetGeometryMorphFraction(surface, SurfaceMorphFraction.unsafe(0.5))))
    )
    actions.foreach: (name, path, changes) =>
      changes.foreach(action => state = SurfaceViewer.reduce(model, state, action).toOption.get)
      val plan = SurfaceCompiler.compile(model, state).toOption.get
      val update = backend.renderObserved(plan, size, path).toOption.get
      require(update.native.geometryUploads == 0, s"$name rebuilt geometry")
      require(update.native.geometryUpdates == (if name == "morph" then 1 else 0), s"$name geometry update count")
      require(update.native.colorUploads == (if name == "timepoint" || name == "threshold" then 1 else 0), s"$name layer upload count")
      require(SurfaceBackendAdmission.validate(update.observation).isEmpty, s"$name admission failed")
      val pick = backend.pick(72.5, 62.5).toOption.flatten.get
      require(pick.face >= 0 && pick.face < 2 && pick.vertex >= 0 && pick.vertex < 4, s"$name pick lost scientific IDs")
      rows.push(js.Dynamic.literal(path = name, geometryUploads = update.native.geometryUploads,
        geometryUpdates = update.native.geometryUpdates, layerUploads = update.native.colorUploads,
        uploadedBytes = update.native.uploadedBytes.toDouble, pickPassed = true))
    val gl = canvas.applyDynamic("getContext")("webgl2")
    def pixels(): Uint8Array =
      val out = new Uint8Array(size.width * size.height * 4)
      gl.applyDynamic("readPixels")(0, 0, size.width, size.height, gl.RGBA, gl.UNSIGNED_BYTE, out)
      out
    val before = pixels()
    val keys = backend.resourceKeys
    val stats = backend.stats
    val invalidModel = SurfaceScalarFixture.model(values = Array(-1e100, 1e100, 1e100, -1e100))
    val invalid = SurfaceCompiler.compile(invalidModel, SurfaceFaceFixture.state(invalidModel)).toOption.get
    require(backend.render(invalid, size).isLeft)
    require(backend.resourceKeys == keys && backend.stats == stats, "rejected precision changed resource state")
    val after = pixels()
    require((0 until before.length).forall(i => before(i) == after(i)), "rejected precision changed the framebuffer")
    rows
