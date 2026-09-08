package scalafim.examples.surfaceview

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Uint8Array
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import scalafim.surface.view.three.*

/** Actual WebGL framebuffer and original-face ray evidence on pinned cortex.
  * The host supplies arrays exported with the existing JVM readers.
  */
object ThreeCorticalMapProbe:
  @JSExportTopLevel("runScalafimThreeCorticalMapProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic, fixture: js.Dynamic,
      modeName: String, perspective: Boolean, lit: Boolean, diagnosticStage: String): js.Dynamic =
    def doubles(values: js.Dynamic): Array[Double] = values.asInstanceOf[js.Array[Double]].toArray
    val indices = doubles(fixture.faces).map(_.toInt)
    def geometry(values: js.Dynamic, kind: SurfaceKind): SurfaceGeometry =
      SurfaceGeometry(TriangleMesh.fromArrays(doubles(values), indices), Hemisphere.Left, kind)
    val pial = geometry(fixture.pial, SurfaceKind.Pial)
    require(pial.vertexCount == 40962 && pial.faceCount == 81920)
    val surfaces = SurfaceSet.of(SurfaceKind.Pial, pial,
      SurfaceKind.White -> geometry(fixture.white, SurfaceKind.White),
      SurfaceKind.Inflated -> geometry(fixture.inflated, SurfaceKind.Inflated))
    val folding = SurfaceField(pial, Array.tabulate(pial.vertexCount)(identity), doubles(fixture.folding))
    val mode = CorticalMapMode.valueOf(modeName)
    val example = CorticalMapSemantics.build(surfaces, folding, mode)
    val runtime = ThreeJsRuntime.create(three, canvas).fold(e => throw new AssertionError(e.message), identity)
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    var state = example.state
    def reduce(action: SurfaceViewerAction): Unit =
      state = SurfaceViewer.reduce(example.model, state, action).fold(e => throw new AssertionError(e.message), identity)
    reduce(SurfaceViewerAction.SetProjection(if perspective then CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50))
      else CameraProjection.Orthographic(OrthographicScale.unsafe(100))))
    reduce(SurfaceViewerAction.FitCamera)
    reduce(SurfaceViewerAction.SetLighting(if lit then SurfaceLighting.directional(0.4, 0.6, -1, -0.2, 0.3).toOption.get
      else SurfaceLighting.Unlit))
    val rows = new js.Array[js.Dynamic]()
    val images = new js.Array[js.Dynamic]()
    try
      def check(action: String, width: Int, height: Int, noUploads: Boolean = false,
          noGeometry: Boolean = false): Unit =
        val plan = SurfaceCompiler.compile(example.model, state).fold(e => throw new AssertionError(e.message), identity)
        val receipt = backend.render(plan, ThreeCanvasSize.unsafe(width, height), forceDraw = true)
          .fold(e => throw new AssertionError(e.message), identity)
        if noUploads then require(receipt.uploadedBytes == 0, s"$action unexpectedly uploaded resources")
        if noGeometry then require(receipt.geometryUploads == 0 && receipt.geometryUpdates == 0,
          s"$action unexpectedly uploaded geometry")
        if diagnosticStage.isEmpty || diagnosticStage == action then
          val row = verify(plan, backend, runtime, canvas, width, height)
          row.updateDynamic("action")(action)
          row.updateDynamic("geometryUploads")(receipt.geometryUploads)
          row.updateDynamic("geometryUpdates")(receipt.geometryUpdates)
          row.updateDynamic("colorUploads")(receipt.colorUploads)
          row.updateDynamic("uploadedBytes")(receipt.uploadedBytes.toDouble)
          row.updateDynamic("renderNanos")(receipt.elapsedNanos.toDouble)
          rows.push(row)
          js.Dynamic.global.console.log("cortical_stage=" + js.JSON.stringify(row))
          if action == "static-768" || action == "morph-end" then
            images.push(js.Dynamic.literal(action = action, image = canvas.applyDynamic("toDataURL")("image/png")))
      for size <- Vector(384, 768, 1024) do check(s"static-$size", size, size)
      val surface = CorticalMapSemantics.Surface
      val layer = CorticalMapSemantics.Overlay
      val changes = Vector(
        "unchanged" -> Vector.empty,
        "camera" -> Vector(SurfaceViewerAction.OrbitBy(5, 2)),
        "timepoint" -> Vector(SurfaceViewerAction.SetTimepoint(1)),
        "opacity" -> Vector(SurfaceViewerAction.SetLayerOpacity(layer, DisplayOpacity.unsafe(0.6)))) ++
        (if mode == CorticalMapMode.Face || mode == CorticalMapMode.Nearest then Vector.empty else
          Vector("threshold" -> Vector(SurfaceViewerAction.SetLayerThreshold(layer,
            DisplayThreshold.transparentBand(-0.75, 0.75).toOption.get)))) ++
        Vector(
          "morph-half" -> Vector(SurfaceViewerAction.BeginGeometryMorph(surface, SurfaceKind.Inflated),
            SurfaceViewerAction.SetGeometryMorphFraction(surface, SurfaceMorphFraction.unsafe(0.5)), SurfaceViewerAction.FitCamera),
          "morph-end" -> Vector(SurfaceViewerAction.SetGeometryMorphFraction(surface, SurfaceMorphFraction.unsafe(1)), SurfaceViewerAction.FitCamera),
          "white" -> Vector(SurfaceViewerAction.SetGeometryState(surface, SurfaceKind.White), SurfaceViewerAction.FitCamera),
          "restored" -> Vector(SurfaceViewerAction.SetGeometryState(surface, SurfaceKind.Pial), SurfaceViewerAction.FitCamera))
      for (name, actions) <- changes do
        actions.foreach(reduce)
        check(name, 768, 768, noUploads = name == "unchanged" || name == "camera",
          noGeometry = Set("timepoint", "opacity", "threshold")(name))
      check("resize", 800, 600, noUploads = true)
      val resources = backend.resourceKeys.size
      backend.dispose().fold(e => throw new AssertionError(e.message), identity)
      require(backend.resourceKeys.isEmpty, "disposed cortical backend retained resources")
      js.Dynamic.literal(mode = modeName, perspective = perspective, lit = lit, diagnosticStage = diagnosticStage, cases = rows,
        images = images, resourcesBeforeDispose = resources, resourcesAfterDispose = backend.resourceKeys.size)
    finally backend.dispose()

  private def verify(plan: SurfaceRenderPlan, backend: ThreeSurfaceBackend, runtime: ThreeJsRuntime, canvas: js.Dynamic,
      width: Int, height: Int): js.Dynamic =
    val gl = canvas.applyDynamic("getContext")("webgl2")
    val pixels = new Uint8Array(width * height * 4)
    gl.applyDynamic("readPixels")(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, pixels)
    require(gl.applyDynamic("getError")().asInstanceOf[Int] == 0, "WebGL framebuffer read failed")
    val reference = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(width, height),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    var intersection = 0
    var union = 0
    var foreground = 0
    var checked = 0
    var maximumError = 0
    var maximumFootprintError = 0
    var maximumSmoothError = 0
    val occlusionEdges = new js.Array[js.Dynamic]()
    val mappingEdges = new js.Array[js.Dynamic]()
    val nearestEdges = new js.Array[js.Dynamic]()
    val displayCellEdges = new js.Array[js.Dynamic]()
    val failures = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int)]
    val candidates = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, SurfacePick)]
    require(plan.meshes.size == 1, "cortical probe expects one mesh")
    val mesh = plan.meshes.head
    val evaluator = new SurfaceFragmentEvaluator(mesh, plan.layers, plan.lighting, Rgba32.unsafe(184, 184, 184))
    def renderCell(p: ThreePick): (Int, Vector[Double]) =
      val (wa, wb, wc) = p.barycentric.get
      mesh.nearestPartition match
        case None => (p.face, Vector(wa, wb, wc))
        case Some(partition) =>
          (0 until 6).iterator.map: cell =>
            val face = p.face * 6 + cell
            val (ax, ay, _) = partition.weights(face * 3)
            val (bx, by, _) = partition.weights(face * 3 + 1)
            val (cx, cy, _) = partition.weights(face * 3 + 2)
            val determinant = (by - cy) * (ax - cx) + (cx - bx) * (ay - cy)
            val a = ((by - cy) * (wa - cx) + (cx - bx) * (wb - cy)) / determinant
            val b = ((cy - ay) * (wa - cx) + (ax - cx) * (wb - cy)) / determinant
            (face, Vector(a, b, 1 - a - b))
          .find(_._2.forall(_ >= -1e-8)).get
    def referenceColor(p: ThreePick): Rgba32 =
      val (wa, wb, wc) = p.barycentric.get
      if plan.fragmentSurfaces(mesh.surface) then
        val (a, b, c) = mesh.sourceFaceVertices(if mesh.nearestPartition.nonEmpty then p.face * 6 else p.face)
        evaluator.color(p.face, a, b, c, wa, wb, wc)
      else
        // Pure categorical plans retain precomposed render-corner colors.
        // Recover the containing display cell from original barycentrics;
        // the fragment evaluator intentionally has no sample payload here.
        val (face, weights) = renderCell(p)
        val corners = Vector.tabulate(3): corner =>
          val vertex = mesh.indices(face * 3 + corner)
          val composed = plan.layers.foldLeft(Rgba32.unsafe(184, 184, 184)): (under, layer) =>
            layer.blendMode.composite(under, Rgba32.fromPackedInt(layer.colors(vertex)), layer.opacity)
          val factor = plan.lighting match
            case SurfaceLighting.Unlit => 1.0
            case SurfaceLighting.Directional(ambient, diffuse, dx, dy, dz) =>
              val nx = mesh.normals(vertex * 3).toDouble
              val ny = mesh.normals(vertex * 3 + 1).toDouble
              val nz = mesh.normals(vertex * 3 + 2).toDouble
              val dot = math.max(0.0, nx * dx + ny * dy + nz * dz)
              math.min(1.0, ambient.value + diffuse.value * dot)
          (composed, factor)
        def channel(f: Rgba32 => Int): Int = math.round(corners.zip(weights).map:
          case ((color, factor), weight) => f(color) * factor * weight
        .sum).toInt.max(0).min(255)
        Rgba32.unsafe(channel(_.red), channel(_.green), channel(_.blue))
    val slot = plan.viewportFit.resolve(plan.slots, width.toDouble, height.toDouble).head
    val triangles = scala.collection.mutable.Map.empty[Int, Vector[(Double, Double)]]
    def projected(face: Int): Vector[(Double, Double)] = triangles.getOrElseUpdate(face,
      Vector.tabulate(3): corner =>
        val index = (if mesh.nearestPartition.nonEmpty then face * 18 + corner * 6 else mesh.indices(face * 3 + corner)) * 3
        def multiply(matrix: FloatBufferView, p: Vector[Double]): Vector[Double] =
          Vector.tabulate(4)(row => (0 until 4).map(column => matrix(row * 4 + column) * p(column)).sum)
        val world = Vector(mesh.positions(index) + slot.worldOffsetX, mesh.positions(index + 1) + slot.worldOffsetY,
          mesh.positions(index + 2) + slot.worldOffsetZ, 1.0)
        val clip = multiply(plan.camera.projectionMatrix, multiply(plan.camera.viewMatrix, world))
        ((slot.viewport.x + (clip(0) / clip(3) + 1) * 0.5 * slot.viewport.width) * width,
          (slot.viewport.y + (1 - clip(1) / clip(3)) * 0.5 * slot.viewport.height) * height))
    def containsPixel(face: Int, x: Int, y: Int): Boolean =
      val triangle = projected(face)
      def cross(a: (Double, Double), b: (Double, Double), p: (Double, Double)): Double =
        (b._1 - a._1) * (p._2 - a._2) - (b._2 - a._2) * (p._1 - a._1)
      val area = cross(triangle(0), triangle(1), triangle(2))
      area != 0 && Vector((x.toDouble, y.toDouble), (x + 1.0, y.toDouble),
        (x.toDouble, y + 1.0), (x + 1.0, y + 1.0)).forall: point =>
        (0 until 3).forall(i => cross(triangle(i), triangle((i + 1) % 3), point) / area > 1e-6)
    for y <- 1 until height - 1; x <- 1 until width - 1 do
      val offset = ((height - 1 - y) * width + x) * 4
      val expected = reference.image.pixelUnsafe(x, y)
      val refForeground = expected.toPackedInt != -1
      val nativeForeground = pixels(offset) != 255 || pixels(offset + 1) != 255 || pixels(offset + 2) != 255
      if refForeground then foreground += 1
      if refForeground || nativeForeground then union += 1
      if refForeground && nativeForeground then intersection += 1
      reference.pick(x, y).toOption.flatten.foreach: pick =>
        val neighbors = for dy <- -1 to 1; dx <- -1 to 1 yield (x + dx, y + dy)
        // A 3x3 same-face requirement excludes valid small triangles entirely
        // on inflated cortex. Test the complete pixel footprint geometrically.
        val sameFace = containsPixel(pick.face, x, y)
        val smooth = neighbors.forall: (nx, ny) =>
          val c = reference.image.pixelUnsafe(nx, ny)
          math.max(math.abs(c.red - expected.red), math.max(math.abs(c.green - expected.green), math.abs(c.blue - expected.blue))) <= 12
        val error = math.max(math.abs(pixels(offset).toInt - expected.red),
          math.max(math.abs(pixels(offset + 1).toInt - expected.green), math.abs(pixels(offset + 2).toInt - expected.blue)))
        if smooth then maximumSmoothError = math.max(maximumSmoothError, error)
        if sameFace && smooth then
          maximumFootprintError = math.max(maximumFootprintError, error)
          val occlusion = if error <= 3 then false else
            val locations = (for dy <- Vector(0.125, 0.375, 0.625, 0.875); dx <- Vector(0.125, 0.375, 0.625, 0.875)
              yield (dx, dy)) :+ (0.5, 0.5)
            val rays = locations.map: (dx, dy) =>
              backend.pick(x + dx, y + dy).toOption.flatten
            val colors = rays.flatten.map(referenceColor)
            val channels = Vector(colors.map(_.red), colors.map(_.green), colors.map(_.blue))
            // The original triangle covers the whole pixel, yet foreground
            // rays hit another face inside it. This is a subpixel occlusion
            // band, not a fully visible triangle interior. Require center ID
            // agreement and a plausible color mixture; retain its raw error.
            val matchingCenter = rays.last.exists(p => p.face == pick.face && p.surface == pick.surface)
            val mixture = rays.forall(_.nonEmpty) && matchingCenter && channels.zipWithIndex.forall: (values, channel) =>
                pixels(offset + channel).toInt >= values.min - 2 && pixels(offset + channel).toInt <= values.max + 2
            val occluded = rays.flatten.exists(_.face != pick.face)
            val crossesMapping = !occluded && plan.layers.exists: layer =>
              layer.scalarField.exists: field =>
                val renderFace = if mesh.nearestPartition.nonEmpty then pick.face * 6 else pick.face
                val (a, b, c) = mesh.sourceFaceVertices(renderFace)
                val values = rays.flatten.map: p =>
                  val (wa, wb, wc) = p.barycentric.get
                  SurfaceScalarInterpolation.value(field.samples(a), field.samples(b), field.samples(c), wa, wb, wc)
                values.nonEmpty && SurfaceMappingPartition.boundaries(field.mapping).exists(boundary => values.min <= boundary && values.max >= boundary)
            val crossesNearest = !occluded && mesh.nearestPartition.nonEmpty &&
              rays.last.exists(_.vertex == pick.vertex) && rays.flatten.exists(_.vertex != pick.vertex)
            val cells = if mesh.nearestPartition.isEmpty then Vector.empty else rays.flatten.map(p => renderCell(p)._1)
            val crossesCell = !occluded && cells.distinct.size > 1
            val edge = mixture && (occluded || crossesMapping || crossesNearest || crossesCell)
            if edge then (if occluded then occlusionEdges else if crossesNearest then nearestEdges
              else if crossesCell then displayCellEdges else mappingEdges)
              .push(js.Dynamic.literal(x = x, y = y, referenceFace = pick.face,
                nativeFaces = js.Array(rays.flatten.map(_.face)*),
                nativeVertices = js.Array(rays.flatten.map(_.vertex)*),
                nativeRenderFaces = js.Array(cells*), rawError = error))
            else js.Dynamic.global.console.log(s"cortical_outlier=$x,$y face=${pick.face} error=$error expected=$expected actual=${pixels(offset)},${pixels(offset + 1)},${pixels(offset + 2)} colors=${colors.mkString(",")} rays=${rays.mkString(";")}")
            if !edge then
              failures += ((x, y, pick.face))
              val source = mesh.sourceFaceVertices(if mesh.nearestPartition.nonEmpty then pick.face * 6 else pick.face)
              val nearby = neighbors.map: (nx, ny) =>
                reference.pick(nx, ny).toOption.flatten.map: p =>
                  val ids = mesh.sourceFaceVertices(if mesh.nearestPartition.nonEmpty then p.face * 6 else p.face)
                  s"$nx,$ny face=${p.face} vertices=$ids"
              js.Dynamic.global.console.log(s"cortical_neighbors=source=$source nearby=${nearby.mkString(";")}")
            edge
          if !occlusion then
            maximumError = math.max(maximumError, error)
            checked += 1
        val weights = Vector(pick.barycentricA, pick.barycentricB, pick.barycentricC).sorted
        if weights.head > 0.12 && weights(2) - weights(1) > 0.05 then candidates += ((x, y, pick))
    var maximumPickError = 0.0
    require(candidates.size >= 64, s"insufficient original-face pick candidates: ${candidates.size}")
    for index <- 0 until 64 do
      val (x, y, expected) = candidates(index * (candidates.size - 1) / 63)
      val actual = backend.pick(x + 0.5, y + 0.5).fold(e => throw new AssertionError(e.message), identity).get
      require(actual.surface == expected.surface && actual.face == expected.face && actual.vertex == expected.vertex,
        s"cortical WebGL pick mismatch at $x,$y: $actual versus $expected")
      val (a, b, c) = actual.barycentric.get
      maximumPickError = math.max(maximumPickError, math.max(math.abs(a - expected.barycentricA),
        math.max(math.abs(b - expected.barycentricB), math.abs(c - expected.barycentricC))))
    val iou = intersection.toDouble / union
    if failures.nonEmpty then
      js.Dynamic.global.window.updateDynamic("corticalFailureImage")(canvas.applyDynamic("toDataURL")("image/png"))
      val original = if plan.fragmentSurfaces(mesh.surface) then ThreeFragmentShader.compile(mesh, plan.layers).toOption.get
        else ThreeFragmentPacket(mesh.surface, Vector.empty,
          "void main() { gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0); }", "", Vector.empty)
      require(gl.applyDynamic("getParameter")(gl.SAMPLES).asInstanceOf[Int] == 4, "diagnostic requires four samples")
      val decoded = Array.fill(failures.size, 4)(0)
      gl.applyDynamic("enable")(gl.SAMPLE_COVERAGE)
      try
        for bitGroup <- 0 until 8 do
          val values = Array.tabulate(mesh.indices.length * 4): index =>
            val channel = index % 4
            if channel == 3 then 1.0f else
              val id = mesh.sourceFace(index / 12) + 1
              ((id >>> (bitGroup * 3 + channel)) & 1).toFloat
          val packet = original.copy(
            vertexShader = "attribute vec4 aDebugFace; varying vec3 vDebugFace;\n" +
              original.vertexShader.replace("void main() {", "void main() { vDebugFace = aDebugFace.xyz;"),
            fragmentShader = "varying vec3 vDebugFace; void main() { gl_FragColor = vec4(vDebugFace, 1.0); }",
            attributes = original.attributes :+ ThreeFragmentAttribute("aDebugFace", values))
          runtime.uploadFragments(Vector(packet)).toOption.get
          val previous = Array.fill(failures.size, 3)(255)
          for level <- 1 to 4 do
            gl.applyDynamic("sampleCoverage")(level / 4.0, false)
            runtime.draw().toOption.get
            failures.zipWithIndex.foreach: (failure, index) =>
              val (x, y, face) = failure
              val sample = new Uint8Array(4)
              gl.applyDynamic("readPixels")(x, height - 1 - y, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, sample)
              for channel <- 0 until 3 do
                val difference = previous(index)(channel) - sample(channel).toInt
                require(math.abs(difference) <= 1 || math.abs(difference - 64) <= 1,
                  s"non-nested native sample coverage: $difference")
                if difference < 32 then decoded(index)(level - 1) |= 1 << (bitGroup * 3 + channel)
                previous(index)(channel) = sample(channel).toInt
      finally
        gl.applyDynamic("sampleCoverage")(1.0, false)
        gl.applyDynamic("disable")(gl.SAMPLE_COVERAGE)
      failures.indices.foreach: index =>
        js.Dynamic.global.console.log(s"cortical_gpu_id=${failures(index)} sampleFaces=${decoded(index).map(_ - 1).mkString(",")} samples=${gl.applyDynamic("getParameter")(gl.SAMPLES)}")
        val (x, y, face) = failures(index)
        val faces = (Vector(face) ++ decoded(index).map(_ - 1)).distinct.map: sourceFace =>
          val points = Vector.tabulate(3): corner =>
            val offset = mesh.indices(sourceFace * 3 + corner) * 3
            js.Array(mesh.positions(offset).toDouble, mesh.positions(offset + 1).toDouble, mesh.positions(offset + 2).toDouble)
          val (a, b, c) = mesh.sourceFaceVertices(sourceFace)
          val values = plan.layers.head.scalarField.map(f => js.Array(f.samples(a), f.samples(b), f.samples(c))).getOrElse(js.Array[Double]())
          val normals = mesh.sampleNormals.getOrElse(mesh.normals)
          val normalIds = if mesh.sampleNormals.nonEmpty then Vector(a, b, c)
            else Vector.tabulate(3)(corner => mesh.indices(sourceFace * 3 + corner))
          val ns = normalIds.map(id => js.Array(normals(id * 3).toDouble, normals(id * 3 + 1).toDouble, normals(id * 3 + 2).toDouble))
          val layerData = plan.layers.map: layer =>
            js.Dynamic.literal(policy = layer.interpolation.toString, opacity = layer.opacity.toDouble,
              values = layer.scalarField.map(f => js.Array(f.samples(a), f.samples(b), f.samples(c))).getOrElse(js.Array[Double]()),
              colors = layer.sampleColors.map(cs => js.Array(cs(a), cs(b), cs(c)))
                .getOrElse(js.Array(Vector.tabulate(3)(corner => layer.colors(mesh.indices(sourceFace * 3 + corner)))*)))
          js.Dynamic.literal(face = sourceFace, points = js.Array(points*), values = values,
            normals = js.Array(ns*), layers = js.Array(layerData*))
        val geometry = js.Dynamic.literal(x = x, y = y, width = width, height = height, faces = js.Array(faces*),
          view = js.Array(plan.camera.viewMatrix.unsafeArray.map(_.toDouble)*),
          projection = js.Array(plan.camera.projectionMatrix.unsafeArray.map(_.toDouble)*),
          viewport = js.Array(slot.viewport.x, slot.viewport.y, slot.viewport.width, slot.viewport.height),
          offset = js.Array(slot.worldOffsetX, slot.worldOffsetY, slot.worldOffsetZ))
        js.Dynamic.global.window.updateDynamic("corticalFailureGeometry")(geometry)
      if plan.fragmentSurfaces(mesh.surface) then
        runtime.uploadFragments(Vector(original)).toOption.get
        runtime.updateLighting(plan.lighting).toOption.get
        runtime.draw().toOption.get
    require(foreground > width * height * 0.1 && checked > 0, s"insufficient cortical coverage: $foreground / $checked")
    require(iou >= 0.99 && maximumError <= 3 && maximumPickError <= 1e-3,
      s"cortical WebGL budget exceeded: IoU=$iou error=$maximumError pick=$maximumPickError smooth=$maximumSmoothError checked=$checked")
    js.Dynamic.literal(width = width, height = height, foreground = foreground, checkedPixels = checked,
      maximumChannelError = maximumError, maximumSmoothError = maximumSmoothError, maskIoU = iou,
      maximumFootprintError = maximumFootprintError, occlusionEdges = occlusionEdges, mappingEdges = mappingEdges,
      nearestEdges = nearestEdges, displayCellEdges = displayCellEdges,
      checkedPicks = 64, maximumBarycentricError = maximumPickError)
