package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import _root_.javafx.application.Platform
import _root_.javafx.scene.{Group, Scene as FxScene, SceneAntialiasing, SnapshotParameters, SubScene}
import _root_.javafx.scene.image.{PixelFormat, WritableImage}
import _root_.javafx.scene.paint.Color

/** Real bilateral cortex for the face-flat texel encodings (round 2).
  *
  * Inputs are the retained scene of the scalar lookup spike (JavaFxScalarLutCortexProbe):
  * inflated geometry, normals, camera and slots at 1350 x 762, raw beta/FIR values and
  * FreeSurfer sulcal depth. Face plans declare face-flat layers: the binary curvature
  * underlay reduces by its mean, the overlay (saturated onset at the cutoff, transparent
  * below it) by `probe.faceMode` (`mean` or `max`). `probe.cutoffQuantile` replaces the
  * scene cutoff by that quantile of |finite values| (0.9 is the application default);
  * `probe.zoom` scales the orthographic camera about the view centre.
  *
  * modes:
  *  - `stats` (no JavaFX): suprathreshold vertices and same-sign clusters shown or hidden
  *    by each face rule, and coloured surface area against the interpolated reference;
  *  - `geometry` (no JavaFX): reference face ids, edge distances and adjacency;
  *  - `reference` (no JavaFX): rule colours by face id, flat and interpolated rasters;
  *  - `native`: one encoding (`AdaptiveAffineOpaque` renders the vertex-colour plan);
  *    memory, first show, 1,200 picks and, with `bench`, 20 timed palette, cutoff and
  *    map-value updates with the phases separated.
  *
  * args: mode encoding scene output antialiasing lighting [bench]
  */
object JavaFxFaceTexelCortexProbe:
  import JavaFxScalarLutCortexProbe.{Inputs, load, loadValues, overlayMapping, underlayMapping, finalPlan}
  private val Width = 1350
  private val Height = 762

  private def hemi(surface: SurfaceId): String = if surface.value.endsWith("lh") then "lh" else "rh"
  def modeName(reduction: SurfaceFaceReduction): String = if reduction == SurfaceFaceReduction.Mean then "mean" else "max"
  def reductionProperty: SurfaceFaceReduction = System.getProperty("probe.faceMode", "mean") match
    case "mean" => SurfaceFaceReduction.Mean
    case "max" => SurfaceFaceReduction.MaxMagnitude
    case other => throw new IllegalArgumentException(s"unknown face mode $other")

  def quantile(values: Map[SurfaceId, Array[Double]], q: Double): Double =
    val magnitudes = values.values.flatten.filter(_.isFinite).map(math.abs).toArray.sorted
    magnitudes(math.max(0, math.ceil(q * magnitudes.length).toInt - 1))

  def cutoffFor(inputs: Inputs): (Double, String) =
    Option(System.getProperty("probe.cutoffQuantile")).fold((inputs.cutoff, "qscene"))(q => (quantile(inputs.values, q.toDouble), s"q$q"))

  def zoomed(plan: SurfaceRenderPlan, zoom: Double): SurfaceRenderPlan =
    if zoom == 1.0 then plan
    else
      val p = plan.camera.projectionMatrix.unsafeArray.clone()
      var k = 0
      while k < 8 do
        p(k) = (p(k) * zoom).toFloat
        k += 1
      plan.copy(camera = plan.camera.copy(projectionMatrix = new FloatBufferView(p)))

  /** The same split scale and saturated onset with different hues. */
  def paletteMapping(range: Double, cutoff: Double): ScalarMapping =
    ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-range, range), 0.0, -cutoff, cutoff,
      ScalarRamp.linear(Rgba32.unsafe(120, 0, 200), Rgba32.unsafe(220, 160, 255)),
      ScalarRamp.linear(Rgba32.unsafe(0, 150, 60), Rgba32.unsafe(190, 255, 140))).toOption.get)

  def facePlan(inputs: Inputs, values: Map[SurfaceId, Array[Double]], over: ScalarMapping, lighting: SurfaceLighting,
      reduction: SurfaceFaceReduction): SurfaceRenderPlan =
    val under = underlayMapping(inputs.sulcLimit)
    JavaFxScalarLutCortexProbe.compileWith(inputs, lighting, (mesh, geometry) => Vector(
      SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe(s"cortex-${hemi(mesh.surface)}"), mesh.surface, geometry,
        inputs.sulc(mesh.surface), under, SurfaceFaceReduction.Mean).toOption.get,
      SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe(s"result-${hemi(mesh.surface)}"), mesh.surface, geometry,
        values(mesh.surface), over, reduction).toOption.get))

  final case class Rule(colors: Map[SurfaceId, Array[Int]], faces: Int, maxDifference: Int, offByOne: Int, missingFaces: Int, belowCutoffFaces: Int)

  /** Rule colours per surface, checked face by face against the recipe written out directly. */
  def rule(inputs: Inputs, plan: SurfaceRenderPlan, values: Map[SurfaceId, Array[Double]], over: ScalarMapping,
      reduction: SurfaceFaceReduction): Rule =
    val under = underlayMapping(inputs.sulcLimit)
    var faces = 0; var maxDifference = 0; var offByOne = 0; var missing = 0; var below = 0
    val colors = plan.meshes.map: mesh =>
      val computed = SurfaceFaceTexels.colors(plan, mesh).fold(e => throw new IllegalStateException(e.message), identity)
      val v = values(mesh.surface)
      val s = inputs.sulc(mesh.surface)
      val indices = mesh.indices
      var face = 0
      while face < computed.length do
        val corners = Vector(indices(face * 3), indices(face * 3 + 1), indices(face * 3 + 2))
        def mean(x: Array[Double]): Double =
          if corners.forall(i => x(i).isFinite) then corners.map(x(_)).sum / 3 else Double.NaN
        val value = reduction match
          case SurfaceFaceReduction.Mean => mean(v)
          case SurfaceFaceReduction.MaxMagnitude =>
            val finite = corners.map(v(_)).filter(_.isFinite)
            if finite.isEmpty then Double.NaN else finite.filter(x => math.abs(x) == finite.map(math.abs).max).max
        if value.isNaN then missing += 1 else if over.color(value).alpha == 0 then below += 1
        val expected = DisplayBlendMode.Normal.composite(
          DisplayBlendMode.Normal.composite(SurfaceFaceTexels.Base, under.color(mean(s)), DisplayOpacity.Opaque),
          over.color(value), DisplayOpacity.Opaque).toPackedInt
        val difference = Vector(24, 16, 8, 0).map(shift => math.abs(((expected >>> shift) & 255) - ((computed(face) >>> shift) & 255))).max
        if difference > maxDifference then maxDifference = difference
        if difference == 1 then offByOne += 1
        face += 1
      faces += computed.length
      mesh.surface -> computed
    .toMap
    require(maxDifference <= 1, s"face rule differs from the recipe by $maxDifference")
    Rule(colors, faces, maxDifference, offByOne, missing, below)

  /** Area of the part of a triangle where the linear field with corner values (a, b, c) is >= t. */
  private def fractionAtLeast(a: Double, b: Double, c: Double, t: Double): Double =
    val px = Array(0.0, 1.0, 0.0); val py = Array(0.0, 0.0, 1.0); val g = Array(a - t, b - t, c - t)
    val xs = scala.collection.mutable.ArrayBuffer.empty[Double]
    val ys = scala.collection.mutable.ArrayBuffer.empty[Double]
    for k <- 0 until 3 do
      val j = (k + 1) % 3
      if g(k) >= 0 then { xs += px(k); ys += py(k) }
      if (g(k) >= 0) != (g(j) >= 0) then
        val s = g(k) / (g(k) - g(j))
        xs += px(k) + s * (px(j) - px(k)); ys += py(k) + s * (py(j) - py(k))
    if xs.length < 3 then 0.0
    else
      var area = 0.0
      for k <- xs.indices do
        val j = (k + 1) % xs.length
        area += xs(k) * ys(j) - xs(j) * ys(k)
      math.abs(area) / 2 / 0.5

  private final class UnionFind(n: Int):
    private val parent = Array.tabulate(n)(identity)
    def find(x: Int): Int =
      var r = x
      while parent(r) != r do r = parent(r)
      var y = x
      while parent(y) != r do
        val next = parent(y); parent(y) = r; y = next
      r
    def union(a: Int, b: Int): Unit =
      val (ra, rb) = (find(a), find(b))
      if ra != rb then parent(ra) = rb

  /** Visibility statistics of both face rules at one cutoff, as JSON. */
  def stats(inputs: Inputs, values: Map[SurfaceId, Array[Double]], cutoff: Double): String =
    val over = overlayMapping(inputs.range, cutoff)
    def visible(x: Double): Boolean = x.isFinite && over.color(x).alpha > 0
    val rows = SurfaceFaceReduction.values.toVector.map: reduction =>
      var supra = 0L; var supraHidden = 0L; var oppositeOnly = 0L
      var clusters = 0L; var clustersHidden = 0L; var singles = 0L; var singlesHidden = 0L
      var totalArea = 0.0; var flatPos = 0.0; var flatNeg = 0.0; var refPos = 0.0; var refNeg = 0.0
      var agree = 0.0; var flatFaces = 0L
      inputs.scene.meshes.foreach: mesh =>
        val v = values(mesh.surface)
        val n = v.length
        val faces = mesh.indices.length / 3
        val sign = Array.tabulate(n)(i => if visible(v(i)) then math.signum(v(i)).toInt else 0)
        val uf = new UnionFind(n)
        val shown = new Array[Boolean](n)
        val touched = new Array[Boolean](n)
        val p = mesh.positions
        var face = 0
        while face < faces do
          val (a, b, c) = (mesh.indices(face * 3), mesh.indices(face * 3 + 1), mesh.indices(face * 3 + 2))
          for (x, y) <- Vector((a, b), (b, c), (c, a)) if sign(x) != 0 && sign(x) == sign(y) do uf.union(x, y)
          val reduced = reduction.reduce(v(a), v(b), v(c))
          val faceSign = if visible(reduced) then math.signum(reduced).toInt else 0
          for corner <- Vector(a, b, c) if sign(corner) != 0 && faceSign != 0 do
            touched(corner) = true
            if faceSign == sign(corner) then shown(corner) = true
          def d(i: Int, k: Int): Double = p(i * 3 + k).toDouble
          val (ux, uy, uz) = (d(b, 0) - d(a, 0), d(b, 1) - d(a, 1), d(b, 2) - d(a, 2))
          val (wx, wy, wz) = (d(c, 0) - d(a, 0), d(c, 1) - d(a, 1), d(c, 2) - d(a, 2))
          val area = 0.5 * math.sqrt(math.pow(uy * wz - uz * wy, 2) + math.pow(uz * wx - ux * wz, 2) + math.pow(ux * wy - uy * wx, 2))
          totalArea += area
          val (rp, rn) =
            if v(a).isFinite && v(b).isFinite && v(c).isFinite then
              (fractionAtLeast(v(a), v(b), v(c), cutoff), fractionAtLeast(-v(a), -v(b), -v(c), cutoff))
            else (0.0, 0.0)
          refPos += rp * area; refNeg += rn * area
          if faceSign > 0 then { flatPos += area; agree += rp * area; flatFaces += 1 }
          else if faceSign < 0 then { flatNeg += area; agree += rn * area; flatFaces += 1 }
          face += 1
        val members = scala.collection.mutable.Map.empty[Int, (Int, Boolean)]
        var i = 0
        while i < n do
          if sign(i) != 0 then
            supra += 1
            if !shown(i) then supraHidden += 1
            if !shown(i) && touched(i) then oppositeOnly += 1
            val root = uf.find(i)
            val (count, any) = members.getOrElse(root, (0, false))
            members(root) = (count + 1, any || shown(i))
          i += 1
        members.values.foreach: (count, any) =>
          clusters += 1
          if !any then clustersHidden += 1
          if count == 1 then { singles += 1; if !any then singlesHidden += 1 }
      val refArea = refPos + refNeg
      val flatArea = flatPos + flatNeg
      f"""{"rule":"${modeName(reduction)}","suprathresholdVertices":$supra,"suprathresholdVerticesHidden":$supraHidden,""" +
        f""""hiddenVerticesShownOnlyWithOppositeSign":$oppositeOnly,"clusters":$clusters,"clustersHidden":$clustersHidden,""" +
        f""""singleVertexClusters":$singles,"singleVertexClustersHidden":$singlesHidden,"colouredFaces":$flatFaces,""" +
        f""""surfaceArea":$totalArea%.3f,"flatColouredArea":$flatArea%.3f,"interpolatedColouredArea":$refArea%.3f,""" +
        f""""overstatementRatio":${flatArea / refArea}%.4f,"flatPositive":$flatPos%.3f,"flatNegative":$flatNeg%.3f,""" +
        f""""interpolatedPositive":$refPos%.3f,"interpolatedNegative":$refNeg%.3f,"sameSignOverlap":$agree%.3f,""" +
        f""""flatOnlyArea":${flatArea - agree}%.3f,"interpolatedOnlyArea":${refArea - agree}%.3f,"jaccard":${agree / (flatArea + refArea - agree)}%.4f}"""
    s"""{"cutoff":$cutoff,"range":${inputs.range},"rules":[${rows.mkString(",")}]}"""

  private def argb(image: WritableImage): Array[Int] =
    val pixels = new Array[Int](Width * Height)
    image.getPixelReader.getPixels(0, 0, Width, Height, PixelFormat.getIntArgbInstance(), pixels, 0, Width)
    pixels

  private def signature(pixels: Array[Int]): Long =
    var hash = 0xcbf29ce484222325L
    var index = 0
    while index < pixels.length do
      hash = (hash ^ pixels(index).toLong) * 0x100000001b3L
      index += 1
    hash

  private def meshSignature(chunks: Vector[JavaFxSurfaceChunk]): Long =
    var hash = 0xcbf29ce484222325L
    def mix(value: Int): Unit = hash = (hash ^ value.toLong) * 0x100000001b3L
    chunks.foreach: chunk =>
      chunk.mesh.getPoints.toArray(null: Array[Float]).foreach(v => mix(java.lang.Float.floatToIntBits(v)))
      chunk.mesh.getNormals.toArray(null: Array[Float]).foreach(v => mix(java.lang.Float.floatToIntBits(v)))
      chunk.mesh.getTexCoords.toArray(null: Array[Float]).foreach(v => mix(java.lang.Float.floatToIntBits(v)))
      chunk.mesh.getFaces.toArray(null: Array[Int]).foreach(mix)
    hash

  private def median(values: Vector[Double]): Double = values.sorted.apply(values.length / 2)
  private def stats(values: Vector[Double]): String =
    val sorted = values.sorted
    def at(p: Double) = sorted(math.min(sorted.length - 1, (sorted.length * p).toInt))
    f"""{"n":${sorted.length},"median":${at(0.5)}%.3f,"p90":${at(0.9)}%.3f,"max":${sorted.last}%.3f,"min":${sorted.head}%.3f}"""

  private def origin(name: String): String = Class.forName(name).getProtectionDomain.getCodeSource.getLocation.toString

  def main(args: Array[String]): Unit =
    val mode = args(0)
    val encoding = JavaFxAtlasEncoding.valueOf(args(1))
    val sceneName = args(2)
    val output = Path.of(args(3))
    val antialiasing = if args(4) == "DISABLED" then SceneAntialiasing.DISABLED else SceneAntialiasing.BALANCED
    val lit = args(5) == "lit"
    val bench = args.lift(6).contains("bench")
    val inputsDir = Path.of(System.getProperty("probe.inputs", "/Users/bbuchsbaum/code/scala/plsneuro-fixtures/spike-lut-1/inputs"))
    Files.createDirectories(output)
    val lightingName = if lit then "lit" else "unlit"
    val lighting = if lit then SurfaceLighting.Default else SurfaceLighting.Unlit
    val inputs = load(inputsDir, sceneName)
    val reduction = reductionProperty
    val (cutoff, cutoffTag) = cutoffFor(inputs)
    val zoom = System.getProperty("probe.zoom", "1").toDouble
    val zoomTag = s"z${if zoom == math.rint(zoom) then zoom.toInt.toString else zoom.toString}"
    val over = overlayMapping(inputs.range, cutoff)
    val adaptive = encoding.adaptive
    val compileStarted = System.nanoTime()
    val plan = zoomed(if adaptive then finalPlan(inputs, inputs.values, cutoff, lighting) else facePlan(inputs, inputs.values, over, lighting, reduction), zoom)
    val compileMs = (System.nanoTime() - compileStarted) / 1e6
    mode match
      case "stats" =>
        val record = s"""{"scene":"$sceneName","sceneCutoff":${stats(inputs, inputs.values, inputs.cutoff)},""" +
          s""""q0.9":${stats(inputs, inputs.values, quantile(inputs.values, 0.9))}}"""
        Files.writeString(output.resolve(s"stats-$sceneName.json"), record + "\n")
        println(s"face_stats=$record")
        return
      case "geometry" =>
        val geometryPlan = zoomed(finalPlan(inputs, inputs.values, inputs.cutoff, SurfaceLighting.Unlit), zoom)
        val (one, four) = JavaFxFaceTexelReference.exportGeometry(output, s"$sceneName-$zoomTag", geometryPlan, Width, Height)
        println(s"face_geometry=$sceneName-$zoomTag residual1x=$one residual4x=$four")
        return
      case _ => ()
    val faceRule = if adaptive then None else Some(rule(inputs, plan, inputs.values, over, reduction))
    val tag = s"$sceneName-${if adaptive then "vertex" else modeName(reduction)}-$cutoffTag-$zoomTag"
    val ruleRecord = faceRule.fold("\"rule\":null")(r => s""""rule":{"faces":${r.faces},"maxDifferenceFromRecipe":${r.maxDifference},""" +
      s""""offByOne":${r.offByOne},"missingFaces":${r.missingFaces},"belowCutoffFaces":${r.belowCutoffFaces}}""")
    val style = SurfaceRasterStyle(culling = TriangleCulling.None)
    if mode == "reference" then
      val ids = JavaFxFaceTexelReference.offsets(plan)
      faceRule.foreach: r =>
        val colours = JavaFxFaceTexelReference.globalColors(plan, r.colors, ids)
        JavaFxFaceTexelReference.writeInts(output.resolve(s"colours-$tag.i32"), colours)
        val flatRaster = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(Width, Height), style).fold(e => throw new IllegalStateException(e.toString), identity)
        val rasterPixels = JavaFxFaceTexelReference.rasterArgb(flatRaster, Width, Height)
        val byId = JavaFxFaceTexelReference.flat(JavaFxFaceTexelReference.faceIds(flatRaster, Width, Height, ids), colours)
        val differing = rasterPixels.indices.count(i => rasterPixels(i) != byId(i))
        require(differing == 0, s"raster of the face-flat plan differs from rule colours by face id at $differing pixels")
        JavaFxFaceTexelReference.writeRgb(output.resolve(s"ref-flat-$tag.png"), rasterPixels, Width, Height)
      val interp = zoomed(JavaFxScalarLutCortexProbe.scalarPlan(inputs, inputs.values, cutoff, lighting), zoom)
      val interpRaster = SurfaceRasterizer.render(interp, RasterDimensions.unsafe(Width, Height), style).fold(e => throw new IllegalStateException(e.toString), identity)
      JavaFxFaceTexelReference.writeRgb(output.resolve(s"ref-interp-scalar-$sceneName-$cutoffTag-$zoomTag-$lightingName.png"),
        JavaFxFaceTexelReference.rasterArgb(interpRaster, Width, Height), Width, Height)
      val colour = zoomed(finalPlan(inputs, inputs.values, cutoff, lighting), zoom)
      val colourRaster = SurfaceRasterizer.render(colour, RasterDimensions.unsafe(Width, Height), style).fold(e => throw new IllegalStateException(e.toString), identity)
      JavaFxFaceTexelReference.writeRgb(output.resolve(s"ref-interp-colour-$sceneName-$cutoffTag-$zoomTag-$lightingName.png"),
        JavaFxFaceTexelReference.rasterArgb(colourRaster, Width, Height), Width, Height)
      val record = s"""{"mode":"reference","tag":"$tag","cutoff":$cutoff,"cutoffTag":"$cutoffTag","zoom":$zoom,$ruleRecord,"planCompileMs":$compileMs}"""
      Files.writeString(output.resolve(s"reference-$tag-$lightingName.json"), record + "\n")
      println(s"face_cortex_reference=$record")
      return
    val material = origin("com.sun.prism.es2.ES2PhongMaterial")
    val shader = origin("com.sun.prism.es2.ES2PhongShader")
    Option(System.getProperty("probe.expectedOrigin")).foreach(expected =>
      require(material == expected && shader == expected, s"unexpected material/shader origin: $material / $shader"))
    val label = s"native-$encoding-$tag-$lightingName-$antialiasing"
    val raster = Option.when(System.getProperty("probe.raster", "true") == "true"):
      SurfaceRasterizer.render(plan, RasterDimensions.unsafe(Width, Height), style).fold(e => throw new IllegalStateException(e.toString), identity)
    val variants =
      if !bench then Vector.empty
      else
        val other = loadValues(inputsDir, if sceneName == "beta" then "fir" else "beta", plan.meshes.map(_.surface))
        def variant(values: Map[SurfaceId, Array[Double]], mapping: ScalarMapping, variantCutoff: Double) =
          zoomed(if adaptive then finalPlan(inputs, values, variantCutoff, lighting) else facePlan(inputs, values, mapping, lighting, reduction), zoom)
        Vector(
          "palette" -> variant(inputs.values, paletteMapping(inputs.range, cutoff), cutoff),
          "cutoff" -> variant(inputs.values, overlayMapping(inputs.range, cutoff * 1.5), cutoff * 1.5),
          "mapValues" -> variant(other, over, cutoff))
    val done = new CountDownLatch(1)
    @volatile var failure: Option[Throwable] = None
    @volatile var record = ""
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        val config = JavaFxAtlasConfig.make(encoding = encoding).toOption.get
        val backend = JavaFxSurfaceBackend.create(config).fold(e => throw new IllegalStateException(e.message), identity)
        try
          val started = System.nanoTime()
          backend.render(plan).fold(e => throw new IllegalStateException(e.message), identity)
          val buildMs = (System.nanoTime() - started) / 1e6
          val settings = JavaFxSnapshotConfig.make(Width, Height, antialiasing).toOption.get
          val sub = backend.newSubScene(settings).toOption.get
          val host = new FxScene(new Group(sub), Width.toDouble, Height.toDouble)
          host.getRoot.applyCss()
          host.getRoot.layout()
          val firstStarted = System.nanoTime()
          var image = backend.snapshot(settings).toOption.get
          val firstMs = (System.nanoTime() - firstStarted) / 1e6
          val warm = Vector.fill(6):
            val s = System.nanoTime()
            image = backend.snapshot(settings).toOption.get
            (System.nanoTime() - s) / 1e6
          val pixels = argb(image)
          val colours = pixels.distinct.length
          require(colours > 32, s"blank or degenerate frame: $colours colours")
          val base = signature(pixels)
          require(signature(argb(backend.snapshot(settings).toOption.get)) == base, "repeat snapshot differs")
          JavaFxFaceTexelReference.writeRgb(output.resolve(s"$label.png"), pixels, Width, Height)
          val chunks = backend.chunks
          val textureBytes = chunks.map(c => c.atlas.width.toLong * c.atlas.height * 4).sum
          val textures = chunks.map(c => s"${c.atlas.width}x${c.atlas.height}").mkString("[\"", "\",\"", "\"]")
          val meshBytes = chunks.map(c => (c.mesh.getPoints.size.toLong + c.mesh.getNormals.size + c.mesh.getTexCoords.size + c.mesh.getFaces.size) * 4).sum
          val renderedFaces = chunks.map(_.renderedFaceCount).sum
          val meshesBefore = chunks.map(_.mesh)
          val meshHashBefore = meshSignature(chunks)
          val pickRecord = raster.fold("null"): reference =>
            var sampled = 0; var agree = 0; var miss = 0; var other = 0
            var y = 5
            while y < Height && sampled < 1200 do
              var x = 7
              while x < Width && sampled < 1200 do
                reference.pick(x, y).toOption.flatten.foreach: expected =>
                  sampled += 1
                  JavaFxNativePick.at(sub, x + 0.5, y + 0.5).flatMap(result =>
                    chunks.find(_.view eq result.getIntersectedNode).flatMap(chunk =>
                      chunk.packetFace(result.getIntersectedFace).map(face => (chunk.surface, face)))) match
                    case None => miss += 1
                    case Some((surface, face)) => if surface == expected.surface && face == expected.face then agree += 1 else other += 1
                x += 17
              y += 11
            s"""{"sampled":$sampled,"sameFace":$agree,"nativeMiss":$miss,"otherFace":$other}"""
          val benchRecord =
            if variants.isEmpty then "null"
            else
              // Readback baseline: snapshot of an empty 3D SubScene of the same size and multisampling.
              val empty = new SubScene(new Group(), Width.toDouble, Height.toDouble, true, antialiasing)
              new FxScene(new Group(empty), Width.toDouble, Height.toDouble): Unit
              val parameters = new SnapshotParameters()
              parameters.setFill(Color.WHITE)
              val readback = Vector.fill(14):
                val s = System.nanoTime()
                empty.snapshot(parameters, null)
                (System.nanoTime() - s) / 1e6
              .drop(4)
              val readbackMs = median(readback)
              def alternate(name: String, changed: SurfaceRenderPlan): String =
                val rows = Vector.tabulate(24): i =>
                  val target = if i % 2 == 0 then changed else plan
                  val t0 = System.nanoTime()
                  val receipt = backend.render(target).fold(e => throw new IllegalStateException(s"$name: ${e.message}"), identity)
                  val t1 = System.nanoTime()
                  val after = backend.snapshot(settings).toOption.get
                  val t2 = System.nanoTime()
                  backend.snapshot(settings).toOption.get
                  val t3 = System.nanoTime()
                  val shot = signature(argb(after))
                  if i % 2 == 0 then require(shot != base, s"$name update $i did not change the frame")
                  else require(shot == base, s"$name restoration $i differs from the original frame")
                  (receipt, (t1 - t0) / 1e6, (t2 - t1) / 1e6, (t3 - t2) / 1e6)
                val measured = rows.drop(4)
                val receipts = measured.map(_._1)
                val render = measured.map(_._2)
                val after = measured.map(_._3)
                val repeat = measured.map(_._4)
                val upload = measured.map(row => row._3 - row._4)
                val latency = measured.map(row => row._2 + row._3 - readbackMs)
                s""""$name":{"renderMs":${stats(render)},"reductionMs":${stats(receipts.map(_.faceReductionNanos / 1e6))},""" +
                  s""""evaluationMs":${stats(receipts.map(_.faceEvaluationNanos / 1e6))},"textureWriteMs":${stats(receipts.map(_.textureWriteNanos / 1e6))},""" +
                  s""""snapshotAfterUpdateMs":${stats(after)},"snapshotRepeatMs":${stats(repeat)},"uploadAndMipmapMs":${stats(upload)},""" +
                  s""""latencyExcludingReadbackMs":${stats(latency)},"renderAndSnapshotMs":${stats(measured.map(row => row._2 + row._3))},""" +
                  s""""textureCoordinateBytes":[${receipts.map(_.textureCoordinateBytesUpdated).distinct.mkString(",")}],""" +
                  s""""geometryRebuilds":${receipts.count(_.dirty.geometry)},"frameChangeAndRestoreVerified":true}"""
              val body = variants.map(alternate.tupled).mkString(",")
              val unchangedMeshes = adaptive || (backend.chunks.map(_.mesh).zip(meshesBefore).forall(_ eq _) && meshSignature(backend.chunks) == meshHashBefore)
              require(unchangedMeshes, "native meshes changed during colour updates")
              s"""{"readbackBaselineMs":${stats(readback)},$body,"nativeMeshesUnchanged":$unchangedMeshes}"""
          record = s"""{"label":"$label","encoding":"$encoding","scene":"$sceneName","faceMode":"${if adaptive then "vertex" else modeName(reduction)}",""" +
            s""""cutoff":$cutoff,"cutoffTag":"$cutoffTag","zoom":$zoom,"lighting":"$lightingName","antialiasing":"$antialiasing",""" +
            s"""$ruleRecord,"renderedFaces":$renderedFaces,"textures":$textures,"textureBytes":$textureBytes,"meshArrayBytes":$meshBytes,""" +
            s""""planCompileMs":$compileMs,"buildMs":$buildMs,"firstSnapshotMs":$firstMs,"firstShowMs":${buildMs + firstMs},""" +
            s""""warmSnapshotMs":${stats(warm)},"distinctColours":$colours,"picks":$pickRecord,"bench":$benchRecord,""" +
            s""""threads":{"faceTexelWorkers":${JavaFxFaceTexelWork.threads},"availableProcessors":${Runtime.getRuntime.availableProcessors}},""" +
            s""""runtime":{"javaVersion":"${System.getProperty("java.version")}","javafxVersion":"${System.getProperty("javafx.runtime.version")}",""" +
            s""""affineAtlas":"${System.getProperty("scalafim.javafx.affineAtlas")}","materialOrigin":"$material","shaderOrigin":"$shader"}}"""
          Files.writeString(output.resolve(s"$label.json"), record + "\n")
        finally backend.dispose(): Unit
      catch case error: Throwable => failure = Some(error)
      finally done.countDown()
    try
      require(done.await(900, TimeUnit.SECONDS), "cortex probe timed out")
      failure.foreach(throw _)
      println(s"face_cortex=$record")
      println("PASS cortex snapshot written")
    finally Platform.exit()
