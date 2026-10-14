package scalafim.surface.view.three

import intaglio.*
import scalafim.surface.view.*

final case class ThreeFragmentAttribute(name: String, values: Array[Float])
final case class ThreeFragmentPacket(
  surface: SurfaceId,
  resourceKeys: Vector[SurfaceResourceKey],
  vertexShader: String,
  fragmentShader: String,
  attributes: Vector[ThreeFragmentAttribute],
  sourceMesh: Option[SurfaceMeshPacket] = None,
  sourceLayers: Vector[SurfaceLayerPacket] = Vector.empty,
  colorPipeline: SurfaceColorPipeline = SurfaceColorPipeline.LegacyDisplayBytes,
  surfaceBase: Rgba32 = SurfaceDisplayDefaults.LegacySurfaceBase
):
  def attributeBytes: Long = attributes.iterator.map(_.values.length.toLong * 4).sum

/** GLSL evaluates the same unlit display-byte mapping and ordered composition as
  * the reference. Only storage/interpolation uses GPU floats. Values are encoded
  * over the full finite envelope, never clamped at vertices to display limits.
  */
object ThreeFragmentShader:
  val MaxLayers = 8
  val MaxStops = 1024

  private def number(value: Double): String =
    val text = (if value == 0 then 0.0 else value).toString
    if text.contains('.') || text.contains('E') || text.contains('e') then text else text + ".0"
  private def color(value: Rgba32): String =
    s"vec4(${number(value.red)},${number(value.green)},${number(value.blue)},${number(value.alpha)})"

  /** Fixed corner expansion supports per-face validity/coverage without changing
    * scientific topology or subdivision when scalar values or thresholds change.
    */
  def geometry(mesh: SurfaceMeshPacket): SurfaceMeshPacket =
    val count = mesh.indices.length
    val positions = new Array[Float](Math.multiplyExact(count, 3))
    val normals = new Array[Float](positions.length)
    val owners = new Array[Int](count)
    val indices = new Array[Int](count)
    var i = 0
    while i < count do
      val vertex = mesh.indices(i)
      var axis = 0
      while axis < 3 do
        positions(i * 3 + axis) = mesh.positions(vertex * 3 + axis)
        normals(i * 3 + axis) = mesh.normals(vertex * 3 + axis)
        axis += 1
      indices(i) = i
      owners(i) = mesh.sourceVertex(vertex)
      i += 1
    mesh.copy(positions = new FloatBufferView(positions), normals = new FloatBufferView(normals),
      indices = new IntBufferView(indices), sourceVertices = Some(new IntBufferView(owners)))

  def compile(mesh: SurfaceMeshPacket, allLayers: Vector[SurfaceLayerPacket],
      colorPipeline: SurfaceColorPipeline = SurfaceColorPipeline.LegacyDisplayBytes,
      surfaceBase: Rgba32 = SurfaceDisplayDefaults.LegacySurfaceBase): Either[ThreeSurfaceError, ThreeFragmentPacket] =
    val layers = allLayers.filter(_.surface == mesh.surface)
    var packetLayer = 0
    while packetLayer < layers.length do
      SurfaceThresholdEdgeValidation.validatePacket(mesh, layers(packetLayer)) match
        case Left(reason) => return Left(ThreeSurfaceError.InvalidPlan(reason))
        case Right(_) => ()
      packetLayer += 1
    if layers.exists(_.thresholdEdge.nonEmpty) &&
        (colorPipeline != SurfaceColorPipeline.DisplaySrgbLinearLight || layers.dropRight(1).exists(_.thresholdEdge.nonEmpty) ||
          layers.exists(_.interpolation != SurfaceMapInterpolation.VertexColor) || layers.last.blendMode != DisplayBlendMode.Normal) then
      return Left(ThreeSurfaceError.InvalidPlan("threshold edges require one topmost normal layer in a linear vertex-color stack"))
    if layers.length > MaxLayers then return Left(ThreeSurfaceError.InvalidPlan(s"fragment shaders support at most $MaxLayers layers per surface"))
    if mesh.indices.length > Int.MaxValue / 4 then return Left(ThreeSurfaceError.InvalidPlan("fragment attribute indexing exceeds Int capacity"))
    if colorPipeline == SurfaceColorPipeline.DisplaySrgbLinearLight && layers.forall(_.interpolation == SurfaceMapInterpolation.VertexColor) then
      return linearVertexPacket(mesh, layers, surfaceBase)
    val functions = new StringBuilder
    val operations = new StringBuilder
    val attributes = Vector.newBuilder[ThreeFragmentAttribute]
    var layerIndex = 0
    while layerIndex < layers.length do
      val layer = layers(layerIndex)
      val name = s"aLayer$layerIndex"
      val varying = s"vLayer$layerIndex"
      val values = new Array[Float](mesh.indices.length * 4)
      val scalar = layer.interpolation == SurfaceMapInterpolation.VertexScalar
      if scalar && layer.scalarField.isEmpty then return Left(ThreeSurfaceError.InvalidPlan("scalar layer lacks original samples"))
      if !scalar && layer.sampleColors.isEmpty then return Left(ThreeSurfaceError.InvalidPlan("fragment color layer lacks original samples"))
      val envelope = if !scalar then None else
        val field = layer.scalarField.get
        val boundaries = SurfaceMappingPartition.boundaries(field.mapping)
        if field.mapping.scale.segments.map(_.ramp.stops.length).sum > MaxStops then
          return Left(ThreeSurfaceError.InvalidPlan(s"scalar mapping exceeds $MaxStops ramp stops"))
        var low = boundaries.min
        var high = boundaries.max
        // Only covered, referenced samples contribute. Network attachment pads
        // fields for unrelated faces; those values must not dilute precision.
        var sourceTriangle = 0
        while sourceTriangle < mesh.indices.length / 3 do
          if layer.coverage.contains(mesh.sourceFace(sourceTriangle)) then
            val (a, b, c) = mesh.sourceFaceVertices(sourceTriangle)
            if a < 0 || b < 0 || c < 0 || a >= field.samples.length || b >= field.samples.length || c >= field.samples.length then
              return Left(ThreeSurfaceError.InvalidPlan("scalar sample indices exceed original field"))
            def include(id: Int): Unit =
              val x = field.samples(id)
              if x.isFinite then
                low = math.min(low, x)
                high = math.max(high, x)
            include(a)
            include(b)
            include(c)
          sourceTriangle += 1
        val window = DisplayWindow.unsafe(low, high)
        val encoded = boundaries.map(x => window.normalize(x).toFloat)
        if encoded.sliding(2).exists(pair => pair(0) >= pair(1)) then
          return Left(ThreeSurfaceError.InvalidPlan(s"layer ${layer.layer.value} mapping boundaries collapse at GPU float precision"))
        // A source sample may round onto a discontinuity even when all of the
        // discontinuities themselves remain distinct. Reject changed branch
        // ownership before uploading an irreversible GPU approximation.
        var face = 0
        while face < mesh.indices.length / 3 do
          if layer.coverage.contains(mesh.sourceFace(face)) then
            val (a, b, c) = mesh.sourceFaceVertices(face)
            val ids = Array(a, b, c)
            var corner = 0
            while corner < 3 do
              val sample = field.samples(ids(corner))
              if sample.isFinite then
                val exact = window.normalize(sample)
                val rounded = exact.toFloat.toDouble
                if boundaries.exists: boundary =>
                    val threshold = window.normalize(boundary)
                    val roundedThreshold = threshold.toFloat.toDouble
                    java.lang.Double.compare(exact, threshold) != java.lang.Double.compare(rounded, roundedThreshold)
                then return Left(ThreeSurfaceError.InvalidPlan(s"layer ${layer.layer.value} sample collapses onto a mapping boundary at GPU float precision"))
              corner += 1
          face += 1
        functions.append(mappingFunction(s"map$layerIndex", field.mapping, window))
        Some(window)
      var face = 0
      while face < mesh.indices.length / 3 do
        val sourceFace = mesh.sourceFace(face)
        val (a, b, c) = mesh.sourceFaceVertices(face)
        var corner = 0
        while corner < 3 do
          val w = mesh.sourceBarycentric(face, if corner == 0 then 1 else 0, if corner == 1 then 1 else 0, if corner == 2 then 1 else 0)
          val offset = (face * 3 + corner) * 4
          if layer.coverage.contains(sourceFace) then
            if scalar then
              val field = layer.scalarField.get
              if a < 0 || b < 0 || c < 0 || a >= field.samples.length || b >= field.samples.length || c >= field.samples.length then
                return Left(ThreeSurfaceError.InvalidPlan("scalar sample indices exceed original field"))
              def sample(id: Int): Double = if field.samples(id).isFinite then envelope.get.normalize(field.samples(id)) else 0
              values(offset) = (w._1 * sample(a) + w._2 * sample(b) + w._3 * sample(c)).toFloat
              values(offset + 1) = (w._1 * (if field.samples(a).isFinite then 0 else 1) +
                w._2 * (if field.samples(b).isFinite then 0 else 1) + w._3 * (if field.samples(c).isFinite then 0 else 1)).toFloat
              values(offset + 2) = 1
            else
              val colors = layer.sampleColors.get
              val ids = layer.interpolation match
                case SurfaceMapInterpolation.FaceConstant => (sourceFace, sourceFace, sourceFace)
                case SurfaceMapInterpolation.NearestVertex =>
                  val owner = mesh.sourceVertex(mesh.indices(face * 3))
                  (owner, owner, owner)
                case _ => (a, b, c)
              if ids._1 < 0 || ids._2 < 0 || ids._3 < 0 || ids._1 >= colors.length || ids._2 >= colors.length || ids._3 >= colors.length then
                return Left(ThreeSurfaceError.InvalidPlan("color sample indices exceed original field"))
              var channel = 0
              while channel < 4 do
                val shift = (3 - channel) * 8
                def component(id: Int): Double =
                  val byte = (colors(id) >>> shift) & 255
                  if channel < 3 && colorPipeline == SurfaceColorPipeline.DisplaySrgbLinearLight &&
                      layer.interpolation == SurfaceMapInterpolation.VertexColor then Srgb.toLinear(byte) * 255.0
                  else byte.toDouble
                values(offset + channel) = (w._1 * component(ids._1) + w._2 * component(ids._2) + w._3 * component(ids._3)).toFloat
                channel += 1
          corner += 1
        face += 1
      attributes += ThreeFragmentAttribute(name, values)
      val evaluated = if scalar then
        s"($varying.z < 0.5 ? vec4(0.0) : ($varying.y > 0.0 ? ${color(layer.scalarField.get.mapping.invalid)} : map$layerIndex($varying.x)))"
      else if colorPipeline == SurfaceColorPipeline.DisplaySrgbLinearLight && layer.interpolation == SurfaceMapInterpolation.VertexColor then
        s"vec4(floor(clamp(linearToSrgb($varying.rgb / 255.0), 0.0, 1.0) * 255.0 + 0.5), floor(clamp($varying.a, 0.0, 255.0) + 0.5))"
      else s"floor(clamp($varying, 0.0, 255.0) + 0.5)"
      val blend = layer.blendMode match
        case DisplayBlendMode.Normal => "over.rgb"
        case DisplayBlendMode.Additive => "min(vec3(255.0), under + over.rgb)"
        case DisplayBlendMode.Multiply => "under * over.rgb / 255.0"
        case DisplayBlendMode.Screen => "255.0 - (255.0 - under) * (255.0 - over.rgb) / 255.0"
        case DisplayBlendMode.PlusLighter => "over.rgb"
      val blendRgb = blend.replace("under", "under.rgb")
      if layer.blendMode == DisplayBlendMode.PlusLighter then
        operations.append(s"""{ vec4 over = $evaluated; float a = over.a / 255.0 * ${number(layer.opacity.toDouble)};
          |float b = under.a / 255.0; float resultAlpha = min(1.0, a + b);
          |vec3 sum = min(vec3(1.0), under.rgb / 255.0 * b + over.rgb / 255.0 * a);
          |under = resultAlpha <= 0.0 ? vec4(0.0) : floor(vec4(clamp(sum / resultAlpha, 0.0, 1.0), resultAlpha) * 255.0 + 0.5); }
          |""".stripMargin)
      else operations.append(s"""{ vec4 over = $evaluated; float sourceAlpha = over.a / 255.0 * ${number(layer.opacity.toDouble)};
        |float backdropAlpha = under.a / 255.0; float outAlpha = sourceAlpha + backdropAlpha * (1.0 - sourceAlpha);
        |vec3 blended = $blendRgb / 255.0; vec3 premultiplied = (1.0 - sourceAlpha) * backdropAlpha * (under.rgb / 255.0) +
        |  (1.0 - backdropAlpha) * sourceAlpha * (over.rgb / 255.0) + backdropAlpha * sourceAlpha * blended;
        |under = outAlpha <= 0.0 ? vec4(0.0) : vec4(floor(clamp(premultiplied / outAlpha, 0.0, 1.0) * 255.0 + 0.5), floor(outAlpha * 255.0 + 0.5)); }
        |""".stripMargin)
      layerIndex += 1
    // Partially covered MSAA pixels may place ordinary varyings outside the
    // triangle. Keep scalar mapping and lighting inside the same primitive.
    val varyingDeclarations = layers.indices.map(i => s"centroid varying vec4 vLayer$i;").mkString("\n")
    val vertex = s"""
      |$varyingDeclarations
      |${layers.indices.map(i => s"attribute vec4 aLayer$i;").mkString("\n")}
      |centroid varying vec3 vWorldNormal;
      |centroid varying vec3 vWorldPosition;
      |void main() {
      |  vWorldNormal = normal;
      |  vWorldPosition = (modelMatrix * vec4(position, 1.0)).xyz;
      |  ${layers.indices.map(i => s"vLayer$i = aLayer$i;").mkString("\n")}
      |  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
      |}
      |""".stripMargin
    val fragment = s"""
      |$varyingDeclarations
      |centroid varying vec3 vWorldNormal;
      |centroid varying vec3 vWorldPosition;
      |uniform float uAmbient;
      |uniform float uDiffuse;
      |uniform vec3 uLightDirection;
      |uniform float uSurfaceOpacity;
      |uniform int uClipCount;
      |uniform vec4 uClipPlanes[8];
      |$srgb
      |$functions
      |void main() {
      |  for (int i = 0; i < 8; ++i) { if (i >= uClipCount) break; if (dot(vWorldPosition, uClipPlanes[i].xyz) + uClipPlanes[i].w < 0.0) discard; }
      |  vec4 under = ${color(surfaceBase)};
      |  $operations
      |  float magnitude = length(vWorldNormal);
      |  float cosine = magnitude == 0.0 ? 0.0 : max(0.0, dot(vWorldNormal / magnitude, uLightDirection));
      |  float shade = min(1.0, uAmbient + uDiffuse * cosine);
      |  vec3 rgb = ${if colorPipeline == SurfaceColorPipeline.DisplaySrgbLinearLight then "linearToSrgb(srgbToLinear(under.rgb / 255.0) * shade)" else "under.rgb / 255.0 * shade"};
      |  float alpha = under.a / 255.0 * uSurfaceOpacity;
      |  if (alpha <= 0.0) discard;
      |  gl_FragColor = vec4(floor(clamp(rgb, 0.0, 1.0) * 255.0 + 0.5) / 255.0, alpha);
      |}
      |""".stripMargin
    Right(ThreeFragmentPacket(mesh.surface, layers.map(_.resourceKey), vertex, fragment, attributes.result(),
      Some(mesh), layers, colorPipeline, surfaceBase))

  private val srgb = """vec3 srgbToLinear(vec3 x) { return mix(x / 12.92, pow((x + 0.055) / 1.055, vec3(2.4)), step(vec3(0.04045), x)); }
    |vec3 linearToSrgb(vec3 x) { return mix(x * 12.92, 1.055 * pow(max(x, vec3(0.0)), vec3(1.0 / 2.4)) - 0.055, step(vec3(0.0031308), x)); }""".stripMargin

  private def linearVertexPacket(mesh: SurfaceMeshPacket, layers: Vector[SurfaceLayerPacket], base: Rgba32)
      : Either[ThreeSurfaceError, ThreeFragmentPacket] =
    val values = new Array[Float](mesh.indices.length * 4)
    val edgeLayer = layers.lastOption.filter(_.thresholdEdge.nonEmpty)
    val edgeColors = new Array[Float](if edgeLayer.nonEmpty then mesh.indices.length * 4 else 0)
    val edgeSigns = new Array[Float](edgeColors.length)
    var face = 0
    while face < mesh.indices.length / 3 do
      val sourceFace = mesh.sourceFace(face)
      val (a, b, c) = mesh.sourceFaceVertices(face)
      if layers.exists(layer => layer.sampleColors.forall(colors => Vector(a, b, c).exists(i => i < 0 || i >= colors.length))) then
        return Left(ThreeSurfaceError.InvalidPlan("linear vertex stack lacks original colors"))
      def composed(vertex: Int): Rgba32 =
        layers.foldLeft(base): (under, layer) =>
          if !layer.coverage.contains(sourceFace) || layer.thresholdEdge.nonEmpty then under
          else layer.blendMode.composite(under, Rgba32.fromPackedInt(layer.sampleColors.get(vertex)), layer.opacity)
      val colors = Vector(composed(a), composed(b), composed(c))
      var corner = 0
      while corner < 3 do
        val w = mesh.sourceBarycentric(face, if corner == 0 then 1 else 0, if corner == 1 then 1 else 0, if corner == 2 then 1 else 0)
        val weights = Vector(w._1, w._2, w._3)
        val offset = (face * 3 + corner) * 4
        values(offset) = colors.zip(weights).map((color, weight) => Srgb.toLinear(color.red) * weight).sum.toFloat
        values(offset + 1) = colors.zip(weights).map((color, weight) => Srgb.toLinear(color.green) * weight).sum.toFloat
        values(offset + 2) = colors.zip(weights).map((color, weight) => Srgb.toLinear(color.blue) * weight).sum.toFloat
        values(offset + 3) = colors.zip(weights).map((color, weight) => color.alpha / 255.0 * weight).sum.toFloat
        edgeLayer.foreach: layer =>
          val edge = layer.thresholdEdge.get
          val owners = Vector(a, b, c)
          edgeSigns(offset) = (if layer.coverage.contains(sourceFace) then owners.zip(weights).map((id, weight) => edge.signs(id) * weight).sum else -1.0).toFloat
          var channel = 0
          while channel < 4 do
            val shift = (3 - channel) * 8
            edgeColors(offset + channel) = owners.zip(weights).map: (id, weight) =>
              val value = (edge.colors(id) >>> shift) & 255
              (if channel < 3 then Srgb.toLinear(value) else value / 255.0 * layer.opacity.toDouble) * weight
            .sum.toFloat
            channel += 1
        corner += 1
      face += 1
    val edgeVaryings = if edgeLayer.nonEmpty then "varying vec4 vEdgeColor; varying float vEdgeSign;" else ""
    val vertex = s"""
      |attribute vec4 aLinearColor;
      |${if edgeLayer.nonEmpty then "attribute vec4 aEdgeColor; attribute vec4 aEdgeSign;" else ""}
      |$edgeVaryings
      |varying vec4 vLinearColor;
      |varying vec3 vWorldNormal;
      |varying vec3 vWorldPosition;
      |void main() {
      |  vLinearColor = aLinearColor;
      |  ${if edgeLayer.nonEmpty then "vEdgeColor = aEdgeColor; vEdgeSign = aEdgeSign.x;" else ""}
      |  vWorldNormal = normal;
      |  vWorldPosition = (modelMatrix * vec4(position, 1.0)).xyz;
      |  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
      |}
      |""".stripMargin
    val fragment = s"""
      |$edgeVaryings
      |varying vec4 vLinearColor;
      |varying vec3 vWorldNormal;
      |varying vec3 vWorldPosition;
      |uniform float uAmbient;
      |uniform float uDiffuse;
      |uniform vec3 uLightDirection;
      |uniform float uSurfaceOpacity;
      |uniform int uClipCount;
      |uniform vec4 uClipPlanes[8];
      |$srgb
      |void main() {
      |  for (int i = 0; i < 8; ++i) { if (i >= uClipCount) break; if (dot(vWorldPosition, uClipPlanes[i].xyz) + uClipPlanes[i].w < 0.0) discard; }
      |  float magnitude = length(vWorldNormal);
      |  float cosine = magnitude == 0.0 ? 0.0 : max(0.0, dot(vWorldNormal / magnitude, uLightDirection));
      |  float shade = min(1.0, uAmbient + uDiffuse * cosine);
      |  float alpha = vLinearColor.a * uSurfaceOpacity;
      |  if (alpha <= 0.0) discard;
      |  vec3 rgb = vLinearColor.rgb;
      |  ${if edgeLayer.nonEmpty then "float width = max(fwidth(vEdgeSign), 1e-6); float inside = vEdgeSign >= 0.0 ? max(0.5, smoothstep(0.0, width, vEdgeSign)) : 0.0; rgb = mix(rgb, vEdgeColor.rgb, inside * clamp(vEdgeColor.a, 0.0, 1.0));" else ""}
      |  gl_FragColor = vec4(clamp(linearToSrgb(rgb * shade), 0.0, 1.0), alpha);
      |}
      |""".stripMargin
    Right(ThreeFragmentPacket(mesh.surface, layers.map(_.resourceKey), vertex, fragment,
      Vector(ThreeFragmentAttribute("aLinearColor", values)) ++ edgeLayer.toVector.flatMap(_ =>
        Vector(ThreeFragmentAttribute("aEdgeColor", edgeColors), ThreeFragmentAttribute("aEdgeSign", edgeSigns))), Some(mesh), layers,
      SurfaceColorPipeline.DisplaySrgbLinearLight, base))

  private def mappingFunction(name: String, mapping: ScalarMapping, envelope: DisplayWindow): String =
    def encoded(x: Double): String = number(envelope.normalize(x).toFloat.toDouble)
    def inside(interval: ScalarInterval): String =
      val lower = if interval.endpoints == ScalarEndpointInclusion.Lower || interval.endpoints == ScalarEndpointInclusion.Both then ">=" else ">"
      val upper = if interval.endpoints == ScalarEndpointInclusion.Upper || interval.endpoints == ScalarEndpointInclusion.Both then "<=" else "<"
      s"(x $lower ${encoded(interval.lower)} && x $upper ${encoded(interval.upper)})"
    val body = new StringBuilder(s"vec4 $name(float x) {\n")
    mapping.visibility match
      case ScalarVisibility.All => ()
      case ScalarVisibility.Inside(interval) => body.append(s"if (!${inside(interval)}) return ${color(mapping.hidden)};\n")
      case ScalarVisibility.Outside(interval) => body.append(s"if (${inside(interval)}) return ${color(mapping.hidden)};\n")
      case ScalarVisibility.AtLeast(cutoff) => body.append(s"if (x < ${encoded(cutoff)}) return ${color(mapping.hidden)};\n")
      case ScalarVisibility.AtMost(cutoff) => body.append(s"if (x > ${encoded(cutoff)}) return ${color(mapping.hidden)};\n")
      case ScalarVisibility.Band(inner, outer) =>
        body.append(s"if (!${inside(outer)} || ${inside(inner)}) return ${color(mapping.hidden)};\n")
    if mapping.outOfRange == ScalarOutOfRange.Hide then
      body.append(s"if (x < ${encoded(mapping.scale.window.lower)} || x > ${encoded(mapping.scale.window.upper)}) return ${color(mapping.hidden)};\n")
    mapping.scale.omittedInterval.foreach(interval => body.append(s"if (${inside(interval)}) return ${color(mapping.hidden)};\n"))
    mapping.scale.segments.zipWithIndex.foreach: (segment, index) =>
      val start = encoded(segment.window.lower)
      val end = encoded(segment.window.upper)
      val condition = if index < mapping.scale.segments.length - 1 then s"x <= $end" else "true"
      body.append(s"if ($condition) { float t = clamp((x - $start) / ($end - $start), 0.0, 1.0);\n")
      segment.ramp.stops.sliding(2).zipWithIndex.foreach: (pair, stopIndex) =>
        val (lo, a) = pair(0)
        val (hi, b) = pair(1)
        segment.ramp.sampling match
          case ScalarRampSampling.Discrete => body.append(s"if (x < ${encoded(segment.boundaries(stopIndex + 1))}) return ${color(a)};\n")
          case ScalarRampSampling.Linear => body.append(s"if (t <= ${number(hi)}) return floor(mix(${color(a)}, ${color(b)}, (t - ${number(lo)}) / (${number(hi)} - ${number(lo)})) + 0.5);\n")
      body.append(s"return ${color(segment.ramp.stops.last._2)}; }\n")
    body.append(s"return ${color(mapping.hidden)}; }\n").result()
