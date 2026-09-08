package scalafim.surface.view.three

import intaglio.*
import scalafim.surface.view.*

final case class ThreeFragmentAttribute(name: String, values: Array[Float])
final case class ThreeFragmentPacket(
  surface: SurfaceId,
  resourceKeys: Vector[SurfaceResourceKey],
  vertexShader: String,
  fragmentShader: String,
  attributes: Vector[ThreeFragmentAttribute]
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

  def compile(mesh: SurfaceMeshPacket, allLayers: Vector[SurfaceLayerPacket]): Either[ThreeSurfaceError, ThreeFragmentPacket] =
    val layers = allLayers.filter(_.surface == mesh.surface)
    if layers.length > MaxLayers then return Left(ThreeSurfaceError.InvalidPlan(s"fragment shaders support at most $MaxLayers layers per surface"))
    if mesh.indices.length > Int.MaxValue / 4 then return Left(ThreeSurfaceError.InvalidPlan("fragment attribute indexing exceeds Int capacity"))
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
                values(offset + channel) = (w._1 * ((colors(ids._1) >>> shift) & 255) +
                  w._2 * ((colors(ids._2) >>> shift) & 255) + w._3 * ((colors(ids._3) >>> shift) & 255)).toFloat
                channel += 1
          corner += 1
        face += 1
      attributes += ThreeFragmentAttribute(name, values)
      val evaluated = if scalar then
        s"($varying.z < 0.5 ? vec4(0.0) : ($varying.y > 0.0 ? ${color(layer.scalarField.get.mapping.invalid)} : map$layerIndex($varying.x)))"
      else s"floor(clamp($varying, 0.0, 255.0) + 0.5)"
      val blend = layer.blendMode match
        case DisplayBlendMode.Normal => "over.rgb"
        case DisplayBlendMode.Additive => "min(vec3(255.0), under + over.rgb)"
        case DisplayBlendMode.Multiply => "under * over.rgb / 255.0"
        case DisplayBlendMode.Screen => "255.0 - (255.0 - under) * (255.0 - over.rgb) / 255.0"
      operations.append(s"{ vec4 over = $evaluated; float alpha = over.a / 255.0 * ${number(layer.opacity.toDouble)}; under = floor(clamp(mix(under, $blend, alpha), 0.0, 255.0) + 0.5); }\n")
      layerIndex += 1
    // Partially covered MSAA pixels may place ordinary varyings outside the
    // triangle. Keep scalar mapping and lighting inside the same primitive.
    val varyingDeclarations = layers.indices.map(i => s"centroid varying vec4 vLayer$i;").mkString("\n")
    val vertex = s"""
      |$varyingDeclarations
      |${layers.indices.map(i => s"attribute vec4 aLayer$i;").mkString("\n")}
      |centroid varying vec3 vWorldNormal;
      |void main() {
      |  vWorldNormal = normal;
      |  ${layers.indices.map(i => s"vLayer$i = aLayer$i;").mkString("\n")}
      |  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
      |}
      |""".stripMargin
    val fragment = s"""
      |$varyingDeclarations
      |centroid varying vec3 vWorldNormal;
      |uniform float uAmbient;
      |uniform float uDiffuse;
      |uniform vec3 uLightDirection;
      |$functions
      |void main() {
      |  vec3 under = vec3(184.0);
      |  $operations
      |  float magnitude = length(vWorldNormal);
      |  float cosine = magnitude == 0.0 ? 0.0 : max(0.0, dot(vWorldNormal / magnitude, uLightDirection));
      |  float shade = min(1.0, uAmbient + uDiffuse * cosine);
      |  gl_FragColor = vec4(floor(under * shade + 0.5) / 255.0, 1.0);
      |}
      |""".stripMargin
    Right(ThreeFragmentPacket(mesh.surface, layers.map(_.resourceKey), vertex, fragment, attributes.result()))

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
    if mapping.outOfRange == ScalarOutOfRange.Hide then
      body.append(s"if (x < ${encoded(mapping.scale.window.lower)} || x > ${encoded(mapping.scale.window.upper)}) return ${color(mapping.hidden)};\n")
    mapping.scale.omittedInterval.foreach(interval => body.append(s"if (${inside(interval)}) return ${color(mapping.hidden)};\n"))
    mapping.scale.segments.zipWithIndex.foreach: (segment, index) =>
      val start = encoded(segment.window.lower)
      val end = encoded(segment.window.upper)
      val condition = if index < mapping.scale.segments.length - 1 then s"x <= $end" else "true"
      body.append(s"if ($condition) { float t = clamp((x - $start) / ($end - $start), 0.0, 1.0);\n")
      segment.ramp.stops.sliding(2).foreach: pair =>
        val (lo, a) = pair(0)
        val (hi, b) = pair(1)
        body.append(s"if (t <= ${number(hi)}) return floor(mix(${color(a)}, ${color(b)}, (t - ${number(lo)}) / (${number(hi)} - ${number(lo)})) + 0.5);\n")
      body.append(s"return ${color(segment.ramp.stops.last._2)}; }\n")
    body.append(s"return ${color(mapping.hidden)}; }\n").result()
