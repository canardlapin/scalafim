package scalafim.surface.view

import intaglio.*

/** A color interval encloses every interior sample of a mapping partition. */
final case class SurfaceColorBounds private[view] (lower: Rgba32, upper: Rgba32):
  require(lower.red <= upper.red && lower.green <= upper.green && lower.blue <= upper.blue && lower.alpha <= upper.alpha)
  def midpoint: Rgba32 = Rgba32.unsafe((lower.red + upper.red) / 2,
    (lower.green + upper.green) / 2, (lower.blue + upper.blue) / 2, (lower.alpha + upper.alpha) / 2)
  def midpointError: Int =
    val width = Vector(upper.red - lower.red, upper.green - lower.green, upper.blue - lower.blue, upper.alpha - lower.alpha).max
    (width + 1) / 2
  def contains(color: Rgba32): Boolean =
    color.red >= lower.red && color.red <= upper.red && color.green >= lower.green && color.green <= upper.green &&
      color.blue >= lower.blue && color.blue <= upper.blue && color.alpha >= lower.alpha && color.alpha <= upper.alpha

final case class SurfaceFragmentApproximationConfig private (
  maxChannelError: Int,
  maxTriangles: Int,
  maxDepth: Int,
  maxCutsPerFace: Int
)
object SurfaceFragmentApproximationConfig:
  def make(maxChannelError: Int, maxTriangles: Int, maxDepth: Int = 24, maxCutsPerFace: Int = 64)
      : Either[SurfaceApproximationError, SurfaceFragmentApproximationConfig] =
    if maxChannelError < 1 || maxChannelError > 255 || maxTriangles < 1 || maxDepth < 0 || maxDepth > 52 || maxCutsPerFace < 0 then
      Left(SurfaceApproximationError.InvalidConfig)
    else Right(new SurfaceFragmentApproximationConfig(maxChannelError, maxTriangles, maxDepth, maxCutsPerFace))

enum SurfaceApproximationError:
  case InvalidConfig
  case InvalidEdgeAliases
  case Partition(error: SurfacePartitionError)
  case TriangleBudgetExceeded(limit: Int)
  case DepthBudgetExceeded(face: Int, limit: Int, remainingError: Int)
  case MissingOriginalColors(layer: SurfaceLayerId)
  case InvalidOriginalColors(layer: SurfaceLayerId, expected: Int, actual: Int)
  case MissingScalarField(layer: SurfaceLayerId)
  case PrecisionLimit(face: Int, remainingError: Int)

  def message: String = this match
    case InvalidConfig => "approximation needs channel error 1..255, positive triangle limit, depth 0..52, and nonnegative cut limit"
    case InvalidEdgeAliases => "edge aliases need one nonnegative ID per input vertex and three distinct IDs within each face"
    case Partition(error) => error.message
    case TriangleBudgetExceeded(limit) => s"color approximation exceeds $limit triangles"
    case DepthBudgetExceeded(face, limit, error) => s"face $face exceeds subdivision depth $limit with color error bound $error"
    case MissingOriginalColors(layer) => s"layer ${layer.value} lacks original sample colors"
    case InvalidOriginalColors(layer, expected, actual) => s"layer ${layer.value} needs $expected original colors; got $actual"
    case MissingScalarField(layer) => s"layer ${layer.value} lacks raw scalar samples"
    case PrecisionLimit(face, error) => s"face $face reached barycentric precision limits with color error bound $error"

final case class SurfaceConstantFragment(triangle: SurfaceMappingTriangle, color: Rgba32, bounds: SurfaceColorBounds)
final case class SurfaceFragmentApproximation(cells: Vector[SurfaceConstantFragment], initialTriangles: Int, maximumDepth: Int):
  def maximumChannelError: Int = cells.iterator.map(_.bounds.midpointError).maxOption.getOrElse(0)

/** Opt-in geometric approximation of the shared fragment evaluator over an opaque
  * surface base. Each accepted triangle has an interval error certificate; neither
  * a sampled error estimate nor native texture filtering is used to admit it.
  * Boundaries have the partition's one-sided semantics. Native coverage/AA error
  * is separate and must be measured by the backend.
  */
object SurfaceFragmentApproximation:
  private val Base = Rgba32.unsafe(184, 184, 184)

  /** Optional aliases identify coincident geometric vertices across duplicated
    * render corners. They affect edge conformity only; samples and provenance
    * remain indexed by the original input vertices.
    */
  def build(mesh: SurfaceMeshPacket, allLayers: Vector[SurfaceLayerPacket], lighting: SurfaceLighting,
      config: SurfaceFragmentApproximationConfig,
      edgeVertexAliases: Option[IntBufferView] = None): Either[SurfaceApproximationError, SurfaceFragmentApproximation] =
    val layers = allLayers.filter(_.surface == mesh.surface)
    val vertexCount = mesh.sampleNormals.getOrElse(mesh.normals).length / 3
    if edgeVertexAliases.exists(a => a.length != vertexCount || (0 until a.length).exists(a(_) < 0)) then
      return Left(SurfaceApproximationError.InvalidEdgeAliases)
    if edgeVertexAliases.exists: aliases =>
        (0 until mesh.indices.length / 3).exists: face =>
          val (a, b, c) = mesh.sourceFaceVertices(face)
          a < 0 || b < 0 || c < 0 || a >= aliases.length || b >= aliases.length || c >= aliases.length ||
            aliases(a) == aliases(b) || aliases(b) == aliases(c) || aliases(c) == aliases(a)
    then return Left(SurfaceApproximationError.InvalidEdgeAliases)
    def edgeVertex(id: Int): Int = edgeVertexAliases.fold(id)(_(id))
    val faceCount = (0 until mesh.indices.length / 3).iterator.map(mesh.sourceFace).maxOption.fold(0)(_ + 1)
    var layerIndex = 0
    while layerIndex < layers.length do
      val layer = layers(layerIndex)
      if layer.interpolation == SurfaceMapInterpolation.VertexScalar then
        if layer.scalarField.isEmpty then return Left(SurfaceApproximationError.MissingScalarField(layer.layer))
      else
        val expected = if layer.interpolation == SurfaceMapInterpolation.FaceConstant then faceCount else vertexCount
        layer.sampleColors match
          case None => return Left(SurfaceApproximationError.MissingOriginalColors(layer.layer))
          case Some(colors) if colors.length != expected =>
            return Left(SurfaceApproximationError.InvalidOriginalColors(layer.layer, expected, colors.length))
          case _ => ()
      layerIndex += 1
    val budget = SurfacePartitionBudget.make(config.maxTriangles, config.maxCutsPerFace).toOption.get
    SurfaceMappingPartition.build(mesh, layers, budget).left.map(SurfaceApproximationError.Partition.apply).flatMap: initial =>
      val pending = scala.collection.mutable.ArrayBuffer.from(initial.reverse.map(_ -> 0))
      val result = Vector.newBuilder[SurfaceConstantFragment]
      type PointKey = Vector[(Int, Double)]
      type EdgeKey = (Int, PointKey, PointKey)
      val edgeSplits = scala.collection.mutable.Map.empty[EdgeKey, PointKey]
      def pointKey(t: SurfaceMappingTriangle, w: SurfaceFaceWeights): PointKey =
        Vector(edgeVertex(t.sourceVertices._1) -> w.a, edgeVertex(t.sourceVertices._2) -> w.b, edgeVertex(t.sourceVertices._3) -> w.c).filter(_._2 != 0).sortBy(_._1)
      def local(t: SurfaceMappingTriangle, p: PointKey): SurfaceFaceWeights =
        def weight(id: Int): Double = p.find(_._1 == edgeVertex(id)).fold(0.0)(_._2)
        SurfaceFaceWeights(weight(t.sourceVertices._1), weight(t.sourceVertices._2), weight(t.sourceVertices._3))
      def edgeKey(t: SurfaceMappingTriangle, a: SurfaceFaceWeights, b: SurfaceFaceWeights): EdgeKey =
        val x = pointKey(t, a)
        val y = pointKey(t, b)
        val scope = if (x.map(_._1) ++ y.map(_._1)).distinct.length <= 2 then -1 else t.sourceFace
        var i = 0
        var order = 0
        while i < math.min(x.length, y.length) && order == 0 do
          order = java.lang.Integer.compare(x(i)._1, y(i)._1)
          if order == 0 then order = java.lang.Double.compare(x(i)._2, y(i)._2)
          i += 1
        if order == 0 then order = java.lang.Integer.compare(x.length, y.length)
        if order <= 0 then (scope, x, y) else (scope, y, x)
      var accepted = 0
      var maximumDepth = 0
      var failure: Option[SurfaceApproximationError] = None
      while pending.nonEmpty && failure.isEmpty do
        val (triangle, depth) = pending.remove(pending.length - 1)
        val interval = bounds(mesh, layers, lighting, triangle)
        if interval.midpointError <= config.maxChannelError then
          result += SurfaceConstantFragment(triangle, interval.midpoint, interval)
          accepted += 1
          maximumDepth = math.max(maximumDepth, depth)
        else if depth == config.maxDepth then
          failure = Some(SurfaceApproximationError.DepthBudgetExceeded(triangle.sourceFace, config.maxDepth, interval.midpointError))
        else if accepted.toLong + pending.length + 2 > config.maxTriangles then
          failure = Some(SurfaceApproximationError.TriangleBudgetExceeded(config.maxTriangles))
        else
          val (first, second, edgeA, edgeB, middle) = bisect(triangle)
          if middle == edgeA || middle == edgeB || first.areaFraction <= 0 || second.areaFraction <= 0 then
            failure = Some(SurfaceApproximationError.PrecisionLimit(triangle.sourceFace, interval.midpointError))
          else
            edgeSplits(edgeKey(triangle, edgeA, edgeB)) = pointKey(triangle, middle)
            pending += ((second, depth + 1))
            pending += ((first, depth + 1))
      failure match
        case Some(error) => Left(error)
        case None =>
          // Hanging vertices on a coarse edge cause native rasterization cracks.
          // Propagate all bisections across that edge, retaining each leaf's color
          // certificate because every new fan triangle lies inside the leaf.
          val conforming = Vector.newBuilder[SurfaceConstantFragment]
          var count = 0
          val leaves = result.result()
          var leafIndex = 0
          while leafIndex < leaves.length && failure.isEmpty do
            val leaf = leaves(leafIndex)
            val t = leaf.triangle
            def edge(a: SurfaceFaceWeights, b: SurfaceFaceWeights): Vector[SurfaceFaceWeights] =
              edgeSplits.get(edgeKey(t, a, b)) match
                case None => Vector(a)
                case Some(point) =>
                  val mid = local(t, point)
                  edge(a, mid) ++ edge(mid, b)
            val boundary = edge(t.a, t.b) ++ edge(t.b, t.c) ++ edge(t.c, t.a)
            val added = if boundary.length == 3 then 1 else boundary.length
            if count.toLong + added + leaves.length - leafIndex - 1 > config.maxTriangles then
              failure = Some(SurfaceApproximationError.TriangleBudgetExceeded(config.maxTriangles))
            else if boundary.length == 3 then
              conforming += leaf
              count += 1
            else
              val center = t.centroid
              var i = 0
              while i < boundary.length do
                val triangle = t.copy(a = center, b = boundary(i), c = boundary((i + 1) % boundary.length))
                if triangle.areaFraction > 0 then
                  conforming += leaf.copy(triangle = triangle)
                  count += 1
                i += 1
            leafIndex += 1
          failure match
            case Some(error) => Left(error)
            case None => Right(SurfaceFragmentApproximation(conforming.result(), initial.length, maximumDepth))

  private[view] def bounds(mesh: SurfaceMeshPacket, layers: Vector[SurfaceLayerPacket], lighting: SurfaceLighting,
      triangle: SurfaceMappingTriangle): SurfaceColorBounds =
    var under = SurfaceColorBounds(Base, Base)
    var i = 0
    while i < layers.length do
      val layer = layers(i)
      val over = layerBounds(layer, triangle)
      // On an opaque base every supported blend is monotone in backdrop and
      // source channels and affine in source alpha. Endpoint evaluation encloses
      // all combinations, including correlations lost by interval propagation.
      def channel(lower: Rgba32 => Int): (Int, Int) =
        var lo = 255
        var hi = 0
        for u <- Vector(lower(under.lower), lower(under.upper)); o <- Vector(lower(over.lower), lower(over.upper));
            a <- Vector(over.lower.alpha, over.upper.alpha) do
          val color = layer.blendMode.composite(Rgba32.unsafe(u, u, u), Rgba32.unsafe(o, o, o, a), layer.opacity).red
          lo = math.min(lo, color)
          hi = math.max(hi, color)
        (lo, hi)
      val r = channel(_.red)
      val g = channel(_.green)
      val b = channel(_.blue)
      under = SurfaceColorBounds(Rgba32.unsafe(r._1, g._1, b._1), Rgba32.unsafe(r._2, g._2, b._2))
      i += 1
    lighting match
      case SurfaceLighting.Unlit => under
      case SurfaceLighting.Directional(ambient, diffuse, dx, dy, dz) =>
        val normals = mesh.sampleNormals.getOrElse(mesh.normals)
        val (a, b, c) = triangle.sourceVertices
        val corners = Vector(triangle.a, triangle.b, triangle.c)
        val ns = corners.map(w => Vector.tabulate(3)(axis =>
          w.a * normals(a * 3 + axis) + w.b * normals(b * 3 + axis) + w.c * normals(c * 3 + axis)))
        val dots = ns.map(n => n(0) * dx + n(1) * dy + n(2) * dz)
        val normMax = math.sqrt(ns.map(n => n.map(x => x * x).sum).max)
        val closestSquares = (0 until 3).map: axis =>
          val lo = ns.map(_(axis)).min
          val hi = ns.map(_(axis)).max
          val closest = if lo <= 0 && hi >= 0 then 0.0 else math.min(math.abs(lo), math.abs(hi))
          closest * closest
        val normMin = math.sqrt(closestSquares.sum)
        val cosineLo = if normMax == 0 then 0.0 else math.max(0.0, dots.min / normMax)
        val cosineHi = if dots.max <= 0 then 0.0 else if normMin == 0 then 1.0 else math.min(1.0, dots.max / normMin)
        val low = math.min(1.0, ambient.value + diffuse.value * cosineLo)
        val high = math.min(1.0, ambient.value + diffuse.value * cosineHi)
        def lower(v: Int): Int = math.round(v * low - 1e-9).toInt.max(0).min(255)
        def upper(v: Int): Int = math.round(v * high + 1e-9).toInt.max(0).min(255)
        SurfaceColorBounds(Rgba32.unsafe(lower(under.lower.red), lower(under.lower.green), lower(under.lower.blue)),
          Rgba32.unsafe(upper(under.upper.red), upper(under.upper.green), upper(under.upper.blue)))

  private def layerBounds(layer: SurfaceLayerPacket, t: SurfaceMappingTriangle): SurfaceColorBounds =
    if !layer.coverage.contains(t.sourceFace) then
      val transparent = Rgba32.unsafe(0, 0, 0, 0)
      return SurfaceColorBounds(transparent, transparent)
    val (a, b, c) = t.sourceVertices
    val center = t.centroid
    val corners = Vector(t.a, t.b, t.c)
    val colors = layer.interpolation match
      case SurfaceMapInterpolation.VertexScalar =>
        val field = layer.scalarField.get
        def value(w: SurfaceFaceWeights): Double = SurfaceScalarInterpolation.value(
          field.samples(a), field.samples(b), field.samples(c), w.a, w.b, w.c)
        val sample = field.mapping.evaluate(value(center))
        if sample.state != ScalarSampleState.Visible then Vector(sample.color)
        else
          val segment = field.mapping.scale.segments(sample.coordinate.get.segment)
          val values = corners.map(w => segment.window.normalize(value(w)))
          val lo = values.min
          val hi = values.max
          val interiorStops = segment.ramp.stops.collect:
            case (x, color) if x >= lo && x <= hi => color
          Vector(segment.ramp.colorAt(lo), segment.ramp.colorAt(hi)) ++ interiorStops
      case SurfaceMapInterpolation.VertexColor =>
        val colors = layer.sampleColors.get
        corners.map(w => SurfaceFragmentEvaluator.interpolateColor(colors(a), colors(b), colors(c), w.a, w.b, w.c))
      case SurfaceMapInterpolation.NearestVertex =>
        Vector(Rgba32.fromPackedInt(layer.sampleColors.get(SurfaceNearestPartition.nearestVertex(a, b, c, center.a, center.b, center.c))))
      case SurfaceMapInterpolation.FaceConstant => Vector(Rgba32.fromPackedInt(layer.sampleColors.get(t.sourceFace)))
    SurfaceColorBounds(Rgba32.unsafe(colors.map(_.red).min, colors.map(_.green).min, colors.map(_.blue).min, colors.map(_.alpha).min),
      Rgba32.unsafe(colors.map(_.red).max, colors.map(_.green).max, colors.map(_.blue).max, colors.map(_.alpha).max))

  private def bisect(t: SurfaceMappingTriangle): (SurfaceMappingTriangle, SurfaceMappingTriangle, SurfaceFaceWeights, SurfaceFaceWeights, SurfaceFaceWeights) =
    def distance(a: SurfaceFaceWeights, b: SurfaceFaceWeights): Double =
      val x = a.a - b.a
      val y = a.b - b.b
      val z = a.c - b.c
      x * x + y * y + z * z
    val ab = distance(t.a, t.b)
    val bc = distance(t.b, t.c)
    val ca = distance(t.c, t.a)
    if ab >= bc && ab >= ca then
      val mid = t.a.interpolate(t.b, 0.5)
      (t.copy(b = mid), t.copy(a = mid), t.a, t.b, mid)
    else if bc >= ca then
      val mid = t.b.interpolate(t.c, 0.5)
      (t.copy(c = mid), t.copy(b = mid), t.b, t.c, mid)
    else
      val mid = t.c.interpolate(t.a, 0.5)
      (t.copy(c = mid), t.copy(a = mid), t.c, t.a, mid)
