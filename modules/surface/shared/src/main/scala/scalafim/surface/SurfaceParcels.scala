package scalafim.surface

import scala.util.control.NonFatal

enum FragmentedParcelPolicy:
  case Error, Largest, Each, Merge

enum ParcelDistanceMethod:
  case Centroid, Medoid, Minimum

final class ParcelKey private (
  val parcelLabel: ParcelLabel,
  val parcelPart: Option[ParcelPart]
):
  def label: Int =
    parcelLabel.value

  def part: Option[Int] =
    parcelPart.map(_.value)

  def display: String =
    part.fold(label.toString)(p => s"$label.$p")

  override def equals(other: Any): Boolean =
    other match
      case that: ParcelKey =>
        parcelLabel == that.parcelLabel && parcelPart == that.parcelPart
      case _ => false

  override def hashCode(): Int =
    31 * parcelLabel.hashCode() + parcelPart.hashCode()

  override def toString: String =
    part match
      case None => s"ParcelKey($label)"
      case Some(value) => s"ParcelKey($label, Some($value))"

object ParcelKey:
  def apply(label: Int, part: Option[Int] = None): ParcelKey =
    typed(ParcelLabel(label), part.map(ParcelPart(_)))

  def typed(label: ParcelLabel, part: Option[ParcelPart] = None): ParcelKey =
    new ParcelKey(label, part)

  def fromEither(label: Int, part: Option[Int] = None): Either[SurfaceError, ParcelKey] =
    try scala.util.Right(apply(label, part))
    catch case NonFatal(error) => scala.util.Left(SurfaceError.InvalidParcel(SurfaceError.reason(error)))

  def unapply(key: ParcelKey): Some[(Int, Option[Int])] =
    Some((key.label, key.part))

final case class ParcelUnit(
  key: ParcelKey,
  vertices: Vector[VertexId],
  info: Option[LabelInfo]
):
  require(vertices.nonEmpty, "parcel unit must contain at least one vertex")
  require(vertices.map(_.index).distinct.length == vertices.length, "parcel vertices must be unique")

  def label: Int =
    key.label

  def parcelLabel: ParcelLabel =
    key.parcelLabel

  def part: Option[Int] =
    key.part

  def parcelPart: Option[ParcelPart] =
    key.parcelPart

  def size: Int =
    vertices.length

final case class ParcelDistanceMatrix(
  parcels: Vector[ParcelUnit],
  data: Vector[Double]
):
  require(data.length == parcels.length * parcels.length, "parcel distance matrix shape mismatch")

  def size: Int =
    parcels.length

  def apply(row: Int, col: Int): Double =
    require(row >= 0 && row < size, "parcel distance matrix row out of range")
    require(col >= 0 && col < size, "parcel distance matrix column out of range")
    data(row * size + col)

final case class ParcelContactMatrix(
  parcels: Vector[ParcelUnit],
  counts: Vector[Int]
):
  require(counts.length == parcels.length * parcels.length, "parcel contact matrix shape mismatch")

  def size: Int =
    parcels.length

  def count(row: Int, col: Int): Int =
    require(row >= 0 && row < size, "parcel contact matrix row out of range")
    require(col >= 0 && col < size, "parcel contact matrix column out of range")
    counts(row * size + col)

  def touches(row: Int, col: Int): Boolean =
    count(row, col) > 0

/** One canonical mesh edge on which the endpoint parcel assignments differ.
  * `None` represents an unlabeled or explicitly ignored endpoint, which is
  * useful for drawing the exterior of an ROI without a second topology graph.
  */
final case class ParcelBoundaryEdge(
  edge: Edge,
  atA: Option[ParcelLabel],
  atB: Option[ParcelLabel]
):
  require(atA != atB, "parcel boundary endpoints must have different assignments")

object SurfaceParcels:

  def boundaryEdges(
    labeled: LabeledSurface,
    topology: MeshTopology,
    ignoredLabels: Set[Int] = Set.empty,
    includeExterior: Boolean = false
  ): Vector[ParcelBoundaryEdge] =
    require(labeled.geometry.vertexCount == topology.mesh.vertexCount, "labeled surface and topology vertex counts must match")
    require(labeled.geometry.mesh.hasSameTopology(topology.mesh), "labeled surface and topology must share ordered triangle topology")
    val assignments = Array.fill[Option[ParcelLabel]](topology.mesh.vertexCount)(None)
    var index = 0
    while index < labeled.size do
      val label = labeled.labels(index)
      if !ignoredLabels(label) then assignments(labeled.indices(index)) = Some(ParcelLabel(label))
      index += 1
    topology.edges.flatMap: edge =>
      val a = assignments(edge.a.index)
      val b = assignments(edge.b.index)
      if a == b || (!includeExterior && (a.isEmpty || b.isEmpty)) then None
      else Some(ParcelBoundaryEdge(edge, a, b))

  def units(
    labeled: LabeledSurface,
    topology: MeshTopology,
    policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
    ignoredLabels: Set[Int] = Set.empty
  ): Vector[ParcelUnit] =
    require(labeled.geometry.vertexCount == topology.mesh.vertexCount, "labeled surface and topology vertex counts must match")
    require(labeled.geometry.mesh.hasSameTopology(topology.mesh), "labeled surface and topology must share ordered triangle topology")

    val byLabel = scala.collection.mutable.Map.empty[Int, scala.collection.mutable.ArrayBuffer[VertexId]]
    var i = 0
    while i < labeled.size do
      val label = labeled.labels(i)
      if !ignoredLabels(label) then
        byLabel.getOrElseUpdate(label, scala.collection.mutable.ArrayBuffer.empty) += VertexId.unsafe(labeled.indices(i))
      i += 1

    byLabel.toVector.sortBy(_._1).flatMap { case (label, buffer) =>
      val info = labeled.info(label)
      val components =
        SurfaceTopologyTraversal
          .connectedComponents(topology, buffer.iterator.map(_.index).toSet)
          .sortBy(vertices => (-vertices.length, vertices.head))
          .map(_.map(VertexId.unsafe))
      policy match
        case FragmentedParcelPolicy.Error =>
          require(components.length == 1, s"parcel label $label is fragmented")
          Vector(ParcelUnit(ParcelKey(label), components.head, info))
        case FragmentedParcelPolicy.Largest =>
          Vector(ParcelUnit(ParcelKey(label), components.head, info))
        case FragmentedParcelPolicy.Each =>
          components.zipWithIndex.map { case (vertices, idx) =>
            ParcelUnit(ParcelKey(label, Some(idx + 1)), vertices, info)
          }
        case FragmentedParcelPolicy.Merge =>
          val vertices = components.flatten.distinct.sortBy(_.index)
          Vector(ParcelUnit(ParcelKey(label), vertices, info))
    }

  def centroidVertex(topology: MeshTopology, parcel: ParcelUnit): VertexId =
    val centroid =
      parcel.vertices
        .map(topology.mesh.vertex)
        .foldLeft(Point3D.Zero)(_ + _) * (1.0 / parcel.vertices.length.toDouble)

    parcel.vertices.minBy { vertex =>
      val distance = (topology.mesh.vertex(vertex) - centroid).norm
      (distance, vertex.index)
    }

  def geodesicMedoidVertex(
    topology: MeshTopology,
    parcel: ParcelUnit,
    metric: DistanceMetric = DistanceMetric.Geodesic,
    edgeWeights: Option[Seq[Double]] = None
  ): VertexId =
    if parcel.vertices.length == 1 then parcel.vertices.head
    else
      val distances =
        SurfaceGeodesics.distanceMatrix(
          topology = topology,
          sources = parcel.vertices,
          targets = parcel.vertices,
          metric = metric,
          edgeWeights = edgeWeights
        )

      val scored =
        parcel.vertices.zipWithIndex.map { case (vertex, row) =>
          var col = 0
          var score = 0.0
          while col < distances.cols do
            score += distances(row, col)
            col += 1
          vertex -> score
        }

      val best = scored.minBy { case (vertex, score) => (score, vertex.index) }
      require(best._2.isFinite, s"parcel ${parcel.key.display} contains unreachable vertices")
      best._1

  def distanceMatrix(
    labeled: LabeledSurface,
    topology: MeshTopology,
    method: ParcelDistanceMethod = ParcelDistanceMethod.Centroid,
    metric: DistanceMetric = DistanceMetric.Geodesic,
    policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
    ignoredLabels: Set[Int] = Set.empty
  ): ParcelDistanceMatrix =
    val parcels = units(labeled, topology, policy, ignoredLabels)
    distanceMatrix(parcels, topology, method, metric)

  def distanceMatrix(
    parcels: Vector[ParcelUnit],
    topology: MeshTopology,
    method: ParcelDistanceMethod,
    metric: DistanceMetric
  ): ParcelDistanceMatrix =
    val data = Array.fill(parcels.length * parcels.length)(0.0)

    method match
      case ParcelDistanceMethod.Centroid =>
        fillRepresentativeDistances(parcels, topology, metric, data, parcel => centroidVertex(topology, parcel))
      case ParcelDistanceMethod.Medoid =>
        fillRepresentativeDistances(parcels, topology, metric, data, parcel => geodesicMedoidVertex(topology, parcel, metric))
      case ParcelDistanceMethod.Minimum =>
        fillMinimumDistances(parcels, topology, metric, data)

    ParcelDistanceMatrix(parcels, data.toVector)

  def boundaryContacts(
    labeled: LabeledSurface,
    topology: MeshTopology,
    policy: FragmentedParcelPolicy = FragmentedParcelPolicy.Error,
    ignoredLabels: Set[Int] = Set.empty
  ): ParcelContactMatrix =
    boundaryContacts(units(labeled, topology, policy, ignoredLabels), topology)

  def boundaryContacts(parcels: Vector[ParcelUnit], topology: MeshTopology): ParcelContactMatrix =
    val indexByVertex = scala.collection.mutable.Map.empty[Int, Int]
    parcels.zipWithIndex.foreach { case (parcel, idx) =>
      parcel.vertices.foreach(vertex => indexByVertex(vertex.index) = idx)
    }

    val counts = Array.fill(parcels.length * parcels.length)(0)
    topology.edges.foreach { edge =>
      val ai = indexByVertex.get(edge.a.index)
      val bi = indexByVertex.get(edge.b.index)
      (ai, bi) match
        case (Some(a), Some(b)) if a != b =>
          counts(a * parcels.length + b) += 1
          counts(b * parcels.length + a) += 1
        case _ =>
          ()
    }

    ParcelContactMatrix(parcels, counts.toVector)

  private def fillRepresentativeDistances(
    parcels: Vector[ParcelUnit],
    topology: MeshTopology,
    metric: DistanceMetric,
    data: Array[Double],
    representative: ParcelUnit => VertexId
  ): Unit =
    if parcels.nonEmpty then
      val vertices = parcels.map(representative)
      val matrix = SurfaceGeodesics.distanceMatrix(topology, vertices, vertices, metric)
      var row = 0
      while row < parcels.length do
        var col = 0
        while col < parcels.length do
          data(row * parcels.length + col) = matrix(row, col)
          col += 1
        row += 1

  private def fillMinimumDistances(
    parcels: Vector[ParcelUnit],
    topology: MeshTopology,
    metric: DistanceMetric,
    data: Array[Double]
  ): Unit =
    var row = 0
    while row < parcels.length do
      var col = row + 1
      while col < parcels.length do
        val matrix = SurfaceGeodesics.distanceMatrix(topology, parcels(row).vertices, parcels(col).vertices, metric)
        val distance = matrix.data.min
        data(row * parcels.length + col) = distance
        data(col * parcels.length + row) = distance
        col += 1
      row += 1
