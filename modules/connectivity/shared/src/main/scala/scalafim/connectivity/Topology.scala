package scalafim.connectivity

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

enum EdgeTopology:
  case Undirected
  case Directed
  case Rectangular

  def label: String =
    this match
      case Undirected  => "undirected"
      case Directed    => "directed"
      case Rectangular => "rectangular"

enum VectorizationOrder:
  case ScalaNative
  case AriadneCompatible

  def label: String =
    this match
      case ScalaNative        => "scala-native"
      case AriadneCompatible  => "ariadne-compatible"

final case class EdgeRef(
    sourceIndex: Int,
    targetIndex: Int,
    source: NodeId,
    target: NodeId
):
  require(sourceIndex >= 0 && targetIndex >= 0, "edge indices must be non-negative")

final class EdgeSpace private (
    val topology: EdgeTopology,
    val order: VectorizationOrder,
    val sourceAxis: NodeAxis,
    val targetAxis: Option[NodeAxis],
    val edges: Vector[EdgeRef]
):
  def size: Int =
    edges.length

  def isSquare: Boolean =
    topology != EdgeTopology.Rectangular

  def rows: Int =
    sourceAxis.size

  def cols: Int =
    targetAxis.map(_.size).getOrElse(sourceAxis.size)

  def edge(index: Int): EdgeRef =
    edges(index)

  def sameOrderingAs(other: EdgeSpace): Boolean =
    topology == other.topology &&
      order == other.order &&
      sourceAxis.sameIdentityAs(other.sourceAxis) &&
      sourceAxis.sameScientificBasisAs(other.sourceAxis) &&
      ((targetAxis, other.targetAxis) match
        case (None, None) => true
        case (Some(left), Some(right)) =>
          left.sameIdentityAs(right) && left.sameScientificBasisAs(right)
        case _ => false)

  def description: String =
    s"${topology.label}:${order.label}:${rows}x${cols}:${size}"

object EdgeSpace:
  def undirected(
      nodeAxis: NodeAxis,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, EdgeSpace] =
    if nodeAxis.size < 2 then Left(ConnectivityError.InvalidDimension("undirected node count", nodeAxis.size))
    else Right(new EdgeSpace(EdgeTopology.Undirected, order, nodeAxis, None, squareEdges(nodeAxis, directed = false, order)))

  def directed(
      nodeAxis: NodeAxis,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, EdgeSpace] =
    if nodeAxis.size < 2 then Left(ConnectivityError.InvalidDimension("directed node count", nodeAxis.size))
    else Right(new EdgeSpace(EdgeTopology.Directed, order, nodeAxis, None, squareEdges(nodeAxis, directed = true, order)))

  def rectangular(
      seedAxis: NodeAxis,
      targetAxis: NodeAxis,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, EdgeSpace] =
    val edges = rectangularEdges(seedAxis, targetAxis, order)
    Right(new EdgeSpace(EdgeTopology.Rectangular, order, seedAxis, Some(targetAxis), edges))

  def unsafeUndirected(nodeAxis: NodeAxis, order: VectorizationOrder = VectorizationOrder.ScalaNative): EdgeSpace =
    undirected(nodeAxis, order).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def squareEdges(nodeAxis: NodeAxis, directed: Boolean, order: VectorizationOrder): Vector[EdgeRef] =
    val out = Vector.newBuilder[EdgeRef]
    val ids = nodeAxis.ids
    order match
      case VectorizationOrder.ScalaNative =>
        var source = 0
        while source < ids.length do
          var target = 0
          while target < ids.length do
            if source != target && (directed || source < target) then
              out += EdgeRef(source, target, ids(source), ids(target))
            target += 1
          source += 1
      case VectorizationOrder.AriadneCompatible =>
        var target = 0
        while target < ids.length do
          var source = 0
          while source < ids.length do
            if source != target && (directed || source < target) then
              out += EdgeRef(source, target, ids(source), ids(target))
            source += 1
          target += 1
    out.result()

  private def rectangularEdges(
      seedAxis: NodeAxis,
      targetAxis: NodeAxis,
      order: VectorizationOrder
  ): Vector[EdgeRef] =
    val out = Vector.newBuilder[EdgeRef]
    val seeds = seedAxis.ids
    val targets = targetAxis.ids
    order match
      case VectorizationOrder.ScalaNative =>
        var seed = 0
        while seed < seeds.length do
          var target = 0
          while target < targets.length do
            out += EdgeRef(seed, target, seeds(seed), targets(target))
            target += 1
          seed += 1
      case VectorizationOrder.AriadneCompatible =>
        var target = 0
        while target < targets.length do
          var seed = 0
          while seed < seeds.length do
            out += EdgeRef(seed, target, seeds(seed), targets(target))
            seed += 1
          target += 1
    out.result()

final class EdgeVector private (
    val space: EdgeSpace,
    val values: DoubleVector
):
  def length: Int =
    values.length

  def apply(index: Int): Double =
    values(index)

  def toVector: Vector[Double] =
    values.toVector

object EdgeVector:
  def from(space: EdgeSpace, values: Iterable[Double]): Either[ConnectivityError, EdgeVector] =
    val vector = values.toVector
    if vector.length != space.size then Left(ConnectivityError.EdgeVectorLengthMismatch(space.size, vector.length))
    else
      var i = 0
      var error = Option.empty[ConnectivityError]
      while i < vector.length && error.isEmpty do
        val value = vector(i)
        if !value.isFinite then error = Some(ConnectivityError.NonFiniteValue("edge vector", i, value))
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(new EdgeVector(space, DoubleVector.fromSeq(vector)))

  private[connectivity] def unsafe(space: EdgeSpace, values: Array[Double]): EdgeVector =
    new EdgeVector(space, DoubleVector.unsafe(values))

object EdgeVectorizer:
  def fromMatrix(matrix: DoubleMatrix, space: EdgeSpace): Either[ConnectivityError, EdgeVector] =
    if matrix.rows != space.rows || matrix.cols != space.cols then
      Left(ConnectivityError.MatrixShapeMismatch(s"edge space expected ${space.rows}x${space.cols} matrix, got ${matrix.rows}x${matrix.cols}"))
    else
      firstNonFinite(matrix, "connectivity matrix") match
        case Some(error) => Left(error)
        case None =>
          val out = new Array[Double](space.size)
          var i = 0
          while i < space.edges.length do
            val edge = space.edges(i)
            out(i) = matrix(edge.sourceIndex, edge.targetIndex)
            i += 1
          Right(EdgeVector.unsafe(space, out))

  def toMatrix(vector: EdgeVector, diagonalPolicy: DiagonalPolicy = DiagonalPolicy.StructuralZero): DoubleMatrix =
    val matrix = DoubleMatrix.zeros(vector.space.rows, vector.space.cols)
    val out = matrix.dataArray
    var i = 0
    while i < vector.space.edges.length do
      val edge = vector.space.edges(i)
      out(edge.sourceIndex * vector.space.cols + edge.targetIndex) = vector(i)
      if vector.space.topology == EdgeTopology.Undirected then
        out(edge.targetIndex * vector.space.cols + edge.sourceIndex) = vector(i)
      i += 1
    if vector.space.topology != EdgeTopology.Rectangular then
      diagonalPolicy.materializedValue.foreach: value =>
        var diag = 0
        while diag < vector.space.rows do
          out(diag * vector.space.cols + diag) = value
          diag += 1
    DoubleMatrix.unsafe(vector.space.rows, vector.space.cols, out)
