package scalafim.graph

import scalafim.locus.*

trait VertexLocus[K, V]:
  type S
  val basis: VertexBasis[K, V]
  val space: FiniteSpace[S]

  def pointOf(index: VertexIx): Point[S] =
    space.point(index.toInt).get

  def vertexOf(point: Point[S]): VertexIx =
    require(space.contains(point), s"point ${point.ordinal} is outside vertex locus")
    VertexIx.unsafe(point.ordinal)

object VertexLocus:
  def make[K, V](
      key: SpaceKey,
      sourceBasis: VertexBasis[K, V]
  ): VertexLocus[K, V] =
    final class VertexSpace
    val finiteSpace = FiniteSpace.make[VertexSpace](key, sourceBasis.size).toOption.get
    new VertexLocus[K, V]:
      type S = VertexSpace
      val basis: VertexBasis[K, V] = sourceBasis
      val space: FiniteSpace[VertexSpace] = finiteSpace

enum RelationLoopPolicy:
  case Reject
  case Drop

sealed trait RelationGraphError[+K]:
  def message: String

object RelationGraphError:
  final case class BasisMismatch[K](expectedKeys: Vector[K], actualKeys: Vector[K])
      extends RelationGraphError[K]:
    def message: String =
      "graph basis does not match the vertex locus key order"

  final case class WrongSpace(error: SpaceMismatch) extends RelationGraphError[Nothing]:
    def message: String =
      error.message

  final case class SelfLoops[K](keys: Vector[K]) extends RelationGraphError[K]:
    def message: String =
      s"relation contains forbidden self loops at ${keys.mkString(", ")}"

  final case class InvalidGraph[K](errors: GraphBuildErrors[K]) extends RelationGraphError[K]:
    def message: String =
      errors.message

object GraphRelation:
  def fromDirected[K, V, E](
      locus: VertexLocus[K, V],
      graph: DirectedGraph[K, V, E]
  )(
      include: E => Boolean
  ): Either[RelationGraphError[K], Relation[locus.S, locus.S]] =
    if graph.basis != locus.basis then
      Left(RelationGraphError.BasisMismatch(locus.basis.keys, graph.basis.keys))
    else
      val rows = Array.fill(locus.space.size)(Array.newBuilder[Int])
      graph.edges.foreach: edge =>
        if include(edge.value) then
          rows(edge.endpoints.first.toInt) += edge.endpoints.second.toInt
      Right:
        Relation.fromOrdinalRows(
          locus.space,
          locus.space,
          rows.map(_.result())
        ).toOption.get

  def fromUndirected[K, V, E](
      locus: VertexLocus[K, V],
      graph: UndirectedGraph[K, V, E]
  )(
      include: E => Boolean
  ): Either[RelationGraphError[K], SymmetricRelation[locus.S]] =
    if graph.basis != locus.basis then
      Left(RelationGraphError.BasisMismatch(locus.basis.keys, graph.basis.keys))
    else
      val rows = Array.fill(locus.space.size)(Array.newBuilder[Int])
      graph.edges.foreach: edge =>
        if include(edge.value) then
          val first = edge.endpoints.first.toInt
          val second = edge.endpoints.second.toInt
          rows(first) += second
          rows(second) += first
      val relation =
        Relation.fromOrdinalRows(locus.space, locus.space, rows.map(_.result())).toOption.get
      Right(SymmetricRelation.validate(relation).toOption.get)

  def toDirected[K, V, E](
      locus: VertexLocus[K, V]
  )(
      relation: Relation[locus.S, locus.S],
      loopPolicy: RelationLoopPolicy
  )(
      edgeValue: (K, K) => E
  ): Either[RelationGraphError[K], DirectedGraph[K, V, E]] =
    validateSpace(locus, relation).flatMap: _ =>
      val rows = relation.ordinalRows
      val loops = loopKeys(locus, rows)
      if loops.nonEmpty && loopPolicy == RelationLoopPolicy.Reject then
        Left(RelationGraphError.SelfLoops(loops))
      else
        val edges = Vector.newBuilder[(K, K, E)]
        var source = 0
        while source < rows.length do
          var i = 0
          while i < rows(source).length do
            val target = rows(source)(i)
            if source != target then
              val sourceKey = locus.basis.keys(source)
              val targetKey = locus.basis.keys(target)
              edges += ((sourceKey, targetKey, edgeValue(sourceKey, targetKey)))
            i += 1
          source += 1
        Graph.directed(locus.basis, edges.result()).left.map(RelationGraphError.InvalidGraph.apply)

  def toUndirected[K, V, E](
      locus: VertexLocus[K, V]
  )(
      relation: SymmetricRelation[locus.S],
      loopPolicy: RelationLoopPolicy
  )(
      edgeValue: (K, K) => E
  ): Either[RelationGraphError[K], UndirectedGraph[K, V, E]] =
    validateSpace(locus, relation.relation).flatMap: _ =>
      val rows = relation.relation.ordinalRows
      val loops = loopKeys(locus, rows)
      if loops.nonEmpty && loopPolicy == RelationLoopPolicy.Reject then
        Left(RelationGraphError.SelfLoops(loops))
      else
        val edges = Vector.newBuilder[(K, K, E)]
        var source = 0
        while source < rows.length do
          var i = 0
          while i < rows(source).length do
            val target = rows(source)(i)
            if source < target then
              val sourceKey = locus.basis.keys(source)
              val targetKey = locus.basis.keys(target)
              edges += ((sourceKey, targetKey, edgeValue(sourceKey, targetKey)))
            i += 1
          source += 1
        Graph.undirected(locus.basis, edges.result()).left.map(RelationGraphError.InvalidGraph.apply)

  private def validateSpace[K, V](
      locus: VertexLocus[K, V],
      relation: Relation[locus.S, locus.S]
  ): Either[RelationGraphError[K], Unit] =
    if !locus.space.sameIdentityAs(relation.from) then
      Left:
        RelationGraphError.WrongSpace:
          SpaceMismatch(
            locus.space.key,
            locus.space.size,
            relation.from.key,
            relation.from.size
          )
    else if !locus.space.sameIdentityAs(relation.to) then
      Left:
        RelationGraphError.WrongSpace:
          SpaceMismatch(
            locus.space.key,
            locus.space.size,
            relation.to.key,
            relation.to.size
          )
    else
      Right(())

  private def loopKeys[K, V](
      locus: VertexLocus[K, V],
      rows: Array[Array[Int]]
  ): Vector[K] =
    Vector.tabulate(rows.length)(i => i).collect:
      case ordinal if rows(ordinal).contains(ordinal) => locus.basis.keys(ordinal)
