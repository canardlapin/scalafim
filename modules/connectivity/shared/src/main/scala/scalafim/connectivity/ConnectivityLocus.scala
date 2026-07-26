package scalafim.connectivity

import scalafim.locus.{FiniteSpace, Point, Region, SpaceKey}

trait NodeLocusDomain:
  type N
  val axis: NodeAxis
  val space: FiniteSpace[N]

  final def pointFor(id: NodeId): Option[Point[N]] =
    axis.indexOf(id).flatMap(space.point)

  final def nodeAt(point: Point[N]): NodeSpec =
    axis.nodes(point.ordinal)

object NodeLocusDomain:
  private[connectivity] def make(
      requestedAxis: NodeAxis
  ): NodeLocusDomain =
    final class Node
    val key =
      SpaceKey.unsafe(
        s"scalafim:connectivity:nodes:${axisIdentity(requestedAxis)}"
      )
    new NodeLocusDomain:
      type N = Node
      val axis: NodeAxis = requestedAxis
      val space: FiniteSpace[Node] =
        FiniteSpace.make[Node](key, requestedAxis.size).toOption.get

  private def axisIdentity(axis: NodeAxis): String =
    encode(
      axis.provenance.description +:
        axis.nodes.flatMap: node =>
          Vector(
            node.id.value,
            node.label,
            node.system.fold("")(_.value)
          )
    )

  private def encode(values: Vector[String]): String =
    values.map(value => s"${value.length}:$value").mkString

trait EdgeLocusDomain:
  type E
  val edgeSpace: EdgeSpace
  val space: FiniteSpace[E]

  final def pointFor(index: EdgeSpaceIx): Point[E] =
    space.point(index.value).get

  final def edgeAt(point: Point[E]): EdgeRef =
    edgeSpace.edge(point.ordinal)

object EdgeLocusDomain:
  private[connectivity] def make(
      requestedSpace: EdgeSpace
  ): EdgeLocusDomain =
    final class Edge
    val target =
      requestedSpace.targetAxis
        .map(_.locus.space.key.value)
        .getOrElse("square")
    val key =
      SpaceKey.unsafe(
        s"scalafim:connectivity:edges:" +
          s"${requestedSpace.topology.label}:" +
          s"${requestedSpace.order.label}:" +
          s"${requestedSpace.sourceAxis.locus.space.key.value}:$target"
      )
    new EdgeLocusDomain:
      type E = Edge
      val edgeSpace: EdgeSpace = requestedSpace
      val space: FiniteSpace[Edge] =
        FiniteSpace.make[Edge](key, requestedSpace.size).toOption.get

trait EdgeMaskRegion:
  type E
  val space: FiniteSpace[E]
  val value: Region[E]

object EdgeMaskRegion:
  private[connectivity] def from(
      edgeSpace: EdgeSpace,
      indices: IterableOnce[Int]
  ): EdgeMaskRegion =
    val domain = edgeSpace.locus
    val selected =
      Region
        .fromOrdinals(domain.space, indices)
        .toOption
        .get
    new EdgeMaskRegion:
      type E = domain.E
      val space: FiniteSpace[E] = domain.space
      val value: Region[E] = selected
