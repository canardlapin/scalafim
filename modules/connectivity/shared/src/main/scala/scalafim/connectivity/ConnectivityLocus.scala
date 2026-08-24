package scalafim.connectivity

import scalafim.locus.{DomainFactory, FiniteSpace, Point, Region, SpaceKey}

trait NodeLocusDomain:
  type N
  val axis: NodeAxis
  val space: FiniteSpace[N]

  final def pointFor(id: NodeId): Option[Point[N]] =
    axis.indexOf(id).flatMap(space.indexOption)

  final def nodeAt(point: Point[N]): NodeSpec =
    axis.nodes(point.value)

object NodeLocusDomain:
  private[connectivity] def make(
      requestedAxis: NodeAxis
  ): NodeLocusDomain =
    val key =
      SpaceKey.unsafe(
        s"scalafim:connectivity:nodes:${axisIdentity(requestedAxis)}"
      )
    val resolution =
      DomainFactory.unsafeRestore(key, requestedAxis.size)
    new NodeLocusDomain:
      type N = resolution.S
      val axis: NodeAxis = requestedAxis
      val space: FiniteSpace[N] = resolution.space

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
    space.indexOption(index.value).get

  final def edgeAt(point: Point[E]): EdgeRef =
    edgeSpace.edge(point.value)

object EdgeLocusDomain:
  private[connectivity] def make(
      requestedSpace: EdgeSpace
  ): EdgeLocusDomain =
    val target =
      requestedSpace.targetAxis
        .map(_.locus.space.id.value)
        .getOrElse("square")
    val key =
      SpaceKey.unsafe(
        s"scalafim:connectivity:edges:" +
          s"${requestedSpace.topology.label}:" +
          s"${requestedSpace.order.label}:" +
          s"${requestedSpace.sourceAxis.locus.space.id.value}:$target"
      )
    val resolution =
      DomainFactory.unsafeRestore(key, requestedSpace.size)
    new EdgeLocusDomain:
      type E = resolution.S
      val edgeSpace: EdgeSpace = requestedSpace
      val space: FiniteSpace[E] = resolution.space

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
