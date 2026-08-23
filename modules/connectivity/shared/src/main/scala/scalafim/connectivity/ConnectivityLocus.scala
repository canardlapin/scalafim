package scalafim.connectivity

import locus4s.DomainRegistry
import scalafim.locus.{
  DomainFactory,
  DomainFactoryError,
  FiniteSpace,
  Point,
  Region,
  SpaceKey
}

trait NodeLocusDomain:
  type N
  val axis: NodeAxis
  val registry: DomainRegistry
  val space: FiniteSpace[N]

  final def pointFor(id: NodeId): Option[Point[N]] =
    axis.indexOf(id).flatMap(space.indexOption)

  final def nodeAt(point: Point[N]): NodeSpec =
    axis.nodes(point.value)

object NodeLocusDomain:
  private[connectivity] def make(
      registry: DomainRegistry,
      requestedAxis: NodeAxis
  ): Either[DomainFactoryError, NodeLocusDomain] =
    val key =
      SpaceKey.unsafe(
        s"scalafim:connectivity:nodes:${axisIdentity(requestedAxis)}"
      )
    DomainFactory
      .restore(registry, key, requestedAxis.size)
      .map: resolution =>
        new NodeLocusDomain:
          type N = resolution.S
          val axis: NodeAxis = requestedAxis
          val registry: DomainRegistry = resolution.registry
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
  val registry: DomainRegistry
  val space: FiniteSpace[E]

  final def pointFor(index: EdgeSpaceIx): Point[E] =
    space.indexOption(index.value).get

  final def edgeAt(point: Point[E]): EdgeRef =
    edgeSpace.edge(point.value)

object EdgeLocusDomain:
  private[connectivity] def make(
      registry: DomainRegistry,
      requestedSpace: EdgeSpace
  ): Either[DomainFactoryError, EdgeLocusDomain] =
    requestedSpace.sourceAxis.locusIn(registry).flatMap: source =>
      val target =
        requestedSpace.targetAxis match
          case Some(axis) =>
            axis
              .locusIn(source.registry)
              .map(locus => (locus.registry, locus.space.id.value))
          case None =>
            Right((source.registry, "square"))
      target.flatMap: (axisRegistry, targetId) =>
        val key =
          SpaceKey.unsafe(
            s"scalafim:connectivity:edges:" +
              s"${requestedSpace.topology.label}:" +
              s"${requestedSpace.order.label}:" +
              s"${source.space.id.value}:$targetId"
          )
        DomainFactory
          .restore(axisRegistry, key, requestedSpace.size)
          .map: resolution =>
            new EdgeLocusDomain:
              type E = resolution.S
              val edgeSpace: EdgeSpace = requestedSpace
              val registry: DomainRegistry = resolution.registry
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
