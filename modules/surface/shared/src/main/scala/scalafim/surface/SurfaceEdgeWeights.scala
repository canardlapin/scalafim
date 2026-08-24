package scalafim.surface

import locus4s.FiniteDomain
import locus4s.Index
import locus4s.data.Field
import mesh4s.TriangleTopology

enum SurfaceEdgeWeightError derives CanEqual:
  case WrongValueCount(expected: Int, actual: Int)
  case NonFiniteWeight(edgeOrdinal: Int, value: Double)
  case NegativeWeight(edgeOrdinal: Int, value: Double)
  case WrongTopologyOwner(expectedFingerprint: String, actualFingerprint: String)
  case LegacyPermutationMismatch(detail: String)

  def message: String =
    this match
      case WrongValueCount(expected, actual) =>
        s"surface edge weights require $expected values, found $actual"
      case NonFiniteWeight(edge, value) =>
        s"surface edge weight $edge must be finite, found $value"
      case NegativeWeight(edge, value) =>
        s"surface edge weight $edge must be non-negative, found $value"
      case WrongTopologyOwner(expected, actual) =>
        s"surface edge weights belong to another topology owner: " +
          s"expected connectivity $expected, found $actual"
      case LegacyPermutationMismatch(detail) =>
        s"legacy lexicographic edge permutation failed: $detail"

/** Non-negative finite weights over one exact mesh4s edge-domain owner. */
sealed trait SurfaceEdgeWeights:
  type Edge

  val topology: TriangleTopology { type Edge = SurfaceEdgeWeights.this.Edge }
  val values: Field[Edge, Double]

  final def edges: FiniteDomain[Edge] =
    topology.edges

  final def apply(edge: Index[Edge]): Double =
    values(edge)

  final def valuesInTopologyOrder: Vector[Double] =
    values.valuesInDomainOrder.toVector

  final def belongsTo(surface: MeshTopology): Boolean =
    topology eq surface.mesh.topology

  private[surface] final def valueAtOrdinal(ordinal: Int): Double =
    values(topology.edges.indexAtValidatedOrdinal(ordinal))

object SurfaceEdgeWeights:

  /** Construct weights in mesh4s's exact edge-domain order. */
  def fromTopologyOrder(
      surface: MeshTopology,
      input: IterableOnce[Double]
  ): Either[
    SurfaceEdgeWeightError,
    SurfaceEdgeWeights { type Edge = surface.mesh.topology.Edge }
  ] =
    val data = input.iterator.toVector
    validate(surface.edgeCount, data).map: _ =>
      build(surface.mesh.topology, data)

  /** Explicit compatibility bridge for ScalaFIM's former lexicographic
    * `(minVertex, maxVertex)` sequence order. Values are matched by endpoints
    * and permuted into the exact mesh4s edge owner; no positional
    * reinterpretation occurs.
    */
  def fromLegacyLexicographic(
      surface: MeshTopology,
      input: IterableOnce[Double]
  ): Either[
    SurfaceEdgeWeightError,
    SurfaceEdgeWeights { type Edge = surface.mesh.topology.Edge }
  ] =
    val data = input.iterator.toVector
    if data.length != surface.edgeCount then
      Left(SurfaceEdgeWeightError.WrongValueCount(surface.edgeCount, data.length))
    else
      val legacy = surface.edges
      val byEndpoints =
        legacy.iterator.zip(data.iterator).map: (edge, weight) =>
          (edge.a.index, edge.b.index) -> weight
        .toMap
      if byEndpoints.size != surface.edgeCount then
        Left:
          SurfaceEdgeWeightError.LegacyPermutationMismatch(
            s"expected ${surface.edgeCount} unique endpoint pairs, found ${byEndpoints.size}"
          )
      else
        val owner = surface.mesh.topology
        val ordered = Array.ofDim[Double](owner.edges.size)
        var ordinal = 0
        var missing = Option.empty[(Int, Int)]
        while ordinal < owner.edges.size && missing.isEmpty do
          val edge = owner.edges.indexAtValidatedOrdinal(ordinal)
          val endpoints = owner.endpointsOf(edge)
          val key = endpoints.first.ordinal -> endpoints.second.ordinal
          byEndpoints.get(key) match
            case Some(weight) => ordered(ordinal) = weight
            case None => missing = Some(key)
          ordinal += 1
        missing match
          case Some((first, second)) =>
            Left:
              SurfaceEdgeWeightError.LegacyPermutationMismatch(
                s"mesh edge ($first,$second) was absent from the compatibility view"
              )
          case None =>
            fromTopologyOrder(surface, ordered.toVector)

  /** Euclidean realization lengths in the mesh4s edge-domain order. */
  def euclidean(
      surface: MeshTopology
  ): Either[
    SurfaceEdgeWeightError,
    SurfaceEdgeWeights { type Edge = surface.mesh.topology.Edge }
  ] =
    val realization = surface.mesh.realization
    val values =
      surface.mesh.topology.edges.indices.map(realization.edgeLength).toVector
    fromTopologyOrder(surface, values)

  private[surface] def validateOwner(
      surface: MeshTopology,
      weights: SurfaceEdgeWeights
  ): Either[SurfaceEdgeWeightError, Unit] =
    if weights.belongsTo(surface) then Right(())
    else
      Left:
        SurfaceEdgeWeightError.WrongTopologyOwner(
          surface.mesh.connectivityFingerprint.value,
          weights.topology.connectivityFingerprint.value
        )

  private def validate(
      expectedCount: Int,
      values: Vector[Double]
  ): Either[SurfaceEdgeWeightError, Unit] =
    if values.length != expectedCount then
      Left(SurfaceEdgeWeightError.WrongValueCount(expectedCount, values.length))
    else
      var ordinal = 0
      var error = Option.empty[SurfaceEdgeWeightError]
      while ordinal < values.length && error.isEmpty do
        val value = values(ordinal)
        if !value.isFinite then
          error = Some(SurfaceEdgeWeightError.NonFiniteWeight(ordinal, value))
        else if value < 0.0 then
          error = Some(SurfaceEdgeWeightError.NegativeWeight(ordinal, value))
        ordinal += 1
      error.toLeft(())

  private def build[T <: TriangleTopology](
      owner: T,
      data: Vector[Double]
  ): SurfaceEdgeWeights { type Edge = owner.Edge } =
    new SurfaceEdgeWeights:
      type Edge = owner.Edge

      val topology = owner
      val values: Field[Edge, Double] =
        Field.view(owner.edges)(edge => data(edge.ordinal))
