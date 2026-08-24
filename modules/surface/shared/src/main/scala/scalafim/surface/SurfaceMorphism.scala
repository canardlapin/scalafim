package scalafim.surface

import locus4s.FiniteDomain
import locus4s.TotalMap
import mesh4s.TriangleTopology
import scalafim.image.{InverseKind, NeuroVol, SpatialDomainId}

enum SurfaceMorphismKind:
  case VolumeToSurface, SurfaceToSurface

sealed trait SurfaceMorphism:
  def source: SpatialDomainId
  def target: SpatialDomainId
  def kind: SurfaceMorphismKind
  def cost: Double
  def methodTag: String
  def inverseKind: InverseKind

final case class VolToSurfMorphism(
    source: SpatialDomainId,
    target: SpatialDomainId,
    plan: VolumeSurfaceSamplingPlan,
    cost: Double = 10.0,
    methodTag: String = "volume-to-surface"
) extends SurfaceMorphism:
  require(cost.isFinite && cost >= 0.0, "surface morphism cost must be finite and non-negative")

  def kind: SurfaceMorphismKind =
    SurfaceMorphismKind.VolumeToSurface

  def inverseKind: InverseKind =
    InverseKind.Adjoint

  def sample(volume: NeuroVol[Double], mask: Option[NeuroVol[Boolean]] = None): SurfaceSampleResult =
    VolumeSurfaceSampler(plan).sample(volume, mask)

sealed trait SurfaceVertexMap:
  type SourceVertex
  type TargetVertex

  val sourceTopology: TriangleTopology { type Vertex = SurfaceVertexMap.this.SourceVertex }
  val targetTopology: TriangleTopology { type Vertex = SurfaceVertexMap.this.TargetVertex }
  val sourceForTarget: TotalMap[TargetVertex, SourceVertex]

  final def sourceVertices: FiniteDomain[SourceVertex] =
    sourceTopology.vertices

  final def targetVertices: FiniteDomain[TargetVertex] =
    targetTopology.vertices

final case class SurfaceVertexMapping private (
    sourceGeometry: SurfaceGeometry,
    targetGeometry: SurfaceGeometry,
    sourceForTarget: Vector[VertexId]
):
  require(sourceForTarget.length == targetGeometry.vertexCount, "mapping must define one source vertex per target vertex")
  sourceForTarget.foreach { vertex =>
    require(vertex.index < sourceGeometry.vertexCount, "source vertex mapping out of range")
  }

  /** Exact typed target-to-source map over the two topology owners. */
  val locus: SurfaceVertexMap =
    SurfaceVertexMapping.locus(sourceGeometry, targetGeometry, sourceForTarget)

  def apply(field: SurfaceField[Double], label: Option[String] = None): SurfaceField[Double] =
    require(field.geometry == sourceGeometry, "surface field geometry must match mapping source")
    val fieldLocus = field.locus
    val mappingLocus = locus
    require(
      fieldLocus.vertices.sameRuntimeOwnerAs(mappingLocus.sourceVertices),
      "surface field must share the mapping source vertex owner"
    )
    val alignment =
      fieldLocus.vertices
        .align(mappingLocus.sourceVertices)
        .fold(
          _ => throw new IllegalStateException("validated surface vertex owners did not align"),
          identity
        )
    val sourceValues = fieldLocus.optionalValues.rebind(alignment)
    val targetValues = sourceValues.pullback(mappingLocus.sourceForTarget)
    val values = targetValues.valuesInDomainOrder.map(_.getOrElse(Double.NaN)).toVector
    SurfaceField.full(targetGeometry, values, label.getOrElse(field.label))

object SurfaceVertexMapping:
  def nearestIndex(
      sourceGeometry: SurfaceGeometry,
      targetGeometry: SurfaceGeometry,
      sourceForTarget: Vector[VertexId]
  ): SurfaceVertexMapping =
    SurfaceVertexMapping(sourceGeometry, targetGeometry, sourceForTarget)

  private def locus(
      sourceGeometry: SurfaceGeometry,
      targetGeometry: SurfaceGeometry,
      sourceForTarget: Vector[VertexId]
  ): SurfaceVertexMap =
    val source = sourceGeometry.mesh.topology
    val target = targetGeometry.mesh.topology
    val mapping =
      TotalMap
        .fromTargetOrdinals(
          target.vertices,
          source.vertices,
          sourceForTarget.iterator.map(_.index)
        )
        .fold(
          error => throw new IllegalArgumentException(error.message),
          identity
        )
    new SurfaceVertexMap:
      type SourceVertex = source.Vertex
      type TargetVertex = target.Vertex

      val sourceTopology = source
      val targetTopology = target
      val sourceForTarget = mapping

final case class SurfToSurfMorphism(
    source: SpatialDomainId,
    target: SpatialDomainId,
    mapping: SurfaceVertexMapping,
    cost: Double = 5.0,
    methodTag: String = "surface-to-surface"
) extends SurfaceMorphism:
  require(cost.isFinite && cost >= 0.0, "surface morphism cost must be finite and non-negative")

  def kind: SurfaceMorphismKind =
    SurfaceMorphismKind.SurfaceToSurface

  def inverseKind: InverseKind =
    InverseKind.Adjoint

  def resample(field: SurfaceField[Double]): SurfaceField[Double] =
    mapping(field)
