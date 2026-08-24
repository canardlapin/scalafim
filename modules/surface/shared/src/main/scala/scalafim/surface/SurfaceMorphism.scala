package scalafim.surface

import scalafim.image.*

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

  def sample(volume: SomeScalarVolume[Double], mask: Option[SomeMaskVolume] = None): SurfaceSampleResult =
    VolumeSurfaceSampler(plan).sample(volume, mask)

final case class SurfaceVertexMapping private (
    sourceGeometry: SurfaceGeometry,
    targetGeometry: SurfaceGeometry,
    sourceForTarget: Vector[VertexId]
):
  require(sourceForTarget.length == targetGeometry.vertexCount, "mapping must define one source vertex per target vertex")
  sourceForTarget.foreach { vertex =>
    require(vertex.index < sourceGeometry.vertexCount, "source vertex mapping out of range")
  }

  def apply(field: SurfaceField[Double], label: Option[String] = None): SurfaceField[Double] =
    require(field.geometry == sourceGeometry, "surface field geometry must match mapping source")
    val values =
      sourceForTarget.map { vertex =>
        field.valueAt(vertex).getOrElse(Double.NaN)
      }
    SurfaceField.full(targetGeometry, values, label.getOrElse(field.label))

object SurfaceVertexMapping:
  def nearestIndex(
      sourceGeometry: SurfaceGeometry,
      targetGeometry: SurfaceGeometry,
      sourceForTarget: Vector[VertexId]
  ): SurfaceVertexMapping =
    SurfaceVertexMapping(sourceGeometry, targetGeometry, sourceForTarget)

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
