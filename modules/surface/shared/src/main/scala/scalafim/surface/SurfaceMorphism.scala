package scalafim.surface

import scalafim.image.*

opaque type SurfaceDomainId = String

object SurfaceDomainId:
  def apply(value: String): SurfaceDomainId =
    val normalized = value.trim
    require(normalized.nonEmpty, "SurfaceDomainId must be non-empty")
    normalized

  extension (id: SurfaceDomainId)
    def value: String = id

enum SurfaceInverseKind:
  case Adjoint

enum SurfaceMorphismKind:
  case VolumeToSurface, SurfaceToSurface

sealed trait SurfaceMorphism:
  def source: SurfaceDomainId
  def target: SurfaceDomainId
  def kind: SurfaceMorphismKind
  def cost: Double
  def methodTag: String
  def inverseKind: SurfaceInverseKind

final case class VolToSurfMorphism(
    source: SurfaceDomainId,
    target: SurfaceDomainId,
    plan: VolumeSurfaceSamplingPlan,
    cost: Double = 10.0,
    methodTag: String = "volume-to-surface"
) extends SurfaceMorphism:
  require(cost.isFinite && cost >= 0.0, "surface morphism cost must be finite and non-negative")

  def kind: SurfaceMorphismKind =
    SurfaceMorphismKind.VolumeToSurface

  def inverseKind: SurfaceInverseKind =
    SurfaceInverseKind.Adjoint

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
    source: SurfaceDomainId,
    target: SurfaceDomainId,
    mapping: SurfaceVertexMapping,
    cost: Double = 5.0,
    methodTag: String = "surface-to-surface"
) extends SurfaceMorphism:
  require(cost.isFinite && cost >= 0.0, "surface morphism cost must be finite and non-negative")

  def kind: SurfaceMorphismKind =
    SurfaceMorphismKind.SurfaceToSurface

  def inverseKind: SurfaceInverseKind =
    SurfaceInverseKind.Adjoint

  def resample(field: SurfaceField[Double]): SurfaceField[Double] =
    mapping(field)
