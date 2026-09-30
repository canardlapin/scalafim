package scalafim.spatial

import gale.sparse.CSR
import scalafim.surface.{SurfaceGeometry, SurfaceResampling, TemplateResamplingPlan}

/** A checked sampled-surface binding for a template resampling plan.
  *
  * The plan owns frozen sphere/topology snapshots; the endpoint geometries may use white, pial, inflated, or other
  * coordinates, but must retain that exact ordered topology.
  */
final class SurfaceResamplingBinding private (
    val plan: TemplateResamplingPlan,
    val sourceGeometry: SurfaceGeometry,
    val targetGeometry: SurfaceGeometry,
    val normalization: SurfaceResampling.Normalization,
    private val normalized: CSR
):
  /** Frozen, Element-normalized weights prepared once at checked binding construction. */
  def normalizedCsr: CSR = normalized

  /** Revalidate live endpoint topology without rebuilding the frozen operator. */
  private[spatial] def validateEndpoints: Either[SpatialError, Unit] =
    SurfaceResamplingBinding.validateEndpoints(plan, sourceGeometry, targetGeometry)

object SurfaceResamplingBinding:
  def checked(
      plan: TemplateResamplingPlan,
      sourceGeometry: SurfaceGeometry,
      targetGeometry: SurfaceGeometry,
      normalization: SurfaceResampling.Normalization = SurfaceResampling.Normalization.Element
  ): Either[SpatialError, SurfaceResamplingBinding] =
    if normalization != SurfaceResampling.Normalization.Element then
      Left(SpatialError.InvalidMixedPullback("surface resampling compilation currently requires Element normalization"))
    else
      for
        _ <- validateEndpoints(plan, sourceGeometry, targetGeometry)
        csr <- normalizedCsr(plan)
      yield new SurfaceResamplingBinding(plan, sourceGeometry, targetGeometry, normalization, csr)

  private def validateEndpoints(
      plan: TemplateResamplingPlan,
      sourceGeometry: SurfaceGeometry,
      targetGeometry: SurfaceGeometry
  ): Either[SpatialError, Unit] =
    for
      _ <- plan.validateSourceGeometry(sourceGeometry).left.map(error)
      _ <- plan.validateTargetGeometry(targetGeometry).left.map(error)
    yield ()

  private[spatial] def normalizedCsr(plan: TemplateResamplingPlan): Either[SpatialError, CSR] =
    val raw = plan.plan
    val totals = new Array[Double](raw.referenceVertices)
    var i = 0
    while i < raw.vals.length do
      totals(raw.rows(i)) += raw.vals(i)
      i += 1
    val values = new Array[Double](raw.vals.length)
    i = 0
    while i < values.length do
      val total = totals(raw.rows(i))
      values(i) = raw.vals(i) / (if total == 0.0 then 1.0 else total)
      i += 1
    GaleSpatialSupport
      .sparseCsr(raw.referenceVertices, raw.movingVertices, IArray.genericWrapArray(raw.rows).toArray, IArray.genericWrapArray(raw.cols).toArray, values)
      .left
      .map(value => SpatialError.OperatorAssemblyFailed(value.getMessage))

  private def error(value: scalafim.surface.SurfaceError): SpatialError =
    SpatialError.InvalidMixedPullback(value.message)
