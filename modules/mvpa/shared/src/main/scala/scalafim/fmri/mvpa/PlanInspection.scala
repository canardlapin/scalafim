package scalafim.fmri.mvpa

/** Representation-free description of one scientific plan. This is a view of the canonical source, design, frame,
  * estimand, and boundary identities; it is not another plan or result hierarchy.
  */
final case class ScientificPlanInspection(
    plan: ScientificPlanIdentity,
    source: ScientificSourceIdentity,
    design: DesignIdentity,
    designAxes: Vector[DesignAxisReference],
    frame: MeasurementFrameIdentity,
    measurements: Vector[MeasurementIdentity],
    estimand: EstimandIdentity,
    normalization: NormalizationIdentity,
    boundaries: RequestedBoundaries
):
  def measurementCount: Int =
    measurements.length

object ScientificPlanInspection:
  private[mvpa] def from[
      Source <: ScientificSource,
      Design <: EvidenceDesign,
      E <: Estimand[Source, Design],
      Rendition
  ](
      specification: ScientificSpecification[Source, Design, E, Rendition]
  ): ScientificPlanInspection =
    ScientificPlanInspection(
      specification.identity,
      specification.source.identity,
      specification.design.identity,
      specification.design.referencedAxes,
      specification.frame.identity,
      specification.frame.entries.map(_.measurement.identity),
      specification.estimand.identity,
      specification.normalization,
      specification.boundaries
    )
