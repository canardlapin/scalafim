package scalafim.multivar

import multivar.core.{IndexAxis, IndexSet, MultivarError}
import multivar.workflow.RoiPlan
import scalafim.locus.Selection

/** Ordered finite-space selections adapted into general multivar feature plans.
  *
  * The ambient ScalaFIM locus space owns identity and bounds. Multivar retains
  * only the explicit feature order needed by its matrix algorithms.
  */
object LocusSelectionAdapter:
  def indexSet[S](
      selection: Selection[S],
      axis: IndexAxis = IndexAxis.Feature
  ): Either[MultivarError, IndexSet] =
    IndexSet.from(
      selection.ordinals,
      axis,
      limit = Some(selection.space.size)
    )

  def roiPlan[S](
      id: String,
      selection: Selection[S],
      label: Option[String] = None
  ): Either[MultivarError, RoiPlan] =
    RoiPlan.of(id, selection.ordinals, label)
