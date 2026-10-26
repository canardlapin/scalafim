package scalafim.fmri.mvpa.fit

import multivar.family.canonical.{CanonicalFrameConstraint, ConstrainedCanonicalSolverSpec, ResidualRegularization}


enum ConstrainedCanonicalSelection:
  case FixedBeforeFolds

/** Inspectable model specification for the v1 constrained estimand. There is
  * deliberately no response-fitted tuning field: regularization and numerical
  * policy are fixed before any outer fold is evaluated.
  */
final case class NonnegativeCanonicalModelSpec(
    regularization: ResidualRegularization,
    solver: ConstrainedCanonicalSolverSpec = ConstrainedCanonicalSolverSpec.default,
    selection: ConstrainedCanonicalSelection = ConstrainedCanonicalSelection.FixedBeforeFolds
):
  val constraint: CanonicalFrameConstraint = CanonicalFrameConstraint.Nonnegative
