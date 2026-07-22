package scalafim.multivar
package solver

import scalafim.multivar.core.*
import scalafim.multivar.optimization.*

import gale.linalg.DMat

/** Portable reference solver for
  * `min_x 0.5 ||x-y||_F^2 + lambda ||T x||_1`.
  *
  * The primal-dual step uses the Frobenius norm as a certified upper bound for
  * `||T||_2`, so the step-size product is valid on JVM and Scala.js without a
  * platform solver dependency.
  */
object PrimalDualL1Reference:
  def solve[Source <: SemanticSpace, Target <: SemanticSpace](
      plan: CompositePenaltyPlan[Source, Target],
      observation: DMat,
      config: PrimalDualConfig = PrimalDualConfig.portable
  ): Either[CompositeLoweringError, PrimalDualSolution] =
    VariationalSolverCompiler
      .compileL1(plan, observation)
      .flatMap(_.solve(config))
