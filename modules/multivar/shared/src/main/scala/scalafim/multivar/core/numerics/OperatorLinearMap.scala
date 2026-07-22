package scalafim.multivar
package core

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.GaleMatrixBridge
import scalafim.linalg.LinearMap
import scalafim.linalg.LinearMapError

/** Narrow adapter from the semantic operator algebra to the reusable linalg
  * linear-map capability. Program validation and solver compilation share it.
  */
private[multivar] final class OperatorLinearMap[
    From <: Coordinate,
    To <: Coordinate,
    Role <: OperatorRoleTag,
    Evidence <: OperatorEvidence
](
    operator: Op[From, To, Role, Evidence]
) extends LinearMap:
  val rows: Int = operator.rows
  val cols: Int = operator.cols

  def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    operator(GaleMatrixBridge.toGale(input))
      .left
      .map(error => LinearMapError.OperatorApplicationFailed(error.message))
      .map(GaleMatrixBridge.fromGale)

  def adjoint: LinearMap =
    new OperatorLinearMap(operator.dual)
