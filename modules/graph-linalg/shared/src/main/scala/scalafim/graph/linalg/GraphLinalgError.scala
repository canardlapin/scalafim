package scalafim.graph.linalg

import scalafim.linalg.LinearMapError
import scalafim.linalg.LinearAlgebraError

enum WeightRequirement:
  case Finite
  case NonNegative
  case StrictlyPositive

  def label: String =
    this match
      case Finite           => "finite"
      case NonNegative      => "finite and non-negative"
      case StrictlyPositive => "finite and strictly positive"

sealed trait GraphLinalgError[+K]:
  def message: String

object GraphLinalgError:
  final case class InvalidWeight[K](
      edgeIndex: Int,
      from: K,
      to: K,
      value: Double,
      requirement: WeightRequirement
  ) extends GraphLinalgError[K]:
    def message: String =
      s"edge $edgeIndex ('$from','$to') has weight $value; expected ${requirement.label}"

  final case class ZeroStrengthVertex[K](key: K) extends GraphLinalgError[K]:
    def message: String =
      s"vertex '$key' has zero weighted strength"

  final case class LinearMapFailure(error: LinearMapError) extends GraphLinalgError[Nothing]:
    def message: String =
      error.message

  final case class EigenFailure(error: LinearAlgebraError) extends GraphLinalgError[Nothing]:
    def message: String =
      error.message

  final case class TopologyFailure(detail: String) extends GraphLinalgError[Nothing]:
    def message: String =
      detail

  final case class InvalidEmbeddingDimensions(requested: Int, available: Int, dropped: Int)
      extends GraphLinalgError[Nothing]:
    def message: String =
      s"spectral embedding requested $requested dimension(s) after dropping $dropped eigenvector(s), but basis order is $available"
