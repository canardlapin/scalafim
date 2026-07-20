package scalafim.graph

enum EndpointRole:
  case Source
  case Target

sealed trait GraphBuildError[+K]:
  def message: String

object GraphBuildError:
  final case class UnknownVertex[K](edgeIndex: Int, role: EndpointRole, key: K) extends GraphBuildError[K]:
    def message: String =
      s"edge $edgeIndex references unknown ${role.toString.toLowerCase} vertex '$key'"

  final case class SelfEdge[K](edgeIndex: Int, key: K) extends GraphBuildError[K]:
    def message: String =
      s"edge $edgeIndex is a forbidden self-edge at vertex '$key'"

  final case class DuplicateEdge[K](edgeIndex: Int, first: K, second: K) extends GraphBuildError[K]:
    def message: String =
      s"edge $edgeIndex duplicates the simple edge ('$first','$second')"

final class GraphBuildErrors[K] private (val errors: Vector[GraphBuildError[K]]):
  require(errors.nonEmpty, "graph build errors must be non-empty")

  def message: String =
    errors.map(_.message).mkString("; ")

object GraphBuildErrors:
  private[graph] def from[K](errors: Vector[GraphBuildError[K]]): GraphBuildErrors[K] =
    new GraphBuildErrors(errors)
