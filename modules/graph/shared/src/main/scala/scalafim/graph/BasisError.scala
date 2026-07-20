package scalafim.graph

sealed trait BasisError[+K]:
  def message: String

object BasisError:
  final case class DuplicateKey[K](key: K, firstIndex: Int, duplicateIndex: Int) extends BasisError[K]:
    def message: String =
      s"duplicate vertex key '$key' at indices $firstIndex and $duplicateIndex"

  final case class DuplicateSelection[K](key: K) extends BasisError[K]:
    def message: String =
      s"vertex selection contains duplicate key '$key'"

  final case class UnknownKey[K](key: K) extends BasisError[K]:
    def message: String =
      s"vertex selection contains unknown key '$key'"

  final case class KeySetMismatch[K](sourceOnly: Vector[K], targetOnly: Vector[K]) extends BasisError[K]:
    def message: String =
      s"vertex key sets differ: source-only=${sourceOnly.mkString("[", ",", "]")}, target-only=${targetOnly.mkString("[", ",", "]")}"

  final case class IncompatiblePermutation[K](actualTarget: Vector[K], nextSource: Vector[K]) extends BasisError[K]:
    def message: String =
      s"basis permutations are not composable: target=${actualTarget.mkString("[", ",", "]")}, next-source=${nextSource.mkString("[", ",", "]")}"
