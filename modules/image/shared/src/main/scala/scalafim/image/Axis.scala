package scalafim.image

import slash.vector.Vec

final case class Axis(label: String, direction: Option[Vec[3]] = None):
  override def toString: String = label

object Axis:
  val NoneAxis: Axis = Axis("None")
  val LeftRight: Axis =
    Axis("Left-to-Right", Some(Vec[3](1.0, 0.0, 0.0)))
  val RightLeft: Axis =
    Axis("Right-to-Left", Some(Vec[3](-1.0, 0.0, 0.0)))
  val AnteriorPosterior: Axis =
    Axis("Anterior-to-Posterior", Some(Vec[3](0.0, -1.0, 0.0)))
  val PosteriorAnterior: Axis =
    Axis("Posterior-to-Anterior", Some(Vec[3](0.0, 1.0, 0.0)))
  val InferiorSuperior: Axis =
    Axis("Inferior-to-Superior", Some(Vec[3](0.0, 0.0, 1.0)))
  val SuperiorInferior: Axis =
    Axis("Superior-to-Inferior", Some(Vec[3](0.0, 0.0, -1.0)))
  val Time: Axis = Axis("Time")

  private val byAbbrev: Map[String, Axis] = Map(
    "L" -> LeftRight,
    "LEFT" -> LeftRight,
    "R" -> RightLeft,
    "RIGHT" -> RightLeft,
    "A" -> AnteriorPosterior,
    "ANTERIOR" -> AnteriorPosterior,
    "P" -> PosteriorAnterior,
    "POSTERIOR" -> PosteriorAnterior,
    "I" -> InferiorSuperior,
    "INFERIOR" -> InferiorSuperior,
    "S" -> SuperiorInferior,
    "SUPERIOR" -> SuperiorInferior,
    "T" -> Time,
    "TIME" -> Time
  )

  def fromAbbrev(s: String): Option[Axis] =
    byAbbrev.get(s.toUpperCase)
