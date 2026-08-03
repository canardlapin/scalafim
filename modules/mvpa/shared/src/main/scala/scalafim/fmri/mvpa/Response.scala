package scalafim.fmri.mvpa

enum Response:
  case Categorical(labels: Vector[ClassLabel])
  case Probabilistic(membership: ClassMembership)
  case Continuous(values: Vector[Double])

  def length: Int =
    this match
      case Categorical(labels) => labels.length
      case Probabilistic(membership) => membership.samples
      case Continuous(values) => values.length

  def subset(indices: IndexedSeq[Int]): Response =
    this match
      case Categorical(labels) =>
        Categorical(indices.map(i => labels(i)).toVector)
      case Probabilistic(membership) =>
        Probabilistic(membership.subset(indices))
      case Continuous(values) =>
        Continuous(indices.map(i => values(i)).toVector)

  def validate(samples: Int): Either[MvpaError, Response] =
    if length == 0 then Left(MvpaError.EmptyResponse)
    else if length != samples then Left(MvpaError.ResponseLengthMismatch(samples, length))
    else
      this match
        case Categorical(labels) if labels.distinct.length < 2 =>
          Left(MvpaError.SingleClassResponse)
        case _ =>
          Right(this)

object Response:
  def categorical(labels: Seq[String]): Either[MvpaError, Response] =
    val parsed = labels.map(ClassLabel.apply).toVector
    val response = Response.Categorical(parsed)
    response.validate(response.length)

  def continuous(values: Seq[Double]): Either[MvpaError, Response] =
    val response = Response.Continuous(values.toVector)
    response.validate(response.length)

  def probabilistic(
      classes: Seq[ClassLabel],
      memberships: gale.linalg.DMat
  ): Either[MvpaError, Response] =
    ClassMembership.simplex(classes, memberships).map(Response.Probabilistic.apply)
