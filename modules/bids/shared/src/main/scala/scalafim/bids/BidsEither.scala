package scalafim.bids

private[bids] object BidsEither:
  def traverse[A, B](values: IterableOnce[A])(f: A => Either[BidsError, B]): Either[BidsError, Vector[B]] =
    val out = Vector.newBuilder[B]
    val iterator = values.iterator
    var error: Option[BidsError] = None

    while iterator.hasNext && error.isEmpty do
      f(iterator.next()) match
        case Left(err)    => error = Some(err)
        case Right(value) => out += value

    error match
      case Some(err) => Left(err)
      case None      => Right(out.result())
