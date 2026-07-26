package scalafim.locus

final class TotalMap[X, Y] private (
    val from: FiniteSpace[X],
    val to: FiniteSpace[Y],
    private val targets: Array[Int]
):
  def apply(point: Point[X]): Point[Y] =
    Point.unsafe(targets(point.ordinal))

  def targetOrdinals: Array[Int] =
    targets.clone()

  def andThen[Z](that: TotalMap[Y, Z]): Either[SpaceMismatch, TotalMap[X, Z]] =
    if to.sameSpace(that.from) then
      val result = Array.ofDim[Int](targets.length)
      var i = 0
      while i < targets.length do
        result(i) = that.targets(targets(i))
        i += 1
      Right(TotalMap.fromValidated(from, that.to, result))
    else
      Left(to.mismatch(that.from))

  def pullback(region: Region[Y]): Either[SpaceMismatch, Region[X]] =
    if to.sameSpace(region.space) then
      val result = Array.newBuilder[Int]
      var source = 0
      while source < targets.length do
        if region.containsOrdinal(targets(source)) then result += source
        source += 1
      Right(Region.fromSortedOwned(from, result.result()))
    else
      Left(to.mismatch(region.space))

  def existsAlong(region: Region[X]): Either[SpaceMismatch, Region[Y]] =
    if from.sameSpace(region.space) then
      val included = Array.fill(to.size)(false)
      val sources = region.ordinalsInDomainOrder
      var i = 0
      while i < sources.length do
        included(targets(sources(i))) = true
        i += 1
      Right(Region.tabulate(to)(point => included(point.ordinal)))
    else
      Left(from.mismatch(region.space))

  def forallAlong(region: Region[X]): Either[SpaceMismatch, Region[Y]] =
    forallImpl(region, requireImageSupport = false)

  def forallAlongOnImage(region: Region[X]): Either[SpaceMismatch, Region[Y]] =
    forallImpl(region, requireImageSupport = true)

  private def forallImpl(
      region: Region[X],
      requireImageSupport: Boolean
  ): Either[SpaceMismatch, Region[Y]] =
    if from.sameSpace(region.space) then
      val allInside = Array.fill(to.size)(true)
      val supported = Array.fill(to.size)(false)
      var source = 0
      while source < targets.length do
        val target = targets(source)
        supported(target) = true
        if !region.containsOrdinal(source) then allInside(target) = false
        source += 1
      Right:
        Region.tabulate(to): point =>
          allInside(point.ordinal) && (!requireImageSupport || supported(point.ordinal))
    else
      Left(from.mismatch(region.space))

  override def equals(other: Any): Boolean =
    other match
      case that: TotalMap[?, ?] =>
        from == that.from && to == that.to && TotalMap.sameTargets(targets, that.targets)
      case _ =>
        false

  override def hashCode(): Int =
    var result = 31 * from.hashCode() + to.hashCode()
    var i = 0
    while i < targets.length do
      result = 31 * result + targets(i)
      i += 1
    result

  override def toString: String =
    s"TotalMap(${from.key.value} -> ${to.key.value}, size=${from.size})"

object TotalMap:
  def fromTargetOrdinals[X, Y](
      from: FiniteSpace[X],
      to: FiniteSpace[Y],
      targetOrdinals: Array[Int]
  ): Either[TotalMapError, TotalMap[X, Y]] =
    if targetOrdinals.length != from.size then
      Left(TotalMapError.WrongTargetCount(from.size, targetOrdinals.length))
    else
      var source = 0
      var error = Option.empty[TotalMapError]
      while source < targetOrdinals.length && error.isEmpty do
        val target = targetOrdinals(source)
        if !to.containsOrdinal(target) then
          error = Some(TotalMapError.TargetOutOfBounds(source, target, to.size))
        source += 1

      error match
        case Some(value) =>
          Left(value)
        case None =>
          Right(fromValidated(from, to, targetOrdinals.clone()))

  def tabulate[X, Y](
      from: FiniteSpace[X],
      to: FiniteSpace[Y]
  )(mapping: Point[X] => Point[Y]): Either[TotalMapError, TotalMap[X, Y]] =
    val targets = Array.ofDim[Int](from.size)
    var source = 0
    while source < from.size do
      targets(source) = mapping(Point.unsafe(source)).ordinal
      source += 1
    fromTargetOrdinals(from, to, targets)

  def identity[S](space: FiniteSpace[S]): TotalMap[S, S] =
    fromValidated(space, space, Array.tabulate(space.size)(i => i))

  private[locus] def fromValidated[X, Y](
      from: FiniteSpace[X],
      to: FiniteSpace[Y],
      targetOrdinals: Array[Int]
  ): TotalMap[X, Y] =
    new TotalMap(from, to, targetOrdinals)

  private def sameTargets(left: Array[Int], right: Array[Int]): Boolean =
    if left.length != right.length then false
    else
      var i = 0
      var same = true
      while i < left.length && same do
        same = left(i) == right(i)
        i += 1
      same
