package scalafim.locus

enum RelationError:
  case WrongRowCount(expected: Int, actual: Int)
  case TargetOutOfBounds(sourceOrdinal: Int, position: Int, targetOrdinal: Int, targetSize: Int)

  def message: String =
    this match
      case WrongRowCount(expected, actual) =>
        s"relation requires $expected rows, found $actual"
      case TargetOutOfBounds(source, position, target, size) =>
        s"relation row $source target at position $position is outside [0, $size): $target"

enum RelationEvidenceError:
  case EndospaceMismatch(
      expectedKey: SpaceKey,
      expectedSize: Int,
      actualKey: SpaceKey,
      actualSize: Int
  )
  case MissingReflexivePoint(pointOrdinal: Int)
  case MissingConverse(sourceOrdinal: Int, targetOrdinal: Int)

  def message: String =
    this match
      case EndospaceMismatch(expectedKey, expectedSize, actualKey, actualSize) =>
        SpaceMismatch(expectedKey, expectedSize, actualKey, actualSize).message
      case MissingReflexivePoint(point) =>
        s"relation is not reflexive at point $point"
      case MissingConverse(source, target) =>
        s"relation contains ($source, $target) but not ($target, $source)"

final class Relation[X, Y] private (
    val from: FiniteSpace[X],
    val to: FiniteSpace[Y],
    private val rows: Array[Array[Int]]
):
  def row(point: Point[X]): Region[Y] =
    Region.fromSortedOwned(to, rows(point.ordinal).clone())

  def isRelated(source: Point[X], target: Point[Y]): Boolean =
    containsOrdinal(rows(source.ordinal), target.ordinal)

  def andThen[Z](that: Relation[Y, Z]): Either[SpaceMismatch, Relation[X, Z]] =
    if to.sameSpace(that.from) then
      val composed = Array.ofDim[Array[Int]](from.size)
      var source = 0
      while source < from.size do
        val included = Array.fill(that.to.size)(false)
        val intermediates = rows(source)
        var middleIndex = 0
        while middleIndex < intermediates.length do
          val targets = that.rows(intermediates(middleIndex))
          var targetIndex = 0
          while targetIndex < targets.length do
            included(targets(targetIndex)) = true
            targetIndex += 1
          middleIndex += 1
        composed(source) = ordinalsWhere(included)
        source += 1
      Right(Relation.fromValidated(from, that.to, composed))
    else
      Left(to.mismatch(that.from))

  def converse: Relation[Y, X] =
    val builders = Array.fill(to.size)(Array.newBuilder[Int])
    var source = 0
    while source < rows.length do
      val targets = rows(source)
      var i = 0
      while i < targets.length do
        builders(targets(i)) += source
        i += 1
      source += 1
    Relation.fromValidated(to, from, builders.map(_.result()))

  def union(that: Relation[X, Y]): Either[SpaceMismatch, Relation[X, Y]] =
    if !from.sameSpace(that.from) then
      Left(from.mismatch(that.from))
    else if !to.sameSpace(that.to) then
      Left(to.mismatch(that.to))
    else
      val combined = Array.ofDim[Array[Int]](from.size)
      var source = 0
      while source < from.size do
        combined(source) = mergeUnion(rows(source), that.rows(source))
        source += 1
      Right(Relation.fromValidated(from, to, combined))

  def subsetOf(that: Relation[X, Y]): Either[SpaceMismatch, Boolean] =
    if !from.sameSpace(that.from) then
      Left(from.mismatch(that.from))
    else if !to.sameSpace(that.to) then
      Left(to.mismatch(that.to))
    else
      var source = 0
      var subset = true
      while source < from.size && subset do
        subset = sortedSubset(rows(source), that.rows(source))
        source += 1
      Right(subset)

  def image(region: Region[X]): Either[SpaceMismatch, Region[Y]] =
    if from.sameSpace(region.space) then
      val included = Array.fill(to.size)(false)
      val sources = region.ordinalsInDomainOrder
      var sourceIndex = 0
      while sourceIndex < sources.length do
        val targets = rows(sources(sourceIndex))
        var targetIndex = 0
        while targetIndex < targets.length do
          included(targets(targetIndex)) = true
          targetIndex += 1
        sourceIndex += 1
      Right(Region.fromSortedOwned(to, ordinalsWhere(included)))
    else
      Left(from.mismatch(region.space))

  def allRelatedInside(region: Region[Y]): Either[SpaceMismatch, Region[X]] =
    if to.sameSpace(region.space) then
      Right:
        Region.tabulate(from): source =>
          val targets = rows(source.ordinal)
          var i = 0
          var inside = true
          while i < targets.length && inside do
            inside = region.containsOrdinal(targets(i))
            i += 1
          inside
    else
      Left(to.mismatch(region.space))

  def ordinalRows: Array[Array[Int]] =
    rows.map(_.clone())

  override def equals(other: Any): Boolean =
    other match
      case that: Relation[?, ?] =>
        from == that.from && to == that.to && sameRows(rows, that.rows)
      case _ =>
        false

  override def hashCode(): Int =
    var result = 31 * from.hashCode() + to.hashCode()
    var source = 0
    while source < rows.length do
      var i = 0
      while i < rows(source).length do
        result = 31 * result + rows(source)(i)
        i += 1
      result = 31 * result + source
      source += 1
    result

  override def toString: String =
    val size = rows.foldLeft(0)((total, row) => total + row.length)
    s"Relation(${from.key.value} -> ${to.key.value}, pairs=$size)"

  private def containsOrdinal(row: Array[Int], ordinal: Int): Boolean =
    var low = 0
    var high = row.length - 1
    var found = false
    while low <= high && !found do
      val middle = low + (high - low) / 2
      if row(middle) == ordinal then found = true
      else if row(middle) < ordinal then low = middle + 1
      else high = middle - 1
    found

  private def ordinalsWhere(included: Array[Boolean]): Array[Int] =
    val builder = Array.newBuilder[Int]
    var ordinal = 0
    while ordinal < included.length do
      if included(ordinal) then builder += ordinal
      ordinal += 1
    builder.result()

  private def mergeUnion(left: Array[Int], right: Array[Int]): Array[Int] =
    val result = Array.ofDim[Int](left.length + right.length)
    var leftIndex = 0
    var rightIndex = 0
    var out = 0
    while leftIndex < left.length && rightIndex < right.length do
      if left(leftIndex) < right(rightIndex) then
        result(out) = left(leftIndex)
        leftIndex += 1
      else if right(rightIndex) < left(leftIndex) then
        result(out) = right(rightIndex)
        rightIndex += 1
      else
        result(out) = left(leftIndex)
        leftIndex += 1
        rightIndex += 1
      out += 1
    while leftIndex < left.length do
      result(out) = left(leftIndex)
      leftIndex += 1
      out += 1
    while rightIndex < right.length do
      result(out) = right(rightIndex)
      rightIndex += 1
      out += 1
    result.take(out)

  private def sortedSubset(left: Array[Int], right: Array[Int]): Boolean =
    var leftIndex = 0
    var rightIndex = 0
    var subset = true
    while leftIndex < left.length && subset do
      while rightIndex < right.length && right(rightIndex) < left(leftIndex) do
        rightIndex += 1
      if rightIndex >= right.length || right(rightIndex) != left(leftIndex) then
        subset = false
      leftIndex += 1
    subset

  private def sameRows(left: Array[Array[Int]], right: Array[Array[Int]]): Boolean =
    if left.length != right.length then false
    else
      var source = 0
      var same = true
      while source < left.length && same do
        val leftRow = left(source)
        val rightRow = right(source)
        if leftRow.length != rightRow.length then same = false
        else
          var i = 0
          while i < leftRow.length && same do
            same = leftRow(i) == rightRow(i)
            i += 1
        source += 1
      same

object Relation:
  def empty[X, Y](from: FiniteSpace[X], to: FiniteSpace[Y]): Relation[X, Y] =
    fromValidated(from, to, Array.fill(from.size)(Array.emptyIntArray))

  def identity[S](space: FiniteSpace[S]): Relation[S, S] =
    fromValidated(space, space, Array.tabulate(space.size)(i => Array(i)))

  def fromOrdinalRows[X, Y](
      from: FiniteSpace[X],
      to: FiniteSpace[Y],
      inputRows: Array[Array[Int]]
  ): Either[RelationError, Relation[X, Y]] =
    if inputRows.length != from.size then
      Left(RelationError.WrongRowCount(from.size, inputRows.length))
    else
      val canonical = Array.ofDim[Array[Int]](from.size)
      var source = 0
      var error = Option.empty[RelationError]
      while source < inputRows.length && error.isEmpty do
        val input = inputRows(source)
        var position = 0
        while position < input.length && error.isEmpty do
          val target = input(position)
          if !to.containsOrdinal(target) then
            error = Some(RelationError.TargetOutOfBounds(source, position, target, to.size))
          position += 1
        canonical(source) = input.sorted.distinct
        source += 1

      error match
        case Some(value) => Left(value)
        case None => Right(fromValidated(from, to, canonical))

  def tabulate[X, Y](
      from: FiniteSpace[X],
      to: FiniteSpace[Y]
  )(row: Point[X] => Region[Y]): Either[SpaceMismatch, Relation[X, Y]] =
    val rows = Array.ofDim[Array[Int]](from.size)
    var source = 0
    var mismatch = Option.empty[SpaceMismatch]
    while source < from.size && mismatch.isEmpty do
      val region = row(Point.unsafe(source))
      if !to.sameSpace(region.space) then mismatch = Some(to.mismatch(region.space))
      else rows(source) = region.ordinalsInDomainOrder
      source += 1
    mismatch match
      case Some(value) => Left(value)
      case None => Right(fromValidated(from, to, rows))

  private[locus] def fromValidated[X, Y](
      from: FiniteSpace[X],
      to: FiniteSpace[Y],
      rows: Array[Array[Int]]
  ): Relation[X, Y] =
    new Relation(from, to, rows)

final class ReflexiveRelation[S] private (
    val relation: Relation[S, S]
)

object ReflexiveRelation:
  def validate[S](relation: Relation[S, S]): Either[RelationEvidenceError, ReflexiveRelation[S]] =
    if !relation.from.sameIdentityAs(relation.to) then
      Left:
        RelationEvidenceError.EndospaceMismatch(
          relation.from.key,
          relation.from.size,
          relation.to.key,
          relation.to.size
        )
    else
      var ordinal = 0
      var missing = -1
      while ordinal < relation.from.size && missing < 0 do
        val point = relation.from.point(ordinal).get
        if !relation.isRelated(point, point) then missing = ordinal
        ordinal += 1
      if missing >= 0 then Left(RelationEvidenceError.MissingReflexivePoint(missing))
      else Right(new ReflexiveRelation(relation))

final class SymmetricRelation[S] private (
    val relation: Relation[S, S]
)

object SymmetricRelation:
  def validate[S](relation: Relation[S, S]): Either[RelationEvidenceError, SymmetricRelation[S]] =
    if !relation.from.sameIdentityAs(relation.to) then
      Left:
        RelationEvidenceError.EndospaceMismatch(
          relation.from.key,
          relation.from.size,
          relation.to.key,
          relation.to.size
        )
    else
      val rows = relation.ordinalRows
      var source = 0
      var error = Option.empty[RelationEvidenceError]
      while source < rows.length && error.isEmpty do
        var i = 0
        while i < rows(source).length && error.isEmpty do
          val target = rows(source)(i)
          if !contains(rows(target), source) then
            error = Some(RelationEvidenceError.MissingConverse(source, target))
          i += 1
        source += 1
      error match
        case Some(value) => Left(value)
        case None => Right(new SymmetricRelation(relation))

  private def contains(values: Array[Int], target: Int): Boolean =
    var low = 0
    var high = values.length - 1
    var found = false
    while low <= high && !found do
      val middle = low + (high - low) / 2
      if values(middle) == target then found = true
      else if values(middle) < target then low = middle + 1
      else high = middle - 1
    found
