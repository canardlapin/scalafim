package scalafim.response

import ravel.{Array1, NDArray, Shape}

sealed trait TimeAxis
sealed trait SampleAxis

opaque type AxisIndex[A] = Int

object AxisIndex:
  def fromInt[A](value: Int): Either[IndexError, AxisIndex[A]] =
    if value < 0 then Left(IndexError.Negative(value))
    else Right(value)

  private[response] inline def unsafe[A](value: Int): AxisIndex[A] =
    value

  extension [A](index: AxisIndex[A])
    inline def value: Int =
      index

opaque type DomainId[A] = String

object DomainId:
  def fromString[A](value: String): Either[IdentityError, DomainId[A]] =
    ResponseIdentity.validate("domain id", value)

  def unsafe[A](value: String): DomainId[A] =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension [A](id: DomainId[A])
    inline def value: String =
      id

enum DuplicatePolicy:
  case Reject
  case Allow

final class OrderedIndices[A] private (
    val domain: DomainId[A],
    val domainSize: Int,
    private[response] val primitiveValues: Array1[Int]
):
  def size: Int =
    primitiveValues.size

  def apply(position: Int): AxisIndex[A] =
    AxisIndex.unsafe(primitiveValues(position))

  def values: Vector[Int] =
    Vector.tabulate(size)(i => primitiveValues(i))

  def indexOfDuplicate: Option[Int] =
    val seen = scala.collection.mutable.HashSet.empty[Int]
    var position = 0
    while position < primitiveValues.size do
      val value = primitiveValues(position)
      if seen.contains(value) then return Some(value)
      seen += value
      position += 1
    None

  override def equals(other: Any): Boolean =
    other match
      case that: OrderedIndices[?] =>
        if this eq that then true
        else if domain.value != that.domain.value ||
            domainSize != that.domainSize ||
            primitiveValues.size != that.primitiveValues.size
        then false
        else
          var index = 0
          while index < primitiveValues.size do
            if primitiveValues(index) != that.primitiveValues(index) then return false
            index += 1
          true
      case _ =>
        false

  override def hashCode(): Int =
    var hash = 31 * domain.value.hashCode + domainSize
    var index = 0
    while index < primitiveValues.size do
      hash = 31 * hash + primitiveValues(index)
      index += 1
    hash

  override def toString: String =
    s"OrderedIndices(${domain.value},${values.mkString("[", ",", "]")})"

object OrderedIndices:
  def fromInts[A](
      domain: DomainId[A],
      domainSize: Int,
      values: Vector[Int],
      duplicates: DuplicatePolicy = DuplicatePolicy.Reject
  ): Either[IndexError, OrderedIndices[A]] =
    if domainSize <= 0 then Left(IndexError.InvalidDomainSize(domainSize))
    else if values.isEmpty then Left(IndexError.Empty(domain.value))
    else
      val seen = scala.collection.mutable.HashSet.empty[Int]
      var position = 0
      while position < values.length do
        val value = values(position)
        if value < 0 then return Left(IndexError.Negative(value))
        if value >= domainSize then return Left(IndexError.OutOfBounds(value, domainSize))
        if duplicates == DuplicatePolicy.Reject && seen.contains(value) then
          return Left(IndexError.Duplicate(value))
        seen += value
        position += 1
      Right(
        new OrderedIndices(
          domain,
          domainSize,
          NDArray.fromSeq(Shape(values.length), values)
        )
      )

  def all[A](
      domain: DomainId[A],
      domainSize: Int
  ): Either[IndexError, OrderedIndices[A]] =
    if domainSize <= 0 then Left(IndexError.InvalidDomainSize(domainSize))
    else
      val values = NDArray.tabulate[Int](domainSize)(identity)
      Right(new OrderedIndices(domain, domainSize, values))

  private[response] def fromOwned[A](
      domain: DomainId[A],
      domainSize: Int,
      values: Array1[Int]
  ): OrderedIndices[A] =
    new OrderedIndices(domain, domainSize, values)

private[response] object ResponseIdentity:
  def validate(label: String, value: String): Either[IdentityError, String] =
    val normalized = value.trim
    if normalized.isEmpty then Left(IdentityError.Empty(label))
    else if normalized.exists(character => character.isControl || character.isWhitespace) then
      Left(IdentityError.Invalid(label, value))
    else Right(normalized)
