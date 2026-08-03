package scalafim.latent

import cats.{Applicative, Monad}

opaque type RepresentationInstanceId = String

object RepresentationInstanceId:
  def fromString(value: String): Either[LatentError, RepresentationInstanceId] =
    checkedIdentity("representation instance", value)

  def unsafe(value: String): RepresentationInstanceId =
    fromString(value)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: RepresentationInstanceId)
    inline def value: String =
      id

opaque type LogicalSlotId = String

object LogicalSlotId:
  def fromString(value: String): Either[LatentError, LogicalSlotId] =
    checkedIdentity("logical slot", value)

  private[latent] def unsafe(value: String): LogicalSlotId =
    fromString(value)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: LogicalSlotId)
    inline def value: String =
      id

opaque type LogicalPayloadRole = String

object LogicalPayloadRole:
  def fromString(value: String): Either[LatentError, LogicalPayloadRole] =
    checkedIdentity("logical payload role", value)

  private[latent] def unsafe(value: String): LogicalPayloadRole =
    fromString(value)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (role: LogicalPayloadRole)
    inline def value: String =
      role

trait LogicalSlot[A]:
  def id: LogicalSlotId
  def role: LogicalPayloadRole
  def dimensions: Vector[Int]

trait LogicalPayloadRead[A]:
  def slot: LogicalSlot[A]

enum DecodePlanError:
  case DependentReadBarrier

  def message: String =
    this match
      case DependentReadBarrier =>
        "applicative execution stopped at a data-dependent read barrier"

sealed trait DecodePlan[A]

object DecodePlan:
  final case class Pure[A](value: A) extends DecodePlan[A]

  final case class Request[A](
      request: LogicalPayloadRead[A]
  ) extends DecodePlan[A]

  final case class Map[A, B](
      source: DecodePlan[A],
      function: A => B
  ) extends DecodePlan[B]

  final case class Zip[A, B](
      left: DecodePlan[A],
      right: DecodePlan[B]
  ) extends DecodePlan[(A, B)]

  final case class Dependent[A, B](
      first: DecodePlan[A],
      next: A => DecodePlan[B]
  ) extends DecodePlan[B]

  final case class Inspection(
      requests: Vector[LogicalPayloadRead[?]],
      dependentBarriers: Int
  ):
    def combine(other: Inspection): Inspection =
      Inspection(
        requests ++ other.requests,
        dependentBarriers + other.dependentBarriers
      )

  trait Interpreter[F[_]]:
    def apply[A](request: LogicalPayloadRead[A]): F[A]

  def pure[A](value: A): DecodePlan[A] =
    Pure(value)

  def read[A](request: LogicalPayloadRead[A]): DecodePlan[A] =
    Request(request)

  def zip[A, B](
      left: DecodePlan[A],
      right: DecodePlan[B]
  ): DecodePlan[(A, B)] =
    Zip(left, right)

  def map2[A, B, C](
      left: DecodePlan[A],
      right: DecodePlan[B]
  )(
      function: (A, B) => C
  ): DecodePlan[C] =
    Zip(left, right).map(function.tupled)

  def map3[A, B, C, D](
      first: DecodePlan[A],
      second: DecodePlan[B],
      third: DecodePlan[C]
  )(
      function: (A, B, C) => D
  ): DecodePlan[D] =
    map2(map2(first, second)((_, _)), third):
      case ((firstValue, secondValue), thirdValue) =>
        function(firstValue, secondValue, thirdValue)

  def inspect[A](plan: DecodePlan[A]): Inspection =
    plan match
      case Pure(_) =>
        Inspection(Vector.empty, 0)
      case Request(request) =>
        Inspection(Vector(request), 0)
      case Map(source, _) =>
        inspect(source)
      case Zip(left, right) =>
        inspect(left).combine(inspect(right))
      case Dependent(first, _) =>
        val prefix = inspect(first)
        prefix.copy(dependentBarriers = prefix.dependentBarriers + 1)

  def runApplicative[F[_], A](
      plan: DecodePlan[A],
      interpreter: Interpreter[F]
  )(using F: Applicative[F]): Either[DecodePlanError, F[A]] =
    plan match
      case Pure(value) =>
        Right(F.pure(value))
      case Request(request) =>
        Right(interpreter(request))
      case Map(source, function) =>
        runApplicative(source, interpreter).map(F.map(_)(function))
      case Zip(left, right) =>
        for
          leftEffect <- runApplicative(left, interpreter)
          rightEffect <- runApplicative(right, interpreter)
        yield F.map2(leftEffect, rightEffect)((_, _))
      case Dependent(_, _) =>
        Left(DecodePlanError.DependentReadBarrier)

  def runSequential[F[_], A](
      plan: DecodePlan[A],
      interpreter: Interpreter[F]
  )(using F: Monad[F]): F[A] =
    plan match
      case Pure(value) =>
        F.pure(value)
      case Request(request) =>
        interpreter(request)
      case Map(source, function) =>
        F.map(runSequential(source, interpreter))(function)
      case Zip(left, right) =>
        F.map2(
          runSequential(left, interpreter),
          runSequential(right, interpreter)
        )((_, _))
      case Dependent(first, next) =>
        F.flatMap(runSequential(first, interpreter))(value =>
          runSequential(next(value), interpreter)
        )

  extension [A](plan: DecodePlan[A])
    def map[B](function: A => B): DecodePlan[B] =
      Map(plan, function)

    def product[B](other: DecodePlan[B]): DecodePlan[(A, B)] =
      Zip(plan, other)

    def dependent[B](next: A => DecodePlan[B]): DecodePlan[B] =
      Dependent(plan, next)

private def checkedIdentity(
    label: String,
    value: String
): Either[LatentError, String] =
  val normalized = value.trim
  if normalized.isEmpty then Left(LatentError.EmptyIdentifier(label))
  else if normalized.exists(character =>
      character.isWhitespace || character.isControl
    )
  then Left(LatentError.InvalidIdentifier(label, value))
  else Right(normalized)
