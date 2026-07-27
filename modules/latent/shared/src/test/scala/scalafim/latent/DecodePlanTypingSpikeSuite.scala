package scalafim.latent

import scala.compiletime.testing.typeCheckErrors

final case class SpikeCoefficients(values: Vector[Double])
final case class SpikeBasisReference(value: String)
final case class SpikeDecodeInput(
    coefficients: SpikeCoefficients,
    basis: SpikeBasisReference
)

enum SpikePayloadRequest[A]:
  case Coefficients(rows: Int, columns: Int) extends SpikePayloadRequest[SpikeCoefficients]
  case BasisReference(id: String) extends SpikePayloadRequest[SpikeBasisReference]

  def label: String =
    this match
      case Coefficients(rows, columns) => s"coefficients:$rows:$columns"
      case BasisReference(id)          => s"basis:$id"

sealed trait SpikeDecodePlan[A]

object SpikeDecodePlan:
  final case class Pure[A](value: A) extends SpikeDecodePlan[A]
  final case class Request[A](request: SpikePayloadRequest[A]) extends SpikeDecodePlan[A]
  final case class Map[A, B](source: SpikeDecodePlan[A], f: A => B) extends SpikeDecodePlan[B]
  final case class Zip[A, B](left: SpikeDecodePlan[A], right: SpikeDecodePlan[B])
      extends SpikeDecodePlan[(A, B)]
  final case class Dependent[A, B](first: SpikeDecodePlan[A], next: A => SpikeDecodePlan[B])
      extends SpikeDecodePlan[B]

  final case class Inspection(requests: Vector[String], dependentBarriers: Int):
    def combine(other: Inspection): Inspection =
      Inspection(
        requests = requests ++ other.requests,
        dependentBarriers = dependentBarriers + other.dependentBarriers
      )

  trait Interpreter:
    def apply[A](request: SpikePayloadRequest[A]): A

  def request[A](value: SpikePayloadRequest[A]): SpikeDecodePlan[A] =
    Request(value)

  def zip[A, B](left: SpikeDecodePlan[A], right: SpikeDecodePlan[B]): SpikeDecodePlan[(A, B)] =
    Zip(left, right)

  def inspect[A](plan: SpikeDecodePlan[A]): Inspection =
    plan match
      case Pure(_)          => Inspection(Vector.empty, 0)
      case Request(request) => Inspection(Vector(request.label), 0)
      case Map(source, _)   => inspect(source)
      case Zip(left, right) => inspect(left).combine(inspect(right))
      case Dependent(first, _) =>
        val prefix = inspect(first)
        prefix.copy(dependentBarriers = prefix.dependentBarriers + 1)

  def run[A](plan: SpikeDecodePlan[A], interpreter: Interpreter): A =
    plan match
      case Pure(value)      => value
      case Request(request) => interpreter(request)
      case Map(source, f)   => f(run(source, interpreter))
      case Zip(left, right) => (run(left, interpreter), run(right, interpreter))
      case Dependent(first, next) =>
        val value = run(first, interpreter)
        run(next(value), interpreter)

  extension [A](plan: SpikeDecodePlan[A])
    def map[B](f: A => B): SpikeDecodePlan[B] =
      Map(plan, f)

class DecodePlanTypingSpikeSuite extends munit.FunSuite:
  import SpikeDecodePlan.*

  test("typed heterogeneous leaves remain inspectable and batchable"):
    val plan =
      zip(
        request(SpikePayloadRequest.Coefficients(rows = 2, columns = 3)),
        request(SpikePayloadRequest.BasisReference("basis-17"))
      ).map(pair => SpikeDecodeInput(pair._1, pair._2))

    assertEquals(
      inspect(plan),
      Inspection(Vector("coefficients:2:3", "basis:basis-17"), dependentBarriers = 0)
    )

    val decoded =
      run(
        plan,
        new Interpreter:
          def apply[A](request: SpikePayloadRequest[A]): A =
            request match
              case SpikePayloadRequest.Coefficients(rows, columns) =>
                SpikeCoefficients(Vector.fill(rows * columns)(1.0))
              case SpikePayloadRequest.BasisReference(id) =>
                SpikeBasisReference(id)
      )

    assertEquals(decoded.coefficients.values.length, 6)
    assertEquals(decoded.basis, SpikeBasisReference("basis-17"))

  test("a data-dependent read is an explicit inspection barrier"):
    val plan =
      Dependent(
        request(SpikePayloadRequest.BasisReference("selector")),
        basis => request(SpikePayloadRequest.BasisReference(basis.value + "-resolved"))
      )

    assertEquals(
      inspect(plan),
      Inspection(Vector("basis:selector"), dependentBarriers = 1)
    )

  test("payload result types cannot be exchanged"):
    val errors =
      typeCheckErrors(
        """
        val invalid: scalafim.latent.SpikeDecodePlan[scalafim.latent.SpikeBasisReference] =
          scalafim.latent.SpikeDecodePlan.request(
            scalafim.latent.SpikePayloadRequest.Coefficients(2, 3)
          )
        """
      )

    assert(errors.nonEmpty)
