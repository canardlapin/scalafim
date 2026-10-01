package scalafim.fmri.mvpa.spatial

import scala.compiletime.testing.typeCheckErrors

import locus4s.Region
import multivar.core.SpaceRole
import scalafim.fmri.mvpa.AxisRef
import scalafim.fmri.mvpa.measurement.MeasurementId

class SpatialMeasurementScatterSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(name: String): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector("a", "b", "c", "d"), "voxel-order", "psc", "raw"))

  test("weighted overlap uses the declared denominator and leaves unvisited cells explicit"):
    val neural = axis("scatter-source")
    val first = right(Region.fromOrdinals(neural.locus, Vector(0, 1)))
    val second = right(Region.fromOrdinals(neural.locus, Vector(1, 2)))
    val scattered = right(
      SpatialMeasurementScatter.scatter(
        neural.locus,
        Vector(
          SpatialLocalOutcome(MeasurementId.unsafe("first"), first, 1.0, Right(2.0)),
          SpatialLocalOutcome(MeasurementId.unsafe("second"), second, 3.0, Right(10.0))
        ),
        SpatialScatterAlgebra.weightedMeanDouble
      )
    )
    assertEquals(scattered(neural.locus.indexAtValidatedOrdinal(0)), SpatialScatterCell.Aggregated(2.0, Vector(SpatialScatterContributor(MeasurementId.unsafe("first"), 1.0)), 1.0, Vector.empty))
    scattered(neural.locus.indexAtValidatedOrdinal(1)) match
      case SpatialScatterCell.Aggregated(value, contributors, denominator, failures) =>
        assertEqualsDouble(value, 8.0, 1e-12)
        assertEquals(contributors.map(_.measurement.value), Vector("first", "second"))
        assertEqualsDouble(denominator, 4.0, 0.0)
        assertEquals(failures, Vector.empty)
      case other => fail(s"expected aggregated overlap, got $other")
    assertEquals(scattered(neural.locus.indexAtValidatedOrdinal(3)), SpatialScatterCell.Unvisited)
    val reversed = right(
      SpatialMeasurementScatter.scatter(
        neural.locus,
        Vector(
          SpatialLocalOutcome(MeasurementId.unsafe("second"), second, 3.0, Right(10.0)),
          SpatialLocalOutcome(MeasurementId.unsafe("first"), first, 1.0, Right(2.0))
        ),
        SpatialScatterAlgebra.weightedMeanDouble
      )
    )
    assertEquals(reversed.toVector, scattered.toVector)

  test("local failures remain visible and foreign support is rejected by the type boundary"):
    val neural = axis("scatter-failure-source")
    val support = right(Region.fromOrdinals(neural.locus, Vector(1)))
    val failed = right(
      SpatialMeasurementScatter.scatter(
        neural.locus,
        Vector(SpatialLocalOutcome(MeasurementId.unsafe("failed"), support, 1.0, Left("solver failed"))),
        SpatialScatterAlgebra.weightedMeanDouble
      )
    )
    assertEquals(
      failed(neural.locus.indexAtValidatedOrdinal(1)),
      SpatialScatterCell.LocalFailure(Vector(MeasurementId.unsafe("failed") -> "solver failed"))
    )

    val errors = typeCheckErrors("""import locus4s.{FiniteDomain, Region}
import scalafim.fmri.mvpa.measurement.MeasurementId
import scalafim.fmri.mvpa.spatial.{SpatialLocalOutcome, SpatialMeasurementScatter, SpatialScatterAlgebra}
def invalid[S, T](domain: FiniteDomain[S], support: Region[T]) =
  SpatialMeasurementScatter.scatter(domain, Vector(SpatialLocalOutcome(MeasurementId.unsafe("foreign"), support, 1.0, Right(1.0))), SpatialScatterAlgebra.weightedMeanDouble)
""")
    assertEquals(errors.length, 1)
    assert(errors.head.lineContent.contains("SpatialMeasurementScatter.scatter"), errors.head.lineContent)
    assert(errors.head.message.contains("FiniteDomain") || errors.head.message.contains("Region") || errors.head.message.contains("SpatialLocalOutcome"), errors.head.message)

  test("overlap denominator overflow is rejected before aggregation"):
    val neural = axis("scatter-overflow-source")
    val support = right(Region.fromOrdinals(neural.locus, Vector(0)))
    val result = SpatialMeasurementScatter.scatter(
      neural.locus,
      Vector(
        SpatialLocalOutcome(MeasurementId.unsafe("one"), support, Double.MaxValue, Right(0.0)),
        SpatialLocalOutcome(MeasurementId.unsafe("two"), support, Double.MaxValue, Right(0.0))
      ),
      SpatialScatterAlgebra.weightedMeanDouble
    )
    assertEquals(result.left.toOption, Some(SpatialScatterError.DenominatorOverflow(0)))

  test("built-in weighted mean refuses a non-finite weighted accumulator"):
    val neural = axis("scatter-nonfinite-aggregate")
    val support = right(Region.fromOrdinals(neural.locus, Vector(0)))
    val result = SpatialMeasurementScatter.scatter(
      neural.locus,
      Vector(
        SpatialLocalOutcome(MeasurementId.unsafe("one"), support, 1.0, Right(Double.MaxValue)),
        SpatialLocalOutcome(MeasurementId.unsafe("two"), support, 1.0, Right(Double.MaxValue))
      ),
      SpatialScatterAlgebra.weightedMeanDouble
    )
    assertEquals(result.left.toOption, Some(SpatialScatterError.NonFiniteAggregate(0)))
