package scalafim.image.consumer

import scala.compiletime.testing.typeCheckErrors

final class DynamicImageRefinementCompileSuite extends munit.FunSuite:

  test("validated concrete images cross their intended dynamic boundaries"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import scalafim.image.*

def widenVolume[S <: SampleSpace[?, D3], A](
    volume: ScalarVolume[S, A]
): SomeScalarVolume[A] = SomeNeuroVolume.eraseSpace(volume)

def widenSeries[S <: SampleSpace[?, D3], A](
    series: ScalarSeries[S, A]
): SomeScalarSeries[A] = SomeNeuroSeries.eraseSpace(series)
"""
    )

    assertEquals(errors, Nil)

  test("a raw rank-3 Sampled cannot bypass the NeuroVolume admission boundary"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.*
import scalafim.image.*

def bypass[A](
    sampled: Sampled[
      ? <: SampleSpace[?, D3],
      A,
      Continuous,
      Rank[3]
    ]
): SomeScalarVolume[A] = sampled
"""
    )

    assert(errors.nonEmpty)

  test("a raw rank-4 Sampled cannot bypass the NeuroSeries time-axis check"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.*
import scalafim.image.*

def bypass[A](
    sampled: Sampled[
      ? <: SampleSpace[?, D3],
      A,
      Continuous,
      Rank[4]
    ]
): SomeScalarSeries[A] = sampled
"""
    )

    assert(errors.nonEmpty)
