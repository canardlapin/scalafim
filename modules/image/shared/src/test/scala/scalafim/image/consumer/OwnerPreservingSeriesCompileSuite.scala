package scalafim.image.consumer

import scala.compiletime.testing.typeCheckErrors

final class OwnerPreservingSeriesCompileSuite extends munit.FunSuite:
  test("shape-preserving series operations retain the exact static owner"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.{DType, Rank}
import scalafim.image.*

def mapValuesKeepsOwner[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
    series: NeuroSeries[S, A, Sem],
    f: A => B
)(using DType[B], ValueSemantics[B, OutSem]):
    NeuroSeries[S, B, OutSem] =
  series.mapValues[B, OutSem](f)

def mapKeepsOwner[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
    series: NeuroSeries[S, A, Sem],
    f: A => B
)(using DType[B], ValueSemantics[B, OutSem]):
    NeuroSeries[S, B, OutSem] =
  series.map[B, OutSem](f)

def mapVoxelsKeepsOwner[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
    series: NeuroSeries[S, A, Sem],
    f: (VoxelCoord, A) => B
)(using DType[B], ValueSemantics[B, OutSem]):
    NeuroSeries[S, B, OutSem] =
  series.mapVoxels[B, OutSem](f)

def mapSamplesKeepsOwner[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
    series: NeuroSeries[S, A, Sem],
    f: (VoxelCoord, Int, A) => B
)(using DType[B], ValueSemantics[B, OutSem]):
    NeuroSeries[S, B, OutSem] =
  series.mapSamples[B, OutSem](f)

def materializeKeepsOwner[S <: SampleSpace[?, D3], A, Sem](
    series: NeuroSeries[S, A, Sem]
): NeuroSeries[S, A, Sem] =
  series.materializedCanonical

def metadataKeepsOwner[S <: SampleSpace[?, D3], A, Sem](
    series: NeuroSeries[S, A, Sem],
    metadata: ImageMetadata
): NeuroSeries[S, A, Sem] =
  series.withImageMetadata(metadata)

def explicitProviderView[S <: SampleSpace[?, D3], A, Sem](
    series: NeuroSeries[S, A, Sem]
): Sampled[S, A, Sem, Rank[4]] =
  series.sampled

def explicitDynamicView[S <: SampleSpace[?, D3], A, Sem](
    series: NeuroSeries[S, A, Sem]
): SomeNeuroSeries[A, Sem] =
  SomeNeuroSeries.eraseSpace(series)

def selectionChangesOwner[S <: SampleSpace[?, D3], A, Sem](
    series: NeuroSeries[S, A, Sem],
    indices: Seq[Int]
)(using ValueSemantics[A, Sem]):
    Either[NeuroImageError, SomeNeuroSeries[A, Sem]] =
  series.selectTimes(indices)
"""
    )

    assertEquals(errors, Nil)

  test("a raw Sampled cannot claim a NeuroSeries owner"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.*
import scalafim.image.*

def bypass[S <: SampleSpace[?, D3], A, Sem](
    sampled: Sampled[S, A, Sem, Rank[4]]
): NeuroSeries[S, A, Sem] = sampled
"""
    )

    assert(errors.nonEmpty)

  test("a concrete series owner cannot be erased implicitly"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import scalafim.image.*

def erase[S <: SampleSpace[?, D3], A, Sem](
    series: NeuroSeries[S, A, Sem]
): SomeNeuroSeries[A, Sem] = series
"""
    )

    assert(errors.nonEmpty)

  test("time selection cannot claim the source sample-space owner"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import scalafim.image.*

def lie[S <: SampleSpace[?, D3], A, Sem](
    series: NeuroSeries[S, A, Sem],
    indices: Seq[Int]
)(using ValueSemantics[A, Sem]):
    Either[NeuroImageError, NeuroSeries[S, A, Sem]] =
  series.selectTimes(indices)
"""
    )

    assert(errors.nonEmpty)
