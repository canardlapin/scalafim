package scalafim.image.consumer

import scala.compiletime.testing.typeCheckErrors

final class OwnerPreservingVolumeCompileSuite extends munit.FunSuite:
  test("unary volume operations preserve the exact static owner"):
    val errors = typeCheckErrors(
      """
import cats.Applicative
import image4s.*
import image4s.geometry.*
import ravel.{DType, Rank}
import scalafim.image.*
import spire.algebra.{Order, Ring}

def mapValuesKeepsOwner[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
    volume: NeuroVolume[S, A, Sem],
    f: A => B
)(using DType[B], ValueSemantics[B, OutSem]):
    NeuroVolume[S, B, OutSem] =
  volume.mapValues[B, OutSem](f)

def mapValuesSameSemanticsKeepsOwner[S <: SampleSpace[?, D3], A, Sem](
    volume: NeuroVolume[S, A, Sem],
    f: A => A
)(using DType[A], ValueSemantics[A, Sem]): NeuroVolume[S, A, Sem] =
  volume.mapValues(f)

def mapKeepsOwner[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
    volume: NeuroVolume[S, A, Sem],
    f: A => B
)(using DType[B], ValueSemantics[B, OutSem]):
    NeuroVolume[S, B, OutSem] =
  volume.map[B, OutSem](f)

def mapVoxelsKeepsOwner[S <: SampleSpace[?, D3], A, Sem, B, OutSem](
    volume: NeuroVolume[S, A, Sem],
    f: (VoxelCoord, A) => B
)(using DType[B], ValueSemantics[B, OutSem]):
    NeuroVolume[S, B, OutSem] =
  volume.mapVoxels[B, OutSem](f)

def asLogicalKeepsOwner[S <: SampleSpace[?, D3], A, Sem](
    volume: NeuroVolume[S, A, Sem]
)(using Ring[A]): MaskVolume[S] =
  volume.asLogical

def asMaskKeepsOwner[S <: SampleSpace[?, D3], A, Sem](
    volume: NeuroVolume[S, A, Sem]
)(using Ring[A], Order[A]): MaskVolume[S] =
  volume.asMask

def traverseKeepsOwner[F[_], S <: SampleSpace[?, D3], A, Sem, B, OutSem](
    volume: NeuroVolume[S, A, Sem],
    f: A => F[B]
)(using Applicative[F], DType[B], ValueSemantics[B, OutSem]):
    F[NeuroVolume[S, B, OutSem]] =
  volume.traverseValues[F, B, OutSem](f)

def materializeKeepsOwner[S <: SampleSpace[?, D3], A, Sem](
    volume: NeuroVolume[S, A, Sem]
): NeuroVolume[S, A, Sem] =
  volume.materializedCanonical

def metadataKeepsOwner[S <: SampleSpace[?, D3], A, Sem](
    volume: NeuroVolume[S, A, Sem],
    metadata: ImageMetadata
): NeuroVolume[S, A, Sem] =
  volume.withImageMetadata(metadata)

def explicitProviderView[S <: SampleSpace[?, D3], A, Sem](
    volume: NeuroVolume[S, A, Sem]
): Sampled[S, A, Sem, Rank[3]] =
  volume.sampled

def explicitDynamicView[S <: SampleSpace[?, D3], A, Sem](
    volume: NeuroVolume[S, A, Sem]
): SomeNeuroVolume[A, Sem] =
  SomeNeuroVolume.eraseSpace(volume)
"""
    )

    assertEquals(errors, Nil)

  test("a raw Sampled cannot claim a NeuroVolume owner"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.*
import scalafim.image.*

def bypass[S <: SampleSpace[?, D3], A, Sem](
    sampled: Sampled[S, A, Sem, Rank[3]]
): NeuroVolume[S, A, Sem] = sampled
"""
    )

    assert(errors.nonEmpty)

  test("a concrete owner cannot be erased without the explicit boundary"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import scalafim.image.*

def erase[S <: SampleSpace[?, D3], A, Sem](
    volume: NeuroVolume[S, A, Sem]
): SomeNeuroVolume[A, Sem] = volume
"""
    )

    assert(errors.nonEmpty)
