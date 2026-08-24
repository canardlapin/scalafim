package scalafim.image

import image4s.SampleSpace
import image4s.SomeSampleSpace
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.Grid

private[image] object ProviderSpaces:
  def affine(rows: Vector[Vector[Double]]): Affine[D3] =
    Affine
      .fromRowMajor[D3](rows.flatten)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def affine(values: Double*): Affine[D3] =
    Affine
      .fromRowMajor[D3](values)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def volume(
      space: SomeSampleSpace
  ): SampleSpace[? <: Frame[D3], D3] =
    SampleSpaces
      .requireVolumeD3(space)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def grid(
      space: SomeSampleSpace
  ): Grid[? <: Frame[D3], D3] =
    volume(space).grid
