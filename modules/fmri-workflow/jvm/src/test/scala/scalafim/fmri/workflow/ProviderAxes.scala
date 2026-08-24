package scalafim.fmri.workflow

import image4s.Axis
import image4s.AxisKind
import image4s.geometry.{Affine, D3}
import gale.linalg.DMat

private[workflow] object ProviderAxes:
  def time(extent: Int): Axis =
    Axis
      .ordinal("time", AxisKind.Time, extent)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def affineD3(matrix: DMat): Affine[D3] =
    Affine
      .fromRowMajor[D3](matrix.valuesRowMajor)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
