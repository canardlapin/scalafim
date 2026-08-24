package scalafim.dataset.io

import image4s.Axis
import image4s.AxisKind

private[dataset] object ProviderAxes:
  def time(extent: Int): Axis =
    Axis
      .ordinal("time", AxisKind.Time, extent)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
