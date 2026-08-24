package scalafim.fmri.motion

import image4s.Axis
import image4s.AxisKind

private[motion] object ProviderAxes:
  def time(extent: Int): Axis =
    Axis
      .ordinal("time", AxisKind.Time, extent)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
