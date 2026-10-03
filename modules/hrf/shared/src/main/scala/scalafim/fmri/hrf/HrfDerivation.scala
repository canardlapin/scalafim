package scalafim.fmri.hrf

/** Declared construction policy for a derived kernel, independent of its name.
  * Computed kernel values remain a separate numerical-content identity.
  */
enum HrfDerivation:
  case Lagged(by: Seconds)
  case Blocked(width: Seconds, precision: Seconds, halfLife: Double,
      summate: Boolean, normalize: Boolean, integration: Integration)
  case PeakNormalized(referenceStep: Seconds)
  case Normalized(mode: HrfNormalization)
