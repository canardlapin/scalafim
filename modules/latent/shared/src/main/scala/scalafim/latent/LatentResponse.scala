package scalafim.latent

import scalafim.linalg.DoubleMatrix

final case class LatentShape(
    timepoints: Int,
    samples: Int,
    coefficients: Int
):
  require(timepoints > 0, "timepoint count must be positive")
  require(samples > 0, "sample count must be positive")
  require(coefficients > 0, "coefficient count must be positive")

trait LatentResponse:
  def shape: LatentShape
  def sourceDomain: DomainId
  def targetDomain: DomainId
  def label: String
  def metadata: Map[String, String]

  /** Coefficient time series with rows as timepoints and columns as latent coefficients. */
  def coefTime: DoubleMatrix

  /** Decode coefficient columns from coefficient-space rows to target-sample rows. */
  def decodeCoefficients(coefficients: DoubleMatrix): Either[LatentError, DoubleMatrix]

  /** Reconstruct dense response data with rows as timepoints and columns as selected samples. */
  def reconstruct(selection: LatentSelection = LatentSelection.All): Either[LatentError, DoubleMatrix]
