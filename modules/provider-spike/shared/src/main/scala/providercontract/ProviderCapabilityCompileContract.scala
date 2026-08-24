package providercontract

import gale.linalg.DMat
import image4s.Continuous
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.locus.GridDomain
import image4s.locus.SelectedSampled
import image4s.locus.SelectedSampledError
import locus4s.FiniteDomain
import locus4s.Selection
import locus4s.SelectionError
import ravel.AnyRank
import ravel.DType.given
import ravel.NDArray
import reframe4s.lie.FramedAffine
import reframe4s.resample.Interpolation
import reframe4s.resample.ResamplingError
import reframe4s.resample.ResamplingPlan

/** Compile-only proof that the admitted providers compose without a ScalaFIM wrapper. */
object ProviderCapabilityCompileContract:
  val matrix: DMat =
    DMat.dense(
      2,
      2,
      Vector(
        1.0, 0.0,
        0.0, 1.0
      )
    )

  val affine: Affine[D3] =
    Affine.identity[D3]

  def checkedAffine(
      rowMajor: Vector[Double]
  ): Either[GeometryError, Affine[D3]] =
    Affine.fromRowMajor[D3](rowMajor)

  def exactSelection[S](
      domain: FiniteDomain[S],
      ordinals: IterableOnce[Int]
  ): Either[SelectionError, Selection[S]] =
    Selection.fromOrdinals(domain, ordinals)

  def selectedSampling[
      F <: Frame[D3],
      S,
      R <: AnyRank
  ](
      domain: GridDomain[F, D3, S],
      selection: Selection[S],
      nonSpatialAxes: NonSpatialAxes,
      data: NDArray[Double, R]
  ): Either[
    SelectedSampledError,
    SelectedSampled[F, D3, S, Double, Continuous, R]
  ] =
    SelectedSampled.continuous(
      domain,
      selection,
      nonSpatialAxes,
      data
    )

  def resamplingPlan[
      SourceFrame <: Frame[D3],
      TargetFrame <: Frame[D3],
      S <: SampleSpace[SourceFrame, D3],
      R <: AnyRank
  ](
      source: Sampled[S, Double, Continuous, R],
      target: Grid[TargetFrame, D3]
  ): Either[
    ResamplingError,
    ResamplingPlan[SourceFrame, TargetFrame, D3, Continuous, R]
  ] =
    val pull =
      FramedAffine.betweenFrames[TargetFrame, SourceFrame, D3](
        target.frame,
        source.frame
      )(Affine.identity[D3])
    ResamplingPlan.affine(
      source,
      target,
      pull,
      Interpolation.Linear
    )
