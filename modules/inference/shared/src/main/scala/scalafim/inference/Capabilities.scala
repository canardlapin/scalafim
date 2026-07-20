package scalafim.inference

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector
import scalafim.multivar.MvMap
import scalafim.multivar.MvSpace
import scalafim.multivar.Spectrum

enum OrderedSpectrumKind:
  case Eigenvalues
  case SingularValues
  case CanonicalCorrelations
  case Covariance

final case class OrderedSpectrum private (
    kind: OrderedSpectrumKind,
    values: DoubleVector
)

object OrderedSpectrum:
  def from(spectrum: Spectrum): Either[InferenceError, OrderedSpectrum] =
    val kind =
      spectrum match
        case Spectrum.Eigenvalues(_)           => OrderedSpectrumKind.Eigenvalues
        case Spectrum.SingularValues(_)        => OrderedSpectrumKind.SingularValues
        case Spectrum.CanonicalCorrelations(_) => OrderedSpectrumKind.CanonicalCorrelations
        case Spectrum.Covariance(_)            => OrderedSpectrumKind.Covariance
    val source = spectrum.values
    if source.length <= 0 then Left(InferenceError.InvalidSpectrum("spectrum must be non-empty"))
    else
      var i = 0
      var previous = Double.PositiveInfinity
      var error = Option.empty[InferenceError]
      while i < source.length && error.isEmpty do
        val value = source(i)
        if !value.isFinite then error = Some(InferenceError.InvalidSpectrum(s"entry $i is not finite: $value"))
        else if value < 0.0 then error = Some(InferenceError.InvalidSpectrum(s"entry $i is negative: $value"))
        else if value > previous then
          error = Some(InferenceError.InvalidSpectrum(s"entry $i increases from $previous to $value"))
        previous = value
        i += 1
      error.toLeft(OrderedSpectrum(kind, DoubleVector.fromSeq(Vector.tabulate(source.length)(source(_)))))

final case class DomainBundle private (entries: Vector[MvSpace]):
  def find(id: scalafim.multivar.SpaceId): Option[MvSpace] =
    entries.find(_.id == id)

object DomainBundle:
  def from(entries: Iterable[MvSpace]): Either[InferenceError, DomainBundle] =
    val values = entries.toVector
    if values.isEmpty then Left(InferenceError.InvalidCount("fit domains", 0))
    else if values.map(_.id).distinct.length != values.length then
      Left(InferenceError.InvalidIdentifier("fit domains", "duplicate space ids"))
    else Right(DomainBundle(values))

trait Refit[D, F]:
  def fit(data: D): Either[InferenceError, F]

trait OrderedFit[F]:
  def spectrum(fit: F): Either[InferenceError, OrderedSpectrum]
  def domains(fit: F): Either[InferenceError, DomainBundle]
  def coordinates(
      fit: F,
      domain: scalafim.multivar.SpaceId
  ): Either[InferenceError, MvMap]

trait InferenceTarget[F, A]:
  def label: TargetLabel
  def observe(fit: F, unit: LatentUnit): Either[InferenceError, A]
  def alternative: Alternative
  def invariance: TargetInvariance

trait Deflatable[D, F]:
  type State
  def begin(data: D, fit: F): Either[InferenceError, State]
  def remove(state: State, unit: LatentUnit): Either[InferenceError, State]

trait StabilityView[F]:
  def loadings(
      fit: F,
      domain: scalafim.multivar.SpaceId
  ): Either[InferenceError, DoubleMatrix]

  def scores(
      fit: F,
      domain: scalafim.multivar.SpaceId
  ): Either[InferenceError, DoubleMatrix]
