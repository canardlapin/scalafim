package scalafim.graph.linalg

import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.Matrix
import gale.linalg.Vec
import scalafim.graph.VertexBasis

final case class GraphFeatureSpec(name: String, parameters: Vector[Double]):
  require(name.trim.nonEmpty, "graph feature name must be non-empty")
  require(parameters.forall(_.isFinite), "graph feature parameters must be finite")

sealed trait GraphSimilarityError:
  def message: String

object GraphSimilarityError:
  final case class InvalidParameter(name: String, value: Double, reason: String) extends GraphSimilarityError:
    def message: String = s"invalid $name $value: $reason"

  final case class BasisMismatch(left: Vector[String], right: Vector[String]) extends GraphSimilarityError:
    def message: String =
      s"graph feature basis mismatch: left=[${left.mkString(",")}] right=[${right.mkString(",")}]"

  final case class FeatureMismatch(left: GraphFeatureSpec, right: GraphFeatureSpec) extends GraphSimilarityError:
    def message: String =
      s"graph feature specification mismatch: left=${left.name}${left.parameters} right=${right.name}${right.parameters}"

  final case class NonFiniteFeature(index: Int, value: Double) extends GraphSimilarityError:
    def message: String =
      s"graph feature value at linear index $index is not finite: $value"

enum PsdStatus:
  case KnownPsd(construction: String)
  case NotEstablished(reason: String)

trait GraphSimilarity[-A]:
  def name: String
  def psdStatus: PsdStatus
  def apply(left: A, right: A): Either[GraphSimilarityError, Double]

final class VertexFeatureSet[K, V] private[linalg] (
    val basis: VertexBasis[K, V],
    val values: DMat,
    val spec: GraphFeatureSpec
):
  require(values.rows == basis.size, "vertex feature rows must match the basis")

  def featureCount: Int =
    values.cols

  def toVector: DVec =
    Vec.tabulate(values.rows * values.cols): index =>
      values(index / values.cols, index % values.cols)

  /** Align rows by stable keys. This establishes numerical key alignment only;
    * domain layers such as connectivity remain responsible for scientific
    * atlas/provenance compatibility before requesting it.
    */
  def alignTo(target: VertexBasis[K, V]): Either[GraphSimilarityError, VertexFeatureSet[K, V]] =
    if !basis.sameKeySetAs(target) then
      Left(GraphSimilarityError.BasisMismatch(basis.keys.map(_.toString), target.keys.map(_.toString)))
    else
      val aligned = Matrix.newBuilder(target.size, values.cols)
      var row = 0
      while row < target.size do
        val sourceRow = basis.indexOf(target.keyAt(scalafim.graph.VertexIx.unsafe(row))).get.toInt
        var col = 0
        while col < values.cols do
          aligned(row, col) = values(sourceRow, col)
          col += 1
        row += 1
      Right(new VertexFeatureSet(target, aligned.result(), spec))

enum SpectralDiagonalKind:
  case HeatKernel
  case DiffusionEnergy

  private[linalg] def exponentFactor: Double =
    this match
      case HeatKernel      => 1.0
      case DiffusionEnergy => 2.0

  private[linalg] def label: String =
    this match
      case HeatKernel      => "heat-kernel-diagonal"
      case DiffusionEnergy => "diffusion-energy-diagonal"

final class SpectralDiagonalFeature private (
    val times: Vector[Double],
    val kind: SpectralDiagonalKind
):
  def apply[K, V](spectrum: VertexSpectrum[K, V]): Either[GraphSimilarityError, VertexFeatureSet[K, V]] =
    val rows = spectrum.basis.size
    val cols = times.length
    val out = Matrix.newBuilder(rows, cols)
    var row = 0
    var error = Option.empty[GraphSimilarityError]
    while row < rows && error.isEmpty do
      var timeIndex = 0
      while timeIndex < cols && error.isEmpty do
        val time = times(timeIndex)
        var component = 0
        var value = 0.0
        while component < spectrum.result.size do
          val eigenvalue = Math.max(spectrum.result.eigenvalues(component), 0.0)
          val coordinate = spectrum.result.eigenvectors(row, component)
          value += Math.exp(-kind.exponentFactor * time * eigenvalue) * coordinate * coordinate
          component += 1
        if !value.isFinite then error = Some(GraphSimilarityError.NonFiniteFeature(row * cols + timeIndex, value))
        else out(row, timeIndex) = value
        timeIndex += 1
      row += 1
    error match
      case Some(value) => Left(value)
      case None =>
        Right(
          new VertexFeatureSet(
            spectrum.basis,
            out.result(),
            GraphFeatureSpec(kind.label, times)
          )
        )

object SpectralDiagonalFeature:
  def from(
      times: Iterable[Double],
      kind: SpectralDiagonalKind = SpectralDiagonalKind.HeatKernel
  ): Either[GraphSimilarityError, SpectralDiagonalFeature] =
    val values = times.toVector
    if values.isEmpty then
      Left(GraphSimilarityError.InvalidParameter("spectral time count", 0.0, "must be positive"))
    else
      values.find(value => !value.isFinite || value < 0.0) match
        case Some(value) =>
          Left(GraphSimilarityError.InvalidParameter("spectral time", value, "must be finite and non-negative"))
        case None => Right(new SpectralDiagonalFeature(values, kind))

final case class LinearFeatureSimilarity[K, V]() extends GraphSimilarity[VertexFeatureSet[K, V]]:
  val name: String = "spectral-feature-linear"
  val psdStatus: PsdStatus = PsdStatus.KnownPsd("inner product of explicit finite vertex feature vectors")

  def apply(
      left: VertexFeatureSet[K, V],
      right: VertexFeatureSet[K, V]
  ): Either[GraphSimilarityError, Double] =
    validateComparable(left, right).map: _ =>
      var total = 0.0
      var row = 0
      while row < left.values.rows do
        var col = 0
        while col < left.values.cols do
          total += left.values(row, col) * right.values(row, col)
          col += 1
        row += 1
      total

final case class RbfFeatureSimilarity[K, V] private (gamma: Double)
    extends GraphSimilarity[VertexFeatureSet[K, V]]:
  val name: String = "spectral-feature-rbf"
  val psdStatus: PsdStatus = PsdStatus.KnownPsd("Gaussian RBF over an explicit Euclidean feature map")

  def apply(
      left: VertexFeatureSet[K, V],
      right: VertexFeatureSet[K, V]
  ): Either[GraphSimilarityError, Double] =
    validateComparable(left, right).map: _ =>
      var squared = 0.0
      var row = 0
      while row < left.values.rows do
        var col = 0
        while col < left.values.cols do
          val difference = left.values(row, col) - right.values(row, col)
          squared += difference * difference
          col += 1
        row += 1
      Math.exp(-gamma * squared)

object RbfFeatureSimilarity:
  def from[K, V](gamma: Double): Either[GraphSimilarityError, RbfFeatureSimilarity[K, V]] =
    if !gamma.isFinite || gamma <= 0.0 then
      Left(GraphSimilarityError.InvalidParameter("RBF gamma", gamma, "must be finite and positive"))
    else Right(new RbfFeatureSimilarity(gamma))

final case class NegativeEuclideanFeatureSimilarity[K, V]()
    extends GraphSimilarity[VertexFeatureSet[K, V]]:
  val name: String = "negative-spectral-feature-distance"
  val psdStatus: PsdStatus = PsdStatus.NotEstablished("negative Euclidean distance is a similarity, not a declared PSD kernel")

  def apply(
      left: VertexFeatureSet[K, V],
      right: VertexFeatureSet[K, V]
  ): Either[GraphSimilarityError, Double] =
    validateComparable(left, right).map: _ =>
      var squared = 0.0
      var row = 0
      while row < left.values.rows do
        var col = 0
        while col < left.values.cols do
          val difference = left.values(row, col) - right.values(row, col)
          squared += difference * difference
          col += 1
        row += 1
      -Math.sqrt(squared)

private def validateComparable[K, V](
    left: VertexFeatureSet[K, V],
    right: VertexFeatureSet[K, V]
): Either[GraphSimilarityError, Unit] =
  if !left.basis.sameKeyOrderAs(right.basis) then
    Left(GraphSimilarityError.BasisMismatch(left.basis.keys.map(_.toString), right.basis.keys.map(_.toString)))
  else if left.spec != right.spec then Left(GraphSimilarityError.FeatureMismatch(left.spec, right.spec))
  else Right(())
