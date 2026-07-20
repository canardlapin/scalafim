package scalafim.connectivity

import scala.collection.mutable

import scalafim.graph.BasisError
import scalafim.graph.VertexBasis
import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

final case class NodeSpec(id: NodeId, label: String, system: Option[SystemId] = None):
  require(label.trim.nonEmpty, "node label must be non-empty")

final class NodeAxis private (
    val basis: VertexBasis[NodeId, NodeSpec],
    val provenance: NodeAxisProvenance
):
  def nodes: Vector[NodeSpec] =
    basis.values

  def size: Int =
    basis.size

  def ids: Vector[NodeId] =
    basis.keys

  def labels: Vector[String] =
    nodes.map(_.label)

  def systems: Vector[Option[SystemId]] =
    nodes.map(_.system)

  def indexOf(id: NodeId): Option[Int] =
    basis.indexOf(id).map(_.toInt)

  def sameKeyOrderAs(other: NodeAxis): Boolean =
    basis.sameKeyOrderAs(other.basis)

  def sameKeySetAs(other: NodeAxis): Boolean =
    basis.sameKeySetAs(other.basis)

  def sameMetadataAs(other: NodeAxis): Boolean =
    basis.sameMetadataAs(other.basis)

  def sameScientificBasisAs(other: NodeAxis): Boolean =
    sameKeyOrderAs(other) && provenance.compatibleWith(other.provenance)

  def sameIdsAs(other: NodeAxis): Boolean =
    sameKeyOrderAs(other)

  def sameIdentityAs(other: NodeAxis): Boolean =
    sameMetadataAs(other)

object NodeAxis:
  def from(
      nodes: Iterable[NodeSpec],
      provenance: NodeAxisProvenance = NodeAxisProvenance.Unspecified
  ): Either[ConnectivityError, NodeAxis] =
    val values = nodes.toVector
    if values.isEmpty then Left(ConnectivityError.EmptyAxis("node"))
    else
      var i = 0
      var error = Option.empty[ConnectivityError]
      while i < values.length && error.isEmpty do
        val node = values(i)
        if node.label.trim.isEmpty then
          error = Some(ConnectivityError.InvalidId("node label", node.label, "must be non-empty"))
        i += 1

      error match
        case Some(value) => Left(value)
        case None =>
          VertexBasis.from(values.map(node => node.id -> node)) match
            case Left(BasisError.DuplicateKey(key, _, _)) => Left(ConnectivityError.DuplicateId("node", key.value))
            case Left(value)                              => Left(ConnectivityError.InvalidPlan(value.message))
            case Right(basis)                             => Right(new NodeAxis(basis, provenance))

  def fromIdsAndLabels(
      ids: Iterable[NodeId],
      labels: Iterable[String],
      systems: Iterable[Option[SystemId]] = Vector.empty,
      provenance: NodeAxisProvenance = NodeAxisProvenance.Unspecified
  ): Either[ConnectivityError, NodeAxis] =
    val idVector = ids.toVector
    val labelVector = labels.toVector
    val systemVector = systems.toVector
    if idVector.length != labelVector.length then
      Left(ConnectivityError.LabelCountMismatch("node", idVector.length, labelVector.length))
    else if systemVector.nonEmpty && systemVector.length != idVector.length then
      Left(ConnectivityError.LabelCountMismatch("node system", idVector.length, systemVector.length))
    else
      val specs =
        idVector.indices.map { index =>
          val system = if systemVector.isEmpty then None else systemVector(index)
          NodeSpec(idVector(index), labelVector(index), system)
        }
      from(specs, provenance)

  def generated(
      size: Int,
      prefix: String = "node",
      provenance: NodeAxisProvenance = NodeAxisProvenance.Unspecified
  ): Either[ConnectivityError, NodeAxis] =
    if size <= 0 then Left(ConnectivityError.EmptyAxis("node"))
    else
      ConnectivityIdentifier.validate("node prefix", prefix).flatMap { validPrefix =>
        val specs = Vector.newBuilder[NodeSpec]
        var index = 0
        var error = Option.empty[ConnectivityError]
        while index < size && error.isEmpty do
          NodeId(s"$validPrefix-${index + 1}") match
            case Left(value) => error = Some(value)
            case Right(id)   => specs += NodeSpec(id, id.value)
          index += 1
        error match
          case Some(value) => Left(value)
          case None        => from(specs.result(), provenance)
      }

  def unsafe(
      nodes: Iterable[NodeSpec],
      provenance: NodeAxisProvenance = NodeAxisProvenance.Unspecified
  ): NodeAxis =
    from(nodes, provenance).fold(error => throw new IllegalArgumentException(error.message), identity)

final class TimeAxis private (
    val sampleCount: Int,
    val samplePeriod: SamplePeriod
):
  def sampleRateHz: Double =
    samplePeriod.hz

  def durationSeconds: Double =
    sampleCount.toDouble * samplePeriod.seconds

object TimeAxis:
  def fromSamplePeriod(sampleCount: Int, samplePeriod: SamplePeriod): Either[ConnectivityError, TimeAxis] =
    if sampleCount <= 0 then Left(ConnectivityError.InvalidDimension("sample count", sampleCount))
    else Right(new TimeAxis(sampleCount, samplePeriod))

  def fromSeconds(sampleCount: Int, trSeconds: Double): Either[ConnectivityError, TimeAxis] =
    SamplePeriod.fromSeconds(trSeconds).flatMap(fromSamplePeriod(sampleCount, _))

  def fromHz(sampleCount: Int, hz: Double): Either[ConnectivityError, TimeAxis] =
    SamplePeriod.fromHz(hz).flatMap(fromSamplePeriod(sampleCount, _))

  def unsafeSeconds(sampleCount: Int, trSeconds: Double): TimeAxis =
    fromSeconds(sampleCount, trSeconds).fold(error => throw new IllegalArgumentException(error.message), identity)

final class RunAxis private (val runs: Vector[RunId]):
  def size: Int =
    runs.length

object RunAxis:
  def from(runs: Iterable[RunId]): Either[ConnectivityError, RunAxis] =
    val values = runs.toVector
    if values.isEmpty then Left(ConnectivityError.EmptyAxis("run"))
    else
      val seen = mutable.HashSet.empty[String]
      var i = 0
      var error = Option.empty[ConnectivityError]
      while i < values.length && error.isEmpty do
        val run = values(i)
        if seen.contains(run.value) then error = Some(ConnectivityError.DuplicateId("run", run.value))
        else seen += run.value
        i += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(new RunAxis(values))

  def unsafe(runs: Iterable[RunId]): RunAxis =
    from(runs).fold(error => throw new IllegalArgumentException(error.message), identity)

final class FrameWeights private (val timeAxis: TimeAxis, val values: DoubleVector):
  def length: Int =
    values.length

  def apply(index: Int): Double =
    values(index)

object FrameWeights:
  def uniform(timeAxis: TimeAxis): FrameWeights =
    new FrameWeights(timeAxis, DoubleVector.fromSeq(Vector.fill(timeAxis.sampleCount)(1.0)))

  def from(values: Iterable[Double], timeAxis: TimeAxis): Either[ConnectivityError, FrameWeights] =
    val vector = values.toVector
    if vector.length != timeAxis.sampleCount then
      Left(ConnectivityError.FrameWeightLengthMismatch(timeAxis.sampleCount, vector.length))
    else
      var i = 0
      var positiveSum = 0.0
      var error = Option.empty[ConnectivityError]
      while i < vector.length && error.isEmpty do
        val value = vector(i)
        if !value.isFinite || value < 0.0 then error = Some(ConnectivityError.InvalidFrameWeight(i, value))
        else positiveSum += value
        i += 1
      if error.isEmpty && positiveSum <= 0.0 then
        error = Some(ConnectivityError.InvalidScalar("frame weight sum", positiveSum, "must be positive"))

      error match
        case Some(value) => Left(value)
        case None        => Right(new FrameWeights(timeAxis, DoubleVector.fromSeq(vector)))

final class NuisanceMatrix private (
    val timeAxis: TimeAxis,
    val columnLabels: Vector[String],
    val values: DoubleMatrix
):
  def cols: Int =
    values.cols

object NuisanceMatrix:
  def from(
      values: DoubleMatrix,
      timeAxis: TimeAxis,
      columnLabels: Iterable[String]
  ): Either[ConnectivityError, NuisanceMatrix] =
    val labels = columnLabels.toVector
    if values.rows != timeAxis.sampleCount then
      Left(ConnectivityError.MatrixShapeMismatch(s"nuisance matrix expected ${timeAxis.sampleCount} rows, got ${values.rows}"))
    else if values.cols <= 0 then Left(ConnectivityError.InvalidDimension("nuisance column count", values.cols))
    else if labels.length != values.cols then
      Left(ConnectivityError.LabelCountMismatch("nuisance column", values.cols, labels.length))
    else
      firstNonFinite(values, "nuisance matrix") match
        case Some(error) => Left(error)
        case None        => Right(new NuisanceMatrix(timeAxis, labels, ConnectivityStorage.copy(values)))

final class ParcelTimeSeries private (
    val nodeAxis: NodeAxis,
    val timeAxis: TimeAxis,
    val values: DoubleMatrix
):
  def samples: Int =
    timeAxis.sampleCount

  def nodes: Int =
    nodeAxis.size

object ParcelTimeSeries:
  def from(
      values: DoubleMatrix,
      nodeAxis: NodeAxis,
      timeAxis: TimeAxis
  ): Either[ConnectivityError, ParcelTimeSeries] =
    if values.rows <= 0 || values.cols <= 0 then
      Left(ConnectivityError.MatrixShapeMismatch(s"parcel time series must be non-empty, got ${values.rows}x${values.cols}"))
    else if values.rows != timeAxis.sampleCount then
      Left(ConnectivityError.MatrixShapeMismatch(s"parcel time series expected ${timeAxis.sampleCount} rows, got ${values.rows}"))
    else if values.cols != nodeAxis.size then
      Left(ConnectivityError.MatrixShapeMismatch(s"parcel time series expected ${nodeAxis.size} node columns, got ${values.cols}"))
    else
      firstNonFinite(values, "parcel time series") match
        case Some(error) => Left(error)
        case None        => Right(new ParcelTimeSeries(nodeAxis, timeAxis, ConnectivityStorage.copy(values)))

final case class RunTimeSeries(
    runId: RunId,
    series: ParcelTimeSeries,
    frameWeights: Option[FrameWeights] = None,
    nuisance: Option[NuisanceMatrix] = None
):
  frameWeights.foreach { weights =>
    require(weights.timeAxis.sampleCount == series.timeAxis.sampleCount, "frame weights must match run time axis")
  }
  nuisance.foreach { matrix =>
    require(matrix.timeAxis.sampleCount == series.timeAxis.sampleCount, "nuisance matrix must match run time axis")
  }

final class MultiRunTimeSeries private (val runs: Vector[RunTimeSeries]):
  def nodeAxis: NodeAxis =
    runs.head.series.nodeAxis

  def runAxis: RunAxis =
    RunAxis.unsafe(runs.map(_.runId))

  def size: Int =
    runs.length

object MultiRunTimeSeries:
  def from(runs: Iterable[RunTimeSeries]): Either[ConnectivityError, MultiRunTimeSeries] =
    val values = runs.toVector
    if values.isEmpty then Left(ConnectivityError.EmptyAxis("run"))
    else
      val firstAxis = values.head.series.nodeAxis
      val seen = mutable.HashSet.empty[String]
      var index = 0
      var error = Option.empty[ConnectivityError]
      while index < values.length && error.isEmpty do
        val run = values(index)
        if seen.contains(run.runId.value) then error = Some(ConnectivityError.DuplicateId("run", run.runId.value))
        else if !run.series.nodeAxis.sameIdentityAs(firstAxis) ||
            !run.series.nodeAxis.sameScientificBasisAs(firstAxis)
        then
          error = Some(ConnectivityError.AxisMismatch(s"run '${run.runId.value}' node axis does not match first run"))
        else
          seen += run.runId.value
        index += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(new MultiRunTimeSeries(values))

private[connectivity] def firstNonFinite(matrix: DoubleMatrix, role: String): Option[ConnectivityError] =
  var index = 0
  var error = Option.empty[ConnectivityError]
  while index < matrix.dataArray.length && error.isEmpty do
    val value = matrix.dataArray(index)
    if !value.isFinite then error = Some(ConnectivityError.NonFiniteValue(role, index, value))
    index += 1
  error
