package scalafim.connectivity

import scala.collection.mutable

import gale.linalg.{DMat, Matrix}

final class ConnectivityMatrix private (
    val edgeSpace: EdgeSpace,
    val values: DMat,
    val measure: ConnectivityMeasure,
    val diagonalPolicy: DiagonalPolicy
):
  def topology: EdgeTopology =
    edgeSpace.topology

  def edgeVector: Either[ConnectivityError, EdgeVector] =
    EdgeVectorizer.fromMatrix(values, edgeSpace)

object ConnectivityMatrix:
  def from(
      values: DMat,
      edgeSpace: EdgeSpace,
      symmetryTolerance: Double = 1e-12,
      measure: ConnectivityMeasure = ConnectivityMeasure.edgeWeight,
      diagonalPolicy: DiagonalPolicy = DiagonalPolicy.Observed
  ): Either[ConnectivityError, ConnectivityMatrix] =
    val resolvedDiagonal =
      if edgeSpace.topology == EdgeTopology.Rectangular then DiagonalPolicy.NotApplicable else diagonalPolicy
    if values.rows != edgeSpace.rows || values.cols != edgeSpace.cols then
      Left(ConnectivityError.MatrixShapeMismatch(s"connectivity matrix expected ${edgeSpace.rows}x${edgeSpace.cols}, got ${values.rows}x${values.cols}"))
    else if edgeSpace.isSquare && values.rows != values.cols then
      Left(ConnectivityError.MatrixShapeMismatch(s"${edgeSpace.topology.label} connectivity matrix must be square, got ${values.rows}x${values.cols}"))
    else if edgeSpace.topology == EdgeTopology.Rectangular && !measure.supportsRectangular then
      Left(ConnectivityError.InvalidPlan(s"measure '${measure.id}' does not support rectangular edge spaces"))
    else if edgeSpace.topology == EdgeTopology.Undirected && !measure.symmetric then
      Left(ConnectivityError.InvalidPlan(s"measure '${measure.id}' is not declared symmetric"))
    else if !symmetryTolerance.isFinite || symmetryTolerance < 0.0 then
      Left(ConnectivityError.InvalidScalar("symmetry tolerance", symmetryTolerance, "must be finite and non-negative"))
    else
      firstNonFinite(values, "connectivity matrix") match
        case Some(error) => Left(error)
        case None =>
          val diagonalError =
            validateDiagonal(values, resolvedDiagonal, symmetryTolerance)
          diagonalError match
            case Some(error) => Left(error)
            case None =>
              if edgeSpace.topology == EdgeTopology.Undirected then
                firstAsymmetry(values, symmetryTolerance) match
                  case Some(error) => Left(error)
                  case None        => Right(new ConnectivityMatrix(edgeSpace, ConnectivityStorage.copy(values), measure, resolvedDiagonal))
              else Right(new ConnectivityMatrix(edgeSpace, ConnectivityStorage.copy(values), measure, resolvedDiagonal))

  def fromEdgeVector(
      vector: EdgeVector,
      measure: ConnectivityMeasure = ConnectivityMeasure.edgeWeight,
      diagonalPolicy: DiagonalPolicy = DiagonalPolicy.StructuralZero
  ): Either[ConnectivityError, ConnectivityMatrix] =
    from(
      EdgeVectorizer.toMatrix(vector, diagonalPolicy),
      vector.space,
      measure = measure,
      diagonalPolicy = diagonalPolicy
    )

  private def firstAsymmetry(matrix: DMat, tolerance: Double): Option[ConnectivityError] =
    var row = 0
    var error = Option.empty[ConnectivityError]
    while row < matrix.rows && error.isEmpty do
      var col = row + 1
      while col < matrix.cols && error.isEmpty do
        val left = matrix(row, col)
        val right = matrix(col, row)
        if Math.abs(left - right) > tolerance then
          error = Some(ConnectivityError.NonSymmetricMatrix(row, col, left, right))
        col += 1
      row += 1
    error

  private def validateDiagonal(
      matrix: DMat,
      diagonalPolicy: DiagonalPolicy,
      tolerance: Double
  ): Option[ConnectivityError] =
    diagonalPolicy.materializedValue match
      case None => None
      case Some(expected) =>
        var row = 0
        var error = Option.empty[ConnectivityError]
        while row < matrix.rows && error.isEmpty do
          val value = matrix(row, row)
          if Math.abs(value - expected) > tolerance then
            error = Some(
              ConnectivityError.InvalidPlan(
                s"diagonal policy '${diagonalPolicy.label}' expected $expected at ($row,$row), got $value"
              )
            )
          row += 1
        error

final case class StaticConnectivity(
    matrix: ConnectivityMatrix,
    estimator: Option[EstimatorSpec] = None,
    diagnostics: ConnectivityDiagnostics = ConnectivityDiagnostics.empty
):
  def edgeSpace: EdgeSpace =
    matrix.edgeSpace

final case class DynamicSlice(
    window: TimeWindow,
    connectivity: StaticConnectivity
):
  def index: SampleIndex =
    window.start

object DynamicSlice:
  def at(
      index: SampleIndex,
      connectivity: StaticConnectivity,
      length: Int = 1
  ): Either[ConnectivityError, DynamicSlice] =
    TimeWindow.from(index, length).map(DynamicSlice(_, connectivity))

  def unsafe(index: Int, connectivity: StaticConnectivity, length: Int = 1): DynamicSlice =
    at(SampleIndex.unsafe(index), connectivity, length).fold(error => throw new IllegalArgumentException(error.message), identity)

final class DynamicConnectivity private (
    val slices: Vector[DynamicSlice],
    val windowAxis: WindowAxis,
    val edgeSpace: EdgeSpace
):
  def size: Int =
    slices.length

object DynamicConnectivity:
  def from(slices: Iterable[DynamicSlice]): Either[ConnectivityError, DynamicConnectivity] =
    val values = slices.toVector
    if values.isEmpty then Left(ConnectivityError.InvalidDimension("dynamic slice count", 0))
    else
      val space = values.head.connectivity.edgeSpace
      WindowAxis.from(values.map(_.window)) match
        case Left(value) => Left(value)
        case Right(windowAxis) =>
          var i = 0
          var error = Option.empty[ConnectivityError]
          while i < values.length && error.isEmpty do
            val slice = values(i)
            if !slice.connectivity.edgeSpace.sameOrderingAs(space) then
              error = Some(ConnectivityError.IncompatibleEdgeSpace(s"dynamic slice $i edge space does not match first slice"))
            i += 1
          error match
            case Some(value) => Left(value)
            case None        => Right(new DynamicConnectivity(values, windowAxis, space))

final case class SubjectConnectivity(
    subjectId: SubjectId,
    connectivity: StaticConnectivity
)

final class ConnectivitySet private (
    val subjects: Vector[SubjectConnectivity],
    val covariates: Vector[SubjectCovariates],
    val edgeSpace: EdgeSpace
):
  def subjectCount: Int =
    subjects.length

  def edgeCount: Int =
    edgeSpace.size

  def covariatesBySubject: Map[SubjectId, SubjectCovariates] =
    covariates.map(row => row.subjectId -> row).toMap

object ConnectivitySet:
  def from(
      subjects: Iterable[SubjectConnectivity],
      covariates: Iterable[SubjectCovariates] = Vector.empty
  ): Either[ConnectivityError, ConnectivitySet] =
    val subjectVector = subjects.toVector
    val covariateVector = covariates.toVector
    if subjectVector.isEmpty then Left(ConnectivityError.InvalidDimension("subject count", 0))
    else if covariateVector.nonEmpty && subjectVector.length != covariateVector.length then
      Left(ConnectivityError.LabelCountMismatch("subject covariate row", subjectVector.length, covariateVector.length))
    else
      val space = subjectVector.head.connectivity.edgeSpace
      val seen = mutable.HashSet.empty[String]
      var index = 0
      var error = Option.empty[ConnectivityError]
      while index < subjectVector.length && error.isEmpty do
        val subject = subjectVector(index)
        if seen.contains(subject.subjectId.value) then
          error = Some(ConnectivityError.DuplicateId("subject", subject.subjectId.value))
        else if !subject.connectivity.edgeSpace.sameOrderingAs(space) then
          error = Some(ConnectivityError.IncompatibleEdgeSpace(s"subject '${subject.subjectId.value}' edge space does not match first subject"))
        else
          seen += subject.subjectId.value
        index += 1

      if error.isEmpty && covariateVector.nonEmpty then
        val covariateSubjects = mutable.HashSet.empty[String]
        var covIndex = 0
        while covIndex < covariateVector.length && error.isEmpty do
          val row = covariateVector(covIndex)
          val subject = row.subjectId.value
          if covariateSubjects.contains(subject) then
            error = Some(ConnectivityError.DuplicateId("subject covariate", subject))
          else if !seen.contains(subject) then
            error = Some(ConnectivityError.AxisMismatch(s"covariates reference unknown subject '$subject'"))
          else covariateSubjects += subject
          covIndex += 1
        if error.isEmpty && covariateSubjects.size != seen.size then
          error = Some(ConnectivityError.AxisMismatch("covariates must contain exactly one row for each subject when present"))

      error match
        case Some(value) => Left(value)
        case None        => Right(new ConnectivitySet(subjectVector, covariateVector, space))
