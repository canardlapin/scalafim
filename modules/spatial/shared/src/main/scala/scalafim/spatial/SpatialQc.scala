package scalafim.spatial

import scalafim.linalg.{CsrMatrix, DoubleMatrix, LinearMapError, SparseTriplets}

final case class QcTolerance private (absolute: Double, relative: Double):
  require(absolute.isFinite && absolute >= 0.0, "absolute tolerance must be finite and non-negative")
  require(relative.isFinite && relative >= 0.0, "relative tolerance must be finite and non-negative")

  def accepts(actual: Double, expected: Double): Boolean =
    val scale = math.max(1.0, math.max(math.abs(actual), math.abs(expected)))
    math.abs(actual - expected) <= absolute + relative * scale

object QcTolerance:
  val default: QcTolerance =
    unsafe(absolute = 1e-10, relative = 1e-10)

  def apply(absolute: Double, relative: Double = 0.0): Either[SpatialError, QcTolerance] =
    if !absolute.isFinite || absolute < 0.0 then
      Left(SpatialError.OperatorAssemblyFailed(s"invalid absolute tolerance $absolute"))
    else if !relative.isFinite || relative < 0.0 then
      Left(SpatialError.OperatorAssemblyFailed(s"invalid relative tolerance $relative"))
    else Right(new QcTolerance(absolute, relative))

  private[scalafim] def unsafe(absolute: Double, relative: Double): QcTolerance =
    new QcTolerance(absolute, relative)

final case class QcCheck(
  name: String,
  passed: Boolean,
  expected: Double,
  observed: Double,
  maxAbsError: Double,
  tolerance: QcTolerance
):
  require(name.trim.nonEmpty, "QC check name must be non-empty")
  require(expected.isFinite, "expected value must be finite")
  require(observed.isFinite, "observed value must be finite")
  require(maxAbsError.isFinite && maxAbsError >= 0.0, "max absolute error must be finite and non-negative")

final case class QcReport(checks: Vector[QcCheck]):
  def passed: Boolean =
    checks.forall(_.passed)

  def failed: Vector[QcCheck] =
    checks.filterNot(_.passed)

object QcReport:
  def apply(first: QcCheck, rest: QcCheck*): QcReport =
    QcReport(first +: rest.toVector)

final case class TripletFixture private (
  rows: Int,
  cols: Int,
  rowIndices: Vector[Int],
  colIndices: Vector[Int],
  values: Vector[Double],
  origin: TripletIndexOrigin
):
  require(rows >= 0 && cols >= 0, "fixture dimensions must be non-negative")
  require(rowIndices.length == colIndices.length && rowIndices.length == values.length, "fixture triplet lengths must match")

  def toSparseTriplets: Either[SpatialError, SparseTriplets] =
    SparseTriplets(
      rows = rows,
      cols = cols,
      rowIndices = rowIndices.toArray,
      colIndices = colIndices.toArray,
      values = values.toArray
    ).left.map(SpatialQc.linearError)

enum TripletIndexOrigin:
  case ZeroBased, OneBased

object TripletFixture:
  def zeroBased(
    rows: Int,
    cols: Int,
    rowIndices: Vector[Int],
    colIndices: Vector[Int],
    values: Vector[Double]
  ): Either[SpatialError, TripletFixture] =
    build(rows, cols, rowIndices, colIndices, values, TripletIndexOrigin.ZeroBased)

  def oneBased(
    rows: Int,
    cols: Int,
    rowIndices: Vector[Int],
    colIndices: Vector[Int],
    values: Vector[Double]
  ): Either[SpatialError, TripletFixture] =
    build(
      rows,
      cols,
      rowIndices.map(_ - 1),
      colIndices.map(_ - 1),
      values,
      TripletIndexOrigin.OneBased
    )

  private def build(
    rows: Int,
    cols: Int,
    rowIndices: Vector[Int],
    colIndices: Vector[Int],
    values: Vector[Double],
    origin: TripletIndexOrigin
  ): Either[SpatialError, TripletFixture] =
    SparseTriplets(rows, cols, rowIndices.toArray, colIndices.toArray, values.toArray)
      .left.map(SpatialQc.linearError)
      .map(_ => new TripletFixture(rows, cols, rowIndices, colIndices, values, origin))

object SpatialQc:
  def identityLaw(
    operator: SpatialOperator,
    probe: DoubleMatrix,
    tolerance: QcTolerance = QcTolerance.default
  ): Either[SpatialError, QcCheck] =
    for
      actual <- operator.forward(probe).left.map(linearError)
    yield matrixCheck("identity", actual, probe, tolerance)

  def compositionLaw(
    direct: SpatialOperator,
    composed: SpatialOperator,
    probe: DoubleMatrix,
    tolerance: QcTolerance = QcTolerance.default
  ): Either[SpatialError, QcCheck] =
    for
      directOut <- direct.forward(probe).left.map(linearError)
      composedOut <- composed.forward(probe).left.map(linearError)
    yield matrixCheck("composition", directOut, composedOut, tolerance)

  def adjointLaw(
    operator: SpatialOperator,
    sourceProbe: DoubleMatrix,
    targetProbe: DoubleMatrix,
    tolerance: QcTolerance = QcTolerance.default
  ): Either[SpatialError, QcCheck] =
    for
      px <- operator.map.forward(sourceProbe).left.map(linearError)
      pty <- operator.map.adjoint.forward(targetProbe).left.map(linearError)
    yield
      val left = dot(px, targetProbe)
      val right = dot(sourceProbe, pty)
      scalarCheck("adjoint", left, right, tolerance)

  def roiRestrictionLaw(
    full: SpatialOperator,
    restricted: SpatialOperator,
    roiRows: Vector[Int],
    probe: DoubleMatrix,
    tolerance: QcTolerance = QcTolerance.default
  ): Either[SpatialError, QcCheck] =
    for
      fullOut <- full.forward(probe).left.map(linearError)
      restrictedOut <- restricted.forward(probe).left.map(linearError)
    yield
      val expected = fullOut.selectRows(roiRows)
      matrixCheck("roi restriction", restrictedOut, expected, tolerance)

  def coverageLaw(
    operator: SpatialOperator,
    expectedCoverage: Vector[Double],
    tolerance: QcTolerance = QcTolerance.default
  ): QcCheck =
    vectorCheck("coverage", operator.qc.coverage.rowCoverage, expectedCoverage, tolerance)

  def tripletFixtureLaw(
    operator: SpatialOperator,
    fixture: TripletFixture,
    tolerance: QcTolerance = QcTolerance.default
  ): Either[SpatialError, QcCheck] =
    for
      expected <- fixture.toSparseTriplets
      expectedCsr <- CsrMatrix.fromTriplets(expected).left.map(linearError)
      observed <- operatorTriplets(operator)
      observedCsr <- CsrMatrix.fromTriplets(observed).left.map(linearError)
    yield tripletCheck(observedCsr.toTriplets, expectedCsr.toTriplets, tolerance)

  def report(checks: QcCheck*): QcReport =
    QcReport(checks.toVector)

  private[spatial] def linearError(error: LinearMapError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.message)

  private def operatorTriplets(operator: SpatialOperator): Either[SpatialError, SparseTriplets] =
    operator.map match
      case csr: CsrMatrix =>
        Right(csr.toTriplets)
      case other =>
        Left(SpatialError.OperatorAssemblyFailed(s"operator map ${other.getClass.getName} cannot be serialized as triplets"))

  private def matrixCheck(
    name: String,
    actual: DoubleMatrix,
    expected: DoubleMatrix,
    tolerance: QcTolerance
  ): QcCheck =
    if actual.rows != expected.rows || actual.cols != expected.cols then
      QcCheck(name, passed = false, expected = 0.0, observed = Double.MaxValue, maxAbsError = Double.MaxValue, tolerance)
    else
      val error = maxMatrixAbsDiff(actual, expected)
      QcCheck(name, tolerance.accepts(error, 0.0), expected = 0.0, observed = error, maxAbsError = error, tolerance)

  private def vectorCheck(
    name: String,
    actual: Vector[Double],
    expected: Vector[Double],
    tolerance: QcTolerance
  ): QcCheck =
    if actual.length != expected.length then
      QcCheck(name, passed = false, expected = 0.0, observed = Double.MaxValue, maxAbsError = Double.MaxValue, tolerance)
    else
      var i = 0
      var error = 0.0
      while i < actual.length do
        error = math.max(error, math.abs(actual(i) - expected(i)))
        i += 1
      QcCheck(name, tolerance.accepts(error, 0.0), expected = 0.0, observed = error, maxAbsError = error, tolerance)

  private def scalarCheck(
    name: String,
    actual: Double,
    expected: Double,
    tolerance: QcTolerance
  ): QcCheck =
    val error = math.abs(actual - expected)
    QcCheck(name, tolerance.accepts(actual, expected), expected = expected, observed = actual, maxAbsError = error, tolerance)

  private def tripletCheck(
    observed: SparseTriplets,
    expected: SparseTriplets,
    tolerance: QcTolerance
  ): QcCheck =
    val sameShape = observed.rows == expected.rows && observed.cols == expected.cols && observed.nnz == expected.nnz
    val sameRows = observed.rowIndices.toVector == expected.rowIndices.toVector
    val sameCols = observed.colIndices.toVector == expected.colIndices.toVector
    val valueError =
      if !sameShape then Double.MaxValue
      else maxVectorAbsDiff(observed.values.toVector, expected.values.toVector)
    QcCheck(
      name = "triplet fixture",
      passed = sameShape && sameRows && sameCols && tolerance.accepts(valueError, 0.0),
      expected = 0.0,
      observed = valueError,
      maxAbsError = valueError,
      tolerance = tolerance
    )

  private def maxMatrixAbsDiff(left: DoubleMatrix, right: DoubleMatrix): Double =
    val leftData = left.copyData
    val rightData = right.copyData
    maxArrayAbsDiff(leftData, rightData)

  private def maxVectorAbsDiff(left: Vector[Double], right: Vector[Double]): Double =
    var i = 0
    var out = 0.0
    while i < left.length do
      out = math.max(out, math.abs(left(i) - right(i)))
      i += 1
    out

  private def maxArrayAbsDiff(left: Array[Double], right: Array[Double]): Double =
    var i = 0
    var out = 0.0
    while i < left.length do
      out = math.max(out, math.abs(left(i) - right(i)))
      i += 1
    out

  private def dot(left: DoubleMatrix, right: DoubleMatrix): Double =
    require(left.rows == right.rows && left.cols == right.cols, "dot inputs must have matching shape")
    val leftData = left.copyData
    val rightData = right.copyData
    var i = 0
    var out = 0.0
    while i < leftData.length do
      out += leftData(i) * rightData(i)
      i += 1
    out
