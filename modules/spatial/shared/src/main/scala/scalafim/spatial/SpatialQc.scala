package scalafim.spatial

import gale.linalg.{DMat, DoubleLinearOperator, LinAlgError}
import gale.sparse.{COO, CSR}
import scalafim.image.SpatialPoint

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

  def toSparseTriplets: Either[SpatialError, COO] =
    GaleSpatialSupport.sparseTriplets(
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
    GaleSpatialSupport.sparseTriplets(rows, cols, rowIndices.toArray, colIndices.toArray, values.toArray)
      .left.map(SpatialQc.linearError)
      .map(_ => new TripletFixture(rows, cols, rowIndices, colIndices, values, origin))

/** How far two coordinate routes disagree on a set of probe points, in world millimetres.
  *
  * For a round trip A -> B -> A the second route is the identity; for a commutativity check it is another route
  * between the same domains. Distances are Euclidean between the two routes' pulled-back points.
  */
final case class PathDifference(
  label: String,
  first: Vector[MorphismId],
  second: Vector[MorphismId],
  probes: Int,
  maxDistance: Double,
  rmsDistance: Double,
  usedInverses: Boolean
):
  require(probes > 0, "a path difference needs at least one probe")
  require(maxDistance.isFinite && maxDistance >= 0.0, "max distance must be finite and non-negative")
  require(rmsDistance.isFinite && rmsDistance >= 0.0, "rms distance must be finite and non-negative")

  def check(tolerance: QcTolerance = QcTolerance.default): QcCheck =
    QcCheck(label, tolerance.accepts(maxDistance, 0.0), expected = 0.0, observed = maxDistance, maxAbsError = maxDistance, tolerance)

/** Row sums of an operator: its response to a constant source. Interpolating rows sum to 1, uncovered rows to 0. */
final case class RowSumSummary(
  rows: Int,
  mean: Double,
  min: Double,
  max: Double,
  fractionAboveHalf: Double,
  fractionNearOne: Double
)

/** Sanity of an operator's stored weights: counts of non-finite and negative (below -1e-12) entries. */
final case class WeightSummary(
  entries: Int,
  nonFinite: Int,
  negative: Int,
  min: Option[Double],
  max: Option[Double]
):
  def anyNonFinite: Boolean =
    nonFinite > 0

  def anyNegative: Boolean =
    negative > 0

enum WeightInspection:
  /** The operator's stored entries were inspected. */
  case Stored(summary: WeightSummary)

  /** The operator is matrix-free or composite and exposes no stored entries. */
  case Unavailable(representation: String)

/** neurofunctor `projection_metrics`, over every row rather than a random sample. */
final case class ProjectionMetrics(
  targetRows: Int,
  sourceColumns: Int,
  coverage: Double,
  rowSums: Vector[Double],
  rowSumSummary: RowSumSummary,
  weights: WeightInspection
):
  def nnzPerRow: Option[Double] =
    weights match
      case WeightInspection.Stored(summary) => Some(summary.entries.toDouble / math.max(1, targetRows).toDouble)
      case WeightInspection.Unavailable(_) => None

  /** Largest `|row sum - expected|` over rows that receive any weight (`coveredOnly`) or over every row. */
  def rowSumCheck(
    expected: Double = 1.0,
    tolerance: QcTolerance = QcTolerance.unsafe(absolute = 1e-9, relative = 0.0),
    coveredOnly: Boolean = true
  ): QcCheck =
    val considered = if coveredOnly then rowSums.filter(sum => math.abs(sum) > 1e-12) else rowSums
    val error = considered.foldLeft(0.0)((worst, sum) => math.max(worst, math.abs(sum - expected)))
    QcCheck("row sums", tolerance.accepts(error, 0.0), expected = 0.0, observed = error, maxAbsError = error, tolerance)

  /** Finite and non-negative stored weights. An operator without stored entries fails both, as unverified. */
  def weightChecks: Vector[QcCheck] =
    val (nonFinite, negative) =
      weights match
        case WeightInspection.Stored(summary) => (summary.nonFinite.toDouble, summary.negative.toDouble)
        case WeightInspection.Unavailable(_) => (Double.MaxValue, Double.MaxValue)
    Vector(
      QcCheck("finite weights", nonFinite == 0.0, expected = 0.0, observed = nonFinite, maxAbsError = nonFinite, QcTolerance.default),
      QcCheck("non-negative weights", negative == 0.0, expected = 0.0, observed = negative, maxAbsError = negative, QcTolerance.default)
    )

object SpatialQc:
  def identityLaw(
    operator: SpatialOperator,
    probe: DMat,
    tolerance: QcTolerance = QcTolerance.default
  ): Either[SpatialError, QcCheck] =
    for
      actual <- operator.forward(probe).left.map(linearError)
    yield matrixCheck("identity", actual, probe, tolerance)

  def compositionLaw(
    direct: SpatialOperator,
    composed: SpatialOperator,
    probe: DMat,
    tolerance: QcTolerance = QcTolerance.default
  ): Either[SpatialError, QcCheck] =
    for
      directOut <- direct.forward(probe).left.map(linearError)
      composedOut <- composed.forward(probe).left.map(linearError)
    yield matrixCheck("composition", directOut, composedOut, tolerance)

  def adjointLaw(
    operator: SpatialOperator,
    sourceProbe: DMat,
    targetProbe: DMat,
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
    probe: DMat,
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
      observed <- operatorTriplets(operator)
    yield tripletCheck(observed.toCSR.toTriplets, expected.toCSR.toTriplets, tolerance)

  /** Round trip A -> B -> A: route A to B and B back to A (inverses allowed, forward-first), evaluate the composite
    * pullback at `probes` in A's world, and report how far it moves them.
    */
  def roundTrip(
    graph: SpatialGraph,
    a: DomainId,
    b: DomainId,
    probes: Vector[SpatialPoint],
    policy: RoutingPolicy = RoutingPolicy.Shortest,
    inversePenalty: InversePenalty = InversePenalty.default
  ): Either[SpatialError, PathDifference] =
    for
      there <- graph.path(a, b, policy, allowInverses = true, inversePenalty)
      back <- graph.path(b, a, policy, allowInverses = true, inversePenalty)
      loop <- MorphismPath.build(there.morphisms ++ back.morphisms, there.usedInverses || back.usedInverses)
      result <- difference(
        s"round trip ${a.value}->${b.value}->${a.value}",
        loop,
        Vector.empty,
        probes,
        point => loop.pullback(point),
        point => Right(point)
      )
    yield result

  /** Commutativity of two routes between the same domains: their pullbacks at `probes` in the target's world should
    * agree.
    */
  def commutes(
    first: MorphismPath,
    second: MorphismPath,
    probes: Vector[SpatialPoint]
  ): Either[SpatialError, PathDifference] =
    if first.source != second.source || first.target != second.target then
      Left(SpatialError.PathEndpointsMismatch(first.source, first.target, second.source, second.target))
    else
      difference(
        s"commutativity ${first.source.value}->${first.target.value}",
        first,
        second.ids,
        probes,
        point => first.pullback(point),
        point => second.pullback(point)
      ).map(result => result.copy(usedInverses = first.usedInverses || second.usedInverses))

  /** Compare every alternative simple route from `source` to `target` with the cheapest one. */
  def commutativity(
    graph: SpatialGraph,
    source: DomainId,
    target: DomainId,
    probes: Vector[SpatialPoint],
    policy: RoutingPolicy = RoutingPolicy.Shortest,
    maxPaths: Int = 10,
    allowInverses: Boolean = false
  ): Either[SpatialError, Vector[PathDifference]] =
    graph.allPaths(source, target, policy, maxPaths, allowInverses).flatMap { routes =>
      routes.drop(1).foldLeft[Either[SpatialError, Vector[PathDifference]]](Right(Vector.empty)) { (acc, route) =>
        acc.flatMap(out => commutes(routes.head, route, probes).map(out :+ _))
      }
    }

  /** Row sums and weight sanity for a compiled operator (neurofunctor `projection_metrics`). */
  def projectionMetrics(operator: SpatialOperator): Either[SpatialError, ProjectionMetrics] =
    metrics(operator.map, operator.qc.coverage.fraction, storedWeights(Vector(operator.map)))

  /** Row sums and weight sanity for an assembled hybrid operator; weights are inspected part by part. */
  def projectionMetrics(operator: HybridOperator): Either[SpatialError, ProjectionMetrics] =
    metrics(operator.map, operator.meanPartCoverage, storedWeights(operator.parts.map(_.operator.map)))

  private def metrics(
    map: DoubleLinearOperator,
    coverage: Double,
    weights: WeightInspection
  ): Either[SpatialError, ProjectionMetrics] =
    val ones = GaleSpatialSupport.unsafeOwnedMatrix(map.cols, 1, Array.fill(map.cols)(1.0))
    map.forward(ones).left.map(linearError).map { summed =>
      val rowSums = Vector.tabulate(summed.rows)(row => summed(row, 0))
      ProjectionMetrics(map.rows, map.cols, coverage, rowSums, summarizeRowSums(rowSums), weights)
    }

  private def summarizeRowSums(rowSums: Vector[Double]): RowSumSummary =
    if rowSums.isEmpty then RowSumSummary(0, 0.0, 0.0, 0.0, 0.0, 0.0)
    else
      val n = rowSums.length.toDouble
      RowSumSummary(
        rows = rowSums.length,
        mean = rowSums.sum / n,
        min = rowSums.min,
        max = rowSums.max,
        fractionAboveHalf = rowSums.count(_ > 0.5).toDouble / n,
        fractionNearOne = rowSums.count(sum => math.abs(sum - 1.0) < 0.1).toDouble / n
      )

  private def storedWeights(maps: Vector[DoubleLinearOperator]): WeightInspection =
    val stored = maps.collect { case csr: CSR => csr }
    if stored.length != maps.length then
      WeightInspection.Unavailable(maps.find(!_.isInstanceOf[CSR]).fold("unknown")(_.getClass.getName))
    else
      var entries = 0
      var nonFinite = 0
      var negative = 0
      var min = Double.PositiveInfinity
      var max = Double.NegativeInfinity
      stored.foreach { csr =>
        csr.foreachStoredEntry: (_, _, weight) =>
          entries += 1
          if !weight.isFinite then nonFinite += 1
          else
            if weight < -1e-12 then negative += 1
            min = math.min(min, weight)
            max = math.max(max, weight)
      }
      val finiteSeen = entries > nonFinite
      WeightInspection.Stored(
        WeightSummary(entries, nonFinite, negative, Option.when(finiteSeen)(min), Option.when(finiteSeen)(max))
      )

  private def difference(
    label: String,
    first: MorphismPath,
    second: Vector[MorphismId],
    probes: Vector[SpatialPoint],
    left: SpatialPoint => Either[SpatialError, SpatialPoint],
    right: SpatialPoint => Either[SpatialError, SpatialPoint]
  ): Either[SpatialError, PathDifference] =
    if probes.isEmpty then Left(SpatialError.EmptyQcProbes)
    else
      probes
        .foldLeft[Either[SpatialError, Vector[Double]]](Right(Vector.empty)) { (acc, probe) =>
          for
            distances <- acc
            l <- left(probe)
            r <- right(probe)
            distance = math.sqrt(square(l.x - r.x) + square(l.y - r.y) + square(l.z - r.z))
            _ <-
              if distance.isFinite then Right(())
              else Left(SpatialError.CoordinateTransformFailed(s"$label produced a non-finite point for probe $probe"))
          yield distances :+ distance
        }
        .map { distances =>
          PathDifference(
            label,
            first.ids,
            second,
            probes.length,
            distances.max,
            math.sqrt(distances.map(square).sum / distances.length.toDouble),
            first.usedInverses
          )
        }

  private def square(value: Double): Double =
    value * value

  def report(checks: QcCheck*): QcReport =
    QcReport(checks.toVector)

  private[spatial] def linearError(error: LinAlgError): SpatialError =
    SpatialError.OperatorAssemblyFailed(error.getMessage)

  private def operatorTriplets(operator: SpatialOperator): Either[SpatialError, COO] =
    operator.map match
      case csr: CSR =>
        Right(csr.toTriplets)
      case other =>
        Left(SpatialError.OperatorAssemblyFailed(s"operator map ${other.getClass.getName} cannot be serialized as triplets"))

  private def matrixCheck(
    name: String,
    actual: DMat,
    expected: DMat,
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
    observed: COO,
    expected: COO,
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

  private def maxMatrixAbsDiff(left: DMat, right: DMat): Double =
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

  private def dot(left: DMat, right: DMat): Double =
    require(left.rows == right.rows && left.cols == right.cols, "dot inputs must have matching shape")
    val leftData = left.copyData
    val rightData = right.copyData
    var i = 0
    var out = 0.0
    while i < leftData.length do
      out += leftData(i) * rightData(i)
      i += 1
    out
