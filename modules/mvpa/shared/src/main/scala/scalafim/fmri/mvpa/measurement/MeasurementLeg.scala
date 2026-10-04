package scalafim.fmri.mvpa.measurement

import gale.linalg.{DMat, DoubleLinearOperator, LinearOperator}
import gale.sparse.Sparse
import multivar.core.{CoordinateEvidence, Lin, Primal, SemanticProvenance, SemanticProvenanceEvent, SemanticSpace, Table, ValueId, ValueIdentity}
import resample4s.core.Injection
import scalafim.fmri.mvpa.{AxisRef, Observations, ReindexingLeg}

final class MeasuredObservations[S <: SemanticSpace, L <: SemanticSpace] private[measurement] (
    val samples: multivar.core.SpaceEvidence[S],
    val sampleAxis: scalafim.fmri.mvpa.AxisDescriptor,
    val local: multivar.core.SpaceEvidence[L],
    val localAxis: scalafim.fmri.mvpa.AxisDescriptor,
    val patterns: Table[S, L],
    val source: scalafim.fmri.mvpa.EvidenceSource,
    val origins: scalafim.fmri.mvpa.EvidenceOrigins
)

final class MeasurementLeg[N <: SemanticSpace, SK, L <: SemanticSpace] private[measurement] (
    val source: AxisRef[SK] { type Id = N },
    val local: AxisRef[?] { type Id = L },
    val descriptor: MeasurementDescriptor,
    val leg: Lin[Primal[N], Primal[L]]
):
  def measure[S <: SemanticSpace](observations: Observations[S, N]): Either[MeasurementError, MeasuredObservations[S, L]] =
    if observations.neuralAxis != source.descriptor then
      Left(MeasurementError.SourceMismatch(source.descriptor, observations.neuralAxis))
    else
      val measured = leg.star.andThen(observations.patterns)
      Right(new MeasuredObservations[S, L](observations.samples, observations.sampleAxis, local.evidence, local.descriptor, measured, observations.source,
        observations.origins.reindexOutput(observations.sampleAxis, measured.valueIdentity)))

  /** Compose a measurement with explicitly identified observations and retain
    * the exact sample witness. No raw operator is decoded or evaluated here.
    */
  def measureIdentified[S <: SemanticSpace, TK](
      samples: AxisRef[TK] { type Id = S },
      observations: Observations[S, N]
  ): Either[MeasurementError, Observations[S, L]] =
    if samples.descriptor != observations.sampleAxis then
      Left(MeasurementError.SourceMismatch(samples.descriptor, observations.sampleAxis))
    else if observations.neuralAxis != source.descriptor then
      Left(MeasurementError.SourceMismatch(source.descriptor, observations.neuralAxis))
    else
      val measured = leg.star.andThen(observations.patterns)
      Observations.fromTable(
        samples,
        local,
        measured,
        observations.source,
        observations.origins.reindexOutput(samples.descriptor, measured.valueIdentity)
      ).left.map(error => MeasurementError.Axis(error.message))

object MeasurementLeg:
  def identity[K](source: AxisRef[K], id: MeasurementId, rendition: Vector[(String, String)] = Vector.empty): Either[MeasurementError, MeasurementLeg[source.Id, K, source.Id]] =
    build(source, source, id, MeasurementKind.Identity, Sparse.identity(source.size), "identity", MetricCapability.CoordinateIsometry, MaterializationCost.None, rendition)

  def hardSelection[K](source: AxisRef[K], id: MeasurementId, selection: Injection, rendition: Vector[(String, String)] = Vector.empty): Either[MeasurementError, MeasurementLeg[source.Id, K, ?]] =
    if selection.domain == 0 then Left(MeasurementError.EmptySupport)
    else
      ReindexingLeg.bind(source, selection).left.map(error => MeasurementError.Axis(error.message)).map: restriction =>
        val descriptor = MeasurementDescriptor(
          id, MeasurementKind.HardSelection, source.descriptor, restriction.child.descriptor,
          s"hard:${selection.toVector.mkString(",")}", MetricCapability.CoordinateIsometry,
          MaterializationCost.None, rendition
        )
        new MeasurementLeg[source.Id, K, restriction.Child](source, restriction.child, descriptor, restriction.leg)

  def weightedRegion[K](source: AxisRef[K], id: MeasurementId, support: Seq[(K, Double)], rendition: Vector[(String, String)] = Vector.empty): Either[MeasurementError, MeasurementLeg[source.Id, K, ?]] =
    if support.isEmpty then Left(MeasurementError.EmptySupport)
    else
      val entries = Vector.newBuilder[(Int, Double)]
      val seen = scala.collection.mutable.HashSet.empty[Int]
      val iterator = support.iterator
      while iterator.hasNext do
        val (key, weight) = iterator.next()
        source.ordinalOf(key) match
          case None => return Left(MeasurementError.UnknownSupport)
          case Some(position) =>
            if seen.contains(position) then return Left(MeasurementError.DuplicateSupport(position))
            if !weight.isFinite || weight == 0.0 then return Left(MeasurementError.InvalidWeight(position, weight))
            seen += position
            entries += ((position, weight))
      val ordered = entries.result().sortBy(_._1)
      AxisRef.fromStableKeys(
        s"measurement:${id.value}", source.descriptor.role, Vector(s"weighted:${id.value}"), "weighted-region",
        source.descriptor.units, source.descriptor.scale,
        source.descriptor.lineage :+ s"measurement-weighted:${source.descriptor.coordinateSignature.value}:${id.value}"
      ).left.map(error => MeasurementError.Axis(error.message)).flatMap: local =>
        val operator = LinearOperator.fromFunctions(1, source.size)(
          (input, output) =>
            var total = 0.0
            var index = 0
            while index < ordered.length do
              total += ordered(index)._2 * input(ordered(index)._1)
              index += 1
            output(0) = total,
          (input, output) =>
            output.clear()
            var index = 0
            while index < ordered.length do
              output(ordered(index)._1) = ordered(index)._2 * input(0)
              index += 1
        )
        build(source, local, id, MeasurementKind.WeightedRegion, operator, s"weighted:${ordered.map((index, weight) => s"$index=${java.lang.Double.toHexString(weight)}").mkString(",")}",
          MetricCapability.DeclaredMetricRequired("weighted maps are not coordinate isometries"),
          MaterializationCost.LinearOperatorApplications(1, "weighted local measurement"), rendition)

  def basisMap[K, LK](source: AxisRef[K], local: AxisRef[LK], id: MeasurementId, weights: DMat, metric: MetricCapability, cost: MaterializationCost, rendition: Vector[(String, String)] = Vector.empty): Either[MeasurementError, MeasurementLeg[source.Id, K, local.Id]] =
    if weights.rows != local.size || weights.cols != source.size then
      Left(MeasurementError.ShapeMismatch(local.size, source.size, weights.rows, weights.cols))
    else if !finite(weights) then Left(MeasurementError.Semantic("basis-map weights must be finite"))
    else if metric == MetricCapability.CoordinateIsometry then Left(MeasurementError.UnverifiedMetric)
    else build(source, local, id, MeasurementKind.BasisMap, weights, s"basis:${matrixFingerprint(weights)}", metric, cost, rendition)

  private def build[N <: SemanticSpace, SK, LK](source: AxisRef[SK] { type Id = N }, local: AxisRef[LK], id: MeasurementId, kind: MeasurementKind, operator: DoubleLinearOperator, mapFingerprint: String, metric: MetricCapability, cost: MaterializationCost, rendition: Vector[(String, String)]): Either[MeasurementError, MeasurementLeg[N, SK, local.Id]] =
    val descriptor = MeasurementDescriptor(id, kind, source.descriptor, local.descriptor, mapFingerprint, metric, cost, rendition)
    val value = ValueIdentity.source(ValueId.unsafe(s"measurement-${descriptor.semanticId}"))
    Lin.fromLinearMap(operator, CoordinateEvidence.primal(source.evidence), CoordinateEvidence.primal(local.evidence), value,
      SemanticProvenance.source("scalafim-mvpa-measurement").append(SemanticProvenanceEvent.Derived("measurement-leg", Vector(value))))
      .left.map(error => MeasurementError.Semantic(error.message))
      .map(linear => new MeasurementLeg[N, SK, local.Id](source, local, descriptor, linear))

  private def finite(value: DMat): Boolean =
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        if !value(row, column).isFinite then return false
        column += 1
      row += 1
    true

  private def matrixFingerprint(value: DMat): String =
    val builder = new StringBuilder
    builder.append(value.rows).append('x').append(value.cols).append(':')
    var row = 0
    while row < value.rows do
      var column = 0
      while column < value.cols do
        builder.append(java.lang.Double.toHexString(if value(row, column) == 0.0 then 0.0 else value(row, column))).append(',')
        column += 1
      row += 1
    builder.result()
