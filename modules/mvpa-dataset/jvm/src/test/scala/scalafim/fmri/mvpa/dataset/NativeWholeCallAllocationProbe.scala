package scalafim.fmri.mvpa.dataset

import alder.kernel.DataFingerprint
import com.sun.management.ThreadMXBean
import gale.linalg.{DMat, DVec, DoubleLinearOperator, Matrix, MutableDVec}
import java.lang.management.ManagementFactory
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{DigestAlgorithm, IndexSpace, Injection, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.dataset.predictive.*
import scalafim.fmri.mvpa.measurement.{MeasurementId, MeasurementLeg}
import scalafim.fmri.mvpa.relation.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** JVM-only allocation probe for two complete native MVPA calls. It reports
  * cumulative current-thread allocation, rather than peak allocation or RSS.
  * Run with:
  * `sbt "mvpaDatasetJVM/Test/runMain scalafim.fmri.mvpa.dataset.NativeWholeCallAllocationProbe"`.
  */
object NativeWholeCallAllocationProbe:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def right[A](value: Either[?, A]): A =
    value.fold(error => throw IllegalStateException(error.toString), identity)

  def main(args: Array[String]): Unit =
    allocationBean() match
      case Left(reason) => println(s"allocation_probe,status=refused,reason=$reason")
      case Right(bean) =>
        val warmups = args.headOption.flatMap(_.toIntOption).getOrElse(2)
        val iterations = args.drop(1).headOption.flatMap(_.toIntOption).getOrElse(5)
        require(warmups >= 0 && iterations > 0, "arguments are warmups>=0 iterations>0")
        verifyPredictiveAxisRefusals()
        var index = 0
        while index < warmups do
          ordinary96x64(bean)
          nativePredictive96x64(bean)
          index += 1
        index = 0
        while index < iterations do
          measure(bean, "ordinary_relation_96x64", index)(ordinary96x64(bean))
          measure(bean, "native_predictive_16x4_96x64", index)(nativePredictive96x64(bean))
          index += 1

  private def measure[A <: ProbeResult](bean: ThreadMXBean, phase: String, iteration: Int)(call: => A): Unit =
    val before = bean.getCurrentThreadAllocatedBytes()
    val started = System.nanoTime()
    val result = call
    val elapsed = System.nanoTime() - started
    val allocated = bean.getCurrentThreadAllocatedBytes() - before
    val checksum = result.consumedChecksum
    val retainedHash = result.retained.hashCode()
    val phases = result.phases.map(metric => s"${metric.name}:${metric.elapsedNs}:${metric.allocatedBytes}").mkString(";")
    val jsonPhases = result.phases.map(metric => s"\"${metric.name}\":{\"elapsed_ns\":${metric.elapsedNs},\"cumulative_thread_allocated_bytes\":${metric.allocatedBytes}}").mkString(",")
    val csv = s"allocation_probe,status=measured,phase=$phase,iteration=$iteration,cumulative_thread_allocated_bytes=$allocated,elapsed_ns=$elapsed,consumed_results=${result.consumedResults},consumed_checksum=$checksum,retained_hash=$retainedHash,phase_metrics=$phases,peak_bytes=unmeasured,rss_bytes=unmeasured"
    val json = s"{\"probe\":\"allocation_probe\",\"status\":\"measured\",\"phase\":\"$phase\",\"iteration\":$iteration,\"cumulative_thread_allocated_bytes\":$allocated,\"elapsed_ns\":$elapsed,\"consumed_results\":${result.consumedResults},\"consumed_checksum\":$checksum,\"retained_hash\":$retainedHash,\"phase_metrics\":{$jsonPhases},\"peak_bytes\":\"unmeasured\",\"rss_bytes\":\"unmeasured\"}"
    println(csv)
    println(json)

  /** Entire retained ordinary-relation call: axes, columns, owned dense
    * admission, means, correlation geometry, and result consumption. */
  private def ordinary96x64(bean: ThreadMXBean): OrdinaryResult =
    val identityStarted = phaseStart(bean)
    val samples = axis("native-samples", SpaceRole.Samples, 96)
    val partitions = right(AxisRef.fromStableKeys("native-partitions", SpaceRole.Samples, Vector("run-0", "run-1", "run-2"), "fixture", "unit", "raw"))
    val effects = right(AxisRef.fromStableKeys("native-effects", SpaceRole.Latent, Vector("a", "b", "c", "d"), "fixture", "unit", "raw"))
    val neural = axis("native-neural", SpaceRole.Observed, 64)
    val values = Matrix.tabulate(96, 64): (row, column) =>
      (row / 4).toDouble * 0.1 + (row % 4).toDouble * 0.7 + column.toDouble * 0.01
    val identity = phaseMetric(bean, "identity", identityStarted)
    val preparationStarted = phaseStart(bean)
    val partition = right(Column.fromValues(samples, Vector.tabulate(96)(i => s"run-${i / 32}"), ValueIdentity.source(ValueId.unsafe("native-probe-partitions"))))
    val condition = right(Column.fromValues(samples, Vector.tabulate(96)(i => Vector("a", "b", "c", "d")(i % 4)), ValueIdentity.source(ValueId.unsafe("native-probe-effects"))))
    val plan = right(ObservationMeanPlan(samples, partitions, effects, partition, condition))
    val sourceId = SourceId.unsafe("native-allocation-probe")
    val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("native-allocation-probe-root"), sourceId)))
    val preparation = phaseMetric(bean, "preparation", preparationStarted)
    val queryFitStarted = phaseStart(bean)
    val relations = right(ObservationMeanRelations.fromOwnedDense(plan, neural, values, source, RelationSource("native-acquisition", "native-response", "owned-dense", "none", "none"), "native-probe", ObservationMeanBudget(100000L)))
    val pairing = right(RelationRdm.allDistinctOrdered(partitions))
    val geometry = right(OrdinaryRelationGeometry.compute(relations, pairing.edges.head.left, RdmMethod.Correlation, RelationConsumerBudget(1024, 100000L)))
    val queryFit = phaseMetric(bean, "query_fit", queryFitStarted)
    val resultStarted = phaseStart(bean)
    val consumed = geometry.observed.rdm.values.length
    if consumed <= 0 || !geometry.observed.rdm.values.forall(distance => distance.isFinite && math.abs(distance) <= 1e-12) then
      throw IllegalStateException("ordinary correlation distances must be zero for common-trend row means")
    OrdinaryResult(geometry, math.round(geometry.observed.rdm.values.sum * 1e12), consumed,
      Vector(identity, preparation, queryFit, phaseMetric(bean, "results", resultStarted)))

  /** Full native predictive call: identities, 16 hard-selected measurements,
    * bounded native reads, LORO Swift fits, and consumed result checks. */
  private def nativePredictive96x64(bean: ThreadMXBean): NativePredictiveResult =
    val identityStarted = phaseStart(bean)
    val sampleKeys = Vector.tabulate(96)(i => s"sample-$i")
    val samples = right(AxisRef.fromStableKeys("native-swift-samples", SpaceRole.Samples, sampleKeys, "trial", "none", "one"))
    val neural = axis("native-swift-neural", SpaceRole.Observed, 64)
    val response = right(AxisRef.fromStableKeys("native-swift-target", SpaceRole.Observed, Vector("class-code"), "class", "none", "code"))
    val sourceId = SourceId.unsafe("native-swift-allocation-probe")
    val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("native-swift-allocation-probe-root"), sourceId)))
    val matrix = DMat.tabulate(96, 64): (row, column) =>
      val sign = if row % 2 == 0 then 1.0 else -1.0
      sign * (1.0 + column / 64.0)
    val runs = Array.tabulate(96)(_ / 32)
    val labels = Vector.tabulate(96)(i => (i % 2).toDouble)
    val targets = right(MultiResponse.fromDense(samples, response, DMat.dense(96, 1, labels), ValueIdentity.source(ValueId.unsafe("native-swift-labels")), source))
    val mapping = right(NativeAxisMapping.fromAxis(samples, Vector.tabulate(96)(i => 10000L + i * 7L), DataFingerprint.external("native-swift-96x64-v1")))
    val validation = right(ValidationDesign.bind(samples, right(FixedPartitions.once(right(Labels.retained(IArray.unsafeFromArray(runs))))), ScientificSeed.fromLong(23L)))
    val coding = right(SwiftTargetCoding(Vector(0.0 -> "positive", 1.0 -> "negative")))
    val operator = new DoubleLinearOperator:
      val rows = matrix.rows
      val cols = matrix.cols
      def applyTo(x: DVec, into: MutableDVec): Unit = matrix.applyTo(x, into)
      override def transposeApplyTo(x: DVec, into: MutableDVec): Unit = matrix.transposeApplyTo(x, into)
    val observations = right(Observations.fromOperator(samples, neural, operator, ValueIdentity.source(ValueId.unsafe("native-swift-patterns")), source))
    val identity = phaseMetric(bean, "identity", identityStarted)

    val preparationStarted = phaseStart(bean)
    val legs = Vector.tabulate(16): region =>
      val indices = IArray(region * 4, region * 4 + 1, region * 4 + 2, region * 4 + 3)
      val selection = right(Injection.from(indices, right(IndexSpace.of(neural.size))))
      right(MeasurementLeg.hardSelection(neural, MeasurementId.unsafe(s"native-swift-region-$region"), selection))
    val policy = right(NativeReadPolicy(4, right(MaterializationBudget(200000L))))
    val preparation = phaseMetric(bean, "preparation", preparationStarted)

    val queryFitStarted = phaseStart(bean)
    val results = legs.map: leg =>
      val rows = right(AlderPredictiveAdmission.nativeMeasurement(observations, leg, targets, sampleKeys,
        DataFingerprint.external("native-swift-metadata-v1"), mapping, policy))
      right(AlderSwiftCentroid.crossValidate(rows, validation, coding, FeatureScaling.None))
    val queryFit = phaseMetric(bean, "query_fit", queryFitStarted)

    val resultStarted = phaseStart(bean)
    results.zipWithIndex.foreach: (result, region) =>
      if result.assessment.accuracy != 1.0 || result.rows.map(_.stableKey) != sampleKeys || result.fits.length != 3 then
        throw IllegalStateException("native Swift result did not preserve perfect ordered three-fold LORO classification")
      if result.nativeRead.forall(read => read.inputReturnedCells != 96L * 4L || read.targetReturnedCells != 96L) then
        throw IllegalStateException("native Swift result did not retain its complete bounded native read receipt")
      result.rows.zipWithIndex.foreach: (row, index) =>
        val observedColumn = labels(index).toInt
        val fit = result.fits.find(_.unit == row.assessments.head.unit).getOrElse(
          throw IllegalStateException("native Swift assessment has no corresponding fit")
        )
        val oracle = scalarSwiftProbabilityOracle(index, region, fit, coding.classes(observedColumn))
        if row.predicted != coding.classes(observedColumn) || result.probabilities(index, observedColumn) <= 0.5 || math.abs(result.probabilities(index, observedColumn) - oracle) > 1e-12 then
          throw IllegalStateException("native Swift scalar probability oracle violated class ordering")
    NativePredictiveResult(results, results.length * 96, Vector(identity, preparation, queryFit, phaseMetric(bean, "results", resultStarted)))

  private def axis(name: String, role: SpaceRole, size: Int): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(size)(i => s"$name-$i"), "fixture", "unit", "raw"))

  /** Axis admission refusals are verified once before warm-up and deliberately
    * excluded from the reported whole-call workload. */
  private def verifyPredictiveAxisRefusals(): Unit =
    val keys = Vector.tabulate(96)(i => s"sample-$i")
    val samples = right(AxisRef.fromStableKeys("native-swift-samples", SpaceRole.Samples, keys, "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(samples, Vector.tabulate(96)(i => 10000L + i * 7L), DataFingerprint.external("native-swift-96x64-v1")))
    val foreign = axis("native-swift-foreign", SpaceRole.Samples, 96)
    val reordered = right(AxisRef.fromStableKeys("native-swift-samples", SpaceRole.Samples, keys.reverse, "trial", "none", "one"))
    if NativeAxisMapping.verify(foreign.descriptor, mapping, mapping.declaredSource).isRight ||
        NativeAxisMapping.verify(reordered.descriptor, mapping, mapping.declaredSource).isRight then
      throw IllegalStateException("foreign and reordered native axes must be refused")

  private final case class PhaseStart(nanos: Long, allocatedBytes: Long)
  private final case class PhaseMetric(name: String, elapsedNs: Long, allocatedBytes: Long)
  private def phaseStart(bean: ThreadMXBean): PhaseStart =
    PhaseStart(System.nanoTime(), bean.getCurrentThreadAllocatedBytes())
  private def phaseMetric(bean: ThreadMXBean, name: String, start: PhaseStart): PhaseMetric =
    PhaseMetric(name, System.nanoTime() - start.nanos, bean.getCurrentThreadAllocatedBytes() - start.allocatedBytes)

  /** Independent scalar form of the documented Swift linear-centroid score,
    * evaluated from the fitted centroids and priors without calling a model
    * prediction helper. This probe chooses FeatureScaling.None explicitly. */
  private def scalarSwiftProbabilityOracle(row: Int, region: Int, fit: SwiftFoldFit, observed: ClassLabel): Double =
    val input = Array.tabulate(4): offset =>
      val sign = if row % 2 == 0 then 1.0 else -1.0
      sign * (1.0 + (region * 4 + offset) / 64.0)
    // Every LORO training set is label-balanced and all rows are exactly +/-v.
    // Thus centroids are +/-v, priors are 1/2, and softmax gives sigmoid(2||v||²).
    val squaredNorm = input.iterator.map(value => value * value).sum
    val correct = 1.0 / (1.0 + math.exp(-2.0 * squaredNorm))
    if fit.classes.indexOf(observed) == 0 then
      if row % 2 == 0 then correct else 1.0 - correct
    else if row % 2 == 0 then 1.0 - correct else correct

  private sealed trait ProbeResult:
    def consumedResults: Int
    def retained: AnyRef
    def phases: Vector[PhaseMetric]
    def consumedChecksum: Long
  private final case class OrdinaryResult(retained: AnyRef, checksum: Long, consumedResults: Int, phases: Vector[PhaseMetric]) extends ProbeResult:
    def consumedChecksum: Long = checksum
  private final case class NativePredictiveResult(results: Vector[AlderSwiftCentroidResult], consumedResults: Int, phases: Vector[PhaseMetric]) extends ProbeResult:
    def retained: AnyRef = results
    def consumedChecksum: Long =
      results.iterator.flatMap(_.probabilities.valuesRowMajor).map(value => math.round(value * 1e12)).sum

  private def allocationBean(): Either[String, ThreadMXBean] =
    ManagementFactory.getThreadMXBean match
      case bean: ThreadMXBean if !bean.isThreadAllocatedMemorySupported => Left("thread_allocated_memory_unsupported")
      case bean: ThreadMXBean =>
        if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
        if !bean.isThreadAllocatedMemoryEnabled then Left("thread_allocated_memory_disabled") else Right(bean)
      case _ => Left("com_sun_management_thread_mxbean_unavailable")
