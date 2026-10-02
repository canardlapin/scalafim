package scalafim.fmri.mvpa.spatial

import com.sun.management.ThreadMXBean
import gale.linalg.DMat
import gale.optim.{FirstOrderConfig, FirstOrderTolerance}
import java.lang.management.ManagementFactory
import locus4s.Region
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.{AxisRef, EvidenceSource, MultiResponse, Observations}
import scalafim.fmri.mvpa.analysis.*
import scalafim.fmri.mvpa.measurement.MeasurementId
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Complete adopted preparation -> two-stage fit -> prediction -> native
  * scatter probe. Cumulative caller-thread allocation includes retained
  * outputs, but is not a peak-live, RSS, or other-thread memory measurement. */
object PatternWholeCallAllocationProbe:
  private def right[A](value: Either[?, A]): A =
    value.fold(error => throw IllegalStateException(error.toString), identity)
  private def axis(name: String, role: SpaceRole, size: Int): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(size)(i => s"$name-$i"), "allocation-probe", "one", "raw"))
  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private def values(name: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(name))
  private final case class Phase(name: String, bytes: Long)
  private final case class Result(retained: Vector[AnyRef], checksum: Long, phases: Vector[Phase])

  private def call(bean: ThreadMXBean): Result =
    def allocated = bean.getThreadAllocatedBytes(Thread.currentThread().threadId())
    val start = allocated
    val samples = axis("whole-call-training", SpaceRole.Samples, 8)
    val neural = axis("whole-call-neural", SpaceRole.Observed, 3)
    val target = axis("whole-call-target", SpaceRole.Observed, 1)
    val components = axis("whole-call-component", SpaceRole.Latent, 1)
    val y = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
    val z = Vector(
      Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0),
      Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0),
      Vector(1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0))
    val coefficients = Vector(2.0, -1.5, 1.0)
    val noise = Vector(.4, .3, .2)
    val x = DMat.tabulate(8, 3)((row, col) => coefficients(col) * y(row) + noise(col) * z(col)(row))
    val observations = right(Observations.fromDense(samples, neural, x, values("probe-x"), source("probe-x")))
    val responses = right(MultiResponse.fromDense(samples, target, DMat.dense(8, 1, y), values("probe-y"), source("probe-y")))
    val support = right(SupportGraph(neural.descriptor, Vector.empty, SupportTopology.Declared("isolated"), "one"))
    val unit = right(AxisValues(target, Vector(1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val inner = right(FirstOrderConfig.from(100000, right(FirstOrderTolerance.from(1e-10, 1e-10))))
    val structured = right(StructuredPatternConfig(right(SupportPenalty(0.0, 0.0)), maximumOuterIterations = 250,
      inner = inner, stationarityTolerance = 1e-7, objectiveTolerance = 1e-9, maximumWorkspaceCells = 10000000L))
    val covariance = right(ResidualCovarianceFitPolicy(1, maximumIterations = 5000, tolerance = 1e-9,
      diagonalMomentTolerance = 1e-7, centeringTolerance = 1e-10, convergence = ConvergencePolicy.Refuse))
    val policy = TwoStagePatternFitPolicy(structured, covariance, 10000000L)
    val binding = right(TrainingBinding(samples.descriptor, "whole-call-probe", "whole-call-training-v1"))
    val prepared = allocated
    val fit = right(TwoStagePatternFit.fit(samples, neural, target, components)(observations, responses, support, geometry,
      CenteringPolicy.CenteredBeforeFit("probe-x-centered", "probe-y-centered"), binding, Vector("training-only"),
      PatternReplay.Repeatable("probe-training"), policy,
      TwoStagePatternResources(budget = ResourceBudget(ResourceLimit.OwnedNumeric(100000000L), MaterializationPolicy.AllowSourceCopy(192L)),
        route = ObservationProductRoute.OwnedDenseCopy)))
    val fitted = allocated
    val artifact = fit.finalFit.artifact.getOrElse(throw IllegalStateException("converged fit has no artifact"))
    val prediction = right(PatternPrediction.fromArtifact(neural, target, components, artifact, fit.covarianceFit.covariance))
    val mean = right(prediction.encode(unit))
    // Independent scalar OLS oracle: Walsh residuals are orthogonal to y;
    // unpenalized rank-one forward coefficients are exactly [2,-1.5,1].
    mean.values.zip(coefficients).foreach: (actual, expected) =>
      require(math.abs(actual - expected) <= 1e-6, s"forward oracle: $actual != $expected")
    val served = allocated
    val outcomes = Vector.tabulate(3): column =>
      SpatialLocalOutcome(MeasurementId.unsafe(s"fitted-voxel-$column"), right(Region.fromOrdinals(neural.locus, Vector(column))), 1.0, Right(mean.values(column)))
    val field = right(SpatialMeasurementScatter.scatter(neural.locus, outcomes, SpatialScatterAlgebra.weightedMeanDouble))
    val scattered = allocated
    val checksum = field.toVector.zip(coefficients).map:
      case (SpatialScatterCell.Aggregated(actual, contributors, denominator, failures), expected) =>
        require(math.abs(actual - expected) <= 1e-6 && contributors.size == 1 && math.abs(denominator - 1.0) <= 1e-12 && failures.isEmpty)
        math.round(actual * 1e6)
      case other => throw IllegalStateException(s"scatter oracle: $other")
    require(fit.resources.observationWork.fitCalls == 3L && fit.resources.observationWork.copiedCells == 24L)
    require(fit.resources.admission.wholeNumericBytes.isEmpty && fit.resources.admission.unknownCosts.nonEmpty)
    Result(Vector(fit, prediction, field), checksum.sum, Vector(Phase("preparation", prepared - start),
      Phase("fit", fitted - prepared), Phase("serving", served - fitted), Phase("scatter", scattered - served), Phase("consume-retain", allocated - scattered)))

  def main(args: Array[String]): Unit =
    val bean = ManagementFactory.getThreadMXBean match
      case value: ThreadMXBean if value.isThreadAllocatedMemorySupported => value
      case _ => throw IllegalStateException("caller-thread allocation measurement unavailable")
    if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
    val warmups = args.headOption.flatMap(_.toIntOption).getOrElse(2)
    val iterations = args.drop(1).headOption.flatMap(_.toIntOption).getOrElse(5)
    require(warmups >= 0 && iterations > 0)
    var iteration = 0
    while iteration < warmups do
      call(bean)
      iteration += 1
    iteration = 0
    while iteration < iterations do
      val before = bean.getThreadAllocatedBytes(Thread.currentThread().threadId())
      val result = call(bean)
      val bytes = bean.getThreadAllocatedBytes(Thread.currentThread().threadId()) - before
      require(result.retained.size == 3 && result.checksum == 1500000L)
      val phases = result.phases.map(phase => s"${phase.name}:${phase.bytes}").mkString(";")
      println(s"pattern_whole_call,iteration=$iteration,cumulative_caller_thread_allocated_bytes=$bytes,checksum=${result.checksum},retained_outputs=${result.retained.size},phases=$phases,peak_live_bytes=unmeasured,rss_bytes=unmeasured")
      iteration += 1
