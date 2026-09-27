package scalafim.transform

import image4s.{BoundaryPolicy, Continuous, ContinuousImage, SampleSpace}
import image4s.geometry.{D3, Frame, Grid}
import ravel.AnyRank
import reframe4s.core.{ImplementationRevision, InverseEstimate, SpatialMap}
import reframe4s.field.{
  CompositionError,
  CompositionReport,
  CoordinateBoundaryPolicy,
  DenseInverseEvidence,
  DenseMap,
  DeterminantDirection,
  DeterminantField,
  DeterminantValue,
  FieldComposition,
  InversionGates,
  InversionSettings,
  LogDeterminantField,
  NumericalInversion,
  TopologyAssessor
}
import reframe4s.resample.{Interpolation, ModulatedResamplingPlan, ModulationDiagnostics, ResamplingPlan, ResamplingResult, VolumeModulation}

/** A world transform's pullback sampled on a target lattice: an absolute source-coordinate field plus the report of
  * which lattice points every stage covered.
  *
  * `transform` is the field as a [[WorldTransform.Mapped]] without a forward map: the original's forward map inverts
  * the unsampled transform, not this interpolant, so the field must be inverted on its own.
  */
final class MaterializedField[S <: Frame[D3], T <: Frame[D3]] private[transform] (
    val transform: WorldTransform.Mapped[S, T],
    val field: DenseMap[T, S, D3, AnyRank],
    val coverage: CompositionReport[T, D3]
):
  def lattice: Grid[T, D3] = coverage.grid

  /** Estimate the forward map of this field on the source lattice `on`; see [[WorldTransform.Mapped.invertNumerically]].
    * A field whose rejected points were filled by the `fill` policy is refused: fill is not the transform.
    */
  def invertNumerically(on: Grid[S, D3], policy: InversionPolicy): Either[TransformError, WorldTransform.Mapped[S, T]] =
    WarpAlgebra.unfilled(coverage).flatMap(_ => WarpAlgebra.invertNumerically(transform, field, on, policy))

/** The Jacobian determinant of a transform's pullback on a target lattice, with the coverage of the field it was
  * differentiated from. Values are physical (mm³ per mm³): central differences in the interior, one-sided on the
  * lattice boundary. Folded points (`det <= 0`) are masked and counted; the value images hold `0.0` there.
  */
final class JacobianDeterminant[S <: Frame[D3], T <: Frame[D3]] private[transform] (
    val determinant: DeterminantField[T, D3],
    val coverage: CompositionReport[T, D3]
):
  def direction: DeterminantDirection = determinant.direction
  def foldCount: Long = determinant.foldCount
  def logJacobian: LogDeterminantField[T, D3] = determinant.logDeterminant

  /** The determinant at a lattice index, `Folded` where the pullback folds, or `None` outside the lattice. */
  def at(index: Vector[Int]): Option[DeterminantValue] = determinant.at(index)

/** A modulated resampling of a source image onto a target grid, with the determinant range the modulation used. */
final class ModulatedResample[T <: Frame[D3]] private[transform] (
    val result: ResamplingResult[T, D3, Continuous],
    val modulation: VolumeModulation,
    val diagnostics: ModulationDiagnostics
)

/** How a numerical inverse is computed (fixed-point settings) and when it qualifies (gates). Residual gates are in
  * millimetres and apply to points at least `interiorMargin` samples inside their lattice.
  */
final class InversionPolicy private (val settings: InversionSettings, val gates: InversionGates)

object InversionPolicy:
  /** @param minimumCoverage fraction of source-lattice points whose forward image must be found inside the pull's support
    * @param maximumResidual bound on the interior maximum of both `|pull(push(x)) - x|` and `|push(pull(y)) - y|` (mm)
    * @param p99Residual bound on the interior 99th percentile of both residuals (mm)
    */
  def create(
      minimumCoverage: Double,
      maximumResidual: Double,
      p99Residual: Double,
      interiorMargin: Int = 1,
      maximumIterations: Int = 100,
      tolerance: Double = 1e-8,
      divergenceRatio: Double = 4.0
  ): Either[TransformError, InversionPolicy] =
    for
      settings <- InversionSettings.create(maximumIterations, tolerance, divergenceRatio).left.map(TransformError.Inversion(_))
      gates    <- InversionGates.create(minimumCoverage, maximumResidual, p99Residual, interiorMargin).left.map(TransformError.Inversion(_))
    yield InversionPolicy(settings, gates)

/** A numerical inverse that passed every gate of its policy. Only [[WorldTransform.Mapped.invertNumerically]] makes
  * one, and a [[WorldTransform.Mapped]] accepts it only together with `qualifiedPull`, so a
  * [[PushAvailability.Estimated]] forward map always comes with evidence about the pullback it is paired with.
  *
  * `estimate.value` rejects points whose interpolation stencil touches a sample outside the evaluation domain.
  */
final class QualifiedInverse[S <: Frame[D3], T <: Frame[D3]] private[transform] (
    val qualifiedPull: SpatialMap[T, S, D3],
    val estimate: InverseEstimate[S, T, D3],
    val evidence: DenseInverseEvidence[S, D3],
    val samples: DenseMap[S, T, D3, AnyRank],
    val policy: InversionPolicy
)

/** Thin adapters from typed world transforms to the reframe4s field and resampling capabilities. */
private[transform] object WarpAlgebra:
  private val inversionRevision: Either[TransformError, ImplementationRevision] =
    ImplementationRevision
      .parse("reframe4s-field NumericalInversion via scalafim WorldTransform.invertNumerically")
      .left
      .map(error => TransformError.Invalid(error.message))

  def materialize[S <: Frame[D3], T <: Frame[D3]](
      transform: WorldTransform[S, T],
      on: Grid[T, D3],
      fill: CoordinateBoundaryPolicy,
      interpolation: Interpolation[Continuous]
  ): Either[TransformError, MaterializedField[S, T]] =
    FieldComposition
      .materialize(transform.pull, on, fill, interpolation)
      .left
      .map(TransformError.Composition(_))
      .map: composed =>
        val step = TransformProvenance.Step.Derived(s"materialized on a ${on.shape.mkString("x")} lattice (${composed.report.counts.covered} covered)")
        MaterializedField(
          WorldTransform.Mapped(composed.map, PushAvailability.Unavailable(), transform.provenance.andThen(TransformProvenance(Vector(step)))),
          composed.map,
          composed.report
        )

  def invertNumerically[S <: Frame[D3], T <: Frame[D3]](
      transform: WorldTransform.Mapped[S, T],
      pull: DenseMap[T, S, D3, ?],
      on: Grid[S, D3],
      policy: InversionPolicy
  ): Either[TransformError, WorldTransform.Mapped[S, T]] =
    for
      revision <- inversionRevision
      inverse  <- NumericalInversion.invert(pull, on, policy.settings, policy.gates, revision).left.map(TransformError.Inversion(_))
    yield
      val evidence = inverse.evidence
      def worst(summary: Option[reframe4s.field.ResidualSummary]) = summary.fold("none")(s => f"${s.maximum}%.3g")
      val step = TransformProvenance.Step.Derived(
        f"numerical inverse on a ${on.shape.mkString("x")} lattice (coverage ${evidence.coveredFraction}%.4f, " +
          s"max residual forward ${worst(evidence.forwardResidual)} mm, reverse ${worst(evidence.reverseResidual)} mm)"
      )
      WorldTransform.Mapped(
        transform.pull,
        PushAvailability.Estimated(QualifiedInverse(transform.pull, inverse.estimate, evidence, inverse.samples, policy)),
        transform.provenance.andThen(TransformProvenance(Vector(step)))
      )

  /** Right when no lattice point was filled by a `fill` policy (points from a stage's own boundary policy are fine). */
  def unfilled[T <: Frame[D3]](report: CompositionReport[T, D3]): Either[TransformError, Unit] =
    if report.counts.rejected == 0L then Right(()) else Left(TransformError.Composition(CompositionError.RejectedPoints(report)))

  def jacobianDeterminant[S <: Frame[D3], T <: Frame[D3]](
      transform: WorldTransform[S, T],
      on: Grid[T, D3],
      direction: DeterminantDirection
  ): Either[TransformError, JacobianDeterminant[S, T]] =
    for
      materialized <- materialize(transform, on, CoordinateBoundaryPolicy.Reject, Interpolation.Linear)
      determinant  <- TopologyAssessor.determinantField(materialized.field, direction).left.map(TransformError.Determinant(_))
    yield JacobianDeterminant(determinant, materialized.coverage)

  def resample[S <: Frame[D3], T <: Frame[D3], Space <: SampleSpace[S, D3], R <: AnyRank](
      transform: WorldTransform[S, T],
      image: ContinuousImage[Space, Double, R],
      onto: Grid[T, D3],
      interpolation: Interpolation[Continuous],
      boundary: BoundaryPolicy[Double]
  ): Either[TransformError, ResamplingResult[T, D3, Continuous]] =
    for
      plan   <- ResamplingPlan.mapped(image, onto, transform.pull, interpolation, boundary).left.map(TransformError.Resampling(_))
      result <- plan.run(plan.newWorkspace()).left.map(TransformError.Resampling(_))
    yield result

  def resampleModulated[S <: Frame[D3], T <: Frame[D3], Space <: SampleSpace[S, D3], R <: AnyRank](
      transform: WorldTransform[S, T],
      image: ContinuousImage[Space, Double, R],
      onto: Grid[T, D3],
      modulation: VolumeModulation,
      interpolation: Interpolation[Continuous],
      boundary: BoundaryPolicy[Double]
  ): Either[TransformError, ModulatedResample[T]] =
    for
      plan   <- ModulatedResamplingPlan.compile(image, onto, transform.pull, interpolation, modulation, boundary).left.map(TransformError.Resampling(_))
      result <- plan.run(plan.newWorkspace()).left.map(TransformError.Resampling(_))
    yield ModulatedResample(result, modulation, plan.diagnostics)

