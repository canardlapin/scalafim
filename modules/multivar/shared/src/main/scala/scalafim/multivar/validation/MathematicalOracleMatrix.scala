package scalafim.multivar
package validation

import scalafim.multivar.core.*
import scalafim.multivar.contract.*

/** Executable release contract for mathematical oracle coverage.
  *
  * An oracle case binds a model contract to the independent evidence, laws,
  * tolerances, mutation probes, and execution tier that are required to test
  * one implementation path. Passing tests are evidence only for the named
  * case; they do not promote the mathematical model contract by themselves.
  */
opaque type OracleCaseId = String

object OracleCaseId:
  def from(value: String): Either[MathematicalOracleError, OracleCaseId] =
    Identifier
      .validate("oracle case id", value)
      .left
      .map(error => MathematicalOracleError.InvalidDefinition(error.message))

  private[multivar] def unsafe(value: String): OracleCaseId =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: OracleCaseId)
    inline def value: String = id

opaque type OracleSeed = Long

object OracleSeed:
  def from(value: Long): Either[MathematicalOracleError, OracleSeed] =
    if value >= 0L then Right(value)
    else Left(MathematicalOracleError.InvalidDefinition(s"oracle seed must be non-negative, got $value"))

  private[multivar] def unsafe(value: Long): OracleSeed =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (seed: OracleSeed)
    inline def value: Long = seed

final case class ConditioningAwareTolerance private (
    absolute: Double,
    relative: Double,
    conditionEstimate: Double
):
  def threshold(scale: Double): Double =
    absolute + relative * Math.max(1.0, Math.abs(scale)) * conditionEstimate

object ConditioningAwareTolerance:
  def from(
      absolute: Double,
      relative: Double,
      conditionEstimate: Double
  ): Either[MathematicalOracleError, ConditioningAwareTolerance] =
    if !absolute.isFinite || absolute < 0.0 then
      Left(MathematicalOracleError.InvalidDefinition(s"absolute tolerance must be finite and non-negative, got $absolute"))
    else if !relative.isFinite || relative < 0.0 then
      Left(MathematicalOracleError.InvalidDefinition(s"relative tolerance must be finite and non-negative, got $relative"))
    else if !conditionEstimate.isFinite || conditionEstimate < 1.0 then
      Left(
        MathematicalOracleError.InvalidDefinition(
          s"condition estimate must be finite and at least one, got $conditionEstimate"
        )
      )
    else Right(ConditioningAwareTolerance(absolute, relative, conditionEstimate))

  private[multivar] def unsafe(
      absolute: Double,
      relative: Double,
      conditionEstimate: Double
  ): ConditioningAwareTolerance =
    from(absolute, relative, conditionEstimate)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

enum AnalyticOracleFixture:
  case DiagonalSpectrum
  case Laplacian2x2
  case SoftThreshold
  case TinyFusedLasso
  case SingularNullspace

object AnalyticOracleFixture:
  val releaseRequired: Set[AnalyticOracleFixture] =
    Set(DiagonalSpectrum, Laplacian2x2, SoftThreshold, TinyFusedLasso, SingularNullspace)

enum DifferentialOracle:
  case IndependentConvexReference
  case PublishedAllenGmdLimit
  case PublishedGlrmLimit
  case LowRankModelsJl

  def independentlyDecisive: Boolean =
    this match
      case LowRankModelsJl => false
      case IndependentConvexReference | PublishedAllenGmdLimit | PublishedGlrmLimit => true

enum MetamorphicOracleLaw:
  case Permutation
  case Relabeling
  case CommonScaling
  case CoordinateChange
  case ZeroPenaltyLimit
  case LargePenaltyLimit
  case SparseDenseEquivalence

object MetamorphicOracleLaw:
  val releaseRequired: Set[MetamorphicOracleLaw] =
    Set(Permutation, Relabeling, CommonScaling, CoordinateChange, ZeroPenaltyLimit, LargePenaltyLimit, SparseDenseEquivalence)

enum MutationTarget:
  case Adjoint
  case OperatorNormBound
  case ProximalLaw
  case ObservationMask
  case FoldProvenance

object MutationTarget:
  val releaseRequired: Set[MutationTarget] =
    Set(Adjoint, OperatorNormBound, ProximalLaw, ObservationMask, FoldProvenance)

enum TrajectoryLaw:
  case FinalValueOnly
  case MonotoneObjective
  case SufficientDecrease(modulus: Double)
  case ResidualConvergence

enum OracleExecutionTier:
  case PullRequestFast
  case Reference
  case NightlyStress

object OracleExecutionTier:
  val all: Set[OracleExecutionTier] = Set(PullRequestFast, Reference, NightlyStress)

enum RepresentationLaw:
  case SharedJvmAndScalaJs
  case SparseStoragePreserved
  case NoHiddenDensification
  case AllocationControlledKernel
  case ConditioningSweep

final class MathematicalOracleCase private (
    val id: OracleCaseId,
    val contract: MathematicalModelContract,
    val implementationPath: String,
    val evidenceLocation: String,
    val analyticFixtures: Set[AnalyticOracleFixture],
    val differentialOracles: Set[DifferentialOracle],
    val metamorphicLaws: Set[MetamorphicOracleLaw],
    val mutationTargets: Set[MutationTarget],
    val trajectoryLaws: Set[TrajectoryLaw],
    val representationLaws: Set[RepresentationLaw],
    val tier: OracleExecutionTier,
    val seed: OracleSeed,
    val tolerance: ConditioningAwareTolerance
)

object MathematicalOracleCase:
  def from(
      id: OracleCaseId,
      contract: MathematicalModelContract,
      implementationPath: String,
      evidenceLocation: String,
      analyticFixtures: Set[AnalyticOracleFixture],
      differentialOracles: Set[DifferentialOracle],
      metamorphicLaws: Set[MetamorphicOracleLaw],
      mutationTargets: Set[MutationTarget],
      trajectoryLaws: Set[TrajectoryLaw],
      representationLaws: Set[RepresentationLaw],
      tier: OracleExecutionTier,
      seed: OracleSeed,
      tolerance: ConditioningAwareTolerance
  ): Either[MathematicalOracleError, MathematicalOracleCase] =
    val cleanPath = implementationPath.trim
    val cleanEvidence = evidenceLocation.trim
    if cleanPath.isEmpty then Left(MathematicalOracleError.InvalidDefinition("implementation path must be non-empty"))
    else if cleanEvidence.isEmpty then Left(MathematicalOracleError.InvalidDefinition("evidence location must be non-empty"))
    else if analyticFixtures.isEmpty && differentialOracles.isEmpty && metamorphicLaws.isEmpty then
      Left(MathematicalOracleError.InvalidDefinition(s"oracle case '${id.value}' has no independent evidence"))
    else if differentialOracles.contains(DifferentialOracle.LowRankModelsJl) &&
        !differentialOracles.exists(_.independentlyDecisive)
    then Left(MathematicalOracleError.LowRankModelsIsSoleOracle(id))
    else if !representationLaws.contains(RepresentationLaw.SharedJvmAndScalaJs) then
      Left(MathematicalOracleError.MissingCrossPlatformLaw(id))
    else
      Right(
        new MathematicalOracleCase(
          id,
          contract,
          cleanPath,
          cleanEvidence,
          analyticFixtures,
          differentialOracles,
          metamorphicLaws,
          mutationTargets,
          trajectoryLaws,
          representationLaws,
          tier,
          seed,
          tolerance
        )
      )

final class MathematicalOracleMatrix private (
    val cases: Vector[MathematicalOracleCase]
):
  val byContract: Map[MathematicalModelFamily, Vector[MathematicalOracleCase]] =
    cases.groupBy(_.contract.family)

  val byTier: Map[OracleExecutionTier, Vector[MathematicalOracleCase]] =
    cases.groupBy(_.tier)

object MathematicalOracleMatrix:
  def from(cases: Vector[MathematicalOracleCase]): Either[MathematicalOracleError, MathematicalOracleMatrix] =
    if cases.isEmpty then Left(MathematicalOracleError.InvalidDefinition("oracle matrix must contain at least one case"))
    else
      duplicate(cases.map(_.id.value)) match
        case Some(id) => Left(MathematicalOracleError.DuplicateCase(id))
        case None =>
          val missingContracts = MathematicalContractCatalog.all.map(_.family).toSet.diff(cases.map(_.contract.family).toSet)
          val missingFixtures = AnalyticOracleFixture.releaseRequired.diff(cases.flatMap(_.analyticFixtures).toSet)
          val missingLaws = MetamorphicOracleLaw.releaseRequired.diff(cases.flatMap(_.metamorphicLaws).toSet)
          val missingMutations = MutationTarget.releaseRequired.diff(cases.flatMap(_.mutationTargets).toSet)
          val missingTiers = OracleExecutionTier.all.diff(cases.map(_.tier).toSet)
          if missingContracts.nonEmpty then Left(MathematicalOracleError.MissingContracts(missingContracts))
          else if missingFixtures.nonEmpty then Left(MathematicalOracleError.MissingAnalyticFixtures(missingFixtures))
          else if missingLaws.nonEmpty then Left(MathematicalOracleError.MissingMetamorphicLaws(missingLaws))
          else if missingMutations.nonEmpty then Left(MathematicalOracleError.MissingMutationTargets(missingMutations))
          else if missingTiers.nonEmpty then Left(MathematicalOracleError.MissingExecutionTiers(missingTiers))
          else Right(new MathematicalOracleMatrix(cases))

  private def duplicate(values: Vector[String]): Option[String] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    values.find(value => !seen.add(value))

final case class TrajectoryPoint private (
    iteration: Int,
    objective: Double,
    residual: Double,
    stepNorm: Double
)

object TrajectoryPoint:
  def from(
      iteration: Int,
      objective: Double,
      residual: Double,
      stepNorm: Double
  ): Either[MathematicalOracleError, TrajectoryPoint] =
    if iteration < 0 then Left(MathematicalOracleError.InvalidDefinition(s"trajectory iteration must be non-negative, got $iteration"))
    else if !objective.isFinite then Left(MathematicalOracleError.InvalidDefinition("trajectory objective must be finite"))
    else if !residual.isFinite || residual < 0.0 then
      Left(MathematicalOracleError.InvalidDefinition(s"trajectory residual must be finite and non-negative, got $residual"))
    else if !stepNorm.isFinite || stepNorm < 0.0 then
      Left(MathematicalOracleError.InvalidDefinition(s"trajectory step norm must be finite and non-negative, got $stepNorm"))
    else Right(TrajectoryPoint(iteration, objective, residual, stepNorm))

object MathematicalOracleSentinel:
  def adjoint(
      forwardPairing: Double,
      adjointPairing: Double,
      tolerance: ConditioningAwareTolerance
  ): Either[MathematicalOracleError, Unit] =
    val scale = Math.max(Math.abs(forwardPairing), Math.abs(adjointPairing))
    val residual = Math.abs(forwardPairing - adjointPairing)
    if residual <= tolerance.threshold(scale) then Right(())
    else Left(MathematicalOracleError.MutationDetected(MutationTarget.Adjoint, residual))

  def operatorNormBound(
      observedNorm: Double,
      claimedUpperBound: Double,
      tolerance: ConditioningAwareTolerance
  ): Either[MathematicalOracleError, Unit] =
    if !observedNorm.isFinite || observedNorm < 0.0 || !claimedUpperBound.isFinite || claimedUpperBound < 0.0 then
      Left(MathematicalOracleError.InvalidDefinition("operator norms must be finite and non-negative"))
    else
      val excess = observedNorm - claimedUpperBound
      if excess <= tolerance.threshold(observedNorm) then Right(())
      else Left(MathematicalOracleError.MutationDetected(MutationTarget.OperatorNormBound, excess))

  def l1ProximalLaw(
      input: Vector[Double],
      output: Vector[Double],
      weight: Double,
      tolerance: ConditioningAwareTolerance
  ): Either[MathematicalOracleError, Unit] =
    if input.length != output.length then
      Left(MathematicalOracleError.InvalidDefinition("proximal input and output lengths differ"))
    else if !weight.isFinite || weight < 0.0 then
      Left(MathematicalOracleError.InvalidDefinition(s"L1 proximal weight must be finite and non-negative, got $weight"))
    else
      var index = 0
      var maximum = 0.0
      while index < input.length do
        val currentInput = input(index)
        val currentOutput = output(index)
        if !currentInput.isFinite || !currentOutput.isFinite then
          return Left(MathematicalOracleError.InvalidDefinition("proximal values must be finite"))
        val residual =
          if currentOutput > 0.0 then Math.abs(currentOutput - currentInput + weight)
          else if currentOutput < 0.0 then Math.abs(currentOutput - currentInput - weight)
          else Math.max(0.0, Math.abs(currentInput) - weight)
        maximum = Math.max(maximum, residual)
        index += 1
      if maximum <= tolerance.threshold(maxAbs(input)) then Right(())
      else Left(MathematicalOracleError.MutationDetected(MutationTarget.ProximalLaw, maximum))

  def observationMask(
      expectedObserved: Vector[ValueIdentity],
      actualContributions: Vector[ValueIdentity]
  ): Either[MathematicalOracleError, Unit] =
    if expectedObserved == actualContributions then Right(())
    else
      Left(
        MathematicalOracleError.IdentityMutationDetected(
          MutationTarget.ObservationMask,
          expectedObserved,
          actualContributions
        )
      )

  def foldProvenance(
      expectedTrainingIdentity: ValueIdentity,
      actualTrainingIdentity: ValueIdentity
  ): Either[MathematicalOracleError, Unit] =
    if expectedTrainingIdentity == actualTrainingIdentity then Right(())
    else
      Left(
        MathematicalOracleError.IdentityMutationDetected(
          MutationTarget.FoldProvenance,
          Vector(expectedTrainingIdentity),
          Vector(actualTrainingIdentity)
        )
      )

  def trajectory(
      points: Vector[TrajectoryPoint],
      laws: Set[TrajectoryLaw],
      tolerance: ConditioningAwareTolerance
  ): Either[MathematicalOracleError, Unit] =
    if points.isEmpty then Left(MathematicalOracleError.InvalidDefinition("trajectory must contain at least one point"))
    else if points.map(_.iteration) != points.indices.toVector then
      Left(MathematicalOracleError.InvalidDefinition("trajectory iterations must be contiguous and zero-based"))
    else
      val objectiveScale = points.foldLeft(1.0)((scale, point) => Math.max(scale, Math.abs(point.objective)))
      val threshold = tolerance.threshold(objectiveScale)
      val transitions = points.zip(points.drop(1))
      laws.toVector.foldLeft[Either[MathematicalOracleError, Unit]](Right(())):
        (result, law) =>
          result.flatMap: _ =>
            law match
              case TrajectoryLaw.FinalValueOnly => Right(())
              case TrajectoryLaw.MonotoneObjective =>
                transitions.find((before, after) => after.objective > before.objective + threshold) match
                  case Some((before, after)) =>
                    Left(MathematicalOracleError.TrajectoryViolation(law, after.iteration, after.objective - before.objective))
                  case None => Right(())
              case TrajectoryLaw.SufficientDecrease(modulus) =>
                if !modulus.isFinite || modulus <= 0.0 then
                  Left(MathematicalOracleError.InvalidDefinition(s"sufficient-decrease modulus must be positive, got $modulus"))
                else
                  transitions.find: (before, after) =>
                    after.objective > before.objective - modulus * after.stepNorm * after.stepNorm + threshold
                  match
                    case Some((before, after)) =>
                      val deficit = after.objective - before.objective + modulus * after.stepNorm * after.stepNorm
                      Left(MathematicalOracleError.TrajectoryViolation(law, after.iteration, deficit))
                    case None => Right(())
              case TrajectoryLaw.ResidualConvergence =>
                val finalPoint = points.last
                if finalPoint.residual <= tolerance.threshold(1.0) then Right(())
                else Left(MathematicalOracleError.TrajectoryViolation(law, finalPoint.iteration, finalPoint.residual))

  private def maxAbs(values: Vector[Double]): Double =
    var index = 0
    var maximum = 0.0
    while index < values.length do
      maximum = Math.max(maximum, Math.abs(values(index)))
      index += 1
    maximum

enum MathematicalOracleError:
  case InvalidDefinition(detail: String)
  case DuplicateCase(id: String)
  case LowRankModelsIsSoleOracle(id: OracleCaseId)
  case MissingCrossPlatformLaw(id: OracleCaseId)
  case MissingContracts(missing: Set[MathematicalModelFamily])
  case MissingAnalyticFixtures(missing: Set[AnalyticOracleFixture])
  case MissingMetamorphicLaws(missing: Set[MetamorphicOracleLaw])
  case MissingMutationTargets(missing: Set[MutationTarget])
  case MissingExecutionTiers(missing: Set[OracleExecutionTier])
  case MutationDetected(target: MutationTarget, residual: Double)
  case IdentityMutationDetected(
      target: MutationTarget,
      expected: Vector[ValueIdentity],
      actual: Vector[ValueIdentity]
  )
  case TrajectoryViolation(law: TrajectoryLaw, iteration: Int, residual: Double)

  def message: String =
    this match
      case InvalidDefinition(detail) => detail
      case DuplicateCase(id) => s"oracle matrix contains duplicate case '$id'"
      case LowRankModelsIsSoleOracle(id) =>
        s"oracle case '${id.value}' cannot use LowRankModels.jl as its sole differential oracle"
      case MissingCrossPlatformLaw(id) => s"oracle case '${id.value}' must run from shared source on JVM and Scala.js"
      case MissingContracts(missing) => s"oracle matrix is missing model contracts: ${render(missing)}"
      case MissingAnalyticFixtures(missing) => s"oracle matrix is missing analytic fixtures: ${render(missing)}"
      case MissingMetamorphicLaws(missing) => s"oracle matrix is missing metamorphic laws: ${render(missing)}"
      case MissingMutationTargets(missing) => s"oracle matrix is missing mutation targets: ${render(missing)}"
      case MissingExecutionTiers(missing) => s"oracle matrix is missing execution tiers: ${render(missing)}"
      case MutationDetected(target, residual) => s"$target mutation was detected with residual $residual"
      case IdentityMutationDetected(target, expected, actual) =>
        s"$target identity mutation was detected: expected ${expected.mkString(",")}, observed ${actual.mkString(",")}"
      case TrajectoryViolation(law, iteration, residual) =>
        s"trajectory violates $law at iteration $iteration with residual $residual"

  private def render[A](values: Set[A]): String =
    values.toVector.sortBy(_.toString).mkString(", ")

object MathematicalOracleCatalog:
  private val strict = ConditioningAwareTolerance.unsafe(1e-10, 1e-9, 1.0)
  private val conditioned = ConditioningAwareTolerance.unsafe(1e-9, 1e-8, 100.0)

  val cases: Vector[MathematicalOracleCase] = Vector(
    oracleCase(
      "oracle.exact-spectral",
      MathematicalContractCatalog.exactSpectralFrame,
      "ExactSpectralPrograms",
      "ExactSpectralProgramsSuite",
      analytic = Set(AnalyticOracleFixture.DiagonalSpectrum, AnalyticOracleFixture.SingularNullspace),
      differential = Set(DifferentialOracle.PublishedAllenGmdLimit),
      metamorphic = Set(
        MetamorphicOracleLaw.Permutation,
        MetamorphicOracleLaw.Relabeling,
        MetamorphicOracleLaw.CommonScaling,
        MetamorphicOracleLaw.CoordinateChange
      ),
      mutations = Set(MutationTarget.Adjoint),
      trajectory = Set(TrajectoryLaw.FinalValueOnly),
      representation = Set(RepresentationLaw.SharedJvmAndScalaJs, RepresentationLaw.ConditioningSweep),
      tier = OracleExecutionTier.PullRequestFast,
      seed = 1103L,
      tolerance = conditioned
    ),
    oracleCase(
      "oracle.composite-sparse-smooth",
      MathematicalContractCatalog.anchorRegularizedFrame,
      "CompositeSparseSmoothProgram",
      "CompositePenaltyCompilerSuite",
      analytic = Set(
        AnalyticOracleFixture.Laplacian2x2,
        AnalyticOracleFixture.SoftThreshold,
        AnalyticOracleFixture.TinyFusedLasso
      ),
      differential = Set(DifferentialOracle.IndependentConvexReference),
      metamorphic = Set(MetamorphicOracleLaw.ZeroPenaltyLimit, MetamorphicOracleLaw.LargePenaltyLimit),
      mutations = Set(MutationTarget.OperatorNormBound, MutationTarget.ProximalLaw),
      trajectory = Set(TrajectoryLaw.MonotoneObjective, TrajectoryLaw.ResidualConvergence),
      representation = Set(
        RepresentationLaw.SharedJvmAndScalaJs,
        RepresentationLaw.SparseStoragePreserved,
        RepresentationLaw.NoHiddenDensification,
        RepresentationLaw.AllocationControlledKernel
      ),
      tier = OracleExecutionTier.PullRequestFast,
      seed = 2207L,
      tolerance = strict
    ),
    oracleCase(
      "oracle.joint-sparse-functional",
      MathematicalContractCatalog.jointSparseFunctionalFactorization,
      "RankOneStructuredFactorization",
      "SparseFunctionalFactorizationSuite",
      differential = Set(DifferentialOracle.IndependentConvexReference, DifferentialOracle.PublishedAllenGmdLimit),
      metamorphic = Set(MetamorphicOracleLaw.CommonScaling, MetamorphicOracleLaw.CoordinateChange),
      mutations = Set(MutationTarget.OperatorNormBound, MutationTarget.ProximalLaw),
      trajectory = Set(TrajectoryLaw.MonotoneObjective, TrajectoryLaw.ResidualConvergence),
      representation = Set(RepresentationLaw.SharedJvmAndScalaJs, RepresentationLaw.ConditioningSweep),
      tier = OracleExecutionTier.Reference,
      seed = 3301L,
      tolerance = conditioned
    ),
    oracleCase(
      "oracle.generalized-low-rank",
      MathematicalContractCatalog.generalizedLowRankModel,
      "GeneralizedLowRankProgram and FittedLatentEncoder",
      "GeneralizedLowRankModelSuite and FittedLatentEncoderSuite",
      differential = Set(
        DifferentialOracle.IndependentConvexReference,
        DifferentialOracle.PublishedGlrmLimit,
        DifferentialOracle.LowRankModelsJl
      ),
      metamorphic = Set(MetamorphicOracleLaw.Permutation, MetamorphicOracleLaw.SparseDenseEquivalence),
      mutations = Set(MutationTarget.ObservationMask),
      trajectory = Set(TrajectoryLaw.MonotoneObjective, TrajectoryLaw.ResidualConvergence),
      representation = Set(
        RepresentationLaw.SharedJvmAndScalaJs,
        RepresentationLaw.SparseStoragePreserved,
        RepresentationLaw.NoHiddenDensification
      ),
      tier = OracleExecutionTier.Reference,
      seed = 4409L,
      tolerance = strict
    ),
    oracleCase(
      "oracle.convex-low-rank",
      MathematicalContractCatalog.convexifiedLowRankMatrix,
      "ConvexLowRankGlobalAdmission",
      "PalmConvergenceSuite",
      differential = Set(DifferentialOracle.IndependentConvexReference, DifferentialOracle.PublishedGlrmLimit),
      metamorphic = Set(MetamorphicOracleLaw.Permutation, MetamorphicOracleLaw.CommonScaling),
      trajectory = Set(TrajectoryLaw.ResidualConvergence),
      representation = Set(RepresentationLaw.SharedJvmAndScalaJs, RepresentationLaw.ConditioningSweep),
      tier = OracleExecutionTier.Reference,
      seed = 5519L,
      tolerance = conditioned
    ),
    oracleCase(
      "oracle.structured-multiblock",
      MathematicalContractCatalog.structuredMultiblockFactorization,
      "AlignedSharedScoreGlrm and FittedAlignedMultiblockEncoder",
      "StructuredMultiblockGlrmSuite",
      differential = Set(DifferentialOracle.IndependentConvexReference, DifferentialOracle.PublishedGlrmLimit),
      metamorphic = Set(
        MetamorphicOracleLaw.Permutation,
        MetamorphicOracleLaw.Relabeling,
        MetamorphicOracleLaw.SparseDenseEquivalence
      ),
      mutations = Set(MutationTarget.Adjoint, MutationTarget.ObservationMask),
      trajectory = Set(TrajectoryLaw.ResidualConvergence),
      representation = Set(
        RepresentationLaw.SharedJvmAndScalaJs,
        RepresentationLaw.SparseStoragePreserved,
        RepresentationLaw.NoHiddenDensification
      ),
      tier = OracleExecutionTier.Reference,
      seed = 6619L,
      tolerance = strict
    ),
    oracleCase(
      "oracle.palm-trajectories",
      MathematicalContractCatalog.jointSparseFunctionalFactorization,
      "PalmSolver",
      "PalmConvergenceSuite",
      differential = Set(DifferentialOracle.IndependentConvexReference),
      mutations = Set(MutationTarget.OperatorNormBound, MutationTarget.ProximalLaw),
      trajectory = Set(
        TrajectoryLaw.MonotoneObjective,
        TrajectoryLaw.SufficientDecrease(0.25),
        TrajectoryLaw.ResidualConvergence
      ),
      representation = Set(
        RepresentationLaw.SharedJvmAndScalaJs,
        RepresentationLaw.NoHiddenDensification,
        RepresentationLaw.ConditioningSweep
      ),
      tier = OracleExecutionTier.NightlyStress,
      seed = 7723L,
      tolerance = conditioned
    ),
    oracleCase(
      "oracle.fold-provenance",
      MathematicalContractCatalog.generalizedLowRankModel,
      "ModelSpec and FoldSafetyManifest",
      "ModelSpecSuite and RecoveryValidationSuite",
      metamorphic = Set(MetamorphicOracleLaw.Relabeling),
      mutations = Set(MutationTarget.FoldProvenance),
      trajectory = Set(TrajectoryLaw.FinalValueOnly),
      representation = Set(RepresentationLaw.SharedJvmAndScalaJs),
      tier = OracleExecutionTier.PullRequestFast,
      seed = 8837L,
      tolerance = strict
    )
  )

  val matrix: MathematicalOracleMatrix =
    MathematicalOracleMatrix.from(cases).fold(
      error => throw new IllegalStateException(error.message),
      identity
    )

  private def oracleCase(
      id: String,
      contract: MathematicalModelContract,
      implementationPath: String,
      evidenceLocation: String,
      analytic: Set[AnalyticOracleFixture] = Set.empty,
      differential: Set[DifferentialOracle] = Set.empty,
      metamorphic: Set[MetamorphicOracleLaw] = Set.empty,
      mutations: Set[MutationTarget] = Set.empty,
      trajectory: Set[TrajectoryLaw],
      representation: Set[RepresentationLaw],
      tier: OracleExecutionTier,
      seed: Long,
      tolerance: ConditioningAwareTolerance
  ): MathematicalOracleCase =
    MathematicalOracleCase
      .from(
        OracleCaseId.unsafe(id),
        contract,
        implementationPath,
        evidenceLocation,
        analytic,
        differential,
        metamorphic,
        mutations,
        trajectory,
        representation,
        tier,
        OracleSeed.unsafe(seed),
        tolerance
      )
      .fold(error => throw new IllegalStateException(error.message), identity)
