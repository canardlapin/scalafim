package scalafim.multivar
package validation

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.validation.*

class MathematicalOracleMatrixSuite extends munit.FunSuite:
  private val tolerance = acceptedTolerance(ConditioningAwareTolerance.from(1e-12, 1e-10, 1.0))

  test("the release matrix covers every contract, fixture, law, mutation, and execution tier"):
    val matrix = MathematicalOracleCatalog.matrix
    assertEquals(matrix.byContract.keySet, MathematicalContractCatalog.all.map(_.family).toSet)
    assertEquals(matrix.byTier.keySet, OracleExecutionTier.all)
    assertEquals(
      matrix.cases.flatMap(_.analyticFixtures).toSet,
      AnalyticOracleFixture.releaseRequired
    )
    assertEquals(
      matrix.cases.flatMap(_.metamorphicLaws).toSet,
      MetamorphicOracleLaw.releaseRequired
    )
    assertEquals(
      matrix.cases.flatMap(_.mutationTargets).toSet,
      MutationTarget.releaseRequired
    )

  test("LowRankModels.jl cannot be the sole differential oracle"):
    val result = MathematicalOracleCase.from(
      OracleCaseId.unsafe("oracle.invalid-low-rank-models-only"),
      MathematicalContractCatalog.generalizedLowRankModel,
      "GeneralizedLowRankProgram",
      "MathematicalOracleMatrixSuite",
      Set.empty,
      Set(DifferentialOracle.LowRankModelsJl),
      Set.empty,
      Set.empty,
      Set(TrajectoryLaw.FinalValueOnly),
      Set(RepresentationLaw.SharedJvmAndScalaJs),
      OracleExecutionTier.Reference,
      OracleSeed.unsafe(1L),
      tolerance
    )
    assert(result match
      case Left(MathematicalOracleError.LowRankModelsIsSoleOracle(_)) => true
      case _ => false
    )

  test("conditioning-aware tolerances are checked and scale explicitly"):
    val conditioned = acceptedTolerance(ConditioningAwareTolerance.from(1e-8, 1e-6, 100.0))
    assertEqualsDouble(conditioned.threshold(2.0), 0.00020001, 1e-15)
    assert(ConditioningAwareTolerance.from(1e-8, 1e-6, 0.5).isLeft)
    assert(ConditioningAwareTolerance.from(Double.NaN, 1e-6, 1.0).isLeft)

  test("the five analytic fixtures have independent closed-form or exhaustive oracles"):
    val diagonal = Vector(4.0, 2.0, -1.0)
    assertEqualsDouble(diagonal.max, 4.0, 0.0)

    val laplacianVector = Vector(1.0, -1.0)
    val laplacianAction = Vector(
      laplacianVector(0) - laplacianVector(1),
      laplacianVector(1) - laplacianVector(0)
    )
    val roughness = 0.5 * dot(laplacianVector, laplacianAction)
    assertEqualsDouble(roughness, 2.0, 0.0)

    val softThreshold = Vector(3.0, -0.5).map(value => Math.signum(value) * Math.max(0.0, Math.abs(value) - 1.0))
    assertEquals(softThreshold, Vector(2.0, -0.0))
    assertEquals(MathematicalOracleSentinel.l1ProximalLaw(Vector(3.0, -0.5), softThreshold, 1.0, tolerance), Right(()))

    val fusedCandidate = Vector(0.75, -0.75)
    val candidateObjective = fusedObjective(fusedCandidate(0), fusedCandidate(1), 0.25)
    var first = -1.5
    var exhaustiveMinimum = Double.PositiveInfinity
    while first <= 1.5 + 1e-12 do
      var second = -1.5
      while second <= 1.5 + 1e-12 do
        exhaustiveMinimum = Math.min(exhaustiveMinimum, fusedObjective(first, second, 0.25))
        second += 0.01
      first += 0.01
    assertEqualsDouble(candidateObjective, exhaustiveMinimum, 1e-10)

    val constant = Vector(1.0, 1.0)
    val nullAction = Vector(constant(0) - constant(1), constant(1) - constant(0))
    assertEquals(nullAction, Vector(0.0, 0.0))

  test("adjoint and norm-bound mutations are killed for their intended laws"):
    val vector = Vector(2.0, -1.0)
    val covector = Vector(3.0, 4.0)
    val forward = Vector(2.0 * vector(0) + vector(1), -vector(0) + 3.0 * vector(1))
    val exactAdjoint = Vector(2.0 * covector(0) - covector(1), covector(0) + 3.0 * covector(1))
    val mutatedAdjoint = Vector(2.0 * covector(0) + covector(1), covector(0) + 3.0 * covector(1))
    assertEquals(MathematicalOracleSentinel.adjoint(dot(forward, covector), dot(vector, exactAdjoint), tolerance), Right(()))
    assertMutation(
      MathematicalOracleSentinel.adjoint(dot(forward, covector), dot(vector, mutatedAdjoint), tolerance),
      MutationTarget.Adjoint
    )
    assertEquals(MathematicalOracleSentinel.operatorNormBound(Math.sqrt(5.0), 3.0, tolerance), Right(()))
    assertMutation(MathematicalOracleSentinel.operatorNormBound(Math.sqrt(5.0), 2.0, tolerance), MutationTarget.OperatorNormBound)

  test("proximal, mask, and fold-provenance mutations fail through distinct sentinels"):
    assertEquals(
      MathematicalOracleSentinel.l1ProximalLaw(Vector(3.0, -0.5), Vector(2.0, 0.0), 1.0, tolerance),
      Right(())
    )
    assertMutation(
      MathematicalOracleSentinel.l1ProximalLaw(Vector(3.0, -0.5), Vector(2.5, 0.0), 1.0, tolerance),
      MutationTarget.ProximalLaw
    )

    val observedZero = value("oracle.observed-zero")
    val observedOne = value("oracle.observed-one")
    assertEquals(
      MathematicalOracleSentinel.observationMask(Vector(observedZero, observedOne), Vector(observedZero, observedOne)),
      Right(())
    )
    assertIdentityMutation(
      MathematicalOracleSentinel.observationMask(Vector(observedZero, observedOne), Vector(observedOne)),
      MutationTarget.ObservationMask
    )

    val training = value("oracle.training")
    assertEquals(MathematicalOracleSentinel.foldProvenance(training, training), Right(()))
    assertIdentityMutation(
      MathematicalOracleSentinel.foldProvenance(training, value("oracle.full-data")),
      MutationTarget.FoldProvenance
    )

  test("objective, sufficient-decrease, and residual trajectories are checked jointly"):
    val points = Vector(
      point(0, 4.0, 2.0, 0.0),
      point(1, 2.0, 0.5, 2.0),
      point(2, 1.0, 1e-13, 1.0)
    )
    val laws = Set(
      TrajectoryLaw.MonotoneObjective,
      TrajectoryLaw.SufficientDecrease(0.25),
      TrajectoryLaw.ResidualConvergence
    )
    assertEquals(MathematicalOracleSentinel.trajectory(points, laws, tolerance), Right(()))

    val increased = Vector(point(0, 1.0, 1.0, 0.0), point(1, 1.1, 1e-13, 0.1))
    assert(MathematicalOracleSentinel.trajectory(increased, Set(TrajectoryLaw.MonotoneObjective), tolerance) match
      case Left(MathematicalOracleError.TrajectoryViolation(TrajectoryLaw.MonotoneObjective, 1, _)) => true
      case _ => false
    )

  test("trajectory evidence rejects noncontiguous, nonfinite, and unconverged receipts"):
    assert(TrajectoryPoint.from(0, Double.NaN, 0.0, 0.0).isLeft)
    val noncontiguous = Vector(point(0, 1.0, 0.5, 0.0), point(2, 0.5, 0.1, 0.5))
    assert(MathematicalOracleSentinel.trajectory(noncontiguous, Set(TrajectoryLaw.FinalValueOnly), tolerance).isLeft)
    val unresolved = Vector(point(0, 1.0, 0.5, 0.0), point(1, 0.5, 0.1, 0.5))
    assert(MathematicalOracleSentinel.trajectory(unresolved, Set(TrajectoryLaw.ResidualConvergence), tolerance) match
      case Left(MathematicalOracleError.TrajectoryViolation(TrajectoryLaw.ResidualConvergence, 1, _)) => true
      case _ => false
    )

  test("shared-source portability and sparse-preservation obligations are explicit"):
    assert(MathematicalOracleCatalog.cases.forall(_.representationLaws.contains(RepresentationLaw.SharedJvmAndScalaJs)))
    val sparsePaths = MathematicalOracleCatalog.cases.filter: oracle =>
      oracle.implementationPath.contains("Composite") ||
      oracle.implementationPath.contains("GeneralizedLowRank") ||
      oracle.implementationPath.contains("Multiblock")
    assert(sparsePaths.nonEmpty)
    assert(sparsePaths.forall(_.representationLaws.contains(RepresentationLaw.NoHiddenDensification)))

  test("every execution tier records deterministic seeds and finite conditioning-aware tolerances"):
    MathematicalOracleCatalog.matrix.byTier.foreach: (tier, cases) =>
      assert(cases.nonEmpty, s"$tier has no conformance cases")
      assert(cases.forall(_.seed.value >= 0L))
      assert(cases.forall(_.tolerance.threshold(1.0).isFinite))

  private def point(iteration: Int, objective: Double, residual: Double, stepNorm: Double): TrajectoryPoint =
    TrajectoryPoint.from(iteration, objective, residual, stepNorm).fold(
      error => fail(error.message),
      identity
    )

  private def acceptedTolerance(
      value: Either[MathematicalOracleError, ConditioningAwareTolerance]
  ): ConditioningAwareTolerance =
    value.fold(error => fail(error.message), identity)

  private def value(id: String): ValueIdentity = ValueIdentity.source(ValueId.unsafe(id))

  private def dot(left: Vector[Double], right: Vector[Double]): Double =
    var index = 0
    var result = 0.0
    while index < left.length do
      result += left(index) * right(index)
      index += 1
    result

  private def fusedObjective(first: Double, second: Double, weight: Double): Double =
    0.5 * ((first - 1.0) * (first - 1.0) + (second + 1.0) * (second + 1.0)) +
      weight * Math.abs(second - first)

  private def assertMutation(
      result: Either[MathematicalOracleError, Unit],
      target: MutationTarget
  ): Unit =
    assert(result match
      case Left(MathematicalOracleError.MutationDetected(`target`, _)) => true
      case _ => false
    )

  private def assertIdentityMutation(
      result: Either[MathematicalOracleError, Unit],
      target: MutationTarget
  ): Unit =
    assert(result match
      case Left(MathematicalOracleError.IdentityMutationDetected(`target`, _, _)) => true
      case _ => false
    )
