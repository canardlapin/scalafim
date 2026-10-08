package scalafim.fmri.mvpa.analysis

import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import multivar.inference.{Alpha, CanonicalRankMethod, MonteCarloDraws}
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.fmri.mvpa.inference.CalibrationProtocolSupport
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Test-only adapter for the existing production procedure and restricted plan
  * identity. It implements no CCA, fit, null statistic or distribution helper. */
object CalibrationBindings:
  private def adapt[A](value: Either[?, A]): Either[String, A] = value.left.map(_.toString)
  private def axis(name: String, size: Int) =
    adapt(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(size)(i => name + "-" + i), "calibration", "one", "raw"))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    adapt(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(name + "-root"), id)))
  private def values(name: String) = ValueIdentity.source(ValueId.unsafe(name))

  def rank(x: DMat, y: DMat, nuisance: DMat, rootSeed: Long, draws: Int, scenario: String, ordinal: Int = 0,
      method: CanonicalRankMethod = CanonicalRankMethod.GaussianInterlacingWilksV1): Either[String, RankConfirmationResult] =
    val p = x.cols; val q = y.cols; val k = math.min(p, q)
    if x.rows != y.rows || nuisance.rows != x.rows || k <= 0 then Left("rank input shape")
    else CalibrationProtocolSupport.bindingIdentity(scenario, ordinal, rootSeed).flatMap: identity =>
      val plan = PlanId.derived(EstimandId("calibration-" + identity),
        Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64),
        "independent-input", "fixed-discovery", "rank", "remaining-roots", Vector.empty, Vector.empty, "closed", Vector.empty, Set.empty)
      def exposure(key: String) = EvidenceExposure.internal(ExposureReference(plan, key, "declared fixture source", ResultIdentity(identity)))
      for
        rows <- axis(identity + "-confirmation", x.rows)
        units <- axis(identity + "-confirmation-units", x.rows)
        training <- axis(identity + "-discovery", math.max(p, q) + 2)
        trainingUnits <- axis(identity + "-discovery-units", training.size)
        neural <- axis(identity + "-brain", p)
        target <- axis(identity + "-target", q)
        components <- axis(identity + "-components", k)
        brainCandidate <- axis(identity + "-brain-candidates", p)
        targetCandidate <- axis(identity + "-target-candidates", q)
        factors <- adapt(PatternFactors(neural, target, components,
          DMat.tabulate(p, k)((i, j) => if i == j then 1.0 else 0.0),
          DMat.tabulate(q, k)((i, j) => if i == j then 1.0 else 0.0), GaugeEvidence.PendingNumericalCheck))
        unit <- adapt(AxisValues(target, Vector.fill(q)(1.0)))
        geometry <- adapt(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
        binding <- adapt(TrainingBinding(training.descriptor, identity + "-predeclared-independent-subspaces", "fixed"))
        diagnostics <- adapt(PatternFitDiagnostics(Vector(0.0), "predeclared calibration projections", Vector.empty))
        artifact <- adapt(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("zero-population-mean", "zero-population-mean"),
          DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.NotFitted, binding, Vector("independent discovery"), diagnostics))
        trainSamples <- adapt(ConfirmationUnits(training, trainingUnits, Vector.range(0, training.size)))
        frozenBrain <- adapt(FrozenProjection(neural, components, factors.neuralByComponent, ProjectionKind.DeclaredLinearProjection))
        frozenTarget <- adapt(FrozenProjection(target, components, factors.targetByComponent, ProjectionKind.DeclaredLinearProjection))
        discovery <- adapt(DiscoverySnapshot(trainSamples, artifact, frozenBrain, frozenTarget, "predeclared support", "unrotated", "fixed preparation"))
        confirmSamples <- adapt(ConfirmationUnits(rows, units, Vector.range(0, x.rows)))
        confirmation <- adapt(ConfirmationSnapshot(confirmSamples, "fixed confirmation preparation"))
        z <- adapt(ConfirmationNuisance(rows, nuisance))
        design <- adapt(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery, confirmation, exposure(confirmation.identity),
          C1Contract("fixed independently declared projections", "remaining canonical roots zero", "independent rows in candidate spaces",
            Vector.tabulate(k)(i => "rank-" + (i + 1)), "spherical joint Gaussian declared", Vector("candidate projections"),
            Vector("nuisance residual coordinates", method.identity)), z, ConfirmationErrorLaw.IndependentGaussian))
        candidateBrain <- adapt(FrozenProjection(neural, brainCandidate, DMat.eye(p), ProjectionKind.DeclaredLinearProjection))
        candidateTarget <- adapt(FrozenProjection(target, targetCandidate, DMat.eye(q), ProjectionKind.DeclaredLinearProjection))
        frozen <- adapt(RankFrozenSubspaces.freeze(discovery, candidateBrain, candidateTarget, exposure(discovery.identity), "predeclared independent score-space identity"))
        law <- adapt(RankJointGaussian.declare(design, "spherical Gaussian score errors over independent rows"))
        brainSource <- source(identity + "-brain-source")
        targetSource <- source(identity + "-target-source")
        observations <- adapt(Observations.fromDense(rows, neural, x, values(identity + "-X"), brainSource))
        responses <- adapt(MultiResponse.fromDense(rows, target, y, values(identity + "-Y"), targetSource))
        count <- adapt(MonteCarloDraws(draws))
        alpha <- adapt(Alpha(.05))
        resamplingSeed <- CalibrationProtocolSupport.child(rootSeed, 103, method.ordinal)
        result <- adapt(RankConfirmation.run(design, frozen, law, observations, responses, Seed.fromLong(resamplingSeed), count, alpha, method = method))
      yield result
