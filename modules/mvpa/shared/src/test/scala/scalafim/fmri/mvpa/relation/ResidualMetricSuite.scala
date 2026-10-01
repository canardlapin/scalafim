package scalafim.fmri.mvpa.relation

import gale.linalg.DMat
import multivar.core.{CoordinateEvidence, Lin, SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}
import scalafim.fmri.mvpa.pattern.ResidualCovariance

class ResidualMetricSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis = right(AxisRef.fromStableKeys("voxels", SpaceRole.Observed, Vector("v0", "v1"), "fixture", "unit", "raw"))

  test("residual precision agrees with an independent dense inverse on a small diagonal-plus-rank-one covariance"):
    val covariance = right(ResidualCovariance.fromFactors(axis, Vector(2.0, 3.0), DMat.dense(2, 1, Vector(1.0, 2.0))))
    val provenance = ResidualMetricProvenance(RelationSource("acq", "response", "readout", "prep", "noise"), axis.descriptor, axis.descriptor, "training residuals removed task model", "diagonal-plus-low-rank residual fit", 18.0)
    val metric = right(ResidualPrecisionMetric(covariance, ResidualMetricAdmission.Descriptive(MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), provenance, "numerical oracle only"), 1))
    val closure = right(metric.closure)
    val actual = right(closure(DMat.dense(2, 1, Vector(3.0, -1.0))).left.map(EvidenceError.SemanticFailure.apply))
    // inverse([[3, 2], [2, 7]]) * [3, -1] = [23, -9] / 17
    assertEqualsDouble(actual(0, 0), 23.0 / 17.0, 1e-12)
    assertEqualsDouble(actual(1, 0), -9.0 / 17.0, 1e-12)

  test("a bounded residual precision refuses a wider hidden materialization"):
    val covariance = right(ResidualCovariance.fromFactors(axis, Vector(1.0, 1.0), DMat.dense(2, 1, Vector(0.0, 0.0))))
    val provenance = ResidualMetricProvenance(RelationSource("acq", "response", "readout", "prep", "noise"), axis.descriptor, axis.descriptor, "training residuals removed task model", "diagonal-plus-low-rank residual fit", 18.0)
    val metric = right(ResidualPrecisionMetric(covariance, ResidualMetricAdmission.Descriptive(MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), provenance, "independent metric evidence absent"), 1))
    assert(metric.closure.flatMap(value => value(DMat.eye(2)).left.map(EvidenceError.SemanticFailure.apply)).isLeft)

  test("rejected residual provenance cannot become an identity metric"):
    val covariance = right(ResidualCovariance.fromFactors(axis, Vector(1.0, 1.0), DMat.dense(2, 1, Vector(0.0, 0.0))))
    val metric = right(ResidualPrecisionMetric(covariance, ResidualMetricAdmission.Rejected("residuals unavailable; no covariance substitution"), 1))
    assert(metric.closure.isLeft)

  private final case class BoundNoise(covariance: ResidualCovariance[String], df: Double)
  private given Estimability[BoundNoise] with
    def effectEstimability(value: BoundNoise) = Vector(EffectEstimability.Estimable, EffectEstimability.Estimable)
  private given ResidualMoments[BoundNoise] with
    type Moment = String
    def residualMoments(value: BoundNoise) = "task-removed residual moments"
  private given NoisePrecision[BoundNoise] with
    type Precision = ResidualCovariance[String]
    def noisePrecision(value: BoundNoise) = value.covariance
  private given ResidualDegreesOfFreedom[BoundNoise] with
    def residualDegreesOfFreedom(value: BoundNoise) = value.df

  test("residual admission binds actual precision capability, source and df without an identity qualification"):
    val neural = axis
    val effects = right(AxisRef.fromStableKeys("effects-noise", SpaceRole.Latent, Vector("a", "b"), "effects", "unit", "raw"))
    val samples = right(AxisRef.fromStableKeys("training-noise", SpaceRole.Samples, Vector.tabulate(20)(i => s"s$i"), "sample", "none", "one"))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(2.0, 3.0), DMat.dense(2, 1, Vector(1.0, 2.0))))
    val source = RelationSource("residual-acq", "residual-response", "task-readout", "train-prep", "residual-noise-v1")
    val estimate = right(Lin.fromDenseMatrix(DMat.eye(2), CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence),
      ValueIdentity.source(ValueId.unsafe("bound-residual-fixture"))))
    val relation = right(Relation(effects, neural, estimate, RelationOrigins(source, RelationAccess.OwnedReplay("fixture")), Vector.fill(2)(EffectEstimability.Estimable)))
    val binding = right(relation.bindResidual(source, BoundNoise(covariance, 18.0)))
    val metric = right(ResidualPrecisionMetric.fromBinding(binding, covariance, samples.descriptor,
      "OLS task and intercept removed", "diagonal-plus-rank-one", MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), 1))
    assertEquals(metric.provenance.get.source, source)
    assertEqualsDouble(metric.provenance.get.degreesOfFreedom, 18.0, 1e-12)
    assert(metric.admission.isInstanceOf[ResidualMetricAdmission.Descriptive])
    val foreign = right(ResidualCovariance.fromFactors(neural, Vector(3.0, 4.0), DMat.dense(2, 1, Vector(1.0, 2.0))))
    assert(ResidualPrecisionMetric.fromBinding(binding, foreign, samples.descriptor, "residual", "estimator", MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), 1).isLeft)
    val badDf = right(relation.bindResidual(source, BoundNoise(covariance, 21.0)))
    assert(ResidualPrecisionMetric.fromBinding(badDf, covariance, samples.descriptor, "residual", "estimator", MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), 1).isLeft)
    assert(ResidualPrecisionMetric(covariance, ResidualMetricAdmission.DeclaredConditional(MetricAdmission.Fixed(scalafim.fmri.mvpa.EvidenceOrigins.Unknown), metric.provenance.get), 1).isLeft)
    assert(closureAdmitsWidth(metric, 2).isLeft)

  private def closureAdmitsWidth(metric: ResidualPrecisionMetric[String], columns: Int) =
    metric.closure.flatMap(value => value.star(DMat.zeros(2, columns)).left.map(EvidenceError.SemanticFailure.apply))

  test("residual RDM distinguishes independent declared evidence, endpoint-learned descriptions, and rejected substitutions"):
    val neural = axis
    val effects = right(AxisRef.fromStableKeys("metric-effects", SpaceRole.Latent, Vector("a", "b"), "effects", "unit", "raw"))
    val partitions = right(AxisRef.fromStableKeys("metric-partitions", SpaceRole.Samples, Vector("l", "r"), "partition", "none", "one"))
    val samples = right(AxisRef.fromStableKeys("metric-training", SpaceRole.Samples, Vector.tabulate(20)(i => s"s$i"), "sample", "none", "one"))
    def origins(name: String): RelationOrigins =
      val id = SourceId.unsafe(name)
      val source = right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
      val value = ValueIdentity.source(ValueId.unsafe(s"$name-value"))
      val temporal = right(AxisRef.fromStableKeys(s"scan-$name", SpaceRole.Samples, Vector("t0", "t1"), "scan", "none", "one"))
      val support = right(EvidenceOrigins.make(source, value, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
        ValueSupport.Bounded(temporal.descriptor, value, Vector(0)), PreparationSupport.FixedShared(ValueSupport.Bounded(temporal.descriptor, value, Vector.empty)), effects.descriptor))
      RelationOrigins(RelationSource(name, s"$name-response", "readout", s"$name-prep", s"$name-noise"), RelationAccess.OwnedReplay("fixture"), support)
    def relation(origin: RelationOrigins, values: Vector[Double]) =
      val estimate = right(Lin.fromDenseMatrix(DMat.dense(2, 2, values), CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), ValueIdentity.source(ValueId.unsafe(s"${origin.source.acquisitionRevision}-estimate"))))
      right(Relation(effects, neural, estimate, origin, Vector.fill(2)(EffectEstimability.Estimable)))
    val left = relation(origins("left"), Vector(1.0, 0.0, 0.0, 1.0))
    val rightRelation = relation(origins("right"), Vector(2.0, 0.0, 0.0, 1.0))
    val metricRelation = relation(origins("independent-metric"), Vector(1.0, 0.0, 0.0, 1.0))
    val set = right(RelationSet(partitions, effects, neural, Vector(left, rightRelation)))
    val all = right(RelationRdm.allDistinctOrdered(partitions))
    val single = right(PairingDesign(partitions, all.edges.take(1), EdgeReducer.WeightedSum))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(2.0, 3.0), DMat.dense(2, 1, Vector(1.0, 2.0))))
    val binding = right(metricRelation.bindResidual(metricRelation.origins.source, BoundNoise(covariance, 18.0)))
    val argument = right(ConditionalErrorIndependence(left.origins, rightRelation.origins, left.origins.support, rightRelation.origins.support,
      "synthetic independent acquisition errors conditional on the separately fitted residual metric", Some(metricRelation.origins.support)))
    val admitted = right(ResidualPrecisionMetric.fromBinding(binding, covariance, samples.descriptor, "task residuals", "regularized factor covariance", MetricAdmission.IndependentlySourced(metricRelation.origins.support, argument), 1))
    val result = right(RelationRdm.residual(set, single, admitted, Vector(argument), IdentityRdmPolicy(false)))
    assert(result.pairing.claim.isInstanceOf[PairingClaim.DeclaredUnbiased])
    assertEquals(result.residualProvenance.get.source, metricRelation.origins.source)
    result.cells.head match
      case RelationRdmCell.Estimated(value) => assertEqualsDouble(value, 23.0 / 17.0, 1e-12)
      case other => fail(other.toString)
    val endpoint = right(left.bindResidual(left.origins.source, BoundNoise(covariance, 18.0)))
    val endpointArgument = right(ConditionalErrorIndependence(left.origins, rightRelation.origins, left.origins.support, rightRelation.origins.support,
      "endpoint-learned metric needs a separate conditional argument", Some(left.origins.support)))
    val endpointMetric = right(ResidualPrecisionMetric.fromBinding(endpoint, covariance, samples.descriptor, "task residuals", "regularized factor covariance", MetricAdmission.EndpointLearned(left.origins.support, endpointArgument), 1))
    assert(right(RelationRdm.residual(set, single, endpointMetric, policy = IdentityRdmPolicy(false))).pairing.claim.isInstanceOf[PairingClaim.Descriptive])
    assert(ResidualPrecisionMetric.fromBinding(binding, covariance, samples.descriptor, "task residuals", "estimator", MetricAdmission.IndependentlySourced(left.origins.support, argument), 1).isLeft)
    val rejected = right(ResidualPrecisionMetric(covariance, ResidualMetricAdmission.Rejected("residuals unavailable"), 1))
    assert(RelationRdm.residual(set, single, rejected).isLeft)
