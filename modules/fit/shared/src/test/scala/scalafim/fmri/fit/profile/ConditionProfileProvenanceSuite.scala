package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.fmri.design.ColumnId
import scalafim.fmri.design.hrf.{HrfKernelBasis, KernelBasisProvenance, KernelBasisSpec}
import scalafim.fmri.fit.{
  ResponsePreparationPlan,
  ResponsePreparationProvenance,
  RunIndex,
  TaskBasisStructure,
  VolumeWeightNormalization,
  VolumeWeightPartitionReceipt,
  VolumeWeightingReceipt,
  VolumeWeightingSource
}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule}
import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  DvarsWeightEstimator,
  DvarsWeightFunction,
  DvarsWeightScope,
  FixedWeightAlignment,
  MissingDataPolicy,
  NuisanceProjection,
  Regularization,
  RobustOptions,
  RobustPsi,
  ScaleScope,
  SoftThresholdSteepness,
  VolumeWeightThreshold,
  VolumeWeighting
}

import scala.compiletime.constValueTuple
import scala.deriving.Mirror

class ConditionProfileProvenanceSuite extends munit.FunSuite:

  private inline def labels[T](using m: Mirror.ProductOf[T]): Vector[String] =
    constValueTuple[m.MirroredElemLabels].toList.map(_.toString).toVector

  private def assertEncodesFields(kind: String, fields: Vector[String], encoded: String, marker: String => String): Unit =
    val missing = fields.filterNot(name => encoded.contains(marker(name)))
    assertEquals(missing, Vector.empty, s"$kind encoding omits fields; encode them and bump its version: $encoded")

  private lazy val basis = HrfKernelBasis
    .compile(KernelBasisSpec(
      GaussianFamily.Default, PositiveSeconds.unsafe(Seconds(0.2)), Vector(8, 7), tolerance = 0.5,
      maxRank = 4, heldOutPoints = 1))
    .fold(error => fail(error.message), identity)

  private def admission: ObservedFamilyAdmission =
    new ObservedFamilyAdmission(
      ObservedFamilyCertificate(Vector.empty, 0.0, 0.0, 1.0, 1.0, 0),
      ObservedFamilyRequirements(1e-2, 1e8, 1.0),
      geometry = "unused",
      planGeometry = None,
      basisProvenance = basis.provenance.canonical
    )

  private def structure: TaskBasisStructure =
    TaskBasisStructure
      .make(Vector("cond_a", "cond_b").map(condition =>
        Vector.tabulate(basis.rank)(j => ColumnId(s"${condition}_$j").fold(error => fail(error.message), identity))
      ))
      .fold(error => fail(error.message), identity)

  private def query(label: String, weights: Vector[Double], tolerance: Double): SignedQuery =
    SignedQuery.make(label, weights, tolerance).fold(error => fail(error.message), identity)

  private def policy(prior: Option[ShapePrior]): ConditionProfilePolicy =
    ConditionProfilePolicy(
      basis = basis,
      structure = structure,
      nodesPerAxis = Vector(2, 21),
      budget = DecodeBudget(
        coarseStride = 3,
        maxNewtonSteps = 4,
        maxJets = 5,
        maxExactEvaluations = 6,
        weakSdLimit = Vector(-0.0, 0.1),
        ambiguityEnergy = 1e-6,
        maxCandidateAttempts = 7,
        stationarityStepTolerance = 1e-9
      ),
      prior = prior,
      noiseVariance = 0.1,
      output = OutputRequest.ConditionQueries(
        Vector(query("A-B", Vector(1.0, -1.0), 1e-6), query("mean|=;", Vector(0.5, 0.5), 0.1)),
        NormalizationRule.UnitPeak
      ),
      admission = admission,
      blockSize = 8
    )

  /** A preparation exercising every double-bearing branch with non-integral values. */
  private def preparation: ResponsePreparationProvenance =
    val plan = ResponsePreparationPlan(
      missingData = MissingDataPolicy.OmitRowsPerVoxel,
      censoredTimepoints = Vector(3),
      volumeWeighting = VolumeWeighting.Fixed(Vector(0.1, 1e-6, 1.0), FixedWeightAlignment.SelectedRows),
      nuisanceProjection = NuisanceProjection.MatrixProjection(
        DMat.tabulate(2, 2)((row, col) => Vector(-1.0, 0.1, 1e-6, 2.5)(row * 2 + col)),
        Regularization.Fixed(0.1)
      ),
      autocorrelation = ArOptions(ArStructure.Ar(1), rho = Some(-0.1), censoredTimepoints = Vector(3)),
      robust = RobustOptions(RobustPsi.Huber(1.345), maxIterations = 3, scaleScope = ScaleScope.Voxel)
    )
    val receipt = VolumeWeightingReceipt(
      source = VolumeWeightingSource.ResponseDvars(DvarsWeightEstimator(
        DvarsWeightFunction.SoftThreshold(VolumeWeightThreshold.unsafe(1.5), SoftThresholdSteepness.unsafe(0.1)),
        DvarsWeightScope.AcrossSelection
      )),
      normalization = VolumeWeightNormalization.MeanOne(DvarsWeightScope.WithinRun),
      inputTimepoints = Vector(0, 1, 2),
      retainedTimepoints = Vector(0, 2),
      excludedTimepoints = Vector(1),
      zeroWeightTimepoints = Vector(1),
      weights = Vector(0.1, 0.0, 1e-6),
      qualityMetric = Some(Vector(-1.0, 0.1, 1e-6)),
      partitions = Vector(VolumeWeightPartitionReceipt(RunIndex.unsafe(0), Vector(0, 1, 2)))
    )
    ResponsePreparationProvenance(plan.records, Some(receipt))

  test("the producer records every numerics-affecting policy field and documents the rest"):
    val policyFields = labels[ConditionProfilePolicy]
    val encoded = ConditionProfileProvenance.encodedPolicyFields
    val excluded = ConditionProfileProvenance.unencodedPolicyFields.keySet
    assertEquals(encoded.toSet ++ excluded, policyFields.toSet)
    assertEquals(encoded.toSet & excluded, Set.empty[String])
    val provenanceFields = labels[ConditionProfileProvenance]
    assertEquals(encoded.filterNot(provenanceFields.contains), Vector.empty)
    val canonical = ConditionProfileProvenance.of(policy(None), preparation).canonical
    assertEncodesFields("condition-profile", provenanceFields, canonical, name => s"|$name=")

  test("nested encodings name every field of their case classes"):
    val canonical = ConditionProfileProvenance.of(policy(Some(ShapePrior(Vector(0.0), Vector(1.0)))), preparation).canonical
    assertEncodesFields("decode-budget", labels[DecodeBudget], canonical, name => s"|$name=")
    assertEncodesFields("shape prior", labels[ShapePrior], canonical, name => s":$name=")
    assertEncodesFields("AR options", labels[ArOptions], canonical, name => s":$name=")
    assertEncodesFields("robust options", labels[RobustOptions], canonical, name => s":$name=")
    assertEncodesFields("volume-weighting receipt", labels[VolumeWeightingReceipt], canonical, name => s":$name=")
    assert(!canonical.contains("@"), canonical)

  test("prior presence and values change the identity"):
    val none = ConditionProfileProvenance.of(policy(None), preparation).canonical
    val some = ConditionProfileProvenance.of(policy(Some(ShapePrior(Vector(5.0, -1.0), Vector(0.1, 0.0, 0.0, 1e-6)))), preparation).canonical
    val other = ConditionProfileProvenance.of(policy(Some(ShapePrior(Vector(5.0, -1.0), Vector(0.1, 0.0, 0.0, 2e-6)))), preparation).canonical
    assert(none.contains("|prior=none|"), none)
    assertNotEquals(none, some)
    assertNotEquals(some, other)

  test("condition profile provenance from the real producer has a platform-independent golden"):
    val provenance = ConditionProfileProvenance.of(
      policy(Some(ShapePrior(Vector(5.0, -1.0), Vector(0.1, 0.0, 0.0, 1e-6)))),
      preparation
    )
    val basisField = KernelBasisProvenance.field(basis.provenance.canonical)
    assertEquals(
      provenance.canonical,
      s"condition-profile/v2|basis=$basisField|" + ConditionProfileProvenanceSuite.Golden
    )

object ConditionProfileProvenanceSuite:
  val Golden: String =
    "structure=conditions(21:condition(8:cond_a_0),21:condition(8:cond_b_0))|preparation=2063:response-preparation/v1|records=records(168:record(36:missing_data(19:omit_rows_per_voxel),116:applied(103:response columns are grouped by exact finite-row mask and each observation pattern is fit independently)),100:record(29:censoring(15:timepoints(1:3)),56:deferred(43:censoring is consumed by AR/GLS preparation)),297:record(138:volume_weights(118:fixed(91:values(24:bits:4591870180066957722,24:bits:4517329193108106637,24:bits:4607182418800017408),13:selected_rows)),142:planned(129:resolve one temporal weight vector, apply sqrt(w) to design and response, and exclude exact zero-weight rows before factorization)),226:record(128:nuisance_projection(103:matrix_projection(43:matrix(1:2,1:2,24:fnv1a64:34e02fa5ddeae381),34:fixed(24:bits:4591870180066957722))),82:deferred(69:soft nuisance projection is represented but not applied in this slice)),296:record(208:whitening(193:ar_options(17:structure=ar(1:1),12:iterations=1,12:global=false,15:voxelwise=false,15:exactFirst=true,34:censoredTimepoints=timepoints(1:3),38:rho=some(25:bits:-4631501856787818086),8:phi=none)),72:deferred(59:autocorrelation whitening is handled by the GLS interpreter)),242:record(151:robust_weights(131:robust_options(38:psi=huber(24:bits:4608736160671460229),15:maxIterations=3,16:scaleScope=voxel,31:reestimateAutocorrelation=false)),75:deferred(62:robust IWLS weights require the RobustLeastSquares interpreter)))|volumeWeighting=some(637:volume_weighting_receipt(128:source=response_dvars(101:dvars(71:soft_threshold(24:bits:4609434218613702656,24:bits:4591870180066957722),16:across_selection)),37:normalization=mean_one(10:within_run),39:inputTimepoints=timepoints(1:0,1:1,1:2),38:retainedTimepoints=timepoints(1:0,1:2),34:excludedTimepoints=timepoints(1:1),36:zeroWeightTimepoints=timepoints(1:1),80:weights=values(24:bits:4591870180066957722,6:bits:0,24:bits:4517329193108106637),115:qualityMetric=some(92:values(25:bits:-4616189618054758400,24:bits:4591870180066957722,24:bits:4517329193108106637)),67:partitions=partitions(41:partition(1:0,23:timepoints(1:0,1:1,1:2)))))|nodesPerAxis=nodes(1:2,2:21)|budget=272:decode-budget/v1|coarseStride=3|maxNewtonSteps=4|maxJets=5|maxExactEvaluations=6|weakSdLimit=values(25:bits:-9223372036854775808,24:bits:4591870180066957722)|ambiguityEnergy=bits:4517329193108106637|maxCandidateAttempts=7|stationarityStepTolerance=bits:4472406533629990549|prior=some(180:shape_prior(69:mean=values(24:bits:4617315517961601024,25:bits:-4616189618054758400),91:precision=values(24:bits:4591870180066957722,6:bits:0,6:bits:0,24:bits:4517329193108106637)))|output=condition_queries(9:unit_peak,289:queries(134:query(3:A-B,72:weights=values(24:bits:4607182418800017408,25:bits:-4616189618054758400),42:absoluteTolerance=bits:4517329193108106637),137:query(7:mean|=;,71:weights=values(24:bits:4602678819172646912,24:bits:4602678819172646912),42:absoluteTolerance=bits:4591870180066957722)))|noiseVariance=bits:4591870180066957722"
