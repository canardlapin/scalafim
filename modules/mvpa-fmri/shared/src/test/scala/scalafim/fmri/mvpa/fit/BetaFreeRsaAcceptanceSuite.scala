package scalafim.fmri.mvpa.fit

import scalafim.scenarios.{ScenarioHarness, ScenarioObservation, ScenarioTolerance}
import scalafim.dataset.RunId
import scalafim.fmri.fit.{LeastSquaresSeparate, LssTrialDesign, ResponseBlock}
import mvpa4s.*
import gale.linalg.{DMat, Matrix}
import multivar.core.{CoordinateEvidence, Lin, SemanticProvenance, SpaceRole, ValueId, ValueIdentity}
import mvpa4s.relation.*

class BetaFreeRsaAcceptanceSuite extends munit.FunSuite:

  private def right[E,A](value: Either[E,A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, keys: Vector[String]) =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "fixture", "unit", "raw", Vector("v1")))

  test("native scoped trial relations match independent beta-free signed distances and RSA"):
    val partitions = axis("runs", Vector("run-1", "run-2", "run-3"))
    val effects = axis("effects", Vector("a", "b", "c"))
    val neural = axis("neural", Vector("0", "1", "2", "3"))
    var acquired = 0
    var closed = 0
    val resource = new ReadoutResource:
      def acquire() = { acquired += 1; Right(()) }
      def close() = { closed += 1; Right(()) }
    val inputs = runBlocks().map: run =>
      val origins = ReadoutRelationOrigins(s"acquisition:${run.runId}", "fixture-response", "lss-v1", "fixed", "none", ReadoutRelationAccess.ScopedReplay("fixture-readout", "v1"))
      RunReadoutRelation.scopedReplay(run, origins, resource)
    // Independent arithmetic on the literal responses below: average each two
    // condition rows, multiply condition differences across all six distinct
    // ordered run pairs, then divide by six pairs and four neural features.
    val expected = Vector(0.3245833333333333333, 0.9120833333333333333, 1.22)
    val metric = right(Lin.fromDenseMatrix(DMat.eye(4), CoordinateEvidence.primal(neural.evidence), CoordinateEvidence.dual(neural.evidence), ValueIdentity.source(ValueId.unsafe("identity-metric")), SemanticProvenance.source("fixture")))
    val model = right(SquareRelationModel("hypothesis", Vector("c", "b", "a"), Map(("b", "c") -> 1.0, ("a", "c") -> 3.0, ("a", "b") -> 2.0), ValueIdentity.source(ValueId.unsafe("hypothesis")), "unit", "fixed", "none"))
    val numeric = right(IdentifiedReadoutRelations.withRuns(partitions, effects, neural, inputs): relations =>
      val pairing = right(RelationRdm.allDistinctOrdered(partitions))
      val observed = right(RelationRdm.identity(relations, pairing))
      val distances = observed.cells.zipWithIndex.map:
        case (RelationRdmCell.Estimated(actual), i) =>
          ScenarioHarness.scalar(s"distance-$i", actual, expected(i), ScenarioTolerance.absolute(1e-12))
        case (other, i) => ScenarioHarness.fact(s"distance-$i", false, other.toString)
      val request: RelationRdmRequest[partitions.Id,effects.Id,neural.Id,String] = RelationRdmRequest(pairing, metric, MetricAdmission.Fixed(EvidenceOrigins.Unknown), Vector.empty, IdentityRdmPolicy())
      val result = right(RelationConsumers.rsaDirect(relations, request, model, RelationRsaMethod.Pearson, QueryReuseBudget(8,64,0), RelationConsumerBudget(16,4096)))
      val rsa = result.outcome match
        case RelationRsaOutcome.Defined(value) =>
          ScenarioHarness.scalar("rsa", value, -0.338425830031662, ScenarioTolerance.absolute(1e-12))
        case other => ScenarioHarness.fact("rsa", false, other.toString)
      Right(distances ++ Vector(rsa,
        ScenarioHarness.fact("complete-distance-family", observed.cells.size == expected.size, observed.cells.size.toString),
        ScenarioHarness.fact("descriptive-pairing", observed.pairing.claim.isInstanceOf[PairingClaim.Descriptive], observed.pairing.claim.toString),
        ScenarioHarness.fact("detached-scalar", !result.receipt.retainedGeometry, "no retained scoped geometry")))
    )
    val reversed = axis("wrong-neural-order", Vector("3", "2", "1", "0"))
    val refused = IdentifiedReadoutRelations.withRuns(partitions, effects, reversed, inputs)(_ => Right(()))
    val result = ScenarioHarness.result("u4-fit-readout-identified-rsa", numeric ++ Vector(
      ScenarioHarness.fact("owned-lifetimes", acquired == 3 && closed == 3, s"acquired=$acquired closed=$closed"),
      ScenarioHarness.fact("wrong-order-refusal", refused.left.toOption.exists(_.isInstanceOf[IdentifiedReadoutRelationError.NeuralFeatureMismatch]), refused.toString)))
    assert(result.ciPass, result.render)

  private def runBlocks(): Vector[RunTrialReadout] =
    val trialDesign = fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 1.0),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val responses = Vector(
      Vector(
        Vector(0.0, 0.2, 0.0, 0.1), Vector(0.2, 0.0, 0.1, -0.1),
        Vector(1.0, 0.0, 0.5, 0.2), Vector(1.2, 0.2, 0.3, 0.0),
        Vector(0.0, 2.0, 0.2, -0.2), Vector(-0.2, 1.8, 0.4, 0.0)
      ),
      Vector(
        Vector(0.1, 0.0, 0.0, 0.2), Vector(-0.1, 0.2, 0.2, 0.0),
        Vector(1.1, 0.1, 0.4, 0.1), Vector(1.3, -0.1, 0.6, -0.1),
        Vector(0.2, 2.1, 0.1, 0.0), Vector(0.0, 1.9, 0.3, -0.2)
      ),
      Vector(
        Vector(-0.1, 0.1, 0.1, 0.0), Vector(0.1, -0.1, -0.1, 0.2),
        Vector(0.9, 0.2, 0.5, 0.0), Vector(1.1, 0.0, 0.5, 0.2),
        Vector(-0.1, 1.9, 0.3, -0.1), Vector(0.1, 2.1, 0.1, 0.1)
      )
    )

    responses.zipWithIndex.map: (rows, index) =>
      val suffix = index + 1
      val readout = LeastSquaresSeparate
        .unsafePrepare(
          LssTrialDesign.unsafe(trialDesign, Vector("a", "b", "c"))
        )
        .trialReadout
        .toOption
        .get
      RunTrialReadout
        .make(RunId(s"run-$suffix"), ResponseBlock.unsafe(fromRows(rows)), readout)
        .toOption
        .get

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    val matrix = Matrix.newBuilder(rows.length, rows.head.length)
    var row = 0
    while row < rows.length do
      var col = 0
      while col < rows.head.length do
        matrix(row, col) = rows(row)(col)
        col += 1
      row += 1
    matrix.result()
