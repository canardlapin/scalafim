package scalafim.fmri.mvpa.relation

import gale.linalg.DMat
import multivar.core.{CoordinateEvidence, Lin, SemanticProvenance, SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{IndexSpace, Injection}
import scalafim.fmri.mvpa.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class PairingSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  private val partitions = right(AxisRef.fromStableKeys("run", SpaceRole.Samples, Vector("r1", "r2", "r3"), "fixture", "unit", "raw", Vector("fixture:v1")))
  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private def origins(acquisition: String, preparation: String): RelationOrigins =
    val value = ValueIdentity.source(ValueId.unsafe(s"$acquisition-$preparation"))
    val acquisitionAxis = right(AxisRef.fromStableKeys(s"scan:$acquisition", SpaceRole.Samples,
      Vector("t0", "t1", "t2"), "fixture", "unit", "raw"))
    val support = right(EvidenceOrigins.make(
      source(acquisition), value, AcquisitionCoordinates.OriginalTemporalAxis(acquisitionAxis.descriptor),
      ValueSupport.Bounded(acquisitionAxis.descriptor, value, Vector(0)), PreparationSupport.FixedShared(ValueSupport.Bounded(acquisitionAxis.descriptor, value, Vector.empty)), partitions.descriptor
    ))
    RelationOrigins(RelationSource(acquisition, "response", "readout", preparation, "noise"), RelationAccess.OneShot, support)
  private def coordinate(ordinal: Int): PartitionCoordinate[partitions.Id, String] =
    val injection = right(Injection.from(IArray(ordinal), right(IndexSpace.of(partitions.size))))
    right(PartitionCoordinate(right(ReindexingLeg.bind(partitions, injection))))
  private def edge(left: Int, rightOrdinal: Int): PairingEdge[partitions.Id, String] =
    right(PairingEdge(coordinate(left), coordinate(rightOrdinal), 1.0, partitions.descriptor))

  test("reversing endpoints preserves their order rather than identifying them"):
    val forward = edge(0, 1)
    assertEquals(forward.reverse.left.ordinal, 1)
    assertEquals(forward.reverse.right.ordinal, 0)

  test("same acquisition and preparation remain descriptive reasons"):
    val design = right(PairingDesign(partitions, Vector(edge(0, 1)), EdgeReducer.WeightedMean))
    val assessment = right(design.assessOrigins(Vector(origins("acq", "prep"), origins("acq", "prep"), origins("other", "other")), MetricAdmission.Fixed(EvidenceOrigins.Unknown), Vector.empty))
    assessment.claim match
      case PairingClaim.Descriptive(reasons) =>
        assert(reasons.exists:
          case ScientificRefusal.SharedAcquisition(_, _) => true
          case _ => false
        )
        assert(reasons.exists:
          case ScientificRefusal.SharedPreparation(_, _) => true
          case _ => false
        )
      case _ => fail("shared origins must not receive an unbiasedness claim")

  test("endpoint-learned metric needs a bound conditional argument"):
    val left = origins("left", "left-prep")
    val rightOrigin = origins("right", "right-prep")
    val design = right(PairingDesign(partitions, Vector(edge(0, 1)), EdgeReducer.WeightedSum))
    val assessment = right(design.assessOrigins(Vector(left, rightOrigin, origins("third", "third-prep")), MetricAdmission.EndpointLearned(rightOrigin.support, right(ConditionalErrorIndependence(left, rightOrigin, left.support, rightOrigin.support, "declared zero conditional error cross-moment", Some(rightOrigin.support)))), Vector.empty))
    assessment.claim match
      case PairingClaim.Descriptive(reasons) => assert(reasons.contains(ScientificRefusal.MissingDeclaredErrorIndependence))
      case _ => fail("unbound endpoint metric admission must remain descriptive")
    val admitted = right(design.assessOrigins(
      Vector(left, rightOrigin, origins("third", "third-prep")),
      MetricAdmission.EndpointLearned(rightOrigin.support, right(ConditionalErrorIndependence(left, rightOrigin, left.support, rightOrigin.support, "declared zero conditional error cross-moment", Some(rightOrigin.support)))),
      Vector(right(ConditionalErrorIndependence(left, rightOrigin, left.support, rightOrigin.support, "declared zero conditional error cross-moment", Some(rightOrigin.support))))
    ))
    admitted.claim match
      case PairingClaim.DeclaredUnbiased(_, _, _) => ()
      case _ => fail("bound endpoint reasoning should admit the declared claim")

  test("repeated partition pairs refuse naive replicate independence"):
    val left = origins("left", "left-prep")
    val middle = origins("middle", "middle-prep")
    val design = right(PairingDesign(partitions, Vector(edge(0, 1), edge(0, 2)), EdgeReducer.WeightedMean))
    val third = origins("right", "right-prep")
    val first = right(ConditionalErrorIndependence(left, middle, left.support, middle.support, "independent fixture acquisition errors"))
    val second = right(ConditionalErrorIndependence(left, third, left.support, third.support, "independent fixture acquisition errors"))
    val assessment = right(design.assessOrigins(Vector(left, middle, third), MetricAdmission.Fixed(EvidenceOrigins.Unknown), Vector(first, second)))
    assessment.claim match
      case PairingClaim.Descriptive(reasons) => assert(reasons.contains(ScientificRefusal.SharedPartitionReplicates))
      case PairingClaim.DeclaredUnbiased(_, _, reasons) => assert(reasons.contains(ScientificRefusal.SharedPartitionReplicates))

  test("signed fixed cross-product oracle retains its sign"):
    val left = DMat.dense(2, 2, Vector(1.0, -2.0, 3.0, 4.0))
    val metric = DMat.dense(2, 2, Vector(2.0, 0.0, 0.0, -1.0))
    val rightMatrix = DMat.dense(2, 2, Vector(-1.0, 5.0, 2.0, 1.0))
    var expected = 0.0
    var row = 0
    while row < 2 do
      var col = 0
      while col < 2 do
        var inner = 0
        var value = 0.0
        while inner < 2 do
          value += left(row, inner) * metric(inner, inner) * rightMatrix(col, inner)
          inner += 1
        expected += value
        col += 1
      row += 1
    assertEqualsDouble(expected, -4.0, 1e-12)
    val connection = edge(0, 1)
    val design = right(PairingDesign(partitions, Vector(connection), EdgeReducer.WeightedMean))
    val reduced = right(design.reduce(Vector(connection -> expected)))
    assertEqualsDouble(reduced.value, -4.0, 1e-12)
    assert(design.reduce(Vector(connection.reverse -> expected)).isLeft)
    assert(design.reduce(Vector(connection -> expected, connection -> expected)).isLeft)

  test("weighted ordered scalar contributions preserve orientation under pair reversal and metric transpose"):
    val effects = right(AxisRef.fromStableKeys("weighted-effects", SpaceRole.Latent, Vector("effect"), "fixture", "unit", "raw"))
    val neural = right(AxisRef.fromStableKeys("weighted-neural", SpaceRole.Observed, Vector("n0", "n1"), "fixture", "unit", "raw"))
    def relation(values: Vector[Double], name: String) =
      val estimate = right(Lin.fromDenseMatrix(DMat.dense(1, 2, values), CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), ValueIdentity.source(ValueId.unsafe(name)), SemanticProvenance.source("pairing")))
      right(Relation(effects, neural, estimate, origins(name, s"$name-prep").copy(access = RelationAccess.OwnedReplay("pairing"), support = EvidenceOrigins.Unknown), Vector(EffectEstimability.Estimable)))
    def closure(values: DMat, name: String) = right(Lin.fromDenseMatrix(values, CoordinateEvidence.primal(neural.evidence), CoordinateEvidence.dual(neural.evidence), ValueIdentity.source(ValueId.unsafe(name)), SemanticProvenance.source("pairing")))
    val b0 = relation(Vector(1.0, 2.0), "b0")
    val b1 = relation(Vector(3.0, -1.0), "b1")
    val b2 = relation(Vector(-2.0, 4.0), "b2")
    val query = right(Lin.fromDenseMatrix(DMat.dense(1, 1, Vector(1.0)), CoordinateEvidence.primal(effects.evidence), CoordinateEvidence.dual(effects.evidence), ValueIdentity.source(ValueId.unsafe("weighted-h")), SemanticProvenance.source("pairing")))
    val k = closure(DMat.dense(2, 2, Vector(2.0, 1.0, -3.0, -1.0)), "weighted-k")
    val first = right(PairingEdge(coordinate(0), coordinate(1), 2.0, partitions.descriptor))
    val second = right(PairingEdge(coordinate(1), coordinate(2), -0.5, partitions.descriptor))
    val endpoints = Vector(b0, b1, b2)
    def contribution(edge: PairingEdge[partitions.Id, String]) = right(SecondOrderQuery(RelationPair(endpoints(edge.left.ordinal), endpoints(edge.right.ordinal)), Some(query), Some(k)).scalar).value
    assertEqualsDouble(contribution(first), -11.0, 1e-12)
    assertEqualsDouble(contribution(second), -2.0, 1e-12)
    val sum = right(right(PairingDesign(partitions, Vector(first, second), EdgeReducer.WeightedSum)).reduce(Vector(first -> contribution(first), second -> contribution(second))))
    val mean = right(right(PairingDesign(partitions, Vector(first, second), EdgeReducer.WeightedMean)).reduce(Vector(first -> contribution(first), second -> contribution(second))))
    assertEqualsDouble(sum.value, -21.0, 1e-12)
    assertEqualsDouble(mean.value, -14.0, 1e-12)
    val reversed = right(SecondOrderQuery(RelationPair(b1, b0), Some(query.star), Some(k.star)).scalar).value
    assertEqualsDouble(reversed, -11.0, 1e-12)
    val reverseEdges = Vector(first.reverse, second.reverse)
    val reverseValues = reverseEdges.map: edge =>
      edge -> right(SecondOrderQuery(RelationPair(endpoints(edge.left.ordinal), endpoints(edge.right.ordinal)), Some(query.star), Some(k.star)).scalar).value
    assertEqualsDouble(right(right(PairingDesign(partitions, reverseEdges, EdgeReducer.WeightedSum)).reduce(reverseValues)).value, -21.0, 1e-12)
    assertEqualsDouble(right(right(PairingDesign(partitions, reverseEdges, EdgeReducer.WeightedMean)).reduce(reverseValues)).value, -14.0, 1e-12)
    val incorrectlyUntransposed = reverseEdges.map(edge => edge -> contribution(edge))
    assertEqualsDouble(right(right(PairingDesign(partitions, reverseEdges, EdgeReducer.WeightedMean)).reduce(incorrectlyUntransposed)).value, 55.0 / 1.5, 1e-12)


  test("foreign conditional evidence and unknown metric cannot grant a declared unbiased claim"):
    val left = origins("left", "lp")
    val rightOrigin = origins("right", "rp")
    val foreign = origins("foreign", "fp")
    assertEquals(ConditionalErrorIndependence(left, rightOrigin, foreign.support, rightOrigin.support,
      "foreign evidence is incompatible").left.toOption, Some(ScientificRefusal.ForeignConditionalEvidence))
    val argument = right(ConditionalErrorIndependence(left, rightOrigin, left.support, rightOrigin.support,
      "declared conditional errors", Some(EvidenceOrigins.Unknown)))
    val design = right(PairingDesign(partitions, Vector(edge(0, 1)), EdgeReducer.WeightedSum))
    val assessment = right(design.assessOrigins(Vector(left, rightOrigin, foreign),
      MetricAdmission.IndependentlySourced(EvidenceOrigins.Unknown, argument), Vector(argument)))
    assert(assessment.claim.isInstanceOf[PairingClaim.Descriptive])

  test("known shared acquisition support survives changed source revision labels"):
    val left = origins("left", "lp")
    val copiedSupport = left.copy(source = RelationSource("new-label", "response", "readout", "new-prep", "noise"))
    val design = right(PairingDesign(partitions, Vector(edge(0, 1)), EdgeReducer.WeightedSum))
    val assessment = right(design.assessOrigins(Vector(left, copiedSupport, origins("third", "tp")),
      MetricAdmission.Fixed(EvidenceOrigins.Unknown), Vector.empty))
    assessment.claim match
      case PairingClaim.Descriptive(reasons) => assert(reasons.exists {
        case ScientificRefusal.SharedAcquisition(_, _) => true
        case _ => false
      })
      case _ => fail("revision labels cannot remove shared acquisition support")

  test("restricted beta rows retain shared original acquisition support and remain descriptive"):
    val effects = right(AxisRef.fromStableKeys("beta-effects", SpaceRole.Observed, Vector("a", "b"), "effect", "unit", "raw"))
    val neural = right(AxisRef.fromStableKeys("beta-neural", SpaceRole.Observed, Vector("v"), "feature", "unit", "raw"))
    val supportValue = ValueIdentity.source(ValueId.unsafe("original-acquisition-values"))
    val support = right(EvidenceOrigins.make(source("original-acquisition"), supportValue,
      AcquisitionCoordinates.OriginalTemporalAxis(partitions.descriptor),
      ValueSupport.Bounded(partitions.descriptor, supportValue, Vector(0, 1)),
      PreparationSupport.FixedShared(ValueSupport.Bounded(partitions.descriptor, supportValue, Vector.empty)), effects.descriptor))
    def beta(sourceName: String, values: Vector[Double]) =
      val estimate = right(Lin.fromDenseMatrix(DMat.dense(2, 1, values), CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), ValueIdentity.source(ValueId.unsafe(sourceName)), SemanticProvenance.source("restricted-beta")))
      right(Relation(effects, neural, estimate, RelationOrigins(RelationSource(sourceName, "response", "readout", "prep", "noise"), RelationAccess.OwnedReplay("pairing"), support), Vector.fill(2)(EffectEstimability.Estimable)))
    val selection = right(Injection.from(IArray(0), right(IndexSpace.of(effects.size))))
    val leg = right(ReindexingLeg.bind(effects, selection))
    val restricted = Vector(beta("beta-0", Vector(1.0, 2.0)), beta("beta-1", Vector(3.0, 4.0)), beta("beta-2", Vector(5.0, 6.0))).map(_.restrict(leg))
    val set = right(RelationSet(partitions, leg.child, neural, restricted))
    val design = right(PairingDesign(partitions, Vector(edge(0, 1)), EdgeReducer.WeightedSum))
    right(design.assess(set, MetricAdmission.Fixed(EvidenceOrigins.Unknown), Vector.empty)).claim match
      case PairingClaim.Descriptive(reasons) => assert(reasons.exists { case ScientificRefusal.SharedAcquisition(_, _) => true; case _ => false })
      case other => fail(s"restricted rows with shared original support must remain descriptive, got $other")

  test("finite weight overflow and zero weighted-mean denominator are refused"):
    val huge = right(PairingEdge(coordinate(0), coordinate(1), Double.MaxValue, partitions.descriptor))
    val other = right(PairingEdge(coordinate(1), coordinate(2), Double.MaxValue, partitions.descriptor))
    assert(PairingDesign(partitions, Vector(huge, other), EdgeReducer.WeightedMean).isLeft)
    val negative = right(PairingEdge(coordinate(1), coordinate(2), -1.0, partitions.descriptor))
    assert(PairingDesign(partitions, Vector(edge(0, 1), negative), EdgeReducer.WeightedMean).isLeft)

  test("public assessment binds actual typed relation set origins"):
    val effects = right(AxisRef.fromStableKeys("effects", SpaceRole.Observed, Vector("contrast"), "effect", "none", "raw"))
    val neural = right(AxisRef.fromStableKeys("neural", SpaceRole.Observed, Vector("voxel"), "feature", "none", "raw"))
    val relations = Vector.tabulate(3): i =>
      val table = right(multivar.core.Lin.fromDenseMatrix(DMat.dense(1, 1, Vector(i.toDouble)), multivar.core.CoordinateEvidence.dual(neural.evidence), multivar.core.CoordinateEvidence.primal(effects.evidence),
        ValueIdentity.source(ValueId.unsafe(s"relation-$i"))))
      right(Relation(effects, neural, table,
        RelationOrigins(RelationSource(s"acq-$i", "response", "readout", s"prep-$i", "noise"), RelationAccess.OneShot),
        Vector(EffectEstimability.Estimable)))
    val set = right(RelationSet(partitions, effects, neural, relations))
    val design = right(PairingDesign(partitions, Vector(edge(0, 1)), EdgeReducer.WeightedSum))
    val assessment = right(design.assess(set, MetricAdmission.Fixed(EvidenceOrigins.Unknown), Vector.empty))
    assert(assessment.claim.isInstanceOf[PairingClaim.Descriptive])
