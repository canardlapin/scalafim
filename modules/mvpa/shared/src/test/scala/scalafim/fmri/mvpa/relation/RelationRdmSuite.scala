package scalafim.fmri.mvpa.relation

import gale.linalg.DMat
import multivar.core.{CoordinateEvidence, Lin, SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.{AxisRef, EvidenceError}

class RelationRdmSuite extends munit.FunSuite:
  private def right[A](value: Either[EvidenceError, A]): A = value.fold(error => fail(error.message), identity)
  private def axis(name: String, role: SpaceRole, n: Int): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(n)(i => s"$name-$i"), "fixture", "unit", "raw"))
  private def table[E, N](effects: AxisRef[E], neural: AxisRef[N], values: DMat) =
    Lin.fromDenseMatrix(values, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), ValueIdentity.source(ValueId.unsafe("relation-rdm-fixture"))).fold(error => fail(error.toString), identity)

  test("fixed identity RDM uses all ordered distinct partitions, preserves effect order and signed values"):
    val partitions = axis("runs", SpaceRole.Samples, 2)
    val effects = axis("effects", SpaceRole.Latent, 3)
    val neural = axis("voxels", SpaceRole.Observed, 2)
    def relation(values: DMat) = right(Relation(effects, neural, table(effects, neural, values),
      RelationOrigins(RelationSource("a", "response", "readout", "prep", "identity"), RelationAccess.OwnedReplay("fixture")),
      Vector.fill(3)(EffectEstimability.Estimable)))
    val set = right(RelationSet(partitions, effects, neural, Vector(
      relation(DMat.dense(3, 2, Vector(1.0, 0.0, 2.0, 0.0, 4.0, 0.0))),
      relation(DMat.dense(3, 2, Vector(1.0, 0.0, -2.0, 0.0, -4.0, 0.0)))
    )))
    val pairing = right(RelationRdm.allDistinctOrdered(partitions))
    val result = right(RelationRdm.identity(set, pairing, policy = IdentityRdmPolicy(normalizeByFeatures = true)))
    assertEquals(pairing.edges.map(edge => edge.left.ordinal -> edge.right.ordinal), Vector(0 -> 1, 1 -> 0))
    assertEquals(result.effectKeys, Vector("effects-0", "effects-1", "effects-2"))
    assertEquals(result.cells.length, 3)
    result.cells.head match
      case RelationRdmCell.Estimated(value) => assertEqualsDouble(value, -1.5, 1e-12)
      case other => fail(s"expected signed estimate, got $other")

  test("a locally non-estimable effect remains visible and is not replaced by another family"):
    val partitions = axis("runs", SpaceRole.Samples, 2)
    val effects = axis("effects", SpaceRole.Latent, 2)
    val neural = axis("voxels", SpaceRole.Observed, 1)
    def relation(status: Vector[EffectEstimability]) = right(Relation(effects, neural, table(effects, neural, DMat.dense(2, 1, Vector(1.0, 2.0))),
      RelationOrigins(RelationSource("a", "response", "readout", "prep", "identity"), RelationAccess.OwnedReplay("fixture")), status))
    val set = right(RelationSet(partitions, effects, neural, Vector(relation(Vector(EffectEstimability.Estimable, EffectEstimability.NotEstimable("rank deficient"))), relation(Vector.fill(2)(EffectEstimability.Estimable)))))
    val result = right(RelationRdm.identity(set, right(RelationRdm.allDistinctOrdered(partitions))))
    result.cells.head match
      case RelationRdmCell.NotEstimable(ordinals, reasons) =>
        assertEquals(ordinals, Vector(1)); assert(reasons.contains("rank deficient"))
      case other => fail(s"expected visible non-estimability, got $other")

  test("four-effect condensed order and three-partition baseline match independent raw loop and legacy oracle"):
    val partitions = axis("runs-three", SpaceRole.Samples, 3)
    val effects = axis("effects-four", SpaceRole.Latent, 4)
    val neural = axis("voxels-three", SpaceRole.Observed, 3)
    val raw = Vector(
      Vector(1.0, 2.0, 0.0, 3.0, -1.0, 2.0, -2.0, 1.0, 4.0, 5.0, 3.0, -1.0),
      Vector(-1.0, 3.0, 1.0, 2.0, 1.0, -3.0, 4.0, -2.0, 2.0, -1.0, 0.0, 5.0),
      Vector(2.0, -1.0, 4.0, -3.0, 2.0, 1.0, 1.0, 5.0, -2.0, 3.0, -4.0, 2.0))
    val relations = raw.zipWithIndex.map: (values, run) =>
      right(Relation(effects, neural, table(effects, neural, DMat.dense(4, 3, values)),
        RelationOrigins(RelationSource(s"acq-$run", s"response-$run", "readout", s"prep-$run", "identity"), RelationAccess.OwnedReplay("fixture")),
        Vector.fill(4)(EffectEstimability.Estimable)))
    val set = right(RelationSet(partitions, effects, neural, relations))
    val pairing = right(RelationRdm.allDistinctOrdered(partitions))
    val order = Vector((1, 0), (2, 0), (3, 0), (2, 1), (3, 1), (3, 2))
    val expected = order.map: (a, b) =>
      var sum = 0.0
      for i <- 0 until 3; j <- 0 until 3 if i != j; v <- 0 until 3 do
        sum += (raw(i)(a * 3 + v) - raw(i)(b * 3 + v)) * (raw(j)(a * 3 + v) - raw(j)(b * 3 + v))
      sum / 6.0
    val legacy = scalafim.fmri.mvpa.Rdm.crossnobisDistances(
      scalafim.fmri.mvpa.PartitionMeans.unsafe(4, 3, 3, raw.flatten.toArray), normalizeByFeatures = false)
    val result = right(RelationRdm.identity(set, pairing, policy = IdentityRdmPolicy(false)))
    val normalized = right(RelationRdm.identity(set, pairing))
    result.cells.zipWithIndex.foreach: (cell, i) =>
      cell match
        case RelationRdmCell.Estimated(value) =>
          assertEqualsDouble(value, expected(i), 1e-12)
          assertEqualsDouble(value, legacy.values(i), 1e-12)
        case other => fail(other.toString)
      normalized.cells(i) match
        case RelationRdmCell.Estimated(value) => assertEqualsDouble(value, expected(i) / 3, 1e-12)
        case other => fail(other.toString)
    assertEquals(result.endpointOrigins, relations.map(_.origins))
    val missingEdge = right(PairingDesign(partitions, pairing.edges.drop(1), EdgeReducer.WeightedMean))
    assert(RelationRdm.identity(set, missingEdge).isLeft)
