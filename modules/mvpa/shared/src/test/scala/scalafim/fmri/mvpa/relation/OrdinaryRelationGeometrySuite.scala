package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import scala.compiletime.testing.typeCheckErrors
import multivar.core.{CoordinateEvidence, Lin, SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.{AxisRef, EvidenceError, RdmMethod, RdmModel, RdmScorer, RdmVector}
import scalafim.fmri.mvpa.{Column, EvidenceSource, MvpaParityFixtures}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class OrdinaryRelationGeometrySuite extends munit.FunSuite:
  private def right[E,A](value: Either[E,A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, keys: Vector[String]): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "fixture", "unit", "raw"))
  private val budget = RelationConsumerBudget(64,4096)

  test("ordinary squared, Euclidean and signed geometry retain distinct numerical semantics"):
    val partitions = axis("runs", Vector("one", "two"))
    val effects = axis("effects", Vector("b", "a", "c"))
    val neural = axis("neural", Vector("x", "y"))
    def relation(sign: Double) =
      val values = DMat.dense(3,2,Vector(sign,0.0,0.0,0.0,0.0,2.0*sign))
      val table = right(Lin.fromDenseMatrix(values, CoordinateEvidence.dual(neural.evidence), CoordinateEvidence.primal(effects.evidence), ValueIdentity.source(ValueId.unsafe(s"owned-literal-$sign"))))
      right(Relation(effects,neural,table,RelationOrigins(RelationSource("shared", "response", "readout", "fixed", "none"), RelationAccess.OwnedReplay("literal-dense-table")),Vector.fill(3)(EffectEstimability.Estimable)))
    val set = right(RelationSet(partitions,effects,neural,Vector(relation(1.0),relation(-1.0))))
    val pairing = right(RelationRdm.allDistinctOrdered(partitions))
    val squared = right(OrdinaryRelationGeometry.compute(set,pairing.edges.head.left,RdmMethod.SquaredEuclidean(),budget))
    val normalized = right(OrdinaryRelationGeometry.compute(set,pairing.edges.head.left,RdmMethod.SquaredEuclidean(true),budget))
    val euclidean = right(OrdinaryRelationGeometry.compute(set,pairing.edges.head.left,RdmMethod.Euclidean,budget))
    assertEquals(squared.observed.labels,Vector("b","a","c"))
    val expected = Vector(1.0,5.0,4.0)
    squared.observed.rdm.values.zip(expected).foreach((a,b) => assertEqualsDouble(a,b,1e-12))
    normalized.observed.rdm.values.zip(expected).foreach((a,b) => assertEqualsDouble(a,b/2,1e-12))
    euclidean.observed.rdm.values.zip(expected).foreach((a,b) => assertEqualsDouble(a,math.sqrt(b),1e-12))
    val signed = right(RelationRdm.identity(set,pairing))
    signed.cells.zip(expected).foreach:
      case (RelationRdmCell.Estimated(value), reference) => assertEqualsDouble(value,-reference/2,1e-12)
      case other => fail(other.toString)
    assert(signed.pairing.claim.isInstanceOf[PairingClaim.Descriptive])
    val model = RdmModel.unsafe("geometry",Vector("a","b","c"),RdmVector.unsafe(3,Vector(1.0,4.0,5.0)))
    assertEqualsDouble(right(squared.score(model,RdmScorer.Pearson)),1.0,1e-12)
    assert(squared.score(RdmModel.unsafe("foreign",Vector("a","b","d"),model.rdm),RdmScorer.Pearson).isLeft)
    assert(OrdinaryRelationGeometry.compute(set,pairing.edges.head.left,RdmMethod.Correlation,budget).isLeft)

  test("partition, budget, non-estimability and replay refusals precede operator reads"):
    val partitions = axis("runs",Vector("one","two"))
    val effects = axis("effects",Vector("a","b"))
    val neural = axis("neural",Vector("x","y"))
    var reads = 0
    val poison = new DoubleLinearOperator:
      val rows = 2
      val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw new IllegalStateException("unexpected forward read")
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw new IllegalStateException("unexpected adjoint read")
    val table = right(Lin.fromLinearMap(poison,CoordinateEvidence.dual(neural.evidence),CoordinateEvidence.primal(effects.evidence),ValueIdentity.source(ValueId.unsafe("poison"))))
    def set(access: RelationAccess, missing: Boolean) =
      val statuses = Vector(EffectEstimability.Estimable,if missing then EffectEstimability.NotEstimable("absent") else EffectEstimability.Estimable)
      val relation = right(Relation(effects,neural,table,RelationOrigins(RelationSource("a","r","readout","fixed","none"),access),statuses))
      right(RelationSet(partitions,effects,neural,Vector(relation,relation)))
    val selected = right(RelationRdm.allDistinctOrdered(partitions)).edges.head.left
    val foreignErrors = typeCheckErrors("""
      import scalafim.fmri.mvpa.*
      import scalafim.fmri.mvpa.relation.*
      val p: AxisRef[String] = ???
      val other: AxisRef[String] = ???
      val e: AxisRef[String] = ???
      val n: AxisRef[String] = ???
      val set: RelationSet[p.Id,e.Id,n.Id] = ???
      val coordinate: PartitionCoordinate[other.Id,String] = ???
      OrdinaryRelationGeometry.compute(set,coordinate,RdmMethod.Euclidean,RelationConsumerBudget(64,4096))
    """)
    val owned = set(RelationAccess.OwnedReplay("poison-provider"),false)
    assert(foreignErrors.nonEmpty)
    assert(OrdinaryRelationGeometry.compute(owned,selected,RdmMethod.Euclidean,RelationConsumerBudget(0,0)).isLeft)
    assert(OrdinaryRelationGeometry.compute(set(RelationAccess.OneShot,false),selected,RdmMethod.Euclidean,budget).isLeft)
    assert(OrdinaryRelationGeometry.compute(set(RelationAccess.OwnedReplay("poison-provider"),true),selected,RdmMethod.Euclidean,budget).isLeft)
    assertEquals(reads,0)

  test("native mean compilation preserves frozen squared, unsquared, normalized and correlation fixtures"):
    val sourceId = SourceId.unsafe("native-parity-literal")
    val source = right(EvidenceSource(sourceId,Provenance.source(ProvenanceId.unsafe("native-parity-root"),sourceId)))
    val cases = Vector(
      (MvpaParityFixtures.Rdm.patterns,RdmMethod.SquaredEuclidean(),MvpaParityFixtures.Rdm.squaredEuclidean),
      (MvpaParityFixtures.Rdm.patterns,RdmMethod.SquaredEuclidean(true),MvpaParityFixtures.Rdm.squaredEuclideanNormalized),
      (MvpaParityFixtures.Rdm.patterns,RdmMethod.Euclidean,MvpaParityFixtures.Rdm.euclidean),
      (MvpaParityFixtures.Correlation.patterns,RdmMethod.Correlation,MvpaParityFixtures.Correlation.distances)
    )
    cases.zipWithIndex.foreach: (fixture,index) =>
      val (values,method,expected) = fixture
      val samples = axis(s"samples-$index",Vector.tabulate(6)(_.toString))
      val partitions = axis(s"runs-$index",Vector("r1","r2"))
      val effects = axis(s"effects-$index",Vector("a","b","c"))
      val neural = axis(s"neural-$index",Vector.tabulate(values.cols)(_.toString))
      val groups = right(Column.fromValues(samples,Vector("r1","r1","r1","r2","r2","r2"),ValueIdentity.source(ValueId.unsafe("fixture-groups"))))
      val conditions = right(Column.fromValues(samples,Vector("a","b","c","a","b","c"),ValueIdentity.source(ValueId.unsafe("fixture-conditions"))))
      val plan = right(ObservationMeanPlan(samples,partitions,effects,groups,conditions))
      val doubled = DMat.tabulate(6,values.cols)((row,column) => values(row%3,column))
      val set = right(ObservationMeanRelations.fromOwnedDense(plan,neural,doubled,source,RelationSource("shared","literal","cell-mean","fixed","none"),"owned-parity-fixture",ObservationMeanBudget(4096)))
      val selected = right(RelationRdm.allDistinctOrdered(partitions)).edges.head.left
      val result = right(OrdinaryRelationGeometry.compute(set,selected,method,budget))
      result.observed.rdm.values.zip(expected).foreach((actual,reference) => assertEqualsDouble(actual,reference,1e-12))
