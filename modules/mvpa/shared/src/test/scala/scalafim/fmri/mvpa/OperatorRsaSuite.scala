package scalafim.fmri.mvpa

import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.relation.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Native observation means and operator queries; no generic ROI executor. */
class OperatorRsaSuite extends munit.FunSuite:
  private def right[E,A](value: Either[E,A]): A = value.fold(error => fail(error.toString),identity)
  private def axis(name: String, keys: Vector[String]): AxisRef[String] =
    right(AxisRef.fromStableKeys(name,SpaceRole.Observed,keys,"fixture","unit","raw"))
  private val values = DMat.dense(12,5,Vector(0.0,0.2,0.0,0.1,-0.1,1.0,0.1,0.5,0.0,0.2,0.0,2.0,0.2,-0.1,0.1,1.5,1.0,-0.2,0.3,0.0,0.1,0.0,0.1,0.2,-0.2,1.2,0.2,0.4,-0.1,0.1,-0.1,1.8,0.3,0.0,0.2,1.4,1.1,-0.1,0.2,-0.1,-0.1,0.1,-0.1,0.0,0.0,0.9,-0.1,0.6,0.1,0.3,0.2,2.1,0.1,-0.2,0.0,1.6,0.9,-0.3,0.4,0.1))

  test("identified partition means match independent raw signed products in frozen condensed order"):
    val samples = axis("samples",Vector.tabulate(12)(_.toString))
    val partitions = axis("runs",Vector("r0","r1","r2"))
    val effects = axis("conditions",Vector("a","b","c","d"))
    val neural = axis("neural",Vector.tabulate(5)(_.toString))
    val partition = right(Column.fromValues(samples,Vector.tabulate(12)(i => s"r${i/4}"),ValueIdentity.source(ValueId.unsafe("runs"))))
    val condition = right(Column.fromValues(samples,Vector.fill(3)(Vector("a","b","c","d")).flatten,ValueIdentity.source(ValueId.unsafe("conditions"))))
    val plan = right(ObservationMeanPlan(samples,partitions,effects,partition,condition))
    val sourceId = SourceId.unsafe("owned-response-fixture")
    val source = right(EvidenceSource(sourceId,Provenance.source(ProvenanceId.unsafe("fixture-root"),sourceId)))
    val relations = right(ObservationMeanRelations.fromOwnedDense(plan,neural,values,source,RelationSource("shared-response","fixture-columns","mean-v1","fixed","none"),"copied-fixture",ObservationMeanBudget(4096)))
    val pairing = right(RelationRdm.allDistinctOrdered(partitions))
    val result = right(RelationRdm.identity(relations,pairing))
    val pairs = Vector((1,0),(2,0),(3,0),(2,1),(3,1),(3,2))
    val expected = pairs.map: (a,b) =>
      var sum = 0.0
      for r <- 0 until 3; q <- 0 until 3 if r != q; f <- 0 until 5 do
        sum += (values(r*4+a,f)-values(r*4+b,f))*(values(q*4+a,f)-values(q*4+b,f))
      sum / 30.0
    result.cells.zip(expected).foreach:
      case (RelationRdmCell.Estimated(actual),reference) => assertEqualsDouble(actual,reference,1e-12)
      case other => fail(other.toString)
    assertEquals(result.effectKeys,Vector("a","b","c","d"))
    assert(result.pairing.claim.isInstanceOf[PairingClaim.Descriptive])
    val ordinary = right(OrdinaryRelationGeometry.compute(relations,pairing.edges.head.left,RdmMethod.Euclidean,RelationConsumerBudget(64,4096)))
    // Literal first partition differences: a=(0,.2,0,.1,-.1),
    // b=(1,.1,.5,0,.2), so squared distance is 1.36.
    assertEqualsDouble(ordinary.observed.rdm.values.head,math.sqrt(1.36),1e-12)
