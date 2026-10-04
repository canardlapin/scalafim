package scalafim.fmri.mvpa.relation

import gale.linalg.{DMat,DVec,DoubleLinearOperator,MutableDVec}
import multivar.core.{SemanticSpace,SpaceRole,ValueId,ValueIdentity}
import resample4s.core.{IndexSpace,Injection}
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.measurement.*
import scalafim.response.{Provenance,ProvenanceId,SourceId}

class MeasuredMeanRelationSuite extends munit.FunSuite:
  private def right[E,A](value: Either[E,A]): A = value.fold(error => fail(error.toString),scala.Predef.identity)
  private def axis(name: String,role: SpaceRole,keys: Vector[String]): AxisRef[String] =
    right(AxisRef.fromStableKeys(name,role,keys,"fixture","unit","raw"))
  private def identity(name: String) = ValueIdentity.source(ValueId.unsafe(name))

  test("a real frame preserves selected order and bounded origins through scoped ordinary and signed RSA"):
    val samples = axis("samples",SpaceRole.Samples,Vector("r1a","r1b","r1c","r2a","r2b","r2c"))
    val partitions = axis("runs",SpaceRole.Samples,Vector("r1","r2"))
    val effects = axis("effects",SpaceRole.Latent,Vector("a","b","c"))
    val neural = axis("neural",SpaceRole.Observed,Vector("n0","n1","n2"))
    val groups = right(Column.fromValues(samples,Vector("r1","r1","r1","r2","r2","r2"),identity("groups")))
    val conditions = right(Column.fromValues(samples,Vector("a","b","c","a","b","c"),identity("conditions")))
    val plan = right(ObservationMeanPlan(samples,partitions,effects,groups,conditions))
    val sourceId = SourceId.unsafe("bounded-fixture")
    val evidence = right(EvidenceSource(sourceId,Provenance.source(ProvenanceId.unsafe("fixture-root"),sourceId)))
    val valuesId = identity("bounded-values")
    val direct = ValueSupport.Bounded(samples.descriptor,valuesId,Vector(0,1,2,3,4,5))
    val support = right(EvidenceOrigins.make(evidence,valuesId,AcquisitionCoordinates.OriginalTemporalAxis(samples.descriptor),direct,PreparationSupport.FixedShared(direct),samples.descriptor))
    val values = DMat.dense(6,3,Vector(1.0,10.0,100.0,2.0,20.0,200.0,4.0,40.0,400.0,3.0,30.0,300.0,5.0,50.0,500.0,8.0,80.0,800.0))
    var reads = 0
    var frameOpen = false
    var frameCloses = 0
    var meanAcquires = 0
    var meanCloses = 0
    val counted = new DoubleLinearOperator:
      val rows = 6
      val cols = 3
      def applyTo(input: DVec,output: MutableDVec): Unit =
        assert(frameOpen)
        reads += 1
        values.applyTo(input,output)
      override def transposeApplyTo(input: DVec,output: MutableDVec): Unit =
        assert(frameOpen)
        reads += 1
        values.transposeApplyTo(input,output)
    val observations = right(Observations.fromOperator(samples,neural,counted,valuesId,evidence,support))
    val population = right(IndexSpace.of(3))
    val selected = right(MeasurementLeg.hardSelection(neural,MeasurementId.unsafe("a-selected"),right(Injection.from(IArray(2,0),population))))
    val other = right(MeasurementLeg.hardSelection(neural,MeasurementId.unsafe("b-other"),right(Injection.from(IArray(1),population))))
    val frame = right(MeasurementFrame(neural,Vector(PackedMeasurementEntry(selected,"selected"),PackedMeasurementEntry(other,"other"))))
    assertEquals(reads,0)
    var escaped: () => Unit = () => fail("selected entry was not visited")
    val visitor = new MeasurementVisitor[samples.Id,neural.Id,String,String,Unit]:
      def visit[L <: SemanticSpace](entry: PackedMeasurementEntry[neural.Id,String,String] { type Local = L },measured: MeasuredObservations[samples.Id,L]): Either[MeasurementFailure,Unit] =
        val before = reads
        val identified = right(entry.measurement.measureIdentified(samples,observations))
        assertEquals(reads,before)
        assertEquals(identified.origins,measured.origins)
        val resource = new ObservationMeanResource:
          def acquire(): Either[String,Unit] =
            meanAcquires += 1
            if frameOpen then Right(()) else Left("frame is closed")
          def close(): Either[String,Unit] =
            meanCloses += 1
            Right(())
        ObservationMeanRelations.withSource(plan,entry.measurement.local,
          ObservationMeanSource.scopedReplay(identified,ObservationMeanOrigins("common-acquisition","response","mean","fixed","none",ObservationMeanAccess.ScopedReplay("repeatable-fixture-within-frame",entry.measurement.descriptor.semanticId)),resource),ObservationMeanBudget(4096)): relations =>
          assert(relations.relations.forall(relation => relation.origins.support == support.reindexOutput(effects.descriptor,relation.estimate.valueIdentity)))
          val pairing = right(RelationRdm.allDistinctOrdered(partitions))
          if entry.rendition == "selected" then
            val first = right(relations.relations.head.estimate(DMat.eye(2)))
            assertEqualsDouble(first(0,0),100.0,1e-12)
            assertEqualsDouble(first(0,1),1.0,1e-12)
            assertEqualsDouble(first(2,0),400.0,1e-12)
            val ordinary = right(OrdinaryRelationGeometry.compute(relations,pairing.edges.head.left,RdmMethod.Euclidean,RelationConsumerBudget(64,4096)))
            val expected = Vector(1.0,3.0,2.0).map(_*math.sqrt(10001.0))
            ordinary.observed.rdm.values.zip(expected).foreach((a,b) => assertEqualsDouble(a,b,1e-12))
            val model = RdmModel.unsafe("ordinary-geometry",Vector("c","b","a"),RdmVector.unsafe(3,Vector(2.0,3.0,1.0)))
            assertEqualsDouble(right(ordinary.score(model,RdmScorer.Pearson)),1.0,1e-12)
            val signed = right(RelationRdm.identity(relations,pairing))
            signed.cells.zip(Vector(10001.0,75007.5,30003.0)).foreach:
              case (RelationRdmCell.Estimated(value),reference) => assertEqualsDouble(value,reference,1e-12)
              case other => fail(other.toString)
            assert(signed.pairing.claim.isInstanceOf[PairingClaim.Descriptive])
            escaped = () =>
              relations.relations.head.estimate(DMat.eye(2))
              ()
          else
            val ordinary = right(OrdinaryRelationGeometry.compute(relations,pairing.edges.head.left,RdmMethod.Euclidean,RelationConsumerBudget(64,4096)))
            ordinary.observed.rdm.values.zip(Vector(10.0,30.0,20.0)).foreach((a,b) => assertEqualsDouble(a,b,1e-12))
          Right(())
        .left.map(error => MeasurementFailure.Task(error.message))
    def closeFrame(): Unit =
      frameOpen = false
      frameCloses += 1
    def openFrame: Either[MeasurementError,MeasurementResource[Observations[samples.Id,neural.Id]]] =
      frameOpen = true
      Right(MeasurementResource(observations)(closeFrame()))
    val result = frame.traverse(1)(openFrame)(visitor)
    assertEquals(result.error,None)
    assertEquals(result.value.map(_.value),Vector(Right(()),Right(())))
    assert(reads > 0)
    assertEquals(meanAcquires,2)
    assertEquals(meanCloses,2)
    assertEquals(frameCloses,1)
    val after = reads
    intercept[IllegalStateException](escaped())
    assertEquals(reads,after)
