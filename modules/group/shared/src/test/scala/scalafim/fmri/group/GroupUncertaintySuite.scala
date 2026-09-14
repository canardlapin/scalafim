package scalafim.fmri.group

import scalafim.dataset.SubjectId
import scalafim.estimates.{DfRole, ScientificFact}

class GroupUncertaintySuite extends munit.FunSuite:
  private val subjects = Vector(SubjectId("s1"), SubjectId("s2"))
  private val space = GroupSpace.SampleAxis(2)
  private val response = GroupResponse.unsafeWeighted(
    GroupTestMatrix.fromRows(Seq(Seq(1.0, 2.0), Seq(3.0, 4.0))),
    GroupTestMatrix.fromRows(Seq(Seq(0.1, 0.2), Seq(0.3, 0.4))))

  private def right[A](value: Either[GroupError, A]): A = value.fold(error => fail(error.message), identity)

  test("weighted construction preserves an explicit unknown receipt rather than inventing df") {
    val data = right(GroupData.build(subjects, space, Vector("faces" -> response)))
    val receipt = data.uncertainty.get
    assertEquals(receipt.sources.map(_.subject), subjects)
    assertEquals(receipt.sources.map(_.samples), Vector(Vector(0, 1), Vector(0, 1)))
    assert(receipt.sources.forall(_.origin.isInstanceOf[GroupVarianceOrigin.Unknown]))
    assert(receipt.geometry.isInstanceOf[GroupGeometryEvidence.Unknown])
  }

  test("receipt axes are exact and effects-only data refuses variance provenance") {
    val unknown = ScientificFact.Unknown("not retained by imported fixture")
    val fit = GroupFitProvenance(unknown, unknown, unknown, unknown)
    def source(subject: SubjectId) = GroupUncertaintySource(subject, "faces", Vector(0, 1), None,
      GroupVarianceOrigin.Estimated(GroupDegreesOfFreedom(DfRole.Residual, GroupDfValues.Scalar(20.0), "nominal residual df", false)),
      fit, None)
    val reordered = Vector(source(subjects(1)), source(subjects(0)))
    assert(GroupUncertaintyReceipt.make(subjects, Vector("faces"), Vector(0, 1), reordered,
      GroupGeometryEvidence.Unknown("not registered")).isLeft)

    val receipt = right(GroupUncertaintyReceipt.make(subjects, Vector("faces"), Vector(0, 1),
      subjects.map(source), GroupGeometryEvidence.Unknown("not registered")))
    val effects = GroupResponse.unsafeFromEffects(GroupTestMatrix.fromRows(Seq(Seq(1.0, 2.0), Seq(3.0, 4.0))))
    assert(GroupData.build(subjects, space, Vector("faces" -> effects), Some(receipt)).isLeft)
  }

  test("approximate df cannot masquerade as ordinary residual df") {
    intercept[IllegalArgumentException]:
      GroupDegreesOfFreedom(DfRole.Residual, GroupDfValues.Scalar(7.5), "approximation", true)
    intercept[IllegalArgumentException]:
      GroupDegreesOfFreedom(DfRole.Reference, GroupDfValues.Scalar(7.5), "t reference", false)
  }
