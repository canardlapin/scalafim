package scalafim.fmri.group

import scalafim.fmri.fit.estimates.FitGroupAdapter

import gale.linalg.DVec
import scalafim.dataset.SubjectId
import scalafim.fmri.fit.{ResidualDegreesOfFreedom, TContrastResult}
import scalafim.image.SampleSpaces

class FirstLevelBridgeSuite extends munit.FunSuite:

  private def value[A](e: Either[GroupError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def contrastResult(estimate: Double, se: Double): TContrastResult =
    TContrastResult(
      name = "faces",
      estimates = DVec.fromSeq(Seq(estimate)),
      standardErrors = DVec.fromSeq(Seq(se)),
      statistics = DVec.fromSeq(Seq(estimate / se)),
      residualDegreesOfFreedom = ResidualDegreesOfFreedom.unsafe(100),
      voxelIndices = Vector(0)
    )

  private val subjects = Vector("s1", "s2", "s3").map(SubjectId(_))

  test("bridge assembles a meta-analysis-ready cube from first-level results") {
    // effects 0.4/0.5/0.6, all se 0.2 -> FE mean 0.5, se 1/sqrt(75).
    val results = Map(
      (subjects(0), "faces") -> contrastResult(0.4, 0.2),
      (subjects(1), "faces") -> contrastResult(0.5, 0.2),
      (subjects(2), "faces") -> contrastResult(0.6, 0.2)
    )
    val data = value(FitGroupAdapter.groupData(GroupSpace.SampleAxis(1), subjects, Vector("faces"), results))
    assert(data.hasVariances)

    val fit = value(GroupEngine.fit(value(GroupModel.build(data, GroupDesign.intercept(3), GroupWeighting.InverseVariance)))).fit("faces").get
    assertEqualsDouble(fit.coefficients(0, 0), 0.5, 1e-12)
    assertEqualsDouble(fit.standardErrors(0, 0), 1.0 / math.sqrt(75.0), 1e-12)
  }

  test("bridge reports a missing subject/contrast cell") {
    val results = Map(
      (subjects(0), "faces") -> contrastResult(0.4, 0.2),
      (subjects(1), "faces") -> contrastResult(0.5, 0.2)
    )
    assertEquals(
      FitGroupAdapter.groupData(GroupSpace.SampleAxis(1), subjects, Vector("faces"), results).left.toOption,
      Some(GroupError.MissingSubjectContrast("s3", "faces"))
    )
  }

  test("bridge rejects a result that does not span the sample space") {
    val results = Map(
      (subjects(0), "faces") -> contrastResult(0.4, 0.2),
      (subjects(1), "faces") -> contrastResult(0.5, 0.2),
      (subjects(2), "faces") -> contrastResult(0.6, 0.2)
    )
    assertEquals(
      FitGroupAdapter.groupData(GroupSpace.SampleAxis(2), subjects, Vector("faces"), results).left.toOption,
      Some(GroupError.sampleMismatch(2, 1))
    )
  }

  test("bridge reorders by voxel identity and keeps subject-bound residual df") {
    val subject = subjects.head
    val result = TContrastResult(
      name = "faces",
      estimates = DVec.fromSeq(Seq(10.0, 20.0)),
      standardErrors = DVec.fromSeq(Seq(2.0, 4.0)),
      statistics = DVec.fromSeq(Seq(5.0, 5.0)),
      residualDegreesOfFreedom = ResidualDegreesOfFreedom.unsafe(17),
      voxelIndices = Vector(5, 2)
    )
    val data = value(FitGroupAdapter.groupData(
      GroupSpace.VoxelAxis(SampleSpaces(Vector(6, 1, 1)), Vector(2, 5)),
      Vector(subject), Vector("faces"), Map((subject, "faces") -> result)))
    val response = data.response("faces").get
    assertEqualsDouble(response.effects(0, 0), 20.0, 0.0)
    assertEqualsDouble(response.effects(0, 1), 10.0, 0.0)
    assertEqualsDouble(response.variances.get(0, 0), 16.0, 0.0)
    assertEqualsDouble(response.variances.get(0, 1), 4.0, 0.0)
    val source = data.uncertainty.get.sources.head
    assertEquals(source.samples, Vector(2, 5))
    assertEquals(source.origin,
      GroupVarianceOrigin.Estimated(GroupDegreesOfFreedom(scalafim.estimates.DfRole.Residual,
        GroupDfValues.Scalar(17.0), "native TContrastResult residual degrees of freedom", false)))
    assert(source.fit.serialCorrelation.isInstanceOf[scalafim.estimates.ScientificFact.Unknown])
  }

  test("bridge refuses named-contrast disagreement and invalid standard errors") {
    val subject = subjects.head
    val wrongName = contrastResult(0.4, 0.2).copy(name = "objects")
    assertEquals(
      FitGroupAdapter.groupData(GroupSpace.SampleAxis(1), Vector(subject), Vector("faces"),
        Map((subject, "faces") -> wrongName)).left.toOption,
      Some(GroupError.ContrastIdentityMismatch(subject.value, "faces", "objects"))
    )
    val invalidSe = contrastResult(0.4, 0.2).copy(standardErrors = DVec.fromSeq(Seq(0.0)))
    assertEquals(
      FitGroupAdapter.groupData(GroupSpace.SampleAxis(1), Vector(subject), Vector("faces"),
        Map((subject, "faces") -> invalidSe)).left.toOption,
      Some(GroupError.InvalidStandardError(subject.value, "faces", 0, 0.0))
    )
  }
