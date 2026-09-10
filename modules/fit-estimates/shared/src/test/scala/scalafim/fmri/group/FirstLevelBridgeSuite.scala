package scalafim.fmri.group

import scalafim.fmri.fit.estimates.FitGroupAdapter

import gale.linalg.DVec
import scalafim.dataset.SubjectId
import scalafim.fmri.fit.{ResidualDegreesOfFreedom, TContrastResult}

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
