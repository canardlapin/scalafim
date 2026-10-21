package scalafim.fmri.mvpa.artifacts

import gale.linalg.DMat
import mvpa4s.pattern.*
import scalafim.fmri.group.{GroupDesign, GroupVarianceOrigin, MetaInference, TauEstimator}
import scalafim.fmri.mvpa.group.*
import scalafim.scenarios.{ScenarioHarness, ScenarioTolerance}

/** Cross-domain receipt checks. The group covariance export remains explicitly
  * unavailable: the metadata envelope below is an attempted-fit declaration,
  * never a stored numerical group artifact. JVM archive IO has its own court. */
class GroupArchiveHandoffScenarioSuite extends SubjectGroupScenarioFixture:
  test("actual loading covariance survives group handoff with exact archive declaration and capability refusals"):
    val context = new Context(DMat.dense(2, 2, Vector(1.0, .5, 0.0, 1.0)))
    val subjects = Vector("A", "B").map(name => new context.Subject(name, DMat.dense(2, 2, Vector(1.0, 1.0, 0.0, 1.0))))
    val inputs = subjects.zipWithIndex.map: (subject, i) =>
      val residual = right(LoadingResidualCovariance.bind(subject.loading, subject.jointLaw,
        DMat.dense(2, 2, Vector(.5, .125, .125, .125)), value(s"actual-residual-$i"), "Walsh residual cross-products; df=4"))
      right(subject.loadingSubject(residual))
    val original = right(SubjectGroupBridge.eager(context.shared, context.domain, subjects.map(_.key), inputs))
    val restored = right(SubjectGroupBridge.fromMaterialized(right(original.materialize())))
    val model = right(restored.marginalModel(GroupDesign.intercept(2), restored.data.subjects,
      SubjectGroupCalculation.ApproximateMixedEffects(TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung,
        "numerical integration only; estimated-SE calibration limits remain applicable")))
    val fitted = right(model.fit())
    val nativeFits = fitted.native.contrasts.map(name => fitted.native.fit(name).get)
    val groupMeans = Vector.tabulate(4)(i => nativeFits(i % 2).coefficients(0, i / 2))
    val expectedCovariance = Vector(.078125, -.03125, .01953125, -.0078125,
      -.03125, .0625, -.0078125, .015625, .01953125, -.0078125, .01953125, -.0078125,
      -.0078125, .015625, -.0078125, .015625)
    val numeric = restored.rows.zipWithIndex.flatMap: (row, i) =>
      ScenarioHarness.values(s"subject-$i-estimates", Vector.tabulate(4)(j => row.estimates(j / 2, j % 2)),
        Vector(2.5, -1.0, -2.5, 3.0), ScenarioTolerance.absolute(1e-10)) ++
      ScenarioHarness.values(s"subject-$i-covariance", Vector.tabulate(16)(j => row.covariance(j / 4, j % 4)),
        expectedCovariance, ScenarioTolerance.absolute(1e-10))
    val expectedSubjects = Vector("A", "B")
    val declaration = PatternScientificDeclaration(context.shared.identity, context.domain.axis.stableKey,
      restored.identity, Vector("brain-A" -> subjects(0).brain.identity.toString, "brain-B" -> subjects(1).brain.identity.toString),
      23L, "ordered-subject-handoff/v1", expectedSubjects, expectedSubjects)
    val metadata = PatternProfileMetadata.document(ujson.Obj("Plan" -> declaration.planIdentity,
      "Value" -> declaration.valueIdentity, "Subjects" -> ujson.Arr("A", "B"),
      "CovarianceOrigin" -> "Estimated", "DfRole" -> "Residual", "NumericalExport" -> "unavailable"))
    val expectedMetadata = s"""{
  "Schema": "scalafim-mvpa-pattern-profile-v1",
  "Content": {
    "Plan": "${declaration.planIdentity}",
    "Value": "${declaration.valueIdentity}",
    "Subjects": [
      "A",
      "B"
    ],
    "CovarianceOrigin": "Estimated",
    "DfRole": "Residual",
    "NumericalExport": "unavailable"
  }
}
"""
    val result = ScenarioHarness.result("u4-loading-group-archive-handoff", numeric ++
      ScenarioHarness.values("delegated-group-means", groupMeans, Vector(2.5, -1.0, -2.5, 3.0), ScenarioTolerance.absolute(1e-10)) ++ Vector(
      ScenarioHarness.fact("ordered-subjects", restored.subjectKeys.map(_.value) == expectedSubjects, restored.subjectKeys.toString),
      ScenarioHarness.fact("retained-identity", restored.identity == original.identity, restored.identity),
      ScenarioHarness.fact("actual-sources", restored.rows.zip(subjects).forall((row, subject) =>
        row.source.coordinates.source.brainEvidence == subject.loading.brainEvidenceIdentity &&
          row.source.coordinates.source.targetEvidence == subject.loading.targetEvidenceIdentity), "actual loading brain and target identities"),
      ScenarioHarness.fact("covariance-and-df-roles", restored.rows.forall(row =>
        row.source.coordinates.degreesOfFreedom == SubjectCoordinateDf.Known(4.0, "VoxelLoadingConfirmation residual df n - nuisance/task rank") &&
          row.source.coordinates.degreesOfFreedomRole == SubjectCoordinateDfRole.Residual &&
          row.source.uncertainty.origin.isInstanceOf[GroupVarianceOrigin.Estimated]), "known residual df does not promote estimated covariance"),
      ScenarioHarness.fact("wrong-subject-order-refusal", SubjectGroupBridge.eager(context.shared, context.domain,
        subjects.reverse.map(_.key), inputs).isLeft, "no positional subject relabelling"),
      ScenarioHarness.fact("known-variance-refusal", restored.marginalModel(GroupDesign.intercept(2), restored.data.subjects,
        SubjectGroupCalculation.KnownVarianceGaussianFixedEffects).isLeft, "estimated covariance cannot enter a known-variance reference"),
      ScenarioHarness.fact("exact-envelope", metadata == expectedMetadata, "unchanged v1 UTF-8 framing including trailing newline"),
      ScenarioHarness.fact("metadata-validation", PatternProfileMetadata.content(metadata, 8192).isRight, "shared metadata admission"),
      ScenarioHarness.fact("numerical-export-refusal", restored.durableEstimateExport.isLeft && restored.jointInference.isLeft,
        "no unsupported group covariance export or joint inference"),
      ScenarioHarness.fact("js-io-capability-refusal", PatternProfileMetadata.durableReadUnavailable.isLeft,
        "the shared court supplies no local-filesystem claim")))
    assert(result.ciPass, result.render)
