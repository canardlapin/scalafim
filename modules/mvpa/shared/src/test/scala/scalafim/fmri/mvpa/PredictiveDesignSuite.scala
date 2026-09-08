package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import resample4s.core.*
import resample4s.designs.*

class PredictiveDesignSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private val SamplesName = ScientificAxisName.unsafe("samples")

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private def bound[
      A,
      Cov <: Coverage,
      S <: SemanticSpace,
      Unit
  ](
      design: Design[A, Cov],
      samples: AxisRef.Aux[SampleId, S],
      authority: SeedAuthority
  )(using
      binder: ScheduleUnitBinder.Aux[A, SampleId, S, Unit]
  ): BoundSchedule[
    A,
    Cov,
    SampleId,
    S,
    Unit
  ] =
    val space = right(IndexSpace.of(samples.size))
    val compiled = right(design.compile(space, authority.seed))
    right(
      BoundSchedule(
        compiled,
        samples,
        right(AxisPopulationFingerprint.fromAxis(samples)),
        right(ScheduleLabels.fromDesign(samples, design)),
        authority
      )
    )

  private def generalization[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S]
  ): GeneralizationAxis =
    GeneralizationAxis(SamplesName, samples.identity)

  test("only exact-once validation reconstructs one assessment per SampleId"):
    val samples = MvpaLawFixtures.sampleAxis(9, 1401)
    val design = KFold.ordered(3)
    val schedule = bound(
      design,
      samples,
      SeedAuthority.fromLong(SeedDomain.Validation, 91L)
    )
    val validation = right(
      ValidationDesign(schedule, SamplesName, generalization(samples))
    )
    val reconstruction = right(validation.outOfFold)

    assertEquals(reconstruction.samples.identity, samples.identity)
    assertEquals(reconstruction.locations.map(_.sample), samples.keys)
    assertEquals(reconstruction.locations.map(_.unit).distinct.length, 3)
    samples.keys.foreach: sample =>
      assertEquals(reconstruction.location(sample).map(_.sample), Some(sample))

    val ordinaryErrors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace
      import resample4s.core.Coverage

      def invalid[S <: SemanticSpace, Unit](
          design: ValidationDesign[S, Coverage, Unit]
      ) = design.outOfFold
    """)
    val repeatedErrors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace
      import resample4s.core.Coverage

      def invalid[S <: SemanticSpace, Unit](
          design: ValidationDesign[S, Coverage.Exact, Unit]
      ) = design.outOfFold
    """)

    assert(ordinaryErrors.contains("ExactOnceEvidence"))
    assert(repeatedErrors.contains("ExactOnceEvidence"))

  test("CrossFitDesign is a separate leakage-safety capability"):
    val samples = MvpaLawFixtures.sampleAxis(9, 1501)
    val schedule = bound(
      KFold.ordered(3),
      samples,
      SeedAuthority.fromLong(SeedDomain.CrossFit, 92L)
    )
    val validation = right(
      ValidationDesign(schedule, SamplesName, generalization(samples))
    )
    val crossFit = right(CrossFitDesign.targetBlind(validation))

    assertEquals(crossFit.preparation, PreparationScope.TargetBlindAnalysis)
    assertEquals(crossFit.fitting, FittingScope.OuterAnalysis)
    assertEquals(right(crossFit.outOfFold).locations.map(_.sample), samples.keys)
    assertNotEquals(crossFit.identity, validation.identity)

    val ordinaryCannotMint = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace
      import resample4s.core.Coverage

      def invalid[S <: SemanticSpace, Unit](
          validation: ValidationDesign[S, Coverage, Unit]
      ) = CrossFitDesign.targetBlind(validation)
    """)
    val ordinaryCannotClaimTargetAware = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace
      import resample4s.core.Coverage

      def invalid[S <: SemanticSpace](
          validation: ValidationDesign[
            S,
            Coverage.ExactOnce,
            BoundSelectionSplit[SampleId, S]
          ]
      ) = CrossFitDesign.targetAware(validation)
    """)

    assert(ordinaryCannotMint.nonEmpty)
    assert(ordinaryCannotClaimTargetAware.nonEmpty)

  test("nested outer and inner roles remain explicit on the source sample axis"):
    val samples = MvpaLawFixtures.sampleAxis(12, 1601)
    val nestedSchedule = bound(
      NestedCrossValidation(outerFolds = 3, innerFolds = 2),
      samples,
      SeedAuthority.fromLong(SeedDomain.CrossFit, 93L)
    )
    val validation = right(
      ValidationDesign(
        nestedSchedule,
        SamplesName,
        generalization(samples)
      )
    )
    val crossFit = right(CrossFitDesign.targetAware(validation))
    val outer = right(nestedSchedule.at(UnitKey(0, 0)))
    val inner = right(outer.inner.at(UnitKey(0, 0)))

    assertEquals(
      crossFit.preparation,
      PreparationScope.TargetAwareInnerCrossFit
    )
    assertEquals(
      crossFit.fitting,
      FittingScope.InnerAnalysisThenOuterRefit
    )
    assertEquals(inner.analysis.parentIdentity, samples.identity)
    assertEquals(inner.assessment.parentIdentity, samples.identity)
    assertEquals(
      (inner.analysis.child.keys ++ inner.assessment.child.keys).toSet,
      outer.outer.analysis.child.keys.toSet
    )
    assertEquals(
      inner.assessment.child.keys.toSet.intersect(
        outer.outer.assessment.child.keys.toSet
      ),
      Set.empty[SampleId]
    )
    assertEquals(right(crossFit.outOfFold).locations.map(_.sample), samples.keys)

  test("generalization identity and seed domain are scientific design inputs"):
    val samples = MvpaLawFixtures.sampleAxis(9, 1701)
    val firstSchedule = bound(
      KFold.ordered(3),
      samples,
      SeedAuthority.fromLong(SeedDomain.Validation, 94L)
    )
    val secondSchedule = bound(
      KFold.ordered(3),
      samples,
      SeedAuthority.fromLong(SeedDomain.CrossFit, 94L)
    )
    val first = right(
      ValidationDesign(firstSchedule, SamplesName, generalization(samples))
    )
    val second = right(
      ValidationDesign(secondSchedule, SamplesName, generalization(samples))
    )
    val alternativeAxis = GeneralizationAxis(
      ScientificAxisName.unsafe("trials"),
      samples.identity
    )
    val changedGeneralization = right(
      ValidationDesign(firstSchedule, SamplesName, alternativeAxis)
    )

    assertNotEquals(first.identity, second.identity)
    assertNotEquals(first.identity, changedGeneralization.identity)
    assertEquals(
      changedGeneralization.referencedAxes.map(_.name.value),
      Vector("samples", "trials")
    )
