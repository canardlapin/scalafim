package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import org.scalacheck.Gen
import org.scalacheck.Prop.all
import org.scalacheck.Prop.forAll
import org.scalacheck.rng.Seed as ScalaCheckSeed
import resample4s.core.*
import resample4s.designs.*

private[mvpa] final case class DesignLawCase(
    sampleCount: Int,
    folds: Int,
    salt: Int,
    seed: Long
)

private[mvpa] object DesignLawCase:
  val generated: Gen[DesignLawCase] =
    for
      sampleCount <- Gen.choose(6, 16)
      folds <- Gen.choose(2, math.min(4, sampleCount / 2))
      salt <- Gen.choose(1, 1000000)
      seed <- Gen.choose(Long.MinValue, Long.MaxValue)
    yield DesignLawCase(sampleCount, folds, salt, seed)

private[mvpa] final case class DesignCourtObservation(
    expectedSamples: Vector[SampleId],
    assessedSamples: Vector[SampleId],
    forwardEdges: Vector[(PartitionId, PartitionId)],
    reversedEdges: Vector[(PartitionId, PartitionId)],
    roundTripEdges: Vector[(PartitionId, PartitionId)],
    expectedActions: Int,
    actionKeys: Vector[UnitKey],
    replayStable: Boolean,
    nestedReconstructionValid: Boolean
)

private[mvpa] object DesignLawCourt:
  def violations(value: DesignCourtObservation): Set[String] =
    val output = Set.newBuilder[String]
    if value.assessedSamples != value.expectedSamples then output += "exact-once-reconstruction"
    if value.reversedEdges != value.forwardEdges.map(_.swap).sortBy(edge => (edge._1.value, edge._2.value)) then
      output += "pairing-reversal"
    if value.roundTripEdges != value.forwardEdges then output += "pairing-involution"
    if value.actionKeys.length != value.expectedActions ||
      value.actionKeys.distinct.length != value.expectedActions
    then output += "action-multiplicity"
    if !value.replayStable then output += "seed-replay"
    if !value.nestedReconstructionValid then output += "nested-reconstruction"
    output.result()

class DesignLawsSuite extends MvpaGeneratedLawSuite:
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
    val compiled = right(
      design.compile(right(IndexSpace.of(samples.size)), authority.seed)
    )
    right(
      BoundSchedule(
        compiled,
        samples,
        right(AxisPopulationFingerprint.fromAxis(samples)),
        right(ScheduleLabels.fromDesign(samples, design)),
        authority
      )
    )

  private def partitions(count: Int, salt: Int): AxisRef[PartitionId] =
    right(
      AxisRef.create(
        AxisId.unsafe(s"law-partitions-$salt"),
        AxisPurpose.Partitions,
        Vector.tabulate(count)(index => PartitionId.unsafe(s"run-${index + 1}")),
        CoordinateBasis.unsafe("partition-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("design-law-court", s"case-$salt")
      )
    )

  private def validation[S <: SemanticSpace](
      samples: AxisRef.Aux[SampleId, S],
      folds: Int,
      seed: Long
  ): ValidationDesign[S, Coverage.ExactOnce, BoundSelectionSplit[SampleId, S]] =
    val schedule = bound(
      KFold.ordered(folds),
      samples,
      SeedAuthority.fromLong(SeedDomain.Validation, seed)
    )
    right(
      ValidationDesign(
        schedule,
        SamplesName,
        GeneralizationAxis(SamplesName, samples.identity)
      )
    )

  property("design wrappers obey identity, composition, coverage, nesting, pairing, multiplicity, and seed laws"):
    forAll(DesignLawCase.generated): generated =>
      val samples = MvpaLawFixtures.sampleAxis(
        generated.sampleCount,
        generated.salt
      )
      val firstValidation = validation(samples, generated.folds, generated.seed)
      val replayValidation = validation(samples, generated.folds, generated.seed)
      val outOfFold = right(firstValidation.outOfFold)
      val crossFit = right(CrossFitDesign.targetBlind(firstValidation))
      val crossFitOutOfFold = right(crossFit.outOfFold)

      val nestedSchedule = bound(
        NestedCrossValidation(generated.folds, innerFolds = 2),
        samples,
        SeedAuthority.fromLong(SeedDomain.CrossFit, generated.seed)
      )
      val nestedValidation = right(
        ValidationDesign(
          nestedSchedule,
          SamplesName,
          GeneralizationAxis(SamplesName, samples.identity)
        )
      )
      val targetAware = right(CrossFitDesign.targetAware(nestedValidation))
      val nestedValid = nestedSchedule.iterator.forall:
        case (_, Left(_))      => false
        case (_, Right(outer)) =>
          outer.inner.iterator.forall:
            case (_, Left(_))      => false
            case (_, Right(inner)) =>
              val reconstructed =
                (inner.analysis.child.keys ++ inner.assessment.child.keys).toSet
              reconstructed == outer.outer.analysis.child.keys.toSet &&
              inner.assessment.child.keys.toSet
                .intersect(outer.outer.assessment.child.keys.toSet)
                .isEmpty

      val partitionAxis = partitions(generated.folds, generated.salt)
      val namedPartitions = right(
        PartitionAxis(ScientificAxisName.unsafe("runs"), partitionAxis)
      )
      val independence = right(
        PartitionIndependenceTestSupport.declareAllPairsForDesign(
          namedPartitions,
          s"generated-pairing-${generated.salt}"
        )
      )
      val forward = right(
        PairingDesign.forwardOnly(
          namedPartitions,
          PairingReducer.WeightedMean,
          GeneralizationAxis(
            ScientificAxisName.unsafe("runs"),
            partitionAxis.identity
          ),
          independence
        )
      )
      val reversed = right(forward.reverse)
      val roundTrip = right(reversed.reverse)

      all(
        outOfFold.locations.map(_.sample) == samples.keys,
        crossFitOutOfFold.locations.map(_.sample) == samples.keys,
        firstValidation.identity == replayValidation.identity,
        crossFit.validation.identity == firstValidation.identity,
        crossFit.identity != firstValidation.identity,
        right(targetAware.outOfFold).locations.map(_.sample) == samples.keys,
        nestedValid,
        reversed.edges.map(edge => edge.left -> edge.right) ==
          forward.edges
            .map(edge => edge.right -> edge.left)
            .sortBy(edge => (edge._1.value, edge._2.value)),
        roundTrip.edges == forward.edges,
        roundTrip.identity == forward.identity
      )

  test("negative type and bind cases prevent design-capability forgery"):
    val coverageForgery = compileErrors("""
      import scalafim.fmri.mvpa.*
      import multivar.core.SemanticSpace
      import resample4s.core.Coverage

      def invalid[S <: SemanticSpace, Unit](
          design: ValidationDesign[S, Coverage, Unit]
      ) = design.outOfFold
    """)
    assert(coverageForgery.contains("ExactOnceEvidence"))

    val samples = MvpaLawFixtures.sampleAxis(8, 2101)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 111L)
    val compiled = right(
      KFold.ordered(2).compile(right(IndexSpace.of(6)), authority.seed)
    )
    val mismatch = BoundSchedule(
      compiled,
      samples,
      right(AxisPopulationFingerprint.fromAxis(samples)),
      right(ScheduleLabels.fromDesign(samples, KFold.ordered(2))),
      authority
    )
    assert(mismatch.left.exists:
      case BoundScheduleError.InvalidUnit(
            _,
            ScheduleUnitError.PopulationMismatch(_, 8, 6)
          ) =>
        true
      case _ => false)

  test("the design law court rejects deliberately broken wrappers"):
    val samples = MvpaLawFixtures.sampleAxis(8, 2201)
    val design = validation(samples, folds = 4, seed = 113L)
    val assessed = right(design.outOfFold).locations.map(_.sample)
    val partitionAxis = partitions(4, 2201)
    val namedPartitions = right(
      PartitionAxis(ScientificAxisName.unsafe("runs"), partitionAxis)
    )
    val pairing = right(
      PairingDesign.forwardOnly(
        namedPartitions,
        PairingReducer.WeightedMean,
        GeneralizationAxis(
          ScientificAxisName.unsafe("runs"),
          partitionAxis.identity
        ),
        right(
          PartitionIndependenceTestSupport.declareAllPairsForDesign(
            namedPartitions,
            "broken-wrapper-court"
          )
        )
      )
    )
    val reversed = right(pairing.reverse)
    val roundTrip = right(reversed.reverse)
    val lawful = DesignCourtObservation(
      samples.keys,
      assessed,
      pairing.edges.map(edge => edge.left -> edge.right),
      reversed.edges.map(edge => edge.left -> edge.right),
      roundTrip.edges.map(edge => edge.left -> edge.right),
      expectedActions = 3,
      actionKeys = Vector.tabulate(3)(repeat => UnitKey(repeat, 0)),
      replayStable = true,
      nestedReconstructionValid = true
    )
    val droppedAssessment = lawful.copy(
      assessedSamples = lawful.assessedSamples.dropRight(1)
    )
    val collapsedDirection = lawful.copy(
      reversedEdges = lawful.forwardEdges
    )
    val collapsedMultiplicity = lawful.copy(
      actionKeys = lawful.actionKeys.dropRight(1)
    )
    val unstableSeed = lawful.copy(replayStable = false)
    val brokenNested = lawful.copy(nestedReconstructionValid = false)

    assertEquals(DesignLawCourt.violations(lawful), Set.empty[String])
    assert(
      DesignLawCourt
        .violations(droppedAssessment)
        .contains("exact-once-reconstruction")
    )
    assert(
      DesignLawCourt
        .violations(collapsedDirection)
        .contains("pairing-reversal")
    )
    assert(
      DesignLawCourt
        .violations(collapsedMultiplicity)
        .contains("action-multiplicity")
    )
    assert(DesignLawCourt.violations(unstableSeed).contains("seed-replay"))
    assert(
      DesignLawCourt
        .violations(brokenNested)
        .contains("nested-reconstruction")
    )

  test("design laws share the reproducible cross-platform law budget"):
    assert(ScalaCheckSeed.fromBase64(MvpaLawProfile.initialSeed).isSuccess)
    assertEquals(scalaCheckInitialSeed, MvpaLawProfile.initialSeed)
    assertEquals(scalaCheckTestParameters.workers, 1)
    assertEquals(
      scalaCheckTestParameters.minSuccessfulTests,
      MvpaLawProfile.current.successfulTests
    )
    assertEquals(
      scalaCheckTestParameters.maxSize,
      MvpaLawProfile.current.maximumSize
    )
