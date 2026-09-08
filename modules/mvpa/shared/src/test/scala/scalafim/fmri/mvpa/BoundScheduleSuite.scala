package scalafim.fmri.mvpa

import resample4s.core.*
import resample4s.designs.*

private[mvpa] final class FixedInjectionDesign private (
    injection: Injection,
    descriptor: DesignDescriptor
) extends Design[Injection, Coverage]:
  override val definition: DesignDefinition[Injection, Coverage] =
    DesignDefinition.general(descriptor): context =>
      if context.space.size != injection.codomain then
        Left(DesignError.LengthMismatch(context.space.size, injection.codomain))
      else
        for
          shape <- PlanShape.of(1, 1)
          cost <- PlanCost.of(
            injection.domain.toLong,
            injection.domain.toLong,
            injection.domain.toLong
          )
        yield GeneralPlanSpec(shape, PlanDiagnostics.empty, cost)(
          _ => injection,
          FixedInjectionDesign.Encoder
        )

private[mvpa] object FixedInjectionDesign:
  val Encoder: CanonicalAssignmentEncoder[Injection] =
    new CanonicalAssignmentEncoder[Injection]:
      override def encode(
          value: Injection,
          out: CanonicalWriter
      ): Either[DigestError, Unit] =
        out
          .variant("injection")
          .flatMap: _ =>
            out
              .beginSequence(value.domain)
              .map: _ =>
                value.foreachIndex(out.int)

  def apply(injection: Injection): FixedInjectionDesign =
    val descriptor = DesignDescriptor
      .named(
        "scalafim-fixed-injection/v1",
        "domain" -> DescriptorValue.int(injection.domain),
        "population" -> DescriptorValue.int(injection.codomain)
      )
      .fold(
        error => throw new IllegalArgumentException(error.message),
        identity
      )
    new FixedInjectionDesign(injection, descriptor)

class BoundScheduleSuite extends munit.FunSuite:
  private given DigestAlgorithm = DigestAlgorithm.fnv1a64

  private def right[A](value: Either[?, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, obtained $error")

  private def labels(values: Int*): Labels =
    right(Labels.dense(IArray.unsafeFromArray(values.toArray)))

  private def compile[A, Cov <: Coverage](
      design: Design[A, Cov],
      size: Int,
      authority: SeedAuthority
  ): Compiled[A, Cov] =
    right(
      design.compile(
        right(IndexSpace.of(size)),
        authority.seed
      )
    )

  test("compiled selections bind to authoritative SampleIds with one receipt"):
    val samples = MvpaLawFixtures.sampleAxis(6, 1101)
    val design = KFold.ordered(3)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 81L)
    val compiled = compile(design, samples.size, authority)
    val population = right(AxisPopulationFingerprint.fromAxis(samples))
    val designLabels = right(ScheduleLabels.fromDesign(samples, design))
    val schedule = right(
      BoundSchedule(
        compiled,
        samples,
        population,
        designLabels,
        authority
      )
    )
    val first: BoundSelectionSplit[SampleId, samples.Id] =
      right(schedule.at(UnitKey(0, 0)))
    val freshReceipt = right(compiled.receipt(population.value))

    assertEquals(first.assessment.child.keys, Vector(samples.keys(0), samples.keys(3)))
    assertEquals(
      first.analysis.child.keys,
      Vector(samples.keys(1), samples.keys(2), samples.keys(4), samples.keys(5))
    )
    assertEquals(schedule.receipt, freshReceipt)
    assertEquals(schedule.receipt.population, population.value)
    assertEquals(schedule.seedAuthority.domain, SeedDomain.Validation)

  test("injection, draw, and permutation schedules retain kind, order, and multiplicity"):
    val samples = MvpaLawFixtures.sampleAxis(5, 1201)
    val population = right(AxisPopulationFingerprint.fromAxis(samples))

    val indexSpace = right(IndexSpace.of(samples.size))
    val injection = right(
      Injection.from(IArray.unsafeFromArray(Array(3, 0, 4)), indexSpace)
    )
    val injectionDesign = FixedInjectionDesign(injection)
    val injectionAuthority = SeedAuthority.fromLong(SeedDomain.Validation, 82L)
    val injectionSchedule = right(
      BoundSchedule(
        compile(injectionDesign, samples.size, injectionAuthority),
        samples,
        population,
        right(ScheduleLabels.fromDesign(samples, injectionDesign)),
        injectionAuthority
      )
    )
    val boundInjection: ReindexingLeg[
      samples.Id,
      SampleId,
      SampleId,
      Injection
    ] = right(injectionSchedule.at(UnitKey(0, 0)))
    assertEquals(
      boundInjection.child.keys,
      Vector(samples.keys(3), samples.keys(0), samples.keys(4))
    )

    val bootstrap = Bootstrap.unconditional(1)
    val bootstrapAuthority = SeedAuthority.fromLong(SeedDomain.Bootstrap, 83L)
    val bootstrapCompiled = compile(bootstrap, samples.size, bootstrapAuthority)
    val bootstrapSchedule = right(
      BoundSchedule(
        bootstrapCompiled,
        samples,
        population,
        right(ScheduleLabels.fromDesign(samples, bootstrap)),
        bootstrapAuthority
      )
    )
    val boundDraw: BoundDrawSplit[SampleId, samples.Id] =
      right(bootstrapSchedule.at(UnitKey(0, 0)))
    val rawDraw = bootstrapCompiled.plan.first.analysis.toVector
    assertEquals(
      boundDraw.analysis.child.keys.map(_.source),
      rawDraw.map(samples.keys)
    )
    assertEquals(
      boundDraw.analysis.child.keys.map(_.drawPosition),
      Vector.range(0, rawDraw.length)
    )
    assertEquals(boundDraw.analysis.child.size, rawDraw.length)

    val permutation = PermutationDesign(1)
    val permutationAuthority = SeedAuthority.fromLong(SeedDomain.Randomization, 84L)
    val permutationCompiled = compile(
      permutation,
      samples.size,
      permutationAuthority
    )
    val permutationSchedule = right(
      BoundSchedule(
        permutationCompiled,
        samples,
        population,
        right(ScheduleLabels.fromDesign(samples, permutation)),
        permutationAuthority
      )
    )
    val boundPermutation: ReindexingLeg[
      samples.Id,
      SampleId,
      SampleId,
      Permutation
    ] = right(permutationSchedule.at(UnitKey(0, 0)))
    assertEquals(
      boundPermutation.child.keys,
      permutationCompiled.plan.first.toVector.map(samples.keys)
    )

  test("foreign population, label, and seed authorities fail closed"):
    val samples = MvpaLawFixtures.sampleAxis(6, 1301)
    val population = right(AxisPopulationFingerprint.fromAxis(samples))
    val groups = labels(0, 0, 1, 1, 2, 2)
    val foreignGroups = labels(0, 1, 0, 1, 2, 2)
    val design = KFold.grouped(3, groups)
    val foreignDesign = KFold.grouped(3, foreignGroups)
    val authority = SeedAuthority.fromLong(SeedDomain.Validation, 85L)
    val compiled = compile(design, samples.size, authority)

    val wrongLabels = BoundSchedule(
      compiled,
      samples,
      population,
      right(ScheduleLabels.fromDesign(samples, foreignDesign)),
      authority
    )
    assert(wrongLabels.left.exists:
      case BoundScheduleError.LabelFingerprintMismatch(_, _) => true
      case _                                                 => false)

    val wrongSeed = BoundSchedule(
      compiled,
      samples,
      population,
      right(ScheduleLabels.fromDesign(samples, design)),
      SeedAuthority.fromLong(SeedDomain.Validation, 86L)
    )
    assert(wrongSeed.left.exists:
      case BoundScheduleError.SeedMismatch(86L, 85L) => true
      case _                                         => false)

    val shortDesign = KFold.ordered(2)
    val shortCompiled = compile(shortDesign, 4, authority)
    val wrongPopulation = BoundSchedule(
      shortCompiled,
      samples,
      population,
      right(ScheduleLabels.fromDesign(samples, shortDesign)),
      authority
    )
    assert(wrongPopulation.left.exists:
      case BoundScheduleError.InvalidUnit(
            UnitKey(0, 0),
            ScheduleUnitError.PopulationMismatch(_, 6, 4)
          ) =>
        true
      case _ => false)

  test("schedule, population, and label evidence cannot cross nominal sample axes"):
    val errors = compileErrors("""
      import scalafim.fmri.mvpa.*
      import resample4s.core.*
      import resample4s.designs.*

      def invalid(
          left: AxisRef[SampleId],
          right: AxisRef[SampleId],
          compiled: Compiled[Split[Selection], Coverage.ExactOnce],
          population: AxisPopulationFingerprint[SampleId, right.Id],
          labels: ScheduleLabels[SampleId, right.Id],
          authority: SeedAuthority
      )(using DigestAlgorithm) =
        BoundSchedule(compiled, left, population, labels, authority)
    """)

    assert(errors.nonEmpty)
