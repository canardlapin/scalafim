package scalafim.latent

import cats.instances.either.given
import narr.NArray
import scala.compiletime.testing.typeCheckErrors
import scalafim.response.{
  CalibrationState,
  DecodeConsistency,
  DomainId,
  DomainReference,
  NonFinitePolicy,
  OrderedIndices,
  ReconstructionContract,
  ReconstructionErrorBounds,
  ResolvedResponseSelection,
  ResponseBlock,
  ResponseSchema,
  ResponseSchemaId,
  SampleAxis,
  SampleDomain,
  SampleDomainKind,
  SignalSchema,
  TimeAxis,
  TimeDomain,
  UnitId
}
import scalafim.response.laws.ResponseLawChecks

class TemporalDctRepresentationSuite extends munit.FunSuite:
  test("full-rank temporal DCT obeys its reconstruction contract"):
    val fixture = representationFixture("full-rank")
    val materialized =
      fixture.model
        .encode(fixture.source)
        .fold(error => fail(error.message), identity)
    val decoded =
      materialized
        .decode(fixture.whole)
        .fold(error => fail(error.message), identity)

    assertEquals(decoded.selection, fixture.whole)
    assertBlockAgrees(
      decoded,
      fixture.source,
      fixture.model.reconstructionContract
    )

  test("compiled plan exposes typed independent logical reads"):
    val fixture = representationFixture("inspection")
    val requested =
      selection(fixture.schema, Vector(3, 1), Vector(2, 0))
    val plan =
      fixture.model
        .compile(requested)
        .fold(error => fail(error.message), identity)
    val inspection = DecodePlan.inspect(plan)

    assertEquals(inspection.dependentBarriers, 0)
    assertEquals(
      inspection.requests.map(_.slot.role.value),
      Vector(
        "org.scalafim/temporal-dct/basis",
        "org.scalafim/temporal-dct/loadings",
        "org.scalafim/temporal-dct/offset"
      )
    )
    assertEquals(
      inspection.requests.map(_.slot.dimensions),
      Vector(Vector(4, 4), Vector(3, 4), Vector(3))
    )

  test("selected decoding agrees exactly with selecting a whole decode"):
    val fixture = representationFixture("selected-whole")
    val materialized =
      fixture.model
        .encode(fixture.source)
        .fold(error => fail(error.message), identity)
    val whole =
      materialized
        .decode(fixture.whole)
        .fold(error => fail(error.message), identity)
    val requested =
      selection(fixture.schema, Vector(3, 1), Vector(2, 0))
    val selected =
      materialized
        .decode(requested)
        .fold(error => fail(error.message), identity)

    assertEquals(selected.rows, requested.rows)
    assertEquals(selected.columns, requested.columns)
    assertEquals(
      ResponseLawChecks
        .selection(
          "temporal DCT selected decode",
          selected,
          whole,
          fixture.model.decodeConsistency
        )
        .toEither,
      Right(())
    )

  test("partitioned decoding preserves order and agrees exactly"):
    val fixture = representationFixture("partition")
    val materialized =
      fixture.model
        .encode(fixture.source)
        .fold(error => fail(error.message), identity)
    val samples = Vector(2, 0)
    val together =
      materialized
        .decode(selection(fixture.schema, Vector(3, 0, 2), samples))
        .fold(error => fail(error.message), identity)
    val first =
      materialized
        .decode(selection(fixture.schema, Vector(3), samples))
        .fold(error => fail(error.message), identity)
    val second =
      materialized
        .decode(selection(fixture.schema, Vector(0, 2), samples))
        .fold(error => fail(error.message), identity)
    val consistency = fixture.model.decodeConsistency

    assertEquals(
      ResponseLawChecks
        .partition(
          "temporal DCT partition",
          together,
          Vector(first, second),
          consistency
        )
        .toEither,
      Right(())
    )

  test("encoding enforces reconstruction separately from path consistency"):
    val fixture = representationFixture("contract-enforcement")
    val rankOne =
      DctSpec
        .apply(
          fixture.schema.time.count,
          components = 1,
          norm = DctNorm.Ortho
        )
        .fold(error => fail(error.message), identity)
    val model =
      TemporalDctRepresentation
        .make(
          RepresentationInstanceId.unsafe("contract-enforcement-rank-one"),
          fixture.schema,
          rankOne,
          center = true,
          RidgePenalty.Zero,
          ReconstructionContract.Exact,
          DecodeConsistency.ExactBits
        )
        .fold(error => fail(error.message), identity)

    model.encode(fixture.source) match
      case Left(RepresentationError.ReconstructionContractViolation(
            _,
            _,
            _,
            ReconstructionContract.Exact
          )) =>
        ()
      case other =>
        fail(s"expected an exact reconstruction-contract violation, got $other")

  test("foreign selections are rejected before a plan is built"):
    val fixture = representationFixture("selection-owner")
    val foreign = responseSchema("foreign-owner")
    val foreignSelection =
      selection(foreign, Vector(0, 1), Vector(0, 1))

    fixture.model.compile(foreignSelection) match
      case Left(RepresentationError.InvalidSelection(_)) =>
        ()
      case other =>
        fail(s"expected an invalid-selection error, got $other")

  test("payload result types cannot be exchanged"):
    val errors =
      typeCheckErrors(
        """
        def invalid(
            read: scalafim.latent.TemporalDctRead.BasisRows
        ): scalafim.latent.DecodePlan[scalafim.latent.SpatialLoadingValues] =
          scalafim.latent.DecodePlan.read(read)
        """
      )

    assert(errors.nonEmpty)

  test("data-dependent logical reads are explicit applicative barriers"):
    val fixture = representationFixture("barrier")
    val firstRead =
      TemporalDctRead.BasisRows(
        fixture.model.basisSlot,
        fixture.whole.timepoints
      )
    val plan =
      DecodePlan
        .read(firstRead)
        .dependent(_ => DecodePlan.read(firstRead))
    val inspection = DecodePlan.inspect(plan)

    assertEquals(inspection.requests.map(_.slot.id), Vector(fixture.model.basisSlot.id))
    assertEquals(inspection.dependentBarriers, 1)

    type Result[A] = Either[RepresentationError, A]
    val execution =
      DecodePlan.runApplicative[Result, TemporalBasisValues](
        plan,
        new DecodePlan.Interpreter[Result]:
          def apply[A](
              request: LogicalPayloadRead[A]
          ): Result[A] =
            Left(RepresentationError.UnsupportedLogicalRead(request.slot.id))
      )
    assertEquals(execution, Left(DecodePlanError.DependentReadBarrier))

  private final case class Fixture(
      schema: ResponseSchema,
      whole: ResolvedResponseSelection,
      source: ResponseBlock,
      model: TemporalDctRepresentation
  )

  private def representationFixture(id: String): Fixture =
    val schema = responseSchema(id)
    val whole =
      ResolvedResponseSelection
        .all(schema)
        .fold(error => fail(error.message), identity)
    val source =
      ResponseBlock
        .copyFromRowMajor(
          NArray[Double](
            2.0, 4.0, -1.0,
            4.0, 8.0, 3.0,
            7.0, 5.0, 9.0,
            -2.0, 6.0, 1.5
          ),
          whole
        )
        .fold(error => fail(error.message), identity)
    val spec =
      DctSpec
        .full(schema.time.count)
        .fold(error => fail(error.message), identity)
    val bounds =
      ReconstructionErrorBounds
        .make(absolute = 1e-10, relative = 1e-10)
        .fold(error => fail(error.message), identity)
    val model =
      TemporalDctRepresentation
        .make(
          RepresentationInstanceId.unsafe(s"$id-dct"),
          schema,
          spec,
          center = true,
          RidgePenalty.Zero,
          ReconstructionContract.DeterministicBounded(bounds),
          DecodeConsistency.ExactBits
        )
        .fold(error => fail(error.message), identity)
    Fixture(schema, whole, source, model)

  private def responseSchema(id: String): ResponseSchema =
    val time =
      TimeDomain
        .regular(
          DomainId.unsafe[TimeAxis](s"$id:time"),
          origin = 0.0,
          interval = 1.0,
          count = 4,
          UnitId.unsafe("second")
        )
        .fold(error => fail(error.message), identity)
    val samples =
      SampleDomain
        .make(
          DomainId.unsafe[SampleAxis](s"$id:samples"),
          count = 3,
          SampleDomainKind.Volume(
            DomainReference.unsafe("test-space", s"$id-space"),
            None,
            DomainReference.unsafe("test-order", s"$id-order")
          )
        )
        .fold(error => fail(error.message), identity)
    ResponseSchema
      .make(
        ResponseSchemaId.unsafe(id),
        time,
        samples,
        SignalSchema(
          UnitId.unsafe("percent-signal"),
          CalibrationState.Applied,
          NonFinitePolicy.Reject
        )
      )
      .fold(error => fail(error.message), identity)

  private def selection(
      schema: ResponseSchema,
      times: Vector[Int],
      samples: Vector[Int]
  ): ResolvedResponseSelection =
    val timepoints =
      OrderedIndices
        .fromInts(schema.time.id, schema.time.count, times)
        .fold(error => fail(error.message), identity)
    val sampleIndices =
      OrderedIndices
        .fromInts(schema.samples.id, schema.samples.count, samples)
        .fold(error => fail(error.message), identity)
    ResolvedResponseSelection
      .make(schema, timepoints, sampleIndices)
      .fold(error => fail(error.message), identity)

  private def assertBlockAgrees(
      actual: ResponseBlock,
      expected: ResponseBlock,
      contract: ReconstructionContract
  ): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.columns, expected.columns)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.columns do
        val agrees =
          contract match
            case ReconstructionContract.Exact =>
              DecodeConsistency.ExactBits.agrees(
                actual(row, column),
                expected(row, column)
              )
            case ReconstructionContract.DeterministicBounded(bounds) =>
              bounds.agrees(actual(row, column), expected(row, column))
            case ReconstructionContract.ValidatedScientific(_, _) =>
              true
        assert(agrees)
        column += 1
      row += 1
