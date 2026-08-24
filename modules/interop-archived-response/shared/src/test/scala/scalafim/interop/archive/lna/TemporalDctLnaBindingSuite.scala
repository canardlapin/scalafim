package scalafim.interop.archive.lna

import scalafim.image.SampleSpaces

import scalafim.archive.{CanonicalValue, RepresentationMetadata, RunLabel}
import scalafim.archive.lna.{DatasetRole, Payload}
import scalafim.image.SomeSampleSpace
import scalafim.interop.archive.{PayloadCatalog, RepresentationEnvelope}
import scalafim.latent.{
  DctNorm,
  DctSpec,
  MaterializedTemporalDct,
  RepresentationInstanceId,
  RidgePenalty,
  TemporalDctRepresentation
}
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
  ScientificProfileId,
  SignalSchema,
  TimeAxis,
  TimeDomain,
  UnitId,
  ValidationReportRef
}

class TemporalDctLnaBindingSuite extends munit.FunSuite:
  test("write plan preserves the typed model and logical scalar bits"):
    val fixture = TemporalDctLnaFixtures.fixture("shared-plan")
    val plan =
      TemporalDctLnaWritePlan
        .create(fixture.materialized, fixture.space)
        .fold(error => fail(error.message), identity)

    assertEquals(plan.steps.head, TemporalDctLnaWriteStep.BeginStaging)
    assertEquals(plan.steps.last, TemporalDctLnaWriteStep.Publish)
    assertEquals(
      plan.steps.collect:
        case TemporalDctLnaWriteStep.WritePayload(path) => path
      ,
      plan.archive.manifest.datasets.sortBy(_.path.value).map(_.path)
    )
    assert(
      plan.archive.manifest.header
        .get(TemporalDctLnaProfile.ModelHeaderKey)
        .exists(_.nonEmpty)
    )
    assertEquals(
      plan.archive.manifest.header
        .get(TemporalDctLnaProfile.DependencyHeaderKey),
      Some(TemporalDctLnaProfile.EmbeddedDependencies)
    )
    assert(
      plan.archive.manifest.header
        .get(RepresentationMetadata.DescriptorHeader)
        .exists(_.nonEmpty)
    )
    assert(
      plan.archive.manifest.header
        .get(RepresentationMetadata.OutputSchemaHeader)
        .exists(_.nonEmpty)
    )

    val basis =
      payload(plan, DatasetRole.TemporalBasis) match
        case Payload.DoubleMatrix(data, _) =>
          data
        case other =>
          fail(s"expected temporal basis matrix, got $other")
    val loadings =
      payload(plan, DatasetRole.Loadings) match
        case Payload.DoubleMatrix(data, _) =>
          data
        case other =>
          fail(s"expected loading matrix, got $other")
    val offset =
      payload(plan, DatasetRole.SampleOffset) match
        case Payload.DoubleVector(values, _) =>
          values
        case other =>
          fail(s"expected sample offset vector, got $other")

    assertRawMatrix(
      basis,
      fixture.materialized.basis.rowMajorCopy
    )
    assertRawMatrix(
      loadings,
      fixture.materialized.loadings.rowMajorCopy
    )
    assertRawVector(
      offset,
      fixture.materialized.offset.toVector.flatMap(_.valuesCopy)
    )

  test("narrow descriptor envelope reconstructs the temporal DCT model"):
    val fixture = TemporalDctLnaFixtures.fixture("descriptor-round-trip")
    val envelope =
      RepresentationEnvelope(
        TemporalDctLnaProfile.Representation,
        CanonicalValue.string(
          TemporalDctLnaDescriptor.encodeModel(fixture.model)
        ),
        PayloadCatalog.from(Vector.empty),
        CanonicalValue.string(
          TemporalDctLnaDescriptor.encodeSchema(fixture.schema)
        )
      )

    val decoded =
      TemporalDctLnaDescriptor
        .decode(envelope)
        .fold(error => fail(error.message), identity)

    assertEquals(decoded.schema, fixture.schema)
    assertEquals(
      TemporalDctLnaModel.fingerprint(decoded),
      TemporalDctLnaModel.fingerprint(fixture.model)
    )

    val malformed =
      envelope.copy(descriptor = CanonicalValue.string("4:odd"))
    assert(TemporalDctLnaDescriptor.decode(malformed).isLeft)

  test("descriptor preserves explicit surface schemas and non-default policies"):
    val time =
      TimeDomain
        .explicit(
          DomainId.unsafe[TimeAxis]("descriptor:explicit-time"),
          Vector(-0.0, 0.5, 2.0),
          UnitId.unsafe("second")
        )
        .fold(error => fail(error.message), identity)
    val samples =
      SampleDomain
        .make(
          DomainId.unsafe[SampleAxis]("descriptor:surface-samples"),
          2,
          SampleDomainKind.Surface(
            DomainReference.unsafe("surface", "mesh-1"),
            DomainReference.unsafe("topology", "triangles-1"),
            DomainReference.unsafe("ordering", "vertex-order-1")
          )
        )
        .fold(error => fail(error.message), identity)
    val schema =
      ResponseSchema
        .make(
          ResponseSchemaId.unsafe("descriptor:surface-schema"),
          time,
          samples,
          SignalSchema(
            UnitId.unsafe("arbitrary"),
            CalibrationState.Applied,
            NonFinitePolicy.Preserve
          )
        )
        .fold(error => fail(error.message), identity)
    val consistency =
      DecodeConsistency
        .absoluteRelative(1e-12, 1e-10)
        .fold(error => fail(error.message), identity)
    val model =
      TemporalDctRepresentation
        .make(
          RepresentationInstanceId.unsafe("descriptor:surface-dct"),
          schema,
          DctSpec(3, 2, DctNorm.None)
            .fold(error => fail(error.message), identity),
          center = false,
          RidgePenalty.unsafe(0.25),
          ReconstructionContract.ValidatedScientific(
            ScientificProfileId.unsafe("validated-profile"),
            ValidationReportRef.unsafe("report-1")
          ),
          consistency
        )
        .fold(error => fail(error.message), identity)
    val envelope =
      RepresentationEnvelope(
        TemporalDctLnaProfile.Representation,
        CanonicalValue.string(TemporalDctLnaDescriptor.encodeModel(model)),
        PayloadCatalog.from(Vector.empty),
        CanonicalValue.string(TemporalDctLnaDescriptor.encodeSchema(schema))
      )
    val decoded =
      TemporalDctLnaDescriptor
        .decode(envelope)
        .fold(error => fail(error.message), identity)

    assertEquals(decoded.schema, schema)
    assertEquals(
      TemporalDctLnaModel.fingerprint(decoded),
      TemporalDctLnaModel.fingerprint(model)
    )

  private def payload(
      plan: TemporalDctLnaWritePlan,
      role: DatasetRole
  ): Payload =
    val reference =
      plan.archive.manifest.datasets
        .find(_.role == role)
        .getOrElse(fail(s"missing ${role.value} payload"))
    plan.archive.payload(reference.path).getOrElse(
      fail(s"missing ${reference.path.value}")
    )

  private def assertRawMatrix(
      matrix: gale.linalg.DMat,
      expected: Array[Double]
  ): Unit =
    assertEquals(matrix.rows * matrix.cols, expected.length)
    var index = 0
    while index < expected.length do
      val row = index / matrix.cols
      val column = index % matrix.cols
      assertEquals(
        java.lang.Double.doubleToRawLongBits(matrix(row, column)),
        java.lang.Double.doubleToRawLongBits(expected(index))
      )
      index += 1

  private def assertRawVector(
      actual: Vector[Double],
      expected: Vector[Double]
  ): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < expected.length do
      assertEquals(
        java.lang.Double.doubleToRawLongBits(actual(index)),
        java.lang.Double.doubleToRawLongBits(expected(index))
      )
      index += 1

private[lna] object TemporalDctLnaFixtures:
  final case class Fixture(
      schema: ResponseSchema,
      whole: ResolvedResponseSelection,
      source: ResponseBlock,
      model: TemporalDctRepresentation,
      materialized: MaterializedTemporalDct,
      space: SomeSampleSpace
  )

  def fixture(id: String): Fixture =
    val schema = responseSchema(id)
    val whole =
      ResolvedResponseSelection
        .all(schema)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val source =
      ResponseBlock
        .copyFromRowMajor(
          Array[Double](
            2.0, 4.0, -1.0,
            4.0, 8.0, 3.0,
            7.0, 5.0, 9.0,
            -2.0, 6.0, 1.5
          ),
          whole
        )
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val bounds =
      ReconstructionErrorBounds
        .make(1e-10, 1e-10)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val model =
      TemporalDctRepresentation
        .make(
          RepresentationInstanceId.unsafe(s"$id-dct"),
          schema,
          DctSpec.full(4).toOption.get,
          center = true,
          RidgePenalty.Zero,
          ReconstructionContract.DeterministicBounded(bounds),
          DecodeConsistency.ExactBits
        )
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val materialized =
      model
        .encode(source)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    Fixture(
      schema,
      whole,
      source,
      model,
      materialized,
      SampleSpaces(Vector(3, 1, 1))
    )

  def selection(
      schema: ResponseSchema,
      times: Vector[Int],
      samples: Vector[Int]
  ): ResolvedResponseSelection =
    val timepoints =
      OrderedIndices
        .fromInts(schema.time.id, schema.time.count, times)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    val sampleIndices =
      OrderedIndices
        .fromInts(schema.samples.id, schema.samples.count, samples)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    ResolvedResponseSelection
      .make(schema, timepoints, sampleIndices)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def responseSchema(id: String): ResponseSchema =
    val time =
      TimeDomain
        .regular(
          DomainId.unsafe[TimeAxis](s"$id:time"),
          0.0,
          1.0,
          4,
          UnitId.unsafe("second")
        )
        .toOption
        .get
    val samples =
      SampleDomain
        .make(
          DomainId.unsafe[SampleAxis](s"$id:samples"),
          3,
          SampleDomainKind.Volume(
            DomainReference.unsafe("test-space", s"$id-space"),
            None,
            DomainReference.unsafe("test-order", s"$id-order")
          )
        )
        .toOption
        .get
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
      .toOption
      .get
