package scalafim.estimates.io

import java.nio.file.Files
import java.security.MessageDigest
import scalafim.estimates.*
import scalafim.image.SampleSpaces

/** Physical NIfTI bytes are produced by the Python standard-library generator
  * in docs/verification/estimate-set-core-nifti-2026-09-29, not by either writer.
  */
class IndependentNiftiGoldenSuite extends munit.FunSuite:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)

  private def resource(name: String): Array[Byte] =
    val input = Option(getClass.getResourceAsStream(s"/estimate-golden/$name"))
      .getOrElse(fail(s"missing independent fixture $name"))
    try input.readAllBytes()
    finally input.close()

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes)
      .iterator.map(byte => f"${byte & 0xff}%02x").mkString

  test("independently encoded NIfTI values and validity preserve ordered estimand and sample axes"):
    val values = resource("values.nii")
    val validity = resource("validity.nii")
    assertEquals(sha256(values), "6e52bf5447b7a75c643da98d56a3d4cca3ee97dff1add29d029e0bfd6aa6a594")
    assertEquals(sha256(validity), "846fcfeb5841c41c237456b7b2c7c52af6930ccc2806a1a4fef1784e1c2a95ec")
    val root = Files.createTempDirectory("scalafim-independent-nifti-")
    val store = right(LocalEstimateStore.open(root))
    val dataset = DatasetId("00000000-0000-4000-8000-000000000051")
    val model = ModelRevisionId("00000000-0000-4000-8000-000000000052")
    val unitId = UnitId("00000000-0000-4000-8000-000000000053")
    val revision = UnitRevisionId("00000000-0000-4000-8000-000000000054")
    val a = EstimandId("A")
    val b = EstimandId("B")
    val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
    val catalog = EstimandCatalog(model, Vector(a, b).map(id =>
      EstimandDefinition(id, id.value, EstimandKind.Coefficient, "signal", "unit", id.value)))
    val domain = right(EstimateDomain.make(SampleSpaces(Vector(2, 1, 1)), Vector(0, 1), "scanner"))
    val product = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
      Vector(observation.id), ProductTargets.Scalar(Vector(a, b)), PoolingScope.Run, "signal")
    val unknown = ScientificFact.Unknown("synthetic independent fixture")
    val unit = EstimateUnit(dataset, unitId, revision, catalog, domain, Vector(observation), Vector.empty,
      Vector(product), Map(product.id -> ProductOutcome.Available(product.id)),
      EstimabilityEvidence.Unknown("no rank claim"),
      EstimateProvenance("python-struct", "1", "fixture", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty))
    def publish(name: String, bytes: Array[Byte]): FileReference =
      val saved = store.objects.write(s"units/${revision.value}/$name")(_.write(bytes))
        .left.map(store.fromStore)
      store.reference(right(saved))
    val representation = NiftiRepresentation(product.id, observation.id,
      publish("values.nii", values), publish("validity.nii", validity),
      NumericPrecision.Float64, 1.0, 0.0, Vector(a, b), "scanner-sform")
    val ref = right(store.publishUnit(unit, Vector(representation)))
    val reopened = right(LocalEstimateStore.open(root))
    val source = right(reopened.open(ref, ReadLimits(4)))
    try
      val out = new Array[Double](4)
      val codes = new Array[Byte](4)
      right(source.read(product.id, EstimateSelection(Vector(observation.id), Vector(a, b), Vector(0, 1)), out, codes))
      assertEquals(out.toVector, Vector(2.0, 4.0, 6.0, 8.0))
      assertEquals(codes.toVector, Vector[Byte](0, 3, 0, 0))
    finally right(source.close())
