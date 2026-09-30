package scalafim.estimates.io

import java.nio.file.Files
import java.security.MessageDigest
import image4s.geometry.{Affine, D3}
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
      NumericPrecision.Float64, 1.0, 0.0, Vector(a, b), "scanner-sform",
      storedDatatype = Some(NiftiStoredDatatype.Float64))
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

  test("independent big-endian scaled float32 singleton 3D and gzip decode under an exact disk budget"):
    val hashes = Map(
      "values-3d-be-scaled.nii" -> "7ac6a88c778866ee9f326135a8a847e7185f0b43a7b5609d0f9ae6105bb5b4c4",
      "validity-3d.nii" -> "8e53240f0d0ca64bba16e9623a1c1e3d92faafb0187341a1d43facc95088c633",
      "values-3d-be-scaled.nii.gz" -> "a63261362b4f6ddf8db7f098e511ca17c61b0aa2420ffb8ebf1e2e0c7587cd2c",
      "validity-3d.nii.gz" -> "416d7c4efbafc3518988d10366dc89f7a8247fd07ae560e8c1c87c81bb78c76f")
    hashes.foreach: (name, expected) =>
      assertEquals(sha256(resource(name)), expected)
    val affine = Affine.fromRowMajor[D3](Vector(
      -2.0, 0.0, 0.0, 8.0,
      0.0, 3.0, 0.0, -4.0,
      0.0, 0.0, 4.0, 2.0,
      0.0, 0.0, 0.0, 1.0)).toOption.get
    for compressed <- Vector(false, true) do
      val root = Files.createTempDirectory("scalafim-independent-3d-")
      val store = right(LocalEstimateStore.open(root))
      val dataset = DatasetId("00000000-0000-4000-8000-000000000061")
      val model = ModelRevisionId("00000000-0000-4000-8000-000000000062")
      val unitId = UnitId("00000000-0000-4000-8000-000000000063")
      val revision = UnitRevisionId("00000000-0000-4000-8000-000000000064")
      val target = EstimandId("A")
      val observation = Observation(ObservationId("row"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
      val catalog = EstimandCatalog(model, Vector(EstimandDefinition(target, "A", EstimandKind.Coefficient, "signal", "unit", "A")))
      val domain = right(EstimateDomain.make(SampleSpaces(Vector(2, 1, 1), affine = Some(affine)), Vector(0, 1), "scanner"))
      val product = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
        Vector(observation.id), ProductTargets.Scalar(Vector(target)), PoolingScope.Run, "signal")
      val unknown = ScientificFact.Unknown("independent fixture")
      val unit = EstimateUnit(dataset, unitId, revision, catalog, domain, Vector(observation), Vector.empty,
        Vector(product), Map(product.id -> ProductOutcome.Available(product.id)), EstimabilityEvidence.Unknown("imported"),
        EstimateProvenance("python-struct", "1", "fixture", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty))
      def publish(name: String): FileReference =
        store.reference(right(store.objects.write(s"units/${revision.value}/$name")(_.write(resource(name))).left.map(store.fromStore)))
      val suffix = if compressed then ".nii.gz" else ".nii"
      val representation = NiftiRepresentation(product.id, observation.id,
        publish(s"values-3d-be-scaled$suffix"), publish(s"validity-3d$suffix"),
        NumericPrecision.Float64, 2.0, 1.0, Vector(target), "scanner-sform",
        storedDatatype = Some(NiftiStoredDatatype.Float32))
      val ref = right(store.publishUnit(unit, Vector(representation)))
      val reopened = right(LocalEstimateStore.open(root))
      if compressed then assert(reopened.open(ref, ReadLimits(2, 713)).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
      val source = right(reopened.open(ref, ReadLimits(2, if compressed then 714 else 1))).asInstanceOf[NiftiEstimateSource]
      val staged = source.stagedFiles
      try
        assertEquals(source.stagedPayloadBytes, if compressed then 714L else 0L)
        assertEquals(staged.size, if compressed then 2 else 0)
        assert(staged.forall(Files.exists(_)))
        val out = new Array[Double](2)
        val codes = new Array[Byte](2)
        right(source.read(product.id, EstimateSelection(Vector(observation.id), Vector(target), Vector(0, 1)), out, codes))
        assertEquals(out.toVector, Vector(4.0, -3.0))
        assertEquals(codes.toVector, Vector[Byte](0, 0))
      finally right(source.close())
      assert(staged.forall(path => !Files.exists(path)))
