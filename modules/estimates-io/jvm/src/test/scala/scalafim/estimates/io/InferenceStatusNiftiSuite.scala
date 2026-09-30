package scalafim.estimates.io

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}
import java.security.MessageDigest
import scalafim.estimates.*

class InferenceStatusNiftiSuite extends munit.FunSuite:
  private val prefix = "units/00000000-0000-4000-8000-000000000104"
  private val manifestPath = s"$prefix/estimates.json"
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)
  private def resource(name: String): Array[Byte] =
    val input = Option(getClass.getResourceAsStream(s"/estimate-golden/inference-evidence/$name")).getOrElse(fail(name))
    try input.readAllBytes()
    finally input.close()
  private def hash(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).iterator.map(b => f"${b & 0xff}%02x").mkString
  private def reference(root: Path, path: String): FileReference =
    val data = Files.readAllBytes(root.resolve(path))
    FileReference(path, scalafim.archive.ContentDigest.unsafeSha256(hash(data)), data.length.toLong)
  private def pin(root: Path): PinnedUnit =
    PinnedUnit(UnitId("00000000-0000-4000-8000-000000000103"),
      UnitRevisionId("00000000-0000-4000-8000-000000000104"), reference(root, manifestPath))
  private def fixture(): (Path, LocalEstimateStore, PinnedUnit) =
    val root = Files.createTempDirectory("inference-literal-")
    new String(resource("SHA256SUMS"), java.nio.charset.StandardCharsets.UTF_8).linesIterator.foreach: line =>
      val path = line.drop(66)
      val bytes = resource(path)
      assertEquals(hash(bytes), line.take(64), s"independent literal hash $path")
      if path.startsWith("units/") then
        Files.createDirectories(root.resolve(path).getParent)
        Files.write(root.resolve(path), bytes)
    (root, right(LocalEstimateStore.open(root)), pin(root))
  private def jsonRef(ref: FileReference): ujson.Value =
    ujson.Obj("Path" -> ref.path, "SHA256" -> ref.digest.value, "Bytes" -> ref.bytes.toDouble)
  private def mutate(root: Path)(change: ujson.Value => Unit): PinnedUnit =
    val json = ujson.read(Files.readString(root.resolve(manifestPath)))
    change(json)
    Files.writeString(root.resolve(manifestPath), ujson.write(json, indent = 2) + "\n")
    pin(root)
  private def noStages(store: LocalEstimateStore): Unit =
    val directory = store.root.resolve(".staging")
    if Files.exists(directory) then
      val entries = Files.walk(directory)
      try assert(!entries.anyMatch(Files.isRegularFile(_)))
      finally entries.close()
  private def expected: Vector[Vector[Byte]] =
    ujson.read(new String(resource("expected.json"), java.nio.charset.StandardCharsets.UTF_8))("codes")
      .arr.toVector.map(_.arr.toVector.map(_.num.toByte))
  private def descriptors(): Long =
    val directory = if Files.isDirectory(Path.of("/dev/fd")) then Path.of("/dev/fd") else Path.of("/proc/self/fd")
    val entries = Files.list(directory)
    try entries.count()
    finally entries.close()

  private def deliver(sink: InferenceEvidenceSink, block: Int, change: Boolean = false): Unit =
    val unit = sink.unit
    unit.products.foreach: product =>
      product.observations.foreach: observation =>
        product.targets.estimands.foreach: target =>
          unit.domain.support.reverse.grouped(block).foreach: samples =>
            val selection = EstimateSelection(Vector(observation), Vector(target), samples)
            right(sink.write(product.id, selection, samples.map(_.toDouble).toArray,
              Array.fill[Byte](samples.size)(Validity.NotComputed.code)))
    unit.inferenceEvidence.get.planes.reverse.foreach: plane =>
      val ordinal = unit.inferenceEvidence.get.planes.indexOf(plane)
      unit.domain.support.reverse.grouped(block).foreach: samples =>
        val codes = samples.map(i => if change && ordinal == 0 && i == 2 then InferenceStatusCode.Unrecorded.code else expected(ordinal)(i)).toArray
        val selection = InferenceStatusSelection(Vector(plane), samples)
        assertEquals(right(sink.writeInferenceStatus(selection, codes)).cells, samples.size)
        java.util.Arrays.fill(codes, 99.toByte) // borrowed delivery is not retained

  test("independent Core-3 literal reads all ten codes and permuted sparse planes without fit"):
    val (root, store, ref) = fixture()
    val source = right(store.open(ref, ReadLimits(30))).asInstanceOf[InferenceEvidenceSource]
    try
      val evidence = source.unit.inferenceEvidence.get
      assertEquals(evidence.coefficients.head.inferableColumns, Vector(ColumnId("task")))
      assert(evidence.coefficients.head.conditioning.isInstanceOf[ScientificFact.Unknown])
      assert(source.unit.estimability.isInstanceOf[EstimabilityEvidence.Unknown])
      val codes = new Array[Byte](30)
      right(source.readInferenceStatus(InferenceStatusSelection(evidence.planes, (0 until 10).toVector), codes))
      assertEquals(codes.toVector, expected.flatten)
      val selection = InferenceStatusSelection(Vector(evidence.planes(2), evidence.planes(0), evidence.planes(1)), Vector(9, 2, 0, 4))
      val permuted = new Array[Byte](12)
      right(source.readInferenceStatus(selection, permuted))
      val literal = ujson.read(new String(resource("expected.json"), java.nio.charset.StandardCharsets.UTF_8))
      assertEquals(permuted.toVector, literal("expectedPermuted").arr.toVector.map(_.num.toByte))
      val values = new Array[Double](1)
      val validity = new Array[Byte](1)
      right(source.read(ProductId("effect"), EstimateSelection(Vector(ObservationId("row")), Vector(EstimandId("A")), Vector(2)), values, validity))
      assertEquals(validity(0), Validity.NotComputed.code)
      assertEqualsDouble(values(0), 2.0, 0.0)
      assertEquals(codes(2), InferenceStatusCode.Estimable.code)
    finally right(source.close())
    assert(source.readInferenceStatus(InferenceStatusSelection(source.unit.inferenceEvidence.get.planes.take(1), Vector(1)), new Array[Byte](1)).isLeft)
    assert(Files.exists(root.resolve(manifestPath)))

  test("writer coverage, duplicates, borrowed arrays, block sizes, no-clobber and identical retry"):
    val (_, fixtureStore, literalRef) = fixture()
    val unit = right(fixtureStore.inspect(literalRef))
    val root = Files.createTempDirectory("inference-write-")
    val store = right(LocalEstimateStore.open(root))
    assert(store.newSink(unit, 2).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    assert(store.newSink(unit, 2, CovarianceLayout.SharedNormalizedTable()).isLeft)
    assert(store.newInferenceSink(unit, 2, CovarianceLayout.SharedNormalizedTable()).isLeft)
    noStages(store)
    var first: Option[PinnedUnit] = None
    for block <- Vector(1, 2, 40) do
      val sink = right(store.newInferenceSink(unit, block))
      assert(sink.seal().isLeft)
      deliver(sink, block)
      val repeated = InferenceStatusSelection(unit.inferenceEvidence.get.planes.take(1), Vector(2))
      assert(sink.writeInferenceStatus(repeated, Array[Byte](2)).left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
      val ref = right(sink.seal())
      assertEquals(sink.seal(), Right(ref))
      first.foreach(previous => assertEquals(ref, previous))
      first = Some(ref)
      val reader = right(store.open(ref, ReadLimits(30))).asInstanceOf[InferenceEvidenceSource]
      try
        val codes = new Array[Byte](30)
        right(reader.readInferenceStatus(InferenceStatusSelection(unit.inferenceEvidence.get.planes, (0 until 10).toVector), codes))
        assertEquals(codes.toVector, expected.flatten)
      finally right(reader.close())
      noStages(store)
    val before = Files.readAllBytes(root.resolve(first.get.manifest.path))
    val conflict = right(store.newInferenceSink(unit, 2))
    deliver(conflict, 2, change = true)
    assert(conflict.seal().left.toOption.exists(_.isInstanceOf[EstimateError.Conflict]))
    assertEquals(Files.readAllBytes(root.resolve(first.get.manifest.path)).toVector, before.toVector)
    noStages(store)

  test("status missing delivery cannot seal; invalid codes and support mismatch refuse without consuming coverage"):
    val (_, store, ref) = fixture()
    val unit = right(store.inspect(ref))
    val output = right(LocalEstimateStore.open(Files.createTempDirectory("inference-missing-")))
    val sink = right(output.newInferenceSink(unit, 30))
    unit.products.foreach: product =>
      right(sink.write(product.id, EstimateSelection(product.observations, product.targets.estimands, unit.domain.support),
        Array.fill(18)(0.0), Array.fill[Byte](18)(Validity.NotComputed.code)))
    assert(sink.seal().isLeft)
    assert(!Files.exists(output.root.resolve(manifestPath)))
    val selection = InferenceStatusSelection(unit.inferenceEvidence.get.planes.take(1), Vector(2, 0))
    assert(sink.writeInferenceStatus(selection, Array[Byte](10, 0)).isLeft)
    assert(sink.writeInferenceStatus(selection, Array[Byte](0, 0)).isLeft)
    assert(sink.writeInferenceStatus(selection, Array[Byte](2, 1)).isLeft)
    right(sink.writeInferenceStatus(selection, Array[Byte](2, 0)))
    assert(sink.writeInferenceStatus(selection, Array[Byte](2, 0)).isLeft)
    assert(sink.seal().isLeft)
    right(sink.abort())
    right(sink.abort())
    noStages(output)
    assert(!Files.exists(output.root.resolve(manifestPath)))

  test("code, support, geometry, scaling, plane count, payload length and digest failures refuse opening"):
    val changes: Vector[(String, Array[Byte] => Array[Byte])] = Vector(
      "code" -> (bytes => { bytes(353) = 10; bytes }),
      "outside" -> (bytes => { bytes(352) = 1; bytes }),
      "inside" -> (bytes => { bytes(354) = 0; bytes }),
      "scaling" -> (bytes => { ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).putFloat(112, 2.0f); bytes }),
      "geometry" -> (bytes => { ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).putFloat(292, 40.0f); bytes }),
      "plane-count" -> (bytes => { ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).putShort(48, 2.toShort); bytes }),
      "datatype" -> (bytes => { ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).putShort(70, 16.toShort); bytes }),
      "truncated" -> (bytes => bytes.dropRight(1)),
      "trailing" -> (bytes => bytes :+ 0.toByte))
    changes.foreach: (name, change) =>
      val (root, store, _) = fixture()
      val status = s"$prefix/status.nii"
      Files.write(root.resolve(status), change(Files.readAllBytes(root.resolve(status))))
      val ref = mutate(root)(_("Content")("InferenceStatus")("Content")("file") = jsonRef(reference(root, status)))
      assert(store.open(ref, ReadLimits(2)).isLeft, name)
    val (root, store, ref) = fixture()
    Files.write(root.resolve(s"$prefix/status.nii"), Array[Byte](0))
    assert(store.open(ref, ReadLimits(2)).isLeft)
    val collection = EstimateCollection(right(store.inspect(ref)).dataset,
      CollectionRevisionId("00000000-0000-4000-8000-000000000105"),
      right(store.inspect(ref)).catalog.model, Map(ref.unit -> UnitOutcome.Published(ref)))
    assert(store.publishCollection(collection).isLeft)

  test("strict plane inventory rejects reordered, duplicate and missing status declarations"):
    Vector("reorder", "duplicate", "missing", "extra-field").foreach: mode =>
      val (root, store, _) = fixture()
      val ref = mutate(root): json =>
        val record = json("Content")("InferenceStatus")("Content")
        mode match
          case "reorder" => record("planes") = ujson.Arr.from(record("planes").arr.reverse)
          case "duplicate" => record("planes") = ujson.Arr(record("planes")(0), record("planes")(0))
          case "missing" => json("Content").obj.remove("InferenceStatus")
          case _ => record("extra") = true
      assert(store.open(ref, ReadLimits(2)).isLeft, mode)

  test("corrupt staged status refuses sealing; poisoned status writes abort every owned stage"):
    val (_, store, ref) = fixture()
    val unit = right(store.inspect(ref))
    for corrupt <- Vector(true, false) do
      val output = right(LocalEstimateStore.open(Files.createTempDirectory("inference-seal-failure-")))
      val sink = right(output.newInferenceSink(unit, 2)).asInstanceOf[NiftiEstimateSink]
      if corrupt then
        deliver(sink, 2)
        val path = sink.statusOutput.get.stage.path
        val bytes = Files.readAllBytes(path)
        bytes(353) = 99
        Files.write(path, bytes)
        assert(sink.seal().isLeft)
      else
        right(sink.statusOutput.get.close())
        assert(sink.writeInferenceStatus(InferenceStatusSelection(unit.inferenceEvidence.get.planes.take(1), Vector(2)), Array[Byte](2))
          .left.toOption.exists(_.isInstanceOf[EstimateError.Io]))
        assert(sink.seal().isLeft)
      right(sink.abort())
      noStages(output)
      assert(!Files.exists(output.root.resolve(manifestPath)))

  test("singleton 3D status and gzip use declared geometry and cumulative owned staging budget"):
    for singleton <- Vector(false, true) do
      val (root, store, _) = fixture()
      val file = if singleton then s"$prefix/single-3d.nii" else s"$prefix/status.nii.gz"
      val ref = mutate(root): json =>
        json("Content")("InferenceStatus")("Content")("file") = jsonRef(reference(root, file))
        if singleton then
          val only = ujson.Arr(json("Content")("inferenceEvidence")("planes")(0))
          json("Content")("inferenceEvidence")("planes") = only
          json("Content")("InferenceStatus")("Content")("planes") = only
      if !singleton then assert(store.open(ref, ReadLimits(2, 381)).isLeft)
      val source = right(store.open(ref, ReadLimits(10, 382))).asInstanceOf[NiftiEstimateSource]
      val staged = source.stagedFiles
      assertEquals(staged.size, if singleton then 0 else 1)
      assertEquals(source.stagedPayloadBytes, if singleton then 0L else 382L)
      try
        val out = new Array[Byte](10)
        right(source.readInferenceStatus(InferenceStatusSelection(source.unit.inferenceEvidence.get.planes.take(1), (0 until 10).toVector), out))
        assertEquals(out.toVector, expected.head)
      finally right(source.close())
      assert(source.statusInput.forall(input => !input.channel.isOpen))
      assert(staged.forall(path => !Files.exists(path)))

  test("read caps, cancellations and thrown callbacks return typed failure and keep source ownership"):
    val (_, store, ref) = fixture()
    val source = right(store.open(ref, ReadLimits(2))).asInstanceOf[NiftiEstimateSource]
    val selection = InferenceStatusSelection(source.unit.inferenceEvidence.get.planes.take(1), Vector(2, 9))
    try
      val out = Array.fill[Byte](2)(99)
      assert(source.readInferenceStatus(selection, new Array[Byte](1)).isLeft)
      assert(source.readInferenceStatus(selection.copy(samples = Vector(0, 1, 2)), new Array[Byte](3)).isLeft)
      assertEquals(source.readInferenceStatus(selection, out, () => true), Left(EstimateError.Cancelled))
      assertEquals(out.toVector, Vector[Byte](99, 99))
      var checks = 0
      assertEquals(source.readInferenceStatus(selection, out, () => { checks += 1; checks == 2 }), Left(EstimateError.Cancelled))
      assertEquals(checks, 2) // one cell was read before the cancellation
      assertEquals(out.toVector, Vector[Byte](99, 99))
      checks = 0
      assert(source.readInferenceStatus(selection, out, () =>
        checks += 1
        if checks == 2 then throw new IllegalStateException("late callback failed")
        false)
        .left.toOption.exists(_.isInstanceOf[EstimateError.Io]))
      assertEquals(checks, 2)
      assertEquals(out.toVector, Vector[Byte](99, 99))
      assert(source.statusInput.exists(_.channel.isOpen))
      right(source.readInferenceStatus(selection, out))
      assertEquals(out.toVector, Vector[Byte](2, 9))
    finally right(source.close())

  test("late invalid, support-mismatched and truncated status reads preserve the entire caller buffer"):
    for mode <- Vector("code", "support", "truncated") do
      val (root, store, ref) = fixture()
      val source = right(store.open(ref, ReadLimits(2))).asInstanceOf[NiftiEstimateSource]
      val selection = InferenceStatusSelection(source.unit.inferenceEvidence.get.planes.take(1), Vector(2, 9))
      val path = root.resolve(s"$prefix/status.nii")
      val original = Files.readAllBytes(path)
      val out = Array[Byte](99, 98, 97)
      var checks = 0
      try
        val result = source.readInferenceStatus(selection, out, () =>
          checks += 1
          if checks == 2 then
            // Mutate only after the first selected cell has been read successfully.
            val lateOffset = source.statusInput.get.header.voxOffset.toInt + 9
            val changed = original.clone()
            mode match
              case "code" => changed(lateOffset) = 99
              case "support" => changed(lateOffset) = InferenceStatusCode.OutsideSupport.code
              case _ => ()
            Files.write(path, if mode == "truncated" then changed.take(lateOffset) else changed)
          false)
        assertEquals(checks, 2, mode)
        if mode == "truncated" then assert(result.left.toOption.exists(_.isInstanceOf[EstimateError.Io]))
        else assert(result.left.toOption.exists(_.isInstanceOf[EstimateError.Integrity]))
        assertEquals(out.toVector, Vector[Byte](99, 98, 97), mode)
        assert(source.statusInput.exists(_.channel.isOpen))
        Files.write(path, original)
        assertEquals(right(source.readInferenceStatus(selection, out)).cells, 2)
        assertEquals(out.toVector, Vector[Byte](2, 9, 97), mode)
      finally
        Files.write(path, original)
        right(source.close())

  test("prepayload 32-plane and aggregate writer/reader handle budgets refuse without staging"):
    val (_, store, ref) = fixture()
    val unit = right(store.inspect(ref))
    val observations = (0 until 33).toVector.map(i => unit.observations.head.copy(id = ObservationId(s"row-$i")))
    val planes = observations.map(o => InferenceStatusScope.Fit(o.id))
    val tooMany = unit.copy(observations = unit.observations ++ observations,
      inferenceEvidence = Some(InferenceEvidence(Vector.empty, planes)))
    assert(store.newInferenceSink(tooMany, 2).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    val maximumPlanes = tooMany.copy(inferenceEvidence = Some(InferenceEvidence(Vector.empty, planes.take(32))))
    assertEquals(right(InferenceStatusRepresentation.preflight(maximumPlanes)), 320L)
    val maximumSink = right(store.newInferenceSink(maximumPlanes, 2))
    right(maximumSink.abort())
    val huge = maximumPlanes.copy(domain = EstimateDomain.make(scalafim.image.SampleSpaces(Vector(32767, 32767, 1)),
      Vector(0), "scanner").toOption.get)
    assert(InferenceStatusRepresentation.preflight(huge).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    assert(store.newInferenceSink(huge, 2).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    val products = (0 until 32).toVector.map(i => unit.products.head.copy(id = ProductId(s"effect-$i")))
    val manyProducts = unit.copy(products = products, outcomes = products.map(p => p.id -> ProductOutcome.Available(p.id)).toMap,
      statistics = Vector.empty, inferenceEvidence = Some(InferenceEvidence(Vector.empty, unit.inferenceEvidence.get.planes.take(1))))
    assert(store.newInferenceSink(manyProducts, 2).left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    val allowed = manyProducts.copy(products = products.take(31), outcomes =
      products.take(31).map(p => p.id -> ProductOutcome.Available(p.id)).toMap)
    val allowedSink = right(store.newInferenceSink(allowed, 2))
    right(allowedSink.abort())
    val dummy = FileReference("missing.nii", scalafim.archive.ContentDigest.unsafeSha256("a" * 64), 0)
    val records = products.map(p => EstimateRepresentation.Nifti(NiftiRepresentation(p.id, p.observations.head,
      dummy, dummy, p.precision, 1, 0, p.targets.estimands, "scanner-sform", storedDatatype = Some(NiftiStoredDatatype.Float64))))
    val status = InferenceStatusRepresentation(dummy, manyProducts.inferenceEvidence.get.planes)
    assert(NiftiEstimateSource.openMixed(store, manyProducts, records, ReadLimits(2), Some(status))
      .left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    assert(store.newInferenceSink(unit, 0).isLeft)
    noStages(store)

  test("failed status construction and release close all owned resources without leaked descriptors"):
    val (_, store, ref) = fixture()
    val unit = right(store.inspect(ref))
    var stages = Vector.empty[scalafim.archive.io.StagedFile]
    var calls = 0
    val failed = InferenceStatusNifti.openOutput(store, unit, 2, suffix =>
      calls += 1
      if calls == 2 then Left(EstimateError.Io("injected coverage stage failure"))
      else store.objects.stage(suffix).left.map(store.fromStore).map(stage => { stages :+= stage; stage }))
    assert(failed.isLeft)
    right(NiftiEstimateSink.discard(store, stages))
    noStages(store)
    // Fail after the payload writer has opened: coverage path is an owned directory.
    val beforeOutputs = descriptors()
    for _ <- 0 until 32 do
      stages = Vector.empty
      val lateFailure = InferenceStatusNifti.openOutput(store, unit, 2, suffix =>
        store.objects.stage(suffix).left.map(store.fromStore).map: stage =>
          stages :+= stage
          if suffix == ".coverage" then Files.createDirectory(stage.path)
          stage)
      assert(lateFailure.isLeft)
      right(NiftiEstimateSink.discard(store, stages))
    assert(descriptors() <= beforeOutputs + 4)
    noStages(store)
    val first = FileChannel.open(Files.createTempFile("status-close-a-", ".tmp"), StandardOpenOption.READ)
    val second = FileChannel.open(Files.createTempFile("status-close-b-", ".tmp"), StandardOpenOption.READ)
    var last = false
    val closed = InferenceStatusNifti.closeAll(Vector(
      () => { first.close(); throw new java.io.IOException("injected first close failure") },
      () => { second.close(); Right(()) },
      () => { last = true; Right(()) }))
    assert(closed.isLeft && !first.isOpen && !second.isOpen && last)
    val path = store.root.resolve(s"$prefix/status.nii")
    val bad = Files.readAllBytes(path)
    bad(353) = 99
    Files.write(path, bad)
    val before = descriptors()
    for _ <- 0 until 32 do
      assert(InferenceStatusNifti.openInput(store, unit,
        InferenceStatusRepresentation(reference(store.root, s"$prefix/status.nii"), unit.inferenceEvidence.get.planes), path, 2).isLeft)
    assert(descriptors() <= before + 4)

  test("old unit has explicit absent evidence and status access refuses instead of fabricating success"):
    val (_, store, ref) = fixture()
    val unit = right(store.inspect(ref)).copy(inferenceEvidence = None)
    val output = right(LocalEstimateStore.open(Files.createTempDirectory("inference-old-")))
    val sink = right(output.newSink(unit, 30))
    unit.products.foreach: product =>
      right(sink.write(product.id, EstimateSelection(product.observations, product.targets.estimands, unit.domain.support),
        Array.fill(18)(0.0), Array.fill[Byte](18)(Validity.NotComputed.code)))
    val old = right(sink.seal())
    val source = right(output.open(old, ReadLimits(2))).asInstanceOf[InferenceEvidenceSource]
    try
      assertEquals(source.unit.inferenceEvidence, None)
      assert(source.readInferenceStatus(InferenceStatusSelection(Vector(InferenceStatusScope.Fit(unit.observations.head.id)), Vector(1)), new Array[Byte](1))
        .left.toOption.exists(_.isInstanceOf[EstimateError.Unsupported]))
    finally right(source.close())

/** Fresh-process readback, estimates/IO only; no fitter or producer involved. */
object InferenceEvidenceReadbackProbe:
  def main(args: Array[String]): Unit =
    require(args.length == 1)
    val root = Path.of(args(0))
    val path = "units/00000000-0000-4000-8000-000000000104/estimates.json"
    def right[A](value: Either[EstimateError, A]): A = value.fold(e => throw new IllegalStateException(e.message), identity)
    val store = right(LocalEstimateStore.open(root))
    val manifest = store.reference(right(store.objects.inspect(path).left.map(store.fromStore)))
    val pin = PinnedUnit(UnitId("00000000-0000-4000-8000-000000000103"), UnitRevisionId("00000000-0000-4000-8000-000000000104"), manifest)
    val source = right(store.open(pin, ReadLimits(12))).asInstanceOf[InferenceEvidenceSource]
    try
      val planes = source.unit.inferenceEvidence.get.planes
      val selection = InferenceStatusSelection(Vector(planes(2), planes(0), planes(1)), Vector(9, 2, 0, 4))
      val out = Array.fill[Byte](14)(99)
      var checks = 0
      require(source.readInferenceStatus(selection, out, () => { checks += 1; checks == 2 }) == Left(EstimateError.Cancelled))
      require(checks == 2 && out.forall(_ == 99.toByte))
      checks = 0
      require(source.readInferenceStatus(selection, out, () =>
        checks += 1
        if checks == 2 then throw new IllegalStateException("late callback failed")
        false).left.toOption.exists(_.isInstanceOf[EstimateError.Io]))
      require(checks == 2 && out.forall(_ == 99.toByte))
      right(source.readInferenceStatus(selection, out))
      require(out.toVector == Vector[Byte](9, 2, 0, 2, 9, 2, 0, 4, 2, 2, 0, 4, 99, 99))
      println("INFERENCE_EVIDENCE_LITERAL_READBACK_PASS cells=12 planes=3 lateCancel=true lateCallback=true tailUnchanged=true")
    finally right(source.close())
