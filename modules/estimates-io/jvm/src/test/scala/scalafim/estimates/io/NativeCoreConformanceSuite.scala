package scalafim.estimates.io

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import image4s.geometry.{Affine, D3}
import scalafim.estimates.*
import scalafim.image.SampleSpaces

/** Independent Core-NIfTI reader controls.  The byte encoder here deliberately
  * does not call the producer/sink or any NIfTI writer.
  */
class NativeCoreConformanceSuite extends munit.FunSuite:
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)

  private val dataset = DatasetId("00000000-0000-4000-8000-000000000901")
  private val model = ModelRevisionId("00000000-0000-4000-8000-000000000902")
  private val unitId = UnitId("00000000-0000-4000-8000-000000000903")
  private val revision = UnitRevisionId("00000000-0000-4000-8000-000000000904")
  private val first = Observation(ObservationId("scanner-row-01"), ParticipantId(dataset, "01"), Vector(AcquisitionId("run-1")))
  private val second = Observation(ObservationId("scanner-row-02"), ParticipantId(dataset, "02"), Vector(AcquisitionId("run-2")))
  private val task = EstimandId("task")
  private val drift = EstimandId("drift")
  private val affine = Affine.fromRowMajor[D3](Vector(
    -1.5, 0.25, 0.0, 10.0,
    0.1, 2.0, 0.3, -5.0,
    0.0, 0.2, 3.0, 7.0,
    0.0, 0.0, 0.0, 1.0)).toOption.get
  private val support = Vector.tabulate(24)(identity).filterNot(Set(7, 19))
  private val domain = right(EstimateDomain.make(SampleSpaces(Vector(2, 3, 4), affine = Some(affine)), support, "scanner"))
  private val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    Vector(first.id, second.id), ProductTargets.Scalar(Vector(task, drift)), PoolingScope.Run, "known synthetic signal")
  private val unit = EstimateUnit(dataset, unitId, revision,
    EstimandCatalog(model, Vector(task, drift).map(id => EstimandDefinition(id, id.value, EstimandKind.Coefficient, "signal", "a.u.", id.value))),
    domain, Vector(first, second), Vector.empty, Vector(effect), Map(effect.id -> ProductOutcome.Available(effect.id)),
    EstimabilityEvidence.Unknown("manually encoded conformance fixture"),
    EstimateProvenance("manual-nifti-1", "1", "native-core-conformance", ScientificFact.Unknown("synthetic"),
      ScientificFact.Unknown("synthetic"), ScientificFact.Unknown("synthetic"), ScientificFact.Unknown("synthetic"), Vector.empty, Vector.empty))

  private def expected(observation: ObservationId, estimand: EstimandId, sample: Int): Double =
    val row = unit.observations.indexWhere(_.id == observation)
    val axis = effect.targets.estimands.indexOf(estimand)
    row * 1000.0 + axis * 100.0 + sample

  /** NIfTI-1 little-endian single-file bytes, encoded field-by-field here rather
    * than through the implementation under test.  qform is intentionally an
    * aligned-anatomical alternative; scanner sform is the selected oblique/shear geometry.
    */
  private def nifti(values: Vector[Double], validity: Boolean, badSform: Boolean = false, oversizedDim: Boolean = false): Array[Byte] =
    val payload = if oversizedDim then 0 else values.size * (if validity then 1 else 8)
    val bytes = ByteBuffer.allocate(352 + payload).order(ByteOrder.LITTLE_ENDIAN)
    bytes.putInt(0, 348)
    bytes.putShort(40, 4.toShort)
    bytes.putShort(42, (if oversizedDim then 32768 else 2).toShort)
    bytes.putShort(44, (if oversizedDim then 1 else 3).toShort)
    bytes.putShort(46, (if oversizedDim then 1 else 4).toShort)
    bytes.putShort(48, (if oversizedDim then 1 else 2).toShort)
    bytes.putShort(70, (if validity then 2 else 64).toShort)
    bytes.putShort(72, (if validity then 8 else 64).toShort)
    (0 to 4).foreach(i => bytes.putFloat(76 + i * 4, 1.0f))
    bytes.putFloat(108, 352.0f)
    bytes.putFloat(112, 1.0f)
    bytes.putFloat(116, 0.0f)
    bytes.put(123, 2.toByte) // millimetres, and no temporal unit
    bytes.putShort(252, 2.toShort) // aligned anatomical qform, deliberately distinct
    bytes.putShort(254, 1.toShort) // scanner sform selected by Core-NIfTI
    bytes.putFloat(76, -1.0f) // qfac: the alternate qform has the scanner sform's handedness
    bytes.putFloat(268, 30.0f)
    bytes.putFloat(272, 4.0f)
    bytes.putFloat(276, 2.0f)
    val rows = Vector(
      Vector(if badSform then -1.7f else -1.5f, 0.25f, 0.0f, 10.0f),
      Vector(0.1f, 2.0f, 0.3f, -5.0f),
      Vector(0.0f, 0.2f, 3.0f, 7.0f))
    rows.zipWithIndex.foreach:
      case (row, index) =>
        row.zipWithIndex.foreach:
          case (value, column) => bytes.putFloat(280 + index * 16 + column * 4, value)
    bytes.put(344, 'n'.toByte); bytes.put(345, '+'.toByte); bytes.put(346, '1'.toByte); bytes.put(347, 0.toByte)
    if !oversizedDim then
      bytes.position(352)
      if validity then values.foreach(value => bytes.put(value.toByte)) else values.foreach(bytes.putDouble)
    bytes.array()

  private def payload(observation: ObservationId, validity: Boolean): Vector[Double] =
    (for axis <- 0 until 2; sample <- 0 until 24 yield
      if validity then (if domain.contains(sample) then Validity.Valid.code else Validity.OutsideSupport.code).toDouble
      else expected(observation, effect.targets.estimands(axis), sample)).toVector

  private def publish(store: LocalEstimateStore, name: String, bytes: Array[Byte]): FileReference =
    store.reference(right(store.objects.write(s"units/${revision.value}/$name")(_.write(bytes)).left.map(store.fromStore)))

  private def manualReference(store: LocalEstimateStore): PinnedUnit =
    val representations = unit.observations.map: observation =>
      NiftiRepresentation(effect.id, observation.id,
        publish(store, s"${observation.id.value}-values.nii", nifti(payload(observation.id, false), false)),
        publish(store, s"${observation.id.value}-validity.nii", nifti(payload(observation.id, true), true)),
        NumericPrecision.Float64, 1.0, 0.0, Vector(task, drift), "scanner-sform",
        qformAlternativeFrame = Some("aligned-anatomical"), storedDatatype = Some(NiftiStoredDatatype.Float64))
    right(store.publishUnit(unit, representations))

  test("hand-encoded non-RAS scanner sform selects oblique shear world corners and actual open rejects false binding"):
    val root = Files.createTempDirectory("native-core-sform-")
    val headerPath = root.resolve("manual.nii")
    Files.write(headerPath, nifti(payload(first.id, false), false))
    val header = scalafim.image.io.Nifti.readHeader(headerPath).fold(error => fail(error.message), identity)
    right(NiftiEstimateSource.validateHeader(unit, effect, header, validity = false, Some("aligned-anatomical")))
    val origin = header.sform.get.matrix
    assertEqualsDouble(origin(0, 0), -1.5, 1e-6)
    assertEqualsDouble(origin(0, 1), 0.25, 1e-6)
    assertEqualsDouble(origin(1, 2), 0.3, 1e-6)
    assertEqualsDouble(origin(0, 3), 10.0, 1e-6)
    assertEqualsDouble(origin(2, 3), 7.0, 1e-6)
    def world(x: Double, y: Double, z: Double): Vector[Double] =
      Vector((0 until 4).map(column => origin(0, column) * Vector(x, y, z, 1.0)(column)).sum,
        (0 until 4).map(column => origin(1, column) * Vector(x, y, z, 1.0)(column)).sum,
        (0 until 4).map(column => origin(2, column) * Vector(x, y, z, 1.0)(column)).sum)
    def assertWorld(voxel: (Double, Double, Double), expected: Vector[Double]): Unit =
      world(voxel._1, voxel._2, voxel._3).zip(expected).foreach: (actual, wanted) =>
        assertEqualsDouble(actual, wanted, 1e-6)
    assertWorld((0, 0, 0), Vector(10.0, -5.0, 7.0))
    assertWorld((1, 0, 0), Vector(8.5, -4.9, 7.0))
    assertWorld((0, 2, 0), Vector(10.5, -1.0, 7.4))
    assertWorld((1, 2, 3), Vector(9.0, 0.0, 16.4))
    val store = right(LocalEstimateStore.open(root.resolve("store")))
    val ref = manualReference(store)
    val manifest = Files.readString(store.root.resolve(ref.manifest.path))
    val representations = right(EstimateMetadata.representations(manifest))
    val altered = publish(store, "false-binding-values.nii", nifti(payload(first.id, false), false, badSform = true))
    val changed = representations.updated(0, representations.head.copy(values = altered))
    val replacement = right(store.writeText(s"units/${revision.value}/false-binding.json",
      EstimateMetadata.unit(unit, right(EstimateMetadata.catalogReference(manifest)), changed, right(EstimateMetadata.indexTables(manifest)))))
    val rejected = right(LocalEstimateStore.open(store.root)).open(ref.copy(manifest = replacement), ReadLimits(24))
    assert(rejected.left.toOption.exists(_.isInstanceOf[EstimateError.Integrity]))
    val stages = Files.walk(store.root.resolve(".staging"))
    try assert(!stages.anyMatch(path => Files.isRegularFile(path)), "failed uncompressed open must not leave owned reader stages")
    finally stages.close()

  test("NIfTI-1 signed header probe is separate from legitimate 32768 logical writer refusal and stage cleanup"):
    val root = Files.createTempDirectory("native-core-dimension-")
    val malformed = root.resolve("dim-32768.nii")
    Files.write(malformed, nifti(Vector.empty, validity = false, oversizedDim = true))
    val tiny = right(EstimateDomain.make(SampleSpaces(Vector(2, 3, 4)), Vector(0), "scanner"))
    assertEquals(tiny.sampleCount, 24)
    val header = scalafim.image.io.Nifti.readHeader(malformed).fold(error => fail(error.message), identity)
    val rejected = NiftiEstimateSource.validateHeader(unit, effect, header, validity = false)
    assert(rejected.left.toOption.exists(_.isInstanceOf[EstimateError.Integrity]))
    assert(rejected.left.toOption.exists(_.message.contains("dimensions")))
    assert(!Files.exists(root.resolve(".staging")))
    assertEquals(Files.size(malformed), 352L)
    val largeDomain = right(EstimateDomain.make(SampleSpaces(Vector(32768, 1, 1)), Vector(0), "scanner"))
    val large = unit.copy(revision = UnitRevisionId("00000000-0000-4000-8000-000000000905"), domain = largeDomain)
    val outputRoot = root.resolve("writer")
    val output = right(LocalEstimateStore.open(outputRoot))
    val refused = output.newSink(large, 1)
    assert(refused.left.toOption.exists(_.isInstanceOf[EstimateError.Io]))
    assert(refused.left.toOption.exists(_.message.contains("extent 32768 cannot fit a positive NIfTI-1 dimension")))
    assert(!Files.exists(outputRoot.resolve(s"units/${large.revision.value}/estimates.json")))
    val stages = Files.walk(outputRoot.resolve(".staging"))
    try assert(!stages.anyMatch(path => Files.isRegularFile(path)))
    finally stages.close()

  test("fresh Core-NIfTI reader preserves ordered two-observation selections, validity, caps, duplicates, and cancellation"):
    val root = Files.createTempDirectory("native-core-reader-")
    val ref = manualReference(right(LocalEstimateStore.open(root)))
    val source = right(right(LocalEstimateStore.open(root)).open(ref, ReadLimits(24)))
    try
      // Access map selects both ordered catalog axes in an intentionally reordered request.
      val map = EstimateSelection(Vector(second.id, first.id), Vector(drift, task), Vector(0, 7, 23))
      val mapValues = Array.fill(12)(Double.NaN)
      val mapValidity = new Array[Byte](12)
      right(source.read(effect.id, map, mapValues, mapValidity))
      val expectedMap = for observation <- map.observations; estimand <- map.estimands; sample <- map.samples yield expected(observation, estimand, sample)
      assertEquals(mapValues.toVector, expectedMap)
      assertEquals(mapValidity.toVector, Vector(Validity.Valid.code, Validity.OutsideSupport.code, Validity.Valid.code,
        Validity.Valid.code, Validity.OutsideSupport.code, Validity.Valid.code, Validity.Valid.code, Validity.OutsideSupport.code,
        Validity.Valid.code, Validity.Valid.code, Validity.OutsideSupport.code, Validity.Valid.code))
      // Three z-plane anchors, a z voxel profile, a small ROI, and a cohort slab all use EstimateSelection.
      val planes = EstimateSelection(Vector(first.id), Vector(task), Vector(0, 6, 12))
      val profile = EstimateSelection(Vector(first.id), Vector(drift), Vector(0, 6, 7, 12, 18))
      val roi = EstimateSelection(Vector(second.id), Vector(task), Vector(1, 2, 4, 5))
      val slab = EstimateSelection(Vector(first.id, second.id), Vector(drift), Vector(20, 21, 22, 23))
      Vector(planes, profile, roi, slab).foreach: selection =>
        val values = new Array[Double](selection.cells.toInt)
        val codes = new Array[Byte](selection.cells.toInt)
        right(source.read(effect.id, selection, values, codes))
        val wanted = for observation <- selection.observations; estimand <- selection.estimands; sample <- selection.samples yield expected(observation, estimand, sample)
        assertEquals(values.toVector, wanted)
        val wantedCodes = for _ <- selection.observations; _ <- selection.estimands; sample <- selection.samples
          yield if domain.contains(sample) then Validity.Valid.code else Validity.OutsideSupport.code
        assertEquals(codes.toVector, wantedCodes)
      val tooSmall = source.read(effect.id, planes, new Array[Double](2), new Array[Byte](3))
      assert(tooSmall.left.toOption.exists(_.isInstanceOf[EstimateError.Invalid]))
      val overLimit = map.copy(samples = Vector.tabulate(24)(identity))
      val tooWide = source.read(effect.id, overLimit, new Array[Double](overLimit.cells.toInt), new Array[Byte](overLimit.cells.toInt))
      assert(tooWide.left.toOption.exists(_.isInstanceOf[EstimateError.Invalid]))
      intercept[IllegalArgumentException](EstimateSelection(Vector(first.id, first.id), Vector(task), Vector(0)))
      intercept[IllegalArgumentException](EstimateSelection(Vector(first.id), Vector(task, task), Vector(0)))
      intercept[IllegalArgumentException](EstimateSelection(Vector(first.id), Vector(task), Vector(0, 0)))
      val cancelledValues = Array.fill(3)(-1.0)
      val cancelledValidity = Array.fill[Byte](3)(99)
      assertEquals(source.read(effect.id, planes, cancelledValues, cancelledValidity, () => true), Left(EstimateError.Cancelled))
      assertEquals(cancelledValues.toVector, Vector(-1.0, -1.0, -1.0))
      assertEquals(cancelledValidity.toVector, Vector[Byte](99, 99, 99))
    finally right(source.close())
