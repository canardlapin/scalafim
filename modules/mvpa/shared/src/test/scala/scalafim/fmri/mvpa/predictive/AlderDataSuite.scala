package scalafim.fmri.mvpa.predictive

import alder.kernel.BatchSize
import alder.kernel.Data
import alder.kernel.DataFingerprint
import alder.kernel.Example
import alder.kernel.RowId
import alder.kernel.Use
import multivar.core.ValueId
import resample4s.core.IndexSpace
import resample4s.core.Injection
import scalafim.fmri.mvpa.*

final class AlderDataSuite extends munit.FunSuite:
  private val sampleIds =
    Vector("trial-101", "trial-305", "trial-902").map(SampleId.unsafe)
  private val featureIds =
    Vector("voxel-17", "voxel-41", "voxel-88").map(FeatureId.unsafe)
  private val samples = sampleAxis("predictive-samples", sampleIds)
  private val features = featureAxis("predictive-features", featureIds)
  private val patterns =
    GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 10.0, 100.0),
        Vector(2.0, 20.0, 200.0),
        Vector(3.0, 30.0, 300.0)
      )
    )
  private val evidence =
    EvidenceTable
      .dense(samples, features, patterns, ValueId.unsafe("predictive-patterns"))
      .toOption
      .get
  private val target = Column(samples, Vector("a", "b", "c")).toOption.get
  private val metadata = Column(samples, Vector(11, 22, 33)).toOption.get
  private val plan =
    ScientificPlanFingerprint(
      "scalafim-mvpa-plan-v1-" + Vector.fill(64)("a").mkString
    ).toOption.get

  private def sampleAxis(
      id: String,
      keys: Vector[SampleId]
  ): AxisRef[SampleId] =
    AxisRef
      .create(
        AxisId.unsafe(id),
        AxisPurpose.Samples,
        keys,
        CoordinateBasis.unsafe("trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("fixture", "v1")
      )
      .toOption
      .get

  private def featureAxis(
      id: String,
      keys: Vector[FeatureId]
  ): AxisRef[FeatureId] =
    AxisRef
      .create(
        AxisId.unsafe(id),
        AxisPurpose.NeuralFeatures,
        keys,
        CoordinateBasis.unsafe("voxel-table"),
        Some(AxisUnits.unsafe("percent-signal-change")),
        AxisScale.nominal,
        CoordinateProvenance.unsafe("fixture-mask", "v1")
      )
      .toOption
      .get

  private def indices(values: Int*): IArray[Int] =
    IArray.unsafeFromArray(values.toArray)

  private val restriction =
    val space = IndexSpace.of(samples.size).toOption.get
    ReindexingLeg
      .injection(samples, Injection.from(indices(2, 0), space).toOption.get)
      .toOption
      .get

  private val measurement =
    val space = IndexSpace.of(features.size).toOption.get
    Measurement
      .hardSelection(
        features,
        MeasurementId.unsafe("ordered-roi"),
        Injection.from(indices(1, 0), space).toOption.get
      )
      .toOption
      .get

  private def prepared =
    AlderDataBoundary
      .prepare(
        plan,
        evidence,
        target,
        metadata,
        restriction,
        measurement,
        MaterializationPolicy.Allow(MaterializationBudget.unsafe(4L))
      )
      .toOption
      .get

  test("matrix boundary retains SampleId while Alder RowId remains an ordinal"):
    val value = prepared
    val emitted = value.data.foldRows(
      Vector.empty[(RowId, Example[AlderPatternRow[measurement.local.Id], String, Int])]
    )((rows, id, example) => rows :+ (id, example))

    assertEquals(emitted.map(_._1.value), Vector(0L, 1L))
    assertEquals(
      emitted.map(row => value.ledger.sampleAt(row._1).toOption.get.value),
      Vector("trial-902", "trial-101")
    )
    assertEquals(value.ledger.executionOrdinal(sampleIds(2)), Some(0L))
    assertEquals(value.ledger.executionOrdinal(sampleIds(0)), Some(1L))
    assertEquals(emitted.map(_._2.target), Vector("c", "a"))
    assertEquals(emitted.map(_._2.meta), Vector(33, 11))
    assertEquals(
      emitted.map(row => value.capabilities.featureView.read(row._2.input).toOption.get.toVector),
      Vector(Vector(30.0, 3.0), Vector(10.0, 1.0))
    )
    assertEquals(
      value.ledger.reconstruct(value.data, requireComplete = true).toOption.get.map(_._1.value),
      Vector("trial-902", "trial-101")
    )

  test("boundary receipts restriction, measurement, materialization, and learner input separately"):
    val receipt = prepared.receipt

    assertEquals(receipt.plan, plan)
    assertEquals(receipt.restriction.sourceRows, 3)
    assertEquals(receipt.restriction.selectedRows, 2)
    assertEquals(receipt.restriction.mappingEntries, 2L)
    assertEquals(receipt.measurement.sourceFeatures, 3)
    assertEquals(receipt.measurement.localFeatures, 2)
    assertEquals(receipt.measurement.compositions, 1L)
    assertEquals(receipt.materialization.elements, 4L)
    assertEquals(receipt.learnerInput.rows, 2)
    assertEquals(receipt.learnerInput.features, 2)
    assertEquals(receipt.learnerInput.rowViewsAllocated, 2L)
    assertEquals(receipt.learnerInput.exampleRecordsAllocated, 2L)
    assertEquals(receipt.learnerInput.coordinateArraysAllocatedAtBoundary, 0L)
    assertEquals(receipt.learnerInput.coordinateArraysPerFeatureViewRead, 1)
    assertEquals(receipt.learnerInput.coordinateArraysPerFeatureViewWrite, 0)
    assertEquals(receipt.dataFingerprint.policy, prepared.data.fingerprint.policy)
    assertEquals(receipt.dataFingerprint.digest, prepared.data.fingerprint.digest)

    var batches = Vector.empty[Vector[Long]]
    prepared.data.foreachBatch(BatchSize.const(1)): batch =>
      batches = batches :+ Vector.tabulate(batch.length)(index => batch.rowId(index).value)
    assertEquals(batches, Vector(Vector(0L), Vector(1L)))

  test("materialization policy is an actual gate"):
    assert(
      AlderDataBoundary
        .prepare(
          plan,
          evidence,
          target,
          metadata,
          restriction,
          measurement,
          MaterializationPolicy.Reject
        )
        .left
        .exists:
          case AlderDataError.Evidence(EvidenceTableError.MaterializationRejected) => true
          case _                                                                   => false
    )

  test("ledger rejects duplicate, missing, and foreign Alder outputs"):
    val value = prepared
    val rows =
      value.data.foldRows(Vector.empty[(RowId, String)])((result, id, example) => result :+ (id -> example.target))

    val duplicate = data(value.data.fingerprint, rows.head, rows.head)
    assert(
      value.ledger
        .reconstruct(duplicate, requireComplete = false)
        .left
        .exists:
          case AlderDataError.DuplicateRowId(0L) => true
          case _                                 => false
    )

    val missing = data(value.data.fingerprint, rows.head)
    assert(
      value.ledger
        .reconstruct(missing, requireComplete = true)
        .left
        .exists:
          case AlderDataError.MissingRow(sample) => sample == sampleIds(0)
          case _                                 => false
    )

    val threeRows = alder.data.InMemoryData.unsplit(Vector("x", "y", "z"), "foreign")
    val foreignId = threeRows.foldRows(Option.empty[RowId])((_, id, _) => Some(id)).get
    val foreign = data(value.data.fingerprint, foreignId -> "foreign")
    assert(
      value.ledger
        .reconstruct(foreign, requireComplete = false)
        .left
        .exists:
          case AlderDataError.RowIdOutOfRange(2L, 2) => true
          case _                                     => false
    )

  private def data(
      dataFingerprint: DataFingerprint,
      rows: (RowId, String)*
  ): Data[Use.Unsplit, String] =
    val values = rows.toVector
    new Data[Use.Unsplit, String]:
      override val size: Long = values.length.toLong
      override val fingerprint: DataFingerprint = dataFingerprint
      override def foldRows[B](initial: B)(
          step: (B, RowId, String) => B
      ): B =
        values.foldLeft(initial): (current, row) =>
          step(current, row._1, row._2)
