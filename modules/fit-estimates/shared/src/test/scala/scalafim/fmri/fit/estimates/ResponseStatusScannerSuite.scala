package scalafim.fmri.fit.estimates

import scala.collection.mutable.ArrayBuffer
import scalafim.estimates.*
import scalafim.image.SampleSpaces

/** A provider that validates like a physical store and records every read. */
final class InstrumentedStatusSource(
    val unit: EstimateUnit,
    fitCodes: Map[Int, InferenceStatusCode],
    val limits: ReadLimits = ReadLimits(maximumCells = 64),
    failOnRead: Option[Int] = None,
    tamper: InferenceStatusReceipt => InferenceStatusReceipt = identity
) extends InferenceEvidenceSource:
  val reads: ArrayBuffer[InferenceStatusSelection] = ArrayBuffer.empty
  def samplesRead: Vector[Int] = reads.iterator.flatMap(_.samples).toVector

  def read(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte],
      cancelled: () => Boolean) = Left(EstimateError.Unsupported("status-only fixture"))

  def close() = Right(())

  def readInferenceStatus(selection: InferenceStatusSelection, codes: Array[Byte], cancelled: () => Boolean) =
    InferenceStatusValidation.check(unit, selection, codes.length, limits.maximumCells).flatMap: _ =>
      reads += selection
      if failOnRead.contains(reads.size) then Left(EstimateError.Io("simulated device failure"))
      else if cancelled() then Left(EstimateError.Cancelled)
      else
        var i = 0
        selection.planes.foreach: plane =>
          selection.samples.foreach: sample =>
            codes(i) = (plane match
              case InferenceStatusScope.Fit(_) => fitCodes(sample)
              case _ => InferenceStatusCode.Unrecorded).code
            i += 1
        Right(tamper(InferenceStatusReceipt(selection, selection.cells.toInt)))

object ScannerFixture:
  val dataset: DatasetId = DatasetId("00000000-0000-4000-8000-000000000401")
  val observation: ObservationId = ObservationId("sub-01")
  val other: ObservationId = ObservationId("sub-02")
  val columns: Vector[ColumnId] = Vector("A", "B", "intercept").map(ColumnId.apply)
  val estimands: Vector[EstimandId] = columns.map(c => EstimandId(c.value))
  /** Out-of-grid order on purpose: 8 samples, 5 in support. */
  val support: Vector[Int] = Vector(6, 1, 4, 3, 0)
  val outside: Vector[Int] = Vector(2, 5, 7)
  val codes: Map[Int, InferenceStatusCode] = Map(
    6 -> InferenceStatusCode.Estimable, 1 -> InferenceStatusCode.Constant, 4 -> InferenceStatusCode.Estimable,
    3 -> InferenceStatusCode.Unrecorded, 0 -> InferenceStatusCode.Estimable) ++
    outside.map(_ -> InferenceStatusCode.OutsideSupport)
  val conditioning: ScientificFact = ScientificFact.Known("full-rank shared design; rank-revealing QR")

  def evidence(
      observations: Vector[ObservationId] = Vector(observation),
      planes: Vector[InferenceStatusScope] = Vector(InferenceStatusScope.Fit(observation))
  ): InferenceEvidence =
    InferenceEvidence(observations.map(o => CoefficientInferenceEvidence(o, columns, columns.take(2), "task columns",
      ScientificFact.Known("OLS"), conditioning)), planes)

  def unit(evidence: Option[InferenceEvidence] = Some(ScannerFixture.evidence()), domainSupport: Vector[Int] = support): EstimateUnit =
    val unknown = ScientificFact.Unknown("fixture")
    val observations = Vector(observation, other).map(o => Observation(o, ParticipantId(dataset, "01"), Vector(AcquisitionId("run"))))
    val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64, observations.map(_.id),
      ProductTargets.Scalar(estimands), PoolingScope.Run, "signal")
    EstimateUnit(dataset, UnitId("00000000-0000-4000-8000-000000000402"), UnitRevisionId("00000000-0000-4000-8000-000000000403"),
      EstimandCatalog(ModelRevisionId("00000000-0000-4000-8000-000000000404"),
        estimands.map(id => EstimandDefinition(id, id.value, EstimandKind.Coefficient, "signal", "unit", id.value))),
      EstimateDomain.make(SampleSpaces(Vector(8, 1, 1)), domainSupport, "scanner").toOption.get, observations,
      estimands.zipWithIndex.map((id, i) => EstimandBinding(id, columns, 1, Vector.tabulate(3)(c => if c == i then 1.0 else 0.0))),
      Vector(effect), Map(effect.id -> ProductOutcome.Available(effect.id)), EstimabilityEvidence.Unknown("fixture"),
      EstimateProvenance("fixture", "1", "exec", unknown, unknown, unknown, unknown, Vector.empty, Vector.empty),
      inferenceEvidence = evidence)

  def digest(label: String): ProviderDigest = ResponseDigests.provider("fixture/v1", label)

  def binding(u: EstimateUnit = unit(), selected: Vector[ColumnId] = columns.take(2),
      boundColumns: Vector[ColumnId] = columns): ResponseSourceBinding =
    ResponseSourceBinding(u.revision, observation, digest("design"), digest("preparation"), digest("noise"), digest("run"),
      digest("readout"), ReadoutAxis.parse(Vector(ReadoutRowId(ConditionLevelId("A"), 0), ReadoutRowId(ConditionLevelId("B"), 0))).toOption.get,
      boundColumns, selected, ResponseDigests.features(u.domain), RealizedNoise.White, RealizedCombination.SingleRun)

  def request(b: ResponseSourceBinding): ResponseActionRequest =
    ResponseActionRequest(ResponseActionEvidence.Version, b, Some(DeclaredResponseModel(DeclaredTemporal.White,
      DeclaredCombination.SingleRun, SpatialClaim.KroneckerSeparable, DistributionClaim.Gaussian, ModelOrigin.Declared("prereg"))),
      ConditionAction.Permute(Map(ConditionLevelId("A") -> ConditionLevelId("B"), ConditionLevelId("B") -> ConditionLevelId("A"))),
      NullConstraint.Linear(digest("A = B")))

class ResponseStatusScannerSuite extends munit.FunSuite:
  import ScannerFixture.*

  private val never: () => Boolean = () => false
  private def scan(src: InferenceEvidenceSource, b: ResponseSourceBinding, chunk: Int = 2, cancelled: () => Boolean = never) =
    ResponseStatusScanner.scan(src, b, chunk, cancelled)

  test("multi-chunk scans read only bound in-support features and count every code exactly"):
    val u = unit()
    val b = binding(u)
    for chunk <- Vector(1, 2, 40) do
      val src = InstrumentedStatusSource(u, codes)
      val result = scan(src, b, chunk)
      val expected = StatusSummary(u.revision, new InferenceStatusScope.Fit(observation), b.features,
        Map(InferenceStatusCode.Estimable -> 3L, InferenceStatusCode.Constant -> 1L, InferenceStatusCode.Unrecorded -> 1L), conditioning)
      assertEquals(result, Right(StatusEvidence.Scanned(expected)), s"chunk $chunk")
      assertEquals(src.samplesRead, support, s"chunk $chunk: exactly the ordered support, once")
      assert(src.samplesRead.forall(s => !outside.contains(s)), s"chunk $chunk read an out-of-support sample")
      assert(src.reads.forall(r => r.samples.size <= chunk && r.planes == Vector(InferenceStatusScope.Fit(observation))))
      assertEquals(src.reads.size, (support.size + chunk - 1) / chunk)
      assertEquals(ResponseActionEvidence.evaluate(request(b), b, result.toOption),
        ResponseActionRefusal.Unavailable(UnavailableReason.NonEstimableFeatures(2L)))

  test("chunks are also bounded by the provider's read limit"):
    val u = unit()
    val src = InstrumentedStatusSource(u, codes, ReadLimits(maximumCells = 2))
    assert(scan(src, binding(u), 40).isRight)
    assertEquals(src.reads.map(_.samples.size).toVector, Vector(2, 2, 1))

  test("an OutsideSupport code among bound features is a source mismatch"):
    val u = unit()
    val b = binding(u)
    val result = scan(InstrumentedStatusSource(u, codes + (4 -> InferenceStatusCode.OutsideSupport)), b)
    val outsideCount = result.map {
      case StatusEvidence.Scanned(s) => s.count(InferenceStatusCode.OutsideSupport)
      case other => fail(other.toString)
    }
    assertEquals(outsideCount, Right(1L))
    assertEquals(ResponseActionEvidence.evaluate(request(b), b, result.toOption),
      ResponseActionRefusal.SourceMismatch(SourceField.StatusOutsideSupport, "0", "1"))

  test("a receipt with the wrong selection or cell count is refused"):
    val u = unit()
    val wrongSelection = InstrumentedStatusSource(u, codes, tamper = r => r.copy(selection = r.selection.copy(samples = r.selection.samples.reverse)))
    assert(scan(wrongSelection, binding(u), 2).left.exists(_.isInstanceOf[EstimateError.Integrity]))
    val wrongCells = InstrumentedStatusSource(u, codes, tamper = r => r.copy(cells = r.cells - 1))
    assert(scan(wrongCells, binding(u)).left.exists(_.isInstanceOf[EstimateError.Integrity]))
    val single = InstrumentedStatusSource(u, codes, tamper = r => r.copy(selection = r.selection.copy(samples = Vector(7))))
    assert(scan(single, binding(u), 1).left.exists(_.isInstanceOf[EstimateError.Integrity]))

  test("IO failure and cancellation mid-scan return a typed Left and no summary"):
    val u = unit()
    val failing = InstrumentedStatusSource(u, codes, failOnRead = Some(2))
    assertEquals(scan(failing, binding(u), 2), Left(EstimateError.Io("simulated device failure")))
    assertEquals(failing.reads.size, 2)
    val src = InstrumentedStatusSource(u, codes)
    val cancelAfterFirst: () => Boolean = () => src.reads.nonEmpty
    assertEquals(scan(src, binding(u), 2, cancelAfterFirst), Left(EstimateError.Cancelled))
    assertEquals(src.reads.size, 1, "no read after cancellation is observed")
    val immediate = InstrumentedStatusSource(u, codes)
    assertEquals(scan(immediate, binding(u), 2, () => true), Left(EstimateError.Cancelled))
    assert(immediate.reads.isEmpty)

  test("exact lookups: every absence is typed and reads nothing"):
    def absent(u: EstimateUnit, b: ResponseSourceBinding, reason: StatusAbsence) =
      val src = InstrumentedStatusSource(u, codes)
      assertEquals(scan(src, b), Right(StatusEvidence.Absent(reason)), reason.toString)
      assert(src.reads.isEmpty, s"$reason must not read")
      assertEquals(ResponseActionEvidence.evaluate(request(b), b, Some(StatusEvidence.Absent(reason))),
        ResponseActionRefusal.Unavailable(UnavailableReason.InferenceStatus(reason)))
    val none = unit(evidence = None)
    absent(none, binding(none), StatusAbsence.UnitHasNoEvidence)
    // Evidence exists, but only for another observation.
    val elsewhere = unit(evidence = Some(evidence(observations = Vector(other), planes = Vector(InferenceStatusScope.Fit(other)))))
    absent(elsewhere, binding(elsewhere), StatusAbsence.NoCoefficientEvidence)
    val u = unit()
    absent(u, binding(u, boundColumns = columns.reverse), StatusAbsence.ColumnsDisagree)
    absent(u, binding(u, selected = Vector(columns(0), columns(2))), StatusAbsence.SelectedNotInferable(Vector(columns(2))))
    // Some(InferenceEvidence) with matching coefficients but without a Fit(obs) plane.
    val planeless = unit(evidence = Some(evidence(observations = Vector(observation, other), planes = Vector(InferenceStatusScope.Fit(other)))))
    absent(planeless, binding(planeless), StatusAbsence.FitPlaneAbsent)

  test("a different unit revision is refused before any read; a different feature identity reads nothing"):
    val u = unit()
    val src = InstrumentedStatusSource(u, codes)
    val foreign = binding(u).copy(unit = UnitRevisionId("00000000-0000-4000-8000-000000000499"))
    assert(scan(src, foreign).left.exists(_.isInstanceOf[EstimateError.Conflict]))
    assert(src.reads.isEmpty)
    // Same sample-ID set, different physical order.
    val permuted = unit(domainSupport = support.reverse)
    val stale = binding(u)
    val permutedSource = InstrumentedStatusSource(permuted, codes)
    val result = scan(permutedSource, stale)
    assert(permutedSource.reads.isEmpty, "features the binding does not name are never read")
    assertEquals(ResponseActionEvidence.evaluate(request(stale), stale, result.toOption),
      ResponseActionRefusal.SourceMismatch(SourceField.StatusFeatures, stale.features.render, ResponseDigests.features(permuted.domain).render))

  test("a non-positive chunk is refused"):
    val u = unit()
    assert(scan(InstrumentedStatusSource(u, codes), binding(u), 0).left.exists(_.isInstanceOf[EstimateError.Invalid]))
