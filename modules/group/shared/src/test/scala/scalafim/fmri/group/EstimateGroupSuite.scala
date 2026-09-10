package scalafim.fmri.group

import scalafim.archive.ContentDigest
import scalafim.estimates.*
import scalafim.image.SampleSpaces

class EstimateGroupSuite extends munit.FunSuite:
  private val dataset = DatasetId("00000000-0000-4000-8000-000000000001")
  private val model = ModelRevisionId("00000000-0000-4000-8000-000000000002")
  private val targets = Vector(EstimandId("a"), EstimandId("b"))
  private val catalog = EstimandCatalog(model, targets.map(id =>
    EstimandDefinition(id, "same label", EstimandKind.Coefficient, "signal", "unit", id.value)))
  private val domain = EstimateDomain.make(SampleSpaces(Vector(4, 1, 1)), Vector(0, 1, 2, 3), "scanner").toOption.get
  private val observation = ObservationId("local-row")
  private val effect = ProductDescriptor(ProductId("effect"), ProductKind.Effect, NumericPrecision.Float64,
    Vector(observation), ProductTargets.Scalar(targets), PoolingScope.Run, "signal")
  private val se = effect.copy(id = ProductId("se"), kind = ProductKind.StandardError)
  private def unit(row: Int): EstimateUnit =
    EstimateUnit(dataset, UnitId(f"00000000-0000-4000-8000-${row + 10}%012d"),
      UnitRevisionId(f"00000000-0000-4000-8000-${row + 20}%012d"), catalog, domain,
      Vector(Observation(observation, ParticipantId(dataset, s"subject-$row"), Vector(AcquisitionId("run")))), Vector.empty,
      Vector(effect, se), Map(effect.id -> ProductOutcome.Available(effect.id), se.id -> ProductOutcome.Available(se.id)),
      EstimabilityEvidence.Unknown("fixture"), EstimateProvenance("fixture", "1", "test", ScientificFact.Unknown("imported"),
        ScientificFact.Unknown("imported"), ScientificFact.Unknown("imported"), ScientificFact.Unknown("imported"), Vector.empty, Vector.empty))
  private def pinned(unit: EstimateUnit) = PinnedUnit(unit.unit, unit.revision,
    FileReference(s"${unit.revision.value}.json", ContentDigest.unsafeSha256("a" * 64), 1))
  private def right[A](value: Either[EstimateError, A]): A = value.fold(e => fail(e.message), identity)

  private class Reader(val declared: Vector[EstimateUnit], invalid: Boolean = false) extends EstimateSetReader:
    var opened = 0
    var active = 0
    var maximumActive = 0
    def inspect(reference: PinnedUnit) = declared.find(_.revision == reference.revision).toRight(EstimateError.Invalid("unknown reference"))
    def open(reference: PinnedUnit, requestedLimits: ReadLimits) = inspect(reference).map: declaredUnit =>
      val row = declared.indexOf(declaredUnit)
      opened += 1
      active += 1
      maximumActive = math.max(maximumActive, active)
      new EstimateSource:
        val unit = declaredUnit
        val limits = requestedLimits
        def read(product: ProductId, selection: EstimateSelection, values: Array[Double], validity: Array[Byte], cancelled: () => Boolean) =
          if cancelled() then Left(EstimateError.Cancelled)
          else EstimateReadValidation.check(unit, product, selection, values.length, validity.length, limits).map: _ =>
            selection.samples.zipWithIndex.foreach: (sample, index) =>
              values(index) = if product == effect.id then row * 10.0 + targets.indexOf(selection.estimands.head) * 100.0 + sample else row + 1.0
              validity(index) = if invalid && row == 1 && sample == 1 then Validity.NonEstimable.code else Validity.Valid.code
            EstimateReadReceipt(product, selection, selection.cells.toInt)
        def close() =
          active -= 1
          Right(())

  // The test owns an exactly constructed common grid. Production callers must
  // supply their actual registration/scientific-admission implementation.
  private val admission = new GroupEstimateAdmission:
    def verify(units: Vector[EstimateUnit]) =
      if units.forall(_.domain eq domain) then Right(()) else Left(EstimateError.Invalid("not the admitted fixture grid"))
  private def inputs(units: Vector[EstimateUnit]) = units.map(u => GroupEstimateInput(pinned(u), observation, effect.id,
    Some(GroupMarginalUncertainty.StandardError(se.id))))

  test("pinned group blocks preserve participant, target and sample order with one owned source at a time") {
    val reader = new Reader(Vector(unit(0), unit(1)))
    val source = right(EstimateGroup.prepare(reader, inputs(reader.declared), targets.reverse, admission, 16))
    assertEquals(reader.opened, 0)
    val receipt = right(source.readBlock(Vector(3, 0)))
    assertEquals(receipt.inputs, source.inputs)
    val block = receipt.data
    assertEquals(block.contrasts, Vector("b", "a"))
    assertEquals(block.subjects.map(_.value), Vector(s"${dataset.value}/subject-0", s"${dataset.value}/subject-1"))
    val b = block.response("b").get
    assertEqualsDouble(b.effects(1, 0), 113.0, 0.0)
    assertEqualsDouble(b.effects(0, 1), 100.0, 0.0)
    assertEqualsDouble(b.variances.get(1, 0), 4.0, 0.0)
    assertEquals(reader.maximumActive, 1)
    assertEquals(reader.active, 0)
    assertEquals(block.space.asInstanceOf[GroupSpace.VoxelAxis].sampleIndices, Vector(3, 0))
    assert(source.readBlock(Vector(0, 1, 2)).isLeft)
    assertEquals(reader.opened, 2)
  }

  test("invalid cells reject a complete block and all owned sources close on failure or cancellation") {
    val reader = new Reader(Vector(unit(0), unit(1)), invalid = true)
    val source = right(EstimateGroup.prepare(reader, inputs(reader.declared), targets, admission, 16))
    assert(source.readBlock(Vector(1)).isLeft)
    assertEquals(reader.active, 0)
    assertEquals(source.readBlock(Vector(0), () => true), Left(EstimateError.Cancelled))
    assertEquals(reader.active, 0)
  }

  test("repeated participant rows and unadmitted alignment cannot enter a group source") {
    val first = unit(0)
    val second = unit(1).copy(observations = first.observations)
    val reader = new Reader(Vector(first, second))
    assert(EstimateGroup.prepare(reader, inputs(reader.declared), targets, admission, 16).isLeft)
    val valid = new Reader(Vector(unit(0), unit(1)))
    val refused = new GroupEstimateAdmission:
      def verify(units: Vector[EstimateUnit]) = Left(EstimateError.Invalid("registration not verified"))
    assert(EstimateGroup.prepare(valid, inputs(valid.declared), targets, refused, 16).isLeft)
    assertEquals(valid.opened, 0)
  }
