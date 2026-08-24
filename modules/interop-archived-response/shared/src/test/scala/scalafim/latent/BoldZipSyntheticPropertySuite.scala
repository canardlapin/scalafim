package scalafim.latent

import scalafim.image.SampleSpaces

import scalafim.archive.lna.{DatasetRole, Payload}
import scalafim.image.SomeSampleSpace
import gale.linalg.{DMat, DVec}

class BoldZipSyntheticPropertySuite extends munit.FunSuite:

  test("random synthetic BOLDZip payloads agree with an independent reconstruction oracle") {
    val rng = Lcg(0x5eedB01dL)
    Vector.tabulate(48)(identity).foreach { caseIndex =>
      val synthetic = syntheticCase(s"synthetic-$caseIndex", rng)
      val payload = synthetic.payload
      val selection = synthetic.selection

      val full =
        latentValue(payload.reconstruct())
      val expectedFull =
        referenceReconstruct(payload, 0 until payload.shape.timepoints, 0 until payload.shape.samples, includeEvents = true, includeOffset = true)
      val selected =
        latentValue(payload.reconstruct(selection))
      val expectedSelected =
        referenceReconstruct(payload, selection.timepoints.get, selection.samples.get, includeEvents = true, includeOffset = true)
      val decoded =
        latentValue(payload.decodeCoefficients(payload.coefTime.transpose))
      val expectedDecoded =
        referenceDecodeCarrierColumns(payload, payload.coefTime.transpose, includeEvents = false, includeOffset = false)

      assertFinite(synthetic.id, full)
      assertFinite(synthetic.id, selected)
      assertRowsClose(full.toRows, expectedFull.toRows, 1e-10)
      assertRowsClose(selected.toRows, expectedSelected.toRows, 1e-10)
      assertRowsClose(decoded.toRows, expectedDecoded.toRows, 1e-10)
      assert(payload.decodeSemantics.coefficientDecodeIsLinearOnly)
      assert(!payload.decodeSemantics.coefficientDecodeIncludes(LatentMaterializationTerm.SampleOffset))
      assert(!payload.decodeSemantics.coefficientDecodeIncludes(LatentMaterializationTerm.ResidualEvents))
      assertEquals(payload.metadata("case"), synthetic.id)
    }
  }

  test("BOLDZip coefficient decoding is the transpose of reconstruction when nonlinear terms are absent") {
    val rng = Lcg(0x51ead5L)
    Vector.tabulate(32)(identity).foreach { caseIndex =>
      val synthetic =
        syntheticCase(
          id = s"linear-$caseIndex",
          rng = rng,
          forceNoEvents = true,
          forceNoOffset = true
        )
      val payload = synthetic.payload
      val reconstructed =
        latentValue(payload.reconstruct())
      val decoded =
        latentValue(payload.decodeCoefficients(payload.coefTime.transpose))

      assertRowsClose(decoded.toRows, reconstructed.transpose.toRows, 1e-10)
      assert(!payload.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))
      assert(!payload.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.ResidualEvents))
    }
  }

  test("random synthetic BOLDZip archives roundtrip through the typed codec and preserve selections") {
    val rng = Lcg(0xB01dA11L)
    Vector.tabulate(24)(identity).foreach { caseIndex =>
      val synthetic = syntheticCase(s"archive-$caseIndex", rng)
      val payload = synthetic.payload
      val archive =
        BoldZipLatentArchiveCodec
          .toArchive(payload, SampleSpaces(Vector(payload.shape.samples, 1, 1)))
          .fold(err => fail(err.message), identity)
      val plan =
        LatentArchiveRegistry.standard
          .openPlan(archive)
          .fold(err => fail(err.message), identity)
      val decoded =
        LatentArchiveRegistry.standard
          .fromArchive(archive)
          .fold(err => fail(err.message), identity) match
          case LatentArchiveResponse.BoldZip(response) => response
          case other => fail(s"expected BOLDZip archive response, found $other")

      assert(BoldZipLatentArchiveCodec.isArchive(archive))
      assertEquals(plan.kind, LatentArchiveKind.BoldZip)
      assertEquals(plan.descriptor.kind, LatentArchiveKind.BoldZip)
      assertEquals(decoded.metadata("case"), synthetic.id)
      assertEquals(decoded.texture.length, payload.texture.length)
      assertEquals(decoded.events.length, payload.events.length)
      val decodedFull =
        decoded.reconstruct().fold(err => fail(err.message), identity)
      val expectedFull =
        referenceReconstruct(payload, 0 until payload.shape.timepoints, 0 until payload.shape.samples, includeEvents = true, includeOffset = true)
      assertRowsClose(decodedFull.toRows, expectedFull.toRows, 1e-10)
      assertRowsClose(
        decoded.reconstruct(synthetic.selection).fold(err => fail(err.message), identity).toRows,
        payload.reconstruct(synthetic.selection).fold(err => fail(err.message), identity).toRows,
        1e-10
      )

      val descriptor = archive.manifest.transforms.head
      if payload.texture.nonEmpty then
        val indexRef =
          descriptor.datasets.find(_.role == DatasetRole.Other("boldzip_texture_index")).getOrElse(fail("missing BOLDZip texture index ref"))
        archive.payload(indexRef.path) match
          case Some(Payload.IntMatrix(rows, 3, _, _)) =>
            assertEquals(rows, payload.texture.length)
          case other =>
            fail(s"expected BOLDZip texture index matrix, found $other")
      if payload.events.nonEmpty then
        val eventRef =
          descriptor.datasets.find(_.role == DatasetRole.Other("boldzip_residual_event_index")).getOrElse(fail("missing BOLDZip event index ref"))
        archive.payload(eventRef.path) match
          case Some(Payload.IntMatrix(rows, 3, _, _)) =>
            assertEquals(rows, payload.events.length)
          case other =>
            fail(s"expected BOLDZip event index matrix, found $other")
    }
  }

  test("synthetic invalid BOLDZip variants are rejected at construction") {
    val rng = Lcg(0xBAD5eedL)
    Vector.tabulate(20)(identity).foreach { caseIndex =>
      val synthetic =
        syntheticCase(
          id = s"invalid-base-$caseIndex",
          rng = rng,
          forceNoEvents = true,
          forceNoOffset = true
        )
      val payload = synthetic.payload

      val badAtom =
        BoldZipPayload(
          temporalBasis = payload.temporalBasis,
          carrierTheta = payload.carrierTheta,
          carrierLoadings = payload.carrierLoadings,
          spatialBasis = payload.spatialBasis,
          texture = Vector(BoldZipTextureEntry.unsafe(payload.spatialBasis.detailAtoms, 0, 1.0))
        )
      assert(badAtom.isLeft)

      val badCarrier =
        BoldZipPayload(
          temporalBasis = payload.temporalBasis,
          carrierTheta = payload.carrierTheta,
          carrierLoadings = payload.carrierLoadings,
          spatialBasis = payload.spatialBasis,
          texture = Vector(BoldZipTextureEntry.unsafe(0, payload.shape.coefficients, 1.0))
        )
      assert(badCarrier.isLeft)

      val badLag =
        BoldZipPayload(
          temporalBasis = payload.temporalBasis,
          carrierTheta = payload.carrierTheta,
          carrierLoadings = payload.carrierLoadings,
          spatialBasis = payload.spatialBasis,
          texture = Vector(BoldZipTextureEntry.unsafe(0, 0, 1.0, lag = payload.shape.timepoints))
        )
      assert(badLag.isLeft)

      val badDuration =
        BoldZipPayload(
          temporalBasis = payload.temporalBasis,
          carrierTheta = payload.carrierTheta,
          carrierLoadings = payload.carrierLoadings,
          spatialBasis = payload.spatialBasis,
          events = Vector(BoldZipResidualEvent.unsafe(0, payload.shape.timepoints - 1, 1.0, duration = 2))
        )
      assert(badDuration.isLeft)

      val badOffset =
        BoldZipPayload(
          temporalBasis = payload.temporalBasis,
          carrierTheta = payload.carrierTheta,
          carrierLoadings = payload.carrierLoadings,
          spatialBasis = payload.spatialBasis,
          offset = Some(DVec.fromSeq(Vector.fill(payload.shape.samples + 1)(0.0)))
        )
      assert(badOffset.isLeft)
    }
  }

  private final case class SyntheticCase(
      id: String,
      payload: BoldZipPayload,
      selection: LatentSelection
  )

  private final class Lcg private (private var state: Long):
    def nextInt(bound: Int): Int =
      require(bound > 0, "bound must be positive")
      val next = nextLong() >>> 1
      (next % bound).toInt

    def nextBoolean(): Boolean =
      (nextLong() & 1L) == 0L

    def nextDouble(min: Double, max: Double): Double =
      min + unit() * (max - min)

    private def unit(): Double =
      ((nextLong() >>> 11).toDouble / 9007199254740992.0)

    private def nextLong(): Long =
      state = state * 6364136223846793005L + 1442695040888963407L
      state

  private object Lcg:
    def apply(seed: Long): Lcg =
      new Lcg(seed)

  private def syntheticCase(
      id: String,
      rng: Lcg,
      forceNoEvents: Boolean = false,
      forceNoOffset: Boolean = false
  ): SyntheticCase =
    val timepoints = 3 + rng.nextInt(5)
    val components = 2 + rng.nextInt(3)
    val carriers = 1 + rng.nextInt(3)
    val samples = 2 + rng.nextInt(5)
    val coarseAtoms = rng.nextInt(3)
    val detailUsesIdentity = rng.nextBoolean()
    val detailAtoms = if detailUsesIdentity then samples else 1 + rng.nextInt(math.min(4, samples + 1))
    val temporalBasis = randomMatrix(rng, timepoints, components, scale = 1.0)
    val carrierTheta = randomMatrix(rng, carriers, components, scale = 1.25)
    val carrierLoadings =
      if coarseAtoms == 0 then DMat.zeros(0, carriers)
      else randomMatrix(rng, coarseAtoms, carriers, scale = 1.0)
    val coarse =
      if coarseAtoms == 0 then BoldZipCoarseBasis.Absent
      else BoldZipCoarseBasis.MatrixBasis(randomMatrix(rng, samples, coarseAtoms, scale = 1.0))
    val detail =
      if detailUsesIdentity then BoldZipDetailBasis.IdentitySamples
      else BoldZipDetailBasis.MatrixBasis(randomMatrix(rng, samples, detailAtoms, scale = 0.75))
    val spatialBasis =
      latentValue(
        BoldZipSpatialBasis(
          sampleCount = samples,
          coarse = coarse,
          detail = detail,
          label = s"$id-spatial"
        )
      )
    val textureCount = rng.nextInt(5)
    val texture =
      Vector.tabulate(textureCount) { _ =>
        val lag = rng.nextInt(2 * timepoints - 1) - (timepoints - 1)
        BoldZipTextureEntry.unsafe(
          atom = rng.nextInt(detailAtoms),
          carrier = rng.nextInt(carriers),
          amplitude = nonTiny(rng.nextDouble(-1.4, 1.4)),
          lag = lag
        )
      }
    val eventCount = if forceNoEvents then 0 else rng.nextInt(4)
    val events =
      Vector.tabulate(eventCount) { _ =>
        val frame = rng.nextInt(timepoints)
        BoldZipResidualEvent.unsafe(
          atom = rng.nextInt(detailAtoms),
          frame = frame,
          amplitude = nonTiny(rng.nextDouble(-1.2, 1.2)),
          duration = 1 + rng.nextInt(timepoints - frame)
        )
      }
    val offset =
      if forceNoOffset || !rng.nextBoolean() then None
      else Some(DVec.fromSeq(Vector.tabulate(samples)(_ => rng.nextDouble(-0.75, 0.75))))
    val payload =
      latentValue(
        BoldZipPayload(
          temporalBasis = temporalBasis,
          carrierTheta = carrierTheta,
          carrierLoadings = carrierLoadings,
          spatialBasis = spatialBasis,
          texture = texture,
          events = events,
          offset = offset,
          label = id,
          metadata = Map("case" -> id)
        )
      )
    val selection =
      LatentSelection(
        timepoints = Some(distinctSelection(timepoints)),
        samples = Some(distinctSelection(samples))
      )
    SyntheticCase(id, payload, selection)

  private def randomMatrix(
      rng: Lcg,
      rows: Int,
      cols: Int,
      scale: Double
  ): DMat =
    if rows == 0 then DMat.zeros(0, cols)
    else
      LatentNumerics.matrixFromRows(
        Vector.tabulate(rows) { row =>
          Vector.tabulate(cols) { col =>
            val deterministicDrift = 0.01 * (row + 1) - 0.015 * (col + 1)
            nonTiny(rng.nextDouble(-scale, scale) + deterministicDrift)
          }
        }
      )

  private def distinctSelection(count: Int): Vector[Int] =
    if count == 1 then Vector(0)
    else if count == 2 then Vector(1, 0)
    else Vector(count - 1, 0, count / 2).distinct

  private def nonTiny(value: Double): Double =
    if math.abs(value) < 0.05 then value + (if value < 0.0 then -0.05 else 0.05)
    else value

  private def referenceReconstruct(
      payload: BoldZipPayload,
      timepoints: IndexedSeq[Int],
      samples: IndexedSeq[Int],
      includeEvents: Boolean,
      includeOffset: Boolean
  ): DMat =
    LatentNumerics.matrixFromRows(
      timepoints.map { time =>
        samples.map { sample =>
          referenceSampleValue(payload, sample, time, includeEvents, includeOffset)
        }
      }
    )

  private def referenceDecodeCarrierColumns(
      payload: BoldZipPayload,
      carriersByColumn: DMat,
      includeEvents: Boolean,
      includeOffset: Boolean
  ): DMat =
    require(carriersByColumn.rows == payload.shape.coefficients)
    LatentNumerics.matrixFromRows(
      Vector.tabulate(payload.shape.samples) { sample =>
        Vector.tabulate(carriersByColumn.cols) { column =>
          referenceSampleValueFromCarrierColumn(payload, sample, column, carriersByColumn, includeEvents, includeOffset)
        }
      }
    )

  private def referenceSampleValue(
      payload: BoldZipPayload,
      sample: Int,
      time: Int,
      includeEvents: Boolean,
      includeOffset: Boolean
  ): Double =
    val coarseValue =
      payload.spatialBasis.phiCoarse.fold(0.0) { phi =>
        Vector.tabulate(phi.cols) { atom =>
          phi(sample, atom) * referenceCoarseAtomValue(payload, atom, time)
        }.sum
      }
    val detailValue =
      payload.spatialBasis.phiDetail match
        case Some(phi) =>
          Vector.tabulate(phi.cols) { atom =>
            phi(sample, atom) * referenceDetailAtomValue(payload, atom, time, includeEvents)
          }.sum
        case None =>
          referenceDetailAtomValue(payload, sample, time, includeEvents)
    val offsetValue =
      if includeOffset then payload.offset.fold(0.0)(_(sample)) else 0.0
    coarseValue + detailValue + offsetValue

  private def referenceSampleValueFromCarrierColumn(
      payload: BoldZipPayload,
      sample: Int,
      column: Int,
      carriersByColumn: DMat,
      includeEvents: Boolean,
      includeOffset: Boolean
  ): Double =
    val coarseValue =
      payload.spatialBasis.phiCoarse.fold(0.0) { phi =>
        Vector.tabulate(phi.cols) { atom =>
          phi(sample, atom) * referenceCoarseAtomValueFromCarrierColumn(payload, atom, column, carriersByColumn)
        }.sum
      }
    val detailValue =
      payload.spatialBasis.phiDetail match
        case Some(phi) =>
          Vector.tabulate(phi.cols) { atom =>
            phi(sample, atom) * referenceDetailAtomValueFromCarrierColumn(payload, atom, column, carriersByColumn, includeEvents)
          }.sum
        case None =>
          referenceDetailAtomValueFromCarrierColumn(payload, sample, column, carriersByColumn, includeEvents)
    val offsetValue =
      if includeOffset then payload.offset.fold(0.0)(_(sample)) else 0.0
    coarseValue + detailValue + offsetValue

  private def referenceCoarseAtomValue(payload: BoldZipPayload, atom: Int, time: Int): Double =
    Vector.tabulate(payload.shape.coefficients) { carrier =>
      payload.carrierLoadings(atom, carrier) * referenceCarrierValue(payload, carrier, time)
    }.sum

  private def referenceCoarseAtomValueFromCarrierColumn(
      payload: BoldZipPayload,
      atom: Int,
      column: Int,
      carriersByColumn: DMat
  ): Double =
    Vector.tabulate(payload.shape.coefficients) { carrier =>
      payload.carrierLoadings(atom, carrier) * carriersByColumn(carrier, column)
    }.sum

  private def referenceDetailAtomValue(
      payload: BoldZipPayload,
      atom: Int,
      time: Int,
      includeEvents: Boolean
  ): Double =
    val textureValue =
      payload.texture
        .filter(_.atom.value == atom)
        .map(entry => entry.amplitude.value * referenceLaggedCarrierValue(payload, entry.carrier.value, time, entry.lag.value))
        .sum
    val eventValue =
      if includeEvents then referenceEventValue(payload, atom, time) else 0.0
    textureValue + eventValue

  private def referenceDetailAtomValueFromCarrierColumn(
      payload: BoldZipPayload,
      atom: Int,
      column: Int,
      carriersByColumn: DMat,
      includeEvents: Boolean
  ): Double =
    val textureValue =
      payload.texture
        .filter(_.atom.value == atom)
        .map { entry =>
          val sourceColumn = column - entry.lag.value
          if sourceColumn >= 0 && sourceColumn < carriersByColumn.cols then
            entry.amplitude.value * carriersByColumn(entry.carrier.value, sourceColumn)
          else 0.0
        }
        .sum
    val eventValue =
      if includeEvents then referenceEventValue(payload, atom, column) else 0.0
    textureValue + eventValue

  private def referenceCarrierValue(payload: BoldZipPayload, carrier: Int, time: Int): Double =
    Vector.tabulate(payload.temporalBasis.cols) { component =>
      payload.carrierTheta(carrier, component) * payload.temporalBasis(time, component)
    }.sum

  private def referenceLaggedCarrierValue(
      payload: BoldZipPayload,
      carrier: Int,
      time: Int,
      lag: Int
  ): Double =
    val sourceTime = time - lag
    if sourceTime < 0 || sourceTime >= payload.shape.timepoints then 0.0
    else referenceCarrierValue(payload, carrier, sourceTime)

  private def referenceEventValue(payload: BoldZipPayload, atom: Int, time: Int): Double =
    payload.events
      .filter(event => event.atom.value == atom && time >= event.frame.value && time < event.frame.value + event.duration.value)
      .map(_.amplitude.value)
      .sum

  private def latentValue[A](result: Either[LatentError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  private def assertFinite(label: String, matrix: DMat): Unit =
    matrix.copyData.zipWithIndex.foreach { case (value, index) =>
      assert(value.isFinite, s"$label produced non-finite value at $index: $value")
    }

  private def assertRowsClose(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    assertEquals(if actual.isEmpty then 0 else actual.head.length, if expected.isEmpty then 0 else expected.head.length)
    actual.zip(expected).zipWithIndex.foreach { case ((actualRow, expectedRow), row) =>
      actualRow.zip(expectedRow).zipWithIndex.foreach { case ((actualValue, expectedValue), col) =>
        assertEqualsDouble(actualValue, expectedValue, tol, s"value mismatch at ($row,$col)")
      }
    }
