package scalafim.latent

import scalafim.image.SampleSpaces

import scalafim.archive.{ArchiveError, RunLabel}
import scalafim.archive.lna.{
  DatasetRole,
  LnaArchive,
  LnaPipeline,
  LnaRun,
  Payload,
  SharedBasisArtifact,
  SharedBasisId,
  SharedBasisMask,
  TransformKind,
  TransformParams
}
import scalafim.image.{Mask, SomeSampleSpace}
import gale.linalg.{DMat, DVec, LinAlgError}
import scalafim.archive.lna.GaleArchiveTestData

class LatentArchiveRegistrySuite extends munit.FunSuite:
  private val basis =
    LatentNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.5, 1.0),
        Vector(0.0, 2.0)
      )
    )

  private val loadings =
    LatentNumerics.matrixFromRows(
      Vector(
        Vector(10.0, 1.0),
        Vector(20.0, 2.0),
        Vector(30.0, 3.0),
        Vector(40.0, 4.0)
      )
    )

  private val offset =
    DVec.fromSeq(Vector(1.0, 2.0, 3.0, 4.0))

  test("standard registry installs every typed LNA binding exactly once") {
    assertEquals(
      LatentArchiveRegistry.standard.supportedKinds,
      LatentArchiveKind.values.toVector.sortBy(_.toString)
    )
  }

  test("registry construction rejects empty and duplicate binding sets") {
    assertEquals(
      LatentArchiveRegistry.build(),
      Left(LatentArchiveRegistryError.Empty)
    )
    assertEquals(
      LatentArchiveRegistry.build(
        LatentArchiveBinding.Explicit,
        LatentArchiveBinding.Explicit
      ),
      Left(
        LatentArchiveRegistryError.DuplicateKind(
          LatentArchiveKind.Explicit
        )
      )
    )

    val emptyKinds =
      testBinding("empty-kinds", Vector.empty)
    assertEquals(
      LatentArchiveRegistry.build(emptyKinds),
      Left(
        LatentArchiveRegistryError.InvalidBinding(
          "empty-kinds",
          "at least one archive kind is required"
        )
      )
    )

    val sharedNameA =
      testBinding(
        "shared-name",
        Vector(LatentArchiveKind.Explicit)
      )
    val sharedNameB =
      testBinding(
        "shared-name",
        Vector(LatentArchiveKind.Transport)
      )
    assertEquals(
      LatentArchiveRegistry.build(sharedNameB, sharedNameA),
      Left(
        LatentArchiveRegistryError.DuplicateName(
          "shared-name"
        )
      )
    )
  }

  test("explicit latent responses roundtrip through LNA archives") {
    val source =
      ExplicitLatentResponse(
        basis = basis,
        loadings = loadings,
        offset = Some(offset),
        sourceDomain = DomainId.unsafe("latent.coefficients.demo"),
        targetDomain = DomainId.unsafe("voxels.demo"),
        label = "demo-latent",
        metadata = Map("basis" -> "provided", "subject" -> "sub-01")
      ).fold(err => fail(err.message), identity)

    val archive =
      ExplicitLatentArchiveCodec
        .toArchive(source, SampleSpaces(Vector(2, 2, 1)))
        .fold(err => fail(err.message), identity)
    val plan =
      LatentArchiveRegistry.standard
        .openPlan(archive)
        .fold(err => fail(err.message), identity)
    val descriptor =
      LatentArchiveRegistry.standard
        .openDescriptor(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(plan.kind, LatentArchiveKind.Explicit)
    assertEquals(plan.descriptor.kind, LatentArchiveKind.Explicit)
    assertEquals(descriptor.kind, LatentArchiveKind.Explicit)
    plan.capability match
      case LatentResponseCapability.Response(response) =>
        assertEquals(response.label, "demo-latent")
      case other =>
        fail(s"expected response-bearing capability, found $other")
    assert(plan.latentResponse.nonEmpty)

    val decoded =
      LatentArchiveRegistry.standard
        .fromArchive(archive)
        .fold(err => fail(err.message), identity) match
        case LatentArchiveResponse.Explicit(response) => response
        case other => fail(s"expected explicit archive variant, found $other")

    assertEquals(decoded.sourceDomain.value, "latent.coefficients.demo")
    assertEquals(decoded.targetDomain.value, "voxels.demo")
    assertEquals(decoded.label, "demo-latent")
    assertEquals(decoded.metadata, Map("basis" -> "provided", "subject" -> "sub-01"))
    assertEquals(decoded.basis.toRows, basis.toRows)
    assertEquals(decoded.loadings.toRows, loadings.toRows)
    assertEquals(decoded.offset.map(_.toVector), Some(offset.toVector))

    val selected =
      decoded
        .reconstruct(LatentSelection(timepoints = Some(Vector(2, 0)), samples = Some(Vector(3, 1))))
        .fold(err => fail(err.message), identity)

    assertEquals(selected.toRows, Vector(Vector(12.0, 6.0), Vector(44.0, 22.0)))
  }

  test("latent archive plans are absent for ordinary transform archives") {
    val archive =
      LnaPipeline
        .quantArchive(
          GaleArchiveTestData.matrixFromRows(Vector(Vector(0.0, 1.0, 2.0, 3.0), Vector(4.0, 5.0, 6.0, 7.0))),
          SampleSpaces(Vector(2, 2, 1))
        )
        .fold(err => fail(err.message), identity)
    val plan =
      LatentArchiveRegistry.standard
        .maybeOpenPlan(archive)
        .fold(err => fail(err.message), identity)

    assert(plan.isEmpty)
  }

  test("temporal DCT archives preserve typed DCT params and reconstruct full-rank data") {
    val data =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0, 3.0, 4.0),
          Vector(2.0, 3.0, 5.0, 7.0),
          Vector(3.0, 5.0, 8.0, 11.0),
          Vector(5.0, 8.0, 13.0, 17.0)
        )
      )

    val archive =
      ExplicitLatentArchiveCodec
        .toTemporalDctArchive(
          data = data,
          space = SampleSpaces(Vector(2, 2, 1)),
          components = data.rows,
          norm = DctNorm.Ortho,
          center = true,
          ridge = 0.0
        )
        .fold(err => fail(err.message), identity)

    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Temporal, TransformKind.Embed))
    archive.manifest.transforms.head.params match
      case TransformParams.TemporalDct(params) =>
        assertEquals(params.components, data.rows)
        assertEquals(params.center, true)
      case other =>
        fail(s"expected temporal DCT params, found $other")

    val decodedVariant =
      LatentArchiveRegistry.standard
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)
    val plan =
      LatentArchiveRegistry.standard
        .openPlan(archive)
        .fold(err => fail(err.message), identity)
    val descriptor =
      LatentArchiveRegistry.standard
        .openDescriptor(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(plan.kind, LatentArchiveKind.TemporalDct)
    assertEquals(plan.descriptor.kind, LatentArchiveKind.TemporalDct)
    assertEquals(descriptor.kind, LatentArchiveKind.TemporalDct)

    val decoded =
      decodedVariant match
        case LatentArchiveResponse.TemporalDct(response, spec, center, ridge) =>
          assertEquals(spec.timepoints, data.rows)
          assertEquals(spec.components, data.rows)
          assertEquals(spec.norm, DctNorm.Ortho)
          assertEquals(center, true)
          assertEquals(ridge.value, 0.0)
          response
        case other =>
          fail(s"expected temporal DCT archive variant, found $other")
    val reconstructed =
      decoded
        .reconstruct()
        .fold(err => fail(err.message), identity)
    val pipelineReconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(decoded.metadata("family"), "time_dct")
    assertEquals(decoded.metadata("basis"), "dct")
    assertEquals(decoded.metadata("components"), "4")
    assertEquals(decoded.metadata("center"), "true")
    assertMatrixEquals(reconstructed, data.toRows, 1e-12)
    assertRowsEqual(pipelineReconstructed.toRows, data.toRows, 1e-12)
  }

  test("shared-basis archives are encoded through the latent Gram solver and store offsets") {
    val sharedLoadings =
      GaleArchiveTestData.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 1.0)
        )
      )
    val sharedBasis =
      SharedBasisArtifact(
        loadings = sharedLoadings,
        mask = SharedBasisMask(Vector(3), Vector(true, true, true)),
        kind = "nonorthogonal",
        params = Map("source" -> "codec-suite")
      )
    val basisId = SharedBasisId.unsafe("codec_nonorthogonal_basis")
    val expectedCoefficients =
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, -1.0),
        Vector(-2.0, 0.5)
      )
    val data =
      LatentNumerics.matrixFromRows(
        expectedCoefficients.map { row =>
          Vector.tabulate(sharedLoadings.rows) { voxel =>
            var sum = Vector(10.0, -2.0, 5.0)(voxel)
            var atom = 0
            while atom < sharedLoadings.cols do
              sum += row(atom) * sharedLoadings(voxel, atom)
              atom += 1
            sum
          }
        }
      )

    val archive =
      SharedBasisLatentArchiveCodec
        .toArchive(
          data = data,
          space = SampleSpaces(Vector(3, 1, 1)),
          basis = sharedBasis,
          basisId = basisId,
          center = true
        )
        .fold(err => fail(err.message), identity)

    val descriptor = archive.manifest.transforms.head
    assertEquals(descriptor.kind, TransformKind.Embed)
    assert(descriptor.datasets.exists(_.role == DatasetRole.Offset))
    descriptor.params match
      case params: TransformParams.SharedBasisEmbed =>
        assertEquals(params.basis.basisId, basisId)
        assert(params.centerDataWith.nonEmpty)
        assertEquals(params.metadata("center"), "true")
      case other =>
        fail(s"expected shared-basis embed params, found $other")

    val offsetPath = descriptor.datasets.find(_.role == DatasetRole.Offset).get.path
    val storedOffset =
      archive.payload(offsetPath) match
        case Some(Payload.DoubleVector(values, _)) => values
        case other => fail(s"expected stored offset vector, found $other")

    val encoded =
      SharedBasisEncoder
        .encode(data, sharedBasis, basisId, center = true)
        .fold(err => fail(err.message), identity)
    val coeffPath = descriptor.datasets.find(_.role == DatasetRole.Coefficients).get.path
    val storedCoefficients =
      archive.payload(coeffPath) match
        case Some(Payload.DoubleMatrix(values, _)) => values
        case other => fail(s"expected stored coefficient matrix, found $other")

    assertRowsEqual(Vector(storedOffset), Vector(encoded.offset.get.toVector), 1e-12)
    assertRowsEqual(storedCoefficients.toRows, encoded.coefficients.toRows, 1e-12)
    assert(!rowsClose(storedCoefficients.toRows, rawProjection(data.toRows, storedOffset, sharedLoadings), 1e-8))

    val decodedVariant =
      LatentArchiveRegistry.standard
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)
    val plan =
      LatentArchiveRegistry.standard
        .openPlan(archive)
        .fold(err => fail(err.message), identity)
    val archiveDescriptor =
      LatentArchiveRegistry.standard
        .openDescriptor(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(plan.kind, LatentArchiveKind.SharedBasis)
    assertEquals(plan.descriptor.kind, LatentArchiveKind.SharedBasis)
    assertEquals(archiveDescriptor.kind, LatentArchiveKind.SharedBasis)
    plan.capability match
      case LatentResponseCapability.DeferredSharedBasis(archive) =>
        assertEquals(archive.basis.basisId, basisId)
      case other =>
        fail(s"expected deferred shared-basis capability, found $other")
    assert(plan.selectionResponse.left.toOption.exists(_.message.contains("shared_basis_embed")))
    assert(plan.latentResponse.isEmpty)

    decodedVariant match
      case LatentArchiveResponse.SharedBasis(response) =>
        assertEquals(response.basis.basisId, basisId)
        assertEquals(response.sourceDomain.value, "shared_basis.coefficients")
        assertEquals(response.targetDomain.value, "voxels")
        assertRowsEqual(response.coefficients.toRows, encoded.coefficients.toRows, 1e-12)
        assertEquals(response.offset.map(_.toVector), encoded.offset.map(_.toVector))
        val materialized =
          response
            .materialize(sharedBasis, Some(SampleSpaces(Vector(3, 1, 1))))
            .fold(err => fail(err.message), identity)
        val mask =
          response
            .sampleMask(SampleSpaces(Vector(3, 1, 1)), sharedBasis)
            .fold(err => fail(err.message), identity)
        assertRowsEqual(materialized.basis.toRows, encoded.coefficients.toRows, 1e-12)
        assertRowsEqual(materialized.loadings.toRows, sharedLoadings.toRows, 1e-12)
        assertEquals(materialized.metadata("basis.id"), basisId.value)
        assertEquals(materialized.metadata("basis.checksum"), sharedBasis.checksum.value)
        assertEquals(Vector.tabulate(Mask.indices(mask).size)(Mask.indices(mask)(_)), Vector(0, 1, 2))
      case other =>
        fail(s"expected shared-basis archive variant, found $other")
  }

  test("transport archives preserve dense operator payloads and selected reconstruction") {
    val decoder =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 4,
          cols = 2,
          rowIndices = Array(0, 1, 2, 2, 3, 3),
          colIndices = Array(0, 1, 0, 1, 0, 1),
          values = Array(1.0, 1.0, 1.0, 1.0, 2.0, -1.0)
        )
      )
    val templateDecoder =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 3,
          cols = 2,
          rowIndices = Array(0, 1, 1, 2),
          colIndices = Array(0, 0, 1, 1),
          values = Array(1.0, 0.5, 1.0, 2.0)
        )
      )
    val toAnalysis =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 2,
          cols = 2,
          rowIndices = Array(0, 1),
          colIndices = Array(0, 1),
          values = Array(2.0, 0.5)
        )
      )
    val toRaw =
      mapValue(
        LatentOperators.csrFromTriplets(
          rows = 2,
          cols = 2,
          rowIndices = Array(0, 1),
          colIndices = Array(0, 1),
          values = Array(0.5, 2.0)
        )
      )
    val source =
      TransportLatentResponse(
        coefficientsAnalysis = LatentNumerics.matrixFromRows(
          Vector(
            Vector(2.0, 2.0),
            Vector(4.0, -1.0)
          )
        ),
        nativeDecoder = decoder,
        transform = CoefficientTransform(toAnalysis, toRaw).fold(err => fail(err.message), identity),
        templateDecoder = Some(templateDecoder),
        offset = Some(DVec.fromSeq(Vector(10.0, 20.0, 30.0, 40.0))),
        label = "transport-demo",
        metadata = Map("subject" -> "sub-01")
      ).fold(err => fail(err.message), identity)

    val archive =
      TransportLatentArchiveCodec
        .toArchive(source, SampleSpaces(Vector(2, 2, 1)))
        .fold(err => fail(err.message), identity)

    assert(TransportLatentArchiveCodec.isArchive(archive))
    val descriptor = archive.manifest.transforms.head
    assertEquals(descriptor.kind, TransformKind.Embed)
    assert(descriptor.datasets.exists(_.role == DatasetRole.Other("transport_native_decoder_t")))
    assert(descriptor.datasets.exists(_.role == DatasetRole.Other("transport_template_decoder_t")))
    descriptor.params match
      case params: TransformParams.Embed =>
        assertEquals(params.label, Some("transport-demo"))
        assertEquals(params.metadata("lna.response.kind"), "transport_latent")
        assertEquals(params.metadata("operator_storage"), "dense_transpose")
      case other =>
        fail(s"expected transport embed params, found $other")

    val decoded =
      TransportLatentArchiveCodec
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)
    val decodedVariant =
      LatentArchiveRegistry.standard
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)
    val plan =
      LatentArchiveRegistry.standard
        .openPlan(archive)
        .fold(err => fail(err.message), identity)
    val archiveDescriptor =
      LatentArchiveRegistry.standard
        .openDescriptor(archive)
        .fold(err => fail(err.message), identity)
    val expectedSelection =
      source
        .reconstruct(LatentSelection(timepoints = Some(Vector(1, 0)), samples = Some(Vector(3, 1))))
        .fold(err => fail(err.message), identity)
    val actualSelection =
      decoded
        .reconstruct(LatentSelection(timepoints = Some(Vector(1, 0)), samples = Some(Vector(3, 1))))
        .fold(err => fail(err.message), identity)
    val templateProjection =
      decoded
        .decodeCoefficients(LatentNumerics.matrixFromRows(Vector(Vector(2.0), Vector(2.0))), TransportSpace.Template, CoefficientCoordinates.Analysis)
        .fold(err => fail(err.message), identity)

    assertEquals(decoded.label, "transport-demo")
    assertEquals(plan.kind, LatentArchiveKind.Transport)
    assertEquals(plan.descriptor.kind, LatentArchiveKind.Transport)
    assertEquals(archiveDescriptor.kind, LatentArchiveKind.Transport)
    assertEquals(decoded.metadata("family"), "transport")
    assertEquals(decoded.decoders.templateCapable, true)
    assertEquals(decoded.metadata("subject"), "sub-01")
    decodedVariant match
      case LatentArchiveResponse.Transport(response) =>
        assertEquals(response.label, "transport-demo")
      case other =>
        fail(s"expected transport archive variant, found $other")
    assertRowsEqual(actualSelection.toRows, expectedSelection.toRows, 1e-12)
    assertRowsEqual(templateProjection.toRows, Vector(Vector(2.0), Vector(3.0), Vector(4.0)), 1e-12)
  }

  test("BOLDZip archives preserve codec tables and selected reconstruction") {
    val spatialBasis =
      latentValue(
        BoldZipSpatialBasis(
          sampleCount = 3,
          coarse = BoldZipCoarseBasis.MatrixBasis(LatentNumerics.matrixFromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))),
          detail = BoldZipDetailBasis.IdentitySamples,
          label = "identity-detail"
        )
      )
    val source =
      latentValue(
        BoldZipPayload(
          temporalBasis = DMat.eye(4),
          carrierTheta = LatentNumerics.matrixFromRows(
            Vector(
              Vector(1.0, 2.0, 3.0, 4.0),
              Vector(10.0, 20.0, 30.0, 40.0)
            )
          ),
          carrierLoadings = LatentNumerics.matrixFromRows(Vector(Vector(2.0, 1.0))),
          spatialBasis = spatialBasis,
          texture = Vector(
            BoldZipTextureEntry.unsafe(atom = 0, carrier = 0, amplitude = 0.5, lag = 0),
            BoldZipTextureEntry.unsafe(atom = 1, carrier = 1, amplitude = 1.0, lag = 1)
          ),
          events = Vector(BoldZipResidualEvent.unsafe(atom = 2, frame = 2, amplitude = 3.0)),
          offset = Some(DVec.fromSeq(Vector(10.0, 20.0, 30.0))),
          sourceDomain = DomainId.unsafe("boldzip.carriers.demo"),
          targetDomain = DomainId.unsafe("boldzip.samples.demo"),
          label = "boldzip-demo",
          metadata = Map("subject" -> "sub-01")
        )
      )

    val archive =
      BoldZipLatentArchiveCodec
        .toArchive(source, SampleSpaces(Vector(3, 1, 1)))
        .fold(err => fail(err.message), identity)

    assert(BoldZipLatentArchiveCodec.isArchive(archive))
    val descriptor = archive.manifest.transforms.head
    assertEquals(descriptor.kind, TransformKind.Custom("boldzip_sr"))
    assert(descriptor.datasets.exists(_.role == DatasetRole.Other("boldzip_carrier_theta")))
    assert(descriptor.datasets.exists(_.role == DatasetRole.Other("boldzip_texture_index")))
    descriptor.params match
      case TransformParams.Custom(name, sourceDomain, targetDomain, label, metadata) =>
        assertEquals(name, "boldzip_sr")
        assertEquals(sourceDomain, Some("boldzip.carriers.demo"))
        assertEquals(targetDomain, Some("boldzip.samples.demo"))
        assertEquals(label, Some("boldzip-demo"))
        assertEquals(metadata("lna.response.kind"), "boldzip_sr")
        assertEquals(metadata("spatial_basis.label"), "identity-detail")
      case other =>
        fail(s"expected BOLDZip custom params, found $other")

    val textureIndexPath = descriptor.datasets.find(_.role == DatasetRole.Other("boldzip_texture_index")).get.path
    archive.payload(textureIndexPath) match
      case Some(Payload.IntMatrix(2, 3, values, _)) =>
        assertEquals(values, Vector(0, 0, 0, 1, 1, 1))
      case other =>
        fail(s"expected BOLDZip texture index table, found $other")

    val decoded =
      BoldZipLatentArchiveCodec
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)
    val decodedVariant =
      LatentArchiveRegistry.standard
        .fromArchive(archive)
        .fold(err => fail(err.message), identity)
    val plan =
      LatentArchiveRegistry.standard
        .openPlan(archive)
        .fold(err => fail(err.message), identity)
    val archiveDescriptor =
      LatentArchiveRegistry.standard
        .openDescriptor(archive)
        .fold(err => fail(err.message), identity)
    val expectedSelection =
      source
        .reconstruct(LatentSelection(timepoints = Some(Vector(2, 0)), samples = Some(Vector(2, 0))))
        .fold(err => fail(err.message), identity)
    val actualSelection =
      decoded
        .reconstruct(LatentSelection(timepoints = Some(Vector(2, 0)), samples = Some(Vector(2, 0))))
        .fold(err => fail(err.message), identity)

    assertEquals(decoded.label, "boldzip-demo")
    assertEquals(plan.kind, LatentArchiveKind.BoldZip)
    assertEquals(plan.descriptor.kind, LatentArchiveKind.BoldZip)
    assertEquals(archiveDescriptor.kind, LatentArchiveKind.BoldZip)
    assertEquals(decoded.sourceDomain.value, "boldzip.carriers.demo")
    assertEquals(decoded.targetDomain.value, "boldzip.samples.demo")
    assertEquals(decoded.metadata("family"), "boldzip_sr")
    assertEquals(decoded.metadata("subject"), "sub-01")
    assertEquals(decoded.spatialBasis.label, "identity-detail")
    assertEquals(decoded.texture.map(_.lag.value), Vector(0, 1))
    assertEquals(decoded.events.map(_.duration.value), Vector(1))
    assertRowsEqual(actualSelection.toRows, expectedSelection.toRows, 1e-12)
    decodedVariant match
      case LatentArchiveResponse.BoldZip(response) =>
        assertEquals(response.label, "boldzip-demo")
      case other =>
        fail(s"expected BOLDZip archive variant, found $other")
  }

  private def latentValue[A](result: Either[LatentError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  private def testBinding(
      bindingName: String,
      bindingKinds: Vector[LatentArchiveKind]
  ): LatentArchiveBinding =
    new LatentArchiveBinding:
      val name: String =
        bindingName

      val kinds: Vector[LatentArchiveKind] =
        bindingKinds

      private[latent] def descriptorOption(
          archive: LnaArchive,
          runLabel: RunLabel
      ): Either[ArchiveError, Option[LatentArchiveDescriptor]] =
        Right(None)

      private[latent] def openPlan(
          archive: LnaArchive,
          runLabel: RunLabel,
          run: LnaRun,
          descriptor: LatentArchiveDescriptor
      ): Either[ArchiveError, LatentArchivePlan] =
        Left(
          ArchiveError.InvalidArchive(
            s"$bindingName test binding cannot open ${descriptor.kind.toString}"
          )
        )

  private def mapValue[A](result: Either[LinAlgError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  private def assertMatrixEquals(
      actual: DMat,
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertRowsEqual(actual.toRows, expected, tol)

  private def assertRowsEqual(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    assertEquals(if actual.isEmpty then 0 else actual.head.length, if expected.isEmpty then 0 else expected.head.length)
    actual.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, tol)
      }
    }

  private def rawProjection(
      rows: Vector[Vector[Double]],
      offset: Vector[Double],
      loadings: DMat
  ): Vector[Vector[Double]] =
    rows.map { row =>
      Vector.tabulate(loadings.cols) { atom =>
        var sum = 0.0
        var voxel = 0
        while voxel < loadings.rows do
          sum += (row(voxel) - offset(voxel)) * loadings(voxel, atom)
          voxel += 1
        sum
      }
    }

  private def rowsClose(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Boolean =
    actual.length == expected.length &&
      actual.zip(expected).forall { case (actualRow, expectedRow) =>
        actualRow.length == expectedRow.length &&
          actualRow.zip(expectedRow).forall { case (actualValue, expectedValue) =>
            math.abs(actualValue - expectedValue) <= tol
          }
      }
