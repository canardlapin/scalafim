package scalafim.archive.lna

import scalafim.image.SampleSpaces

import scalafim.archive.{ArchiveDatasetPath, ArchiveError, ArchivePath, CreatorId, DatasetShape, RunLabel, RunScopedPath, TransformName, TransformPort}
import gale.linalg.DMat
import scalafim.image.SomeSampleSpace

class LnaCoreSuite extends munit.FunSuite:

  private val space = SampleSpaces(Vector(2, 2, 1))

  private val data =
    GaleArchiveTestData.matrixFromRows(
      Vector(
        Vector(0.0, 1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0, 7.0),
        Vector(8.0, 9.0, 10.0, 11.0)
      )
    )

  private val curvedData =
    GaleArchiveTestData.matrixFromRows(
      Vector(
        Vector(0.0, 1.0, 2.0, 3.0),
        Vector(1.0, 3.0, 6.0, 10.0),
        Vector(2.0, 6.0, 12.0, 20.0),
        Vector(4.0, 10.0, 20.0, 35.0)
      )
    )

  private val spikyData =
    GaleArchiveTestData.matrixFromRows(
      Vector.tabulate(25) {
        case 23 => Vector(100.0, 0.0, 0.0, 0.0)
        case 24 => Vector(-100.0, 0.0, 0.0, 0.0)
        case _  => Vector(0.0, 0.0, 0.0, 0.0)
      }
    )

  private val explicitLatent =
    LnaExplicitLatent.Response(
      basis = GaleArchiveTestData.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.5, 1.0),
          Vector(0.0, 2.0)
        )
      ),
      loadings = GaleArchiveTestData.matrixFromRows(
        Vector(
          Vector(10.0, 1.0),
          Vector(20.0, 2.0),
          Vector(30.0, 3.0),
          Vector(40.0, 4.0)
        )
      ),
      offset = Some(Vector(1.0, 2.0, 3.0, 4.0)),
      sourceDomain = "latent.coefficients.demo",
      targetDomain = "voxels.demo",
      label = "demo-latent",
      metadata = Map("basis" -> "provided", "subject" -> "sub-01")
    )

  private val identitySharedBasis =
    SharedBasisArtifact(
      loadings = DMat.eye(4),
      mask = SharedBasisMask(Vector(2, 2, 1), Vector(true, true, true, true)),
      kind = "identity",
      params = Map("source" -> "unit-test")
    )

  test("archive paths and run labels enforce basic invariants") {
    assertEquals((ArchivePath("/scans") / "run-01").value, "/scans/run-01")
    assertEquals(RunLabel.indexed(0).value, "run-01")
    assertEquals(ArchivePath.parse("/scans").map(path => (path / "run-01").value), Right("/scans/run-01"))
    assert(ArchivePath.parse("relative").isLeft)
    assert(RunLabel.parse("bad/run").isLeft)
    assert(RunLabel.indexedChecked(-1).isLeft)
    intercept[IllegalArgumentException](ArchivePath("relative"))
    intercept[IllegalArgumentException](RunLabel("bad/run"))
  }

  test("typed archive primitives expose checked constructors") {
    val shape = DatasetShape(Vector(2, 3)).fold(err => fail(err.message), identity)
    assertEquals(shape.rank, 2)
    assertEquals(shape.entries, 6)
    assertEquals(shape.toVector, Vector(2, 3))
    assert(DatasetShape(Vector(2, 0)).isLeft)

    assertEquals(CreatorId("scalafim").map(_.value), Right("scalafim"))
    assertEquals(TransformName("00_quant.json").map(_.value), Right("00_quant.json"))
    assert(TransformName("nested/00_quant.json").isLeft)
    assert(TransformName("00_quant").isLeft)
    assertEquals(TransformPort("latent_response").map(_.value), Right("latent_response"))
    assert(TransformPort("latent/response").isLeft)

    val ref =
      DatasetRef
        .checked(ArchivePath("/scans/run-01/step_00_quant/values"), DatasetRole.Quantized, Vector(2, 3), Some(LnaDType.UInt8))
        .fold(err => fail(err.message), identity)
    assertEquals(ref.shape.entries, 6)
    assert(DatasetRef.checked(ArchivePath("/bad"), DatasetRole.RawData, Vector.empty).isLeft)

    val desc =
      TransformDescriptor
        .checked(
          name = "00_quant.json",
          kind = TransformKind.Quant,
          params = TransformParams.Quant(QuantParams(bits = 8)),
          inputs = Vector("raw"),
          outputs = Vector("quantized"),
          datasets = Vector(ref)
        )
        .fold(err => fail(err.message), identity)
    assertEquals(desc.transformName.value, "00_quant.json")
    assertEquals(desc.inputPorts.map(_.value), Vector("raw"))
    assert(TransformDescriptor.checked("00_quant.json", TransformKind.Quant, TransformParams.Quant(QuantParams()), Vector.empty, Vector("out"), Vector(ref)).isLeft)
    assert(TransformDescriptor.checked("00_quant.json", TransformKind.Quant, TransformParams.Delta(DeltaParams()), Vector("in"), Vector("out"), Vector(ref)).isLeft)
  }

  test("typed archive metadata and transform parameters reject invalid values") {
    val bits = QuantBits(4).fold(err => fail(err.message), identity)
    assertEquals(bits.value, 4)
    assertEquals(bits.levels, 15)
    assertEquals(bits.storageDType, LnaDType.UInt8)
    assert(QuantBits(0).isLeft)

    val params =
      QuantParams.typed(
        bits,
        method = QuantMethod.Sd,
        centering = CenteringPolicy.Uncentered,
        scaleScope = QuantScaleScope.Voxel,
        clipping = ClipPolicy.AllowClipping
      )
    assertEquals(params.bits, 4)
    assertEquals(params.method, QuantMethod.Sd)
    assertEquals(params.center, false)
    assertEquals(params.scaleScope, QuantScaleScope.Voxel)
    assertEquals(params.allowClip, true)
    assertEquals(params.clipPolicy, ClipPolicy.AllowClipping)
    assertEquals(params.centeringPolicy, CenteringPolicy.Uncentered)

    assertEquals(LnaMetadata(Map("subject" -> "sub-01")).map(_.get("subject")), Right(Some("sub-01")))
    assert(LnaMetadata(Map("" -> "bad")).isLeft)
    assert(QuantParams.checked(bits = 17).isLeft)
    assert(QuantReport.checked(bits = 8, method = QuantMethod.Range, scaleScope = QuantScaleScope.Global, nClippedTotal = -1, clipPct = 0.0).isLeft)
    assert(TemporalDctParams.checked(components = 0).isLeft)
    assert(DeltaParams.checked(order = 2).isLeft)
  }

  test("archive dataset paths expose typed run scope by path segment") {
    val label = RunLabel("run-01")
    val runPath = ArchivePath("/scans/run-01/step_00_quant/values")
    val scoped = RunScopedPath.from(runPath, label).getOrElse(fail("expected run-scoped path"))

    assertEquals(scoped.label, label)
    assertEquals(scoped.dataset, ArchiveDatasetPath(runPath))
    assertEquals(ArchiveDatasetPath.from("/basis/00_basis/matrix").runScope, None)
    assertEquals(RunScopedPath.from(ArchivePath("/scans/run-010/step_00_quant/values"), label), None)
  }

  test("integer payload storage validates unsigned dtype ranges") {
    assertEquals(Payload.IntMatrix(1, 2, Vector(0, 255), LnaDType.UInt8).dtype, LnaDType.UInt8)
    assertEquals(Payload.IntMatrix(1, 2, Vector(0, 65535), LnaDType.UInt16).dtype, LnaDType.UInt16)

    val uint8 = intercept[IllegalArgumentException] {
      Payload.IntMatrix(1, 1, Vector(256), LnaDType.UInt8)
    }
    assert(uint8.getMessage.contains("UInt8"))

    val uint16 = intercept[IllegalArgumentException] {
      Payload.IntMatrix(1, 1, Vector(-1), LnaDType.UInt16)
    }
    assert(uint16.getMessage.contains("UInt16"))
  }

  test("quant archive validates and reconstructs with bounded error") {
    val archive =
      LnaPipeline
        .quantArchive(data, space, params = QuantParams(bits = 16))
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Quant))

    val reconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(reconstructed.rows, data.rows)
    assertEquals(reconstructed.cols, data.cols)
    val actual = GaleArchiveTestData.toRows(reconstructed).flatten
    val expected = GaleArchiveTestData.toRows(data).flatten
    actual.zip(expected).foreach { case (a, e) =>
      assert(math.abs(a - e) < 2e-4, s"$a was not close to $e")
    }
  }

  test("voxel-scoped quant archive stores per-column scale and offset vectors") {
    val archive =
      LnaPipeline
        .quantArchive(curvedData, space, params = QuantParams(bits = 16, scaleScope = QuantScaleScope.Voxel))
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    val descriptor = archive.manifest.transforms.head
    descriptor.params match
      case TransformParams.Quant(params) =>
        assertEquals(params.scaleScope, QuantScaleScope.Voxel)
      case other =>
        fail(s"expected quant params, found $other")

    val scaleRef = descriptor.datasets.find(_.role == DatasetRole.Scale).getOrElse(fail("missing scale ref"))
    val offsetRef = descriptor.datasets.find(_.role == DatasetRole.Offset).getOrElse(fail("missing offset ref"))
    assertEquals(scaleRef.dims, Vector(curvedData.cols))
    assertEquals(offsetRef.dims, Vector(curvedData.cols))

    val reconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)

    val actual = GaleArchiveTestData.toRows(reconstructed).flatten
    val expected = GaleArchiveTestData.toRows(curvedData).flatten
    actual.zip(expected).foreach { case (a, e) =>
      assert(math.abs(a - e) < 1e-3, s"$a was not close to $e")
    }
  }

  test("sd quant rejects clipping unless allowClip is enabled") {
    val rejected =
      LnaPipeline.quantArchive(spikyData, space, params = QuantParams(bits = 8, method = QuantMethod.Sd))

    assert(rejected.isLeft)
    assert(rejected.left.toOption.exists(_.message.contains("would clip 2 samples")))
  }

  test("sd quant stores clipping report and hard-clips global archives") {
    val archive =
      LnaPipeline
        .quantArchive(spikyData, space, params = QuantParams(bits = 8, method = QuantMethod.Sd, allowClip = true))
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    val descriptor = archive.manifest.transforms.head
    val report =
      descriptor.report.collect { case TransformReport.Quant(report) => report }.getOrElse(fail("missing quant report"))

    assertEquals(report.method, QuantMethod.Sd)
    assertEquals(report.scaleScope, QuantScaleScope.Global)
    assertEquals(report.nClippedTotal, 2)
    assertEquals(report.clipPct, 2.0)

    val parsed =
      LnaManifestCodec
        .parse(LnaManifestCodec.render(archive.manifest))
        .fold(err => fail(err.message), identity)
    assertEquals(parsed.transforms.head.report, descriptor.report)

    val quantized =
      archive
        .payload(descriptor.datasets.find(_.role == DatasetRole.Quantized).get.path)
        .collect { case p: Payload.IntMatrix => p }
        .getOrElse(fail("missing quantized payload"))

    assertEquals(quantized.values.min, 0)
    assertEquals(quantized.values.max, 255)

    val reconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)
    assertEquals(reconstructed.rows, spikyData.rows)
    assertEquals(reconstructed.cols, spikyData.cols)
    assert(GaleArchiveTestData.toRows(reconstructed).flatten.forall(_.isFinite))
  }

  test("voxel-scoped sd quant stores per-column stats and clipping report") {
    val archive =
      LnaPipeline
        .quantArchive(
          spikyData,
          space,
          params = QuantParams(bits = 8, method = QuantMethod.Sd, scaleScope = QuantScaleScope.Voxel, allowClip = true)
        )
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    val descriptor = archive.manifest.transforms.head
    val report =
      descriptor.report.collect { case TransformReport.Quant(report) => report }.getOrElse(fail("missing quant report"))
    val scaleRef = descriptor.datasets.find(_.role == DatasetRole.Scale).getOrElse(fail("missing scale ref"))
    val offsetRef = descriptor.datasets.find(_.role == DatasetRole.Offset).getOrElse(fail("missing offset ref"))

    assertEquals(report.method, QuantMethod.Sd)
    assertEquals(report.scaleScope, QuantScaleScope.Voxel)
    assertEquals(report.nClippedTotal, 2)
    assertEquals(report.clipPct, 2.0)
    assertEquals(scaleRef.dims, Vector(spikyData.cols))
    assertEquals(offsetRef.dims, Vector(spikyData.cols))
  }

  test("quant checked decoder reports malformed scale and offset vectors") {
    val quantized =
      Payload.IntMatrix(
        rows = 1,
        cols = 3,
        values = Vector(0, 1, 2),
        dtype = LnaDType.UInt8
      )

    assert(Quant.decodeChecked(quantized, Vector.empty, Vector(0.0)).left.toOption.exists(_.message.contains("scale vector")))
    assert(Quant.decodeChecked(quantized, Vector(1.0, 2.0), Vector(0.0, 0.0)).left.toOption.exists(_.message.contains("scale vector")))
    assert(Quant.decodeChecked(quantized, Vector(1.0), Vector(0.0, 0.0, 0.0)).left.toOption.exists(_.message.contains("scale and offset")))
  }

  test("delta archive validates and reconstructs losslessly") {
    val archive =
      LnaPipeline
        .deltaArchive(data, space)
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Delta))

    val reconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(GaleArchiveTestData.toRows(reconstructed), GaleArchiveTestData.toRows(data))
  }

  test("delta checked decoder reports unsupported feature-axis payloads") {
    val deltas = GaleArchiveTestData.matrixFromRows(Vector(Vector(1.0, 2.0)))
    val first = GaleArchiveTestData.matrixFromRows(Vector(Vector(0.0, 0.0)))
    val result = Delta.decodeChecked(deltas, first, DeltaParams(axis = DeltaAxis.Feature))

    assertEquals(result.left.toOption, Some(ArchiveError.UnsupportedTransform("delta axis=feature")))
  }

  test("delta then quant archive reconstructs through the reverse plan") {
    val archive =
      LnaPipeline
        .deltaQuantArchive(curvedData, space, quantParams = QuantParams(bits = 16))
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Delta, TransformKind.Quant))
    assertEquals(archive.manifest.requiredTransforms, Vector(TransformKind.Delta, TransformKind.Quant))

    val reconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)

    val actual = GaleArchiveTestData.toRows(reconstructed).flatten
    val expected = GaleArchiveTestData.toRows(curvedData).flatten
    actual.zip(expected).foreach { case (a, e) =>
      assert(math.abs(a - e) < 1e-3, s"$a was not close to $e")
    }
  }

  test("validation reports missing descriptor payloads") {
    val archive =
      LnaPipeline
        .quantArchive(data, space)
        .fold(err => fail(err.message), identity)

    val broken = archive.copy(payloads = archive.payloads - archive.manifest.datasets.head.path)
    val issues = LnaValidator.validate(broken)

    assert(issues.exists(_.layer == ValidationLayer.References))
    assert(issues.exists(_.message.contains("no payload")))

    broken.validate match
      case Left(ArchiveError.ValidationFailed(structured)) =>
        assert(structured.exists(issue => issue.layer == ValidationLayer.References && issue.path.contains(archive.manifest.datasets.head.path)))
      case other =>
        fail(s"expected structured validation failure, found $other")
  }

  test("validation flags datasets scoped to unknown runs") {
    val archive =
      LnaPipeline
        .quantArchive(data, space)
        .fold(err => fail(err.message), identity)

    val descriptor = archive.manifest.transforms.head
    val quantIndex = descriptor.datasets.indexWhere(_.role == DatasetRole.Quantized)
    val datasetIndex = archive.manifest.datasets.indexWhere(_.role == DatasetRole.Quantized)
    val originalRef = descriptor.datasets(quantIndex)
    val badPath = ArchivePath("/scans/run-02/step_00_quant/values")
    val badRef = originalRef.copy(path = badPath)
    val brokenDescriptor = descriptor.copy(datasets = descriptor.datasets.updated(quantIndex, badRef))
    val brokenDatasets = archive.manifest.datasets.updated(datasetIndex, badRef)
    val broken =
      archive.copy(
        manifest = archive.manifest.copy(
          transforms = archive.manifest.transforms.updated(0, brokenDescriptor),
          datasets = brokenDatasets
        ),
        payloads = (archive.payloads - originalRef.path) + (badPath -> archive.payloads(originalRef.path))
      )

    val issues = LnaValidator.validate(broken)
    assert(issues.exists(issue => issue.path.contains(badPath) && issue.message.contains("unknown run run-02")))
  }

  test("basis and embed archive reconstructs from explicit stored basis") {
    val identityBasis =
      GaleArchiveTestData.matrixFromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

    val archive =
      LnaPipeline
        .basisEmbedArchive(data, space, identityBasis)
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Basis, TransformKind.Embed))

    val reconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)

    assertEquals(GaleArchiveTestData.toRows(reconstructed), GaleArchiveTestData.toRows(data))
  }

  test("shared basis embed archive stores an external basis reference") {
    val basisId = SharedBasisId.unsafe("identity_basis")
    val locator = SharedBasisLocator.unsafe("bases/identity.lna_basis.h5")
    val archive =
      LnaPipeline
        .sharedBasisEmbedArchive(data, space, identitySharedBasis, basisId, locator = Some(locator))
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Embed))
    assertEquals(archive.manifest.requiredTransforms, Vector(TransformKind.Embed))

    val embed = archive.manifest.transforms.head
    embed.params match
      case TransformParams.SharedBasisEmbed(ref, _, _, sourceDomain, targetDomain, _, metadata) =>
        assertEquals(ref.basisId, basisId)
        assertEquals(ref.checksum, identitySharedBasis.checksum)
        assertEquals(ref.locator, Some(locator))
        assertEquals(sourceDomain, Some("voxels"))
        assertEquals(targetDomain, Some("shared_basis.coefficients"))
        assertEquals(metadata("basis.kind"), "identity")
      case other =>
        fail(s"expected shared basis embed params, found $other")

    val parsed =
      LnaManifestCodec
        .parse(LnaManifestCodec.render(archive.manifest))
        .fold(err => fail(err.message), identity)
    assertEquals(LnaManifestCodec.render(parsed), LnaManifestCodec.render(archive.manifest))

    val failed = LnaPipeline.reconstruct(archive)
    assert(failed.isLeft)
    assert(failed.left.toOption.exists(_.message.contains("external shared-basis resolver")))
  }

  test("shared basis embed archive supports active-mask coefficients and offsets") {
    val basisId = SharedBasisId.unsafe("sparse_basis")
    val sparseBasis =
      SharedBasisArtifact(
        loadings = GaleArchiveTestData.matrixFromRows(
          Vector(
            Vector(1.0, 0.0),
            Vector(1.0, 1.0),
            Vector(0.0, 1.0)
          )
        ),
        mask = SharedBasisMask(Vector(2, 2, 1), Vector(true, false, true, true)),
        kind = "nonorthogonal",
        params = Map("source" -> "unit-test")
      )
    val coefficients =
      GaleArchiveTestData.matrixFromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(3.0, -1.0)
        )
      )
    val offset = Vector(10.0, -2.0, 5.0)

    val archive =
      LnaPipeline
        .sharedBasisEmbedArchiveFromCoefficients(coefficients, space, sparseBasis, basisId, offset = Some(offset))
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    val embed = archive.manifest.transforms.head
    val offsetRef = embed.datasets.find(_.role == DatasetRole.Offset).getOrElse(fail("missing offset ref"))
    assertEquals(offsetRef.dims, Vector(3))
    embed.params match
      case TransformParams.SharedBasisEmbed(_, centerDataWith, _, _, _, _, metadata) =>
        assertEquals(centerDataWith, Some(offsetRef.path))
        assertEquals(metadata("basis.mask_size"), "4")
        assertEquals(metadata("basis.mask_active"), "3")
      case other =>
        fail(s"expected shared basis embed params, found $other")
  }

  test("shared basis locators reject traversal") {
    assert(SharedBasisLocator("../bases/identity.lna_basis.h5").isLeft)
    assert(SharedBasisLocator("bases/../identity.lna_basis.h5").isLeft)
    assert(SharedBasisLocator("/tmp/identity.lna_basis.h5").isLeft)
  }

  test("explicit latent archive stores typed basis loadings offset and reconstructs") {
    val archive =
      LnaExplicitLatent
        .archive(explicitLatent, space)
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Basis, TransformKind.Embed))

    val embed = archive.manifest.transforms(1)
    embed.params match
      case TransformParams.Embed(_, _, _, sourceDomain, targetDomain, label, metadata) =>
        assertEquals(sourceDomain, Some("latent.coefficients.demo"))
        assertEquals(targetDomain, Some("voxels.demo"))
        assertEquals(label, Some("demo-latent"))
        assertEquals(metadata("subject"), "sub-01")
        assertEquals(metadata(LnaExplicitLatent.MetadataKindKey), LnaExplicitLatent.MetadataKindValue)
      case other =>
        fail(s"expected embed params, found $other")

    val decoded =
      LnaExplicitLatent
        .read(archive)
        .fold(err => fail(err.message), identity)
    assertEquals(decoded, explicitLatent)

    val reconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)
    assertEquals(
      GaleArchiveTestData.toRows(reconstructed),
      GaleArchiveTestData.toRows(LnaExplicitLatent.dense(explicitLatent))
    )

    val parsed =
      LnaManifestCodec
        .parse(LnaManifestCodec.render(archive.manifest))
        .fold(err => fail(err.message), identity)
    assertEquals(parsed.transforms(1).params, embed.params)
  }

  test("temporal DCT archive stores typed params and reconstructs as explicit latent response") {
    val params = TemporalDctParams(components = 2, norm = TemporalDctNorm.Ortho, center = true, ridge = 0.25)
    val archive =
      LnaTemporalDct
        .archive(explicitLatent, params, space)
        .fold(err => fail(err.message), identity)

    assertEquals(LnaValidator.validate(archive), Vector.empty)
    assertEquals(archive.manifest.requiredTransforms, Vector(TransformKind.Temporal, TransformKind.Embed))
    assertEquals(archive.manifest.transforms.map(_.kind), Vector(TransformKind.Temporal, TransformKind.Embed))
    assertEquals(archive.manifest.header("temporal.basis"), "dct")
    assertEquals(archive.manifest.header("temporal.components"), "2")

    val temporal = archive.manifest.transforms.head
    temporal.params match
      case TransformParams.TemporalDct(actual) =>
        assertEquals(actual, params)
      case other =>
        fail(s"expected temporal DCT params, found $other")
    assertEquals(temporal.datasets.map(_.role), Vector(DatasetRole.TemporalBasis))

    val embed = archive.manifest.transforms(1)
    embed.params match
      case TransformParams.Embed(_, _, _, _, _, _, metadata) =>
        assertEquals(metadata("family"), "time_dct")
        assertEquals(metadata("basis"), "dct")
        assertEquals(metadata("norm"), "ortho")
        assertEquals(metadata("center"), "true")
        assertEquals(metadata("ridge"), "0.25")
      case other =>
        fail(s"expected embed params, found $other")

    val decoded =
      LnaExplicitLatent
        .read(archive)
        .fold(err => fail(err.message), identity)
    assertEquals(decoded.metadata("basis"), "dct")
    assertEquals(decoded.metadata("components"), "2")

    val reconstructed =
      LnaPipeline
        .reconstruct(archive)
        .fold(err => fail(err.message), identity)
    assertEquals(
      GaleArchiveTestData.toRows(reconstructed),
      GaleArchiveTestData.toRows(LnaExplicitLatent.dense(decoded))
    )

    val parsed =
      LnaManifestCodec
        .parse(LnaManifestCodec.render(archive.manifest))
        .fold(err => fail(err.message), identity)
    assertEquals(LnaManifestCodec.render(parsed), LnaManifestCodec.render(archive.manifest))
  }

  test("validation catches missing explicit latent pieces") {
    val archive =
      LnaExplicitLatent
        .archive(explicitLatent, space)
        .fold(err => fail(err.message), identity)

    val brokenEmbed =
      archive.manifest.transforms(1).copy(datasets = Vector.empty)
    val missingLoadings =
      archive.copy(manifest = archive.manifest.copy(transforms = archive.manifest.transforms.updated(1, brokenEmbed)))
    val loadingsIssues = LnaValidator.validate(missingLoadings)
    assert(loadingsIssues.exists(_.message.contains("missing loadings dataset")))

    val missingBasis =
      archive.copy(manifest = archive.manifest.copy(datasets = archive.manifest.datasets.filterNot(_.role == DatasetRole.TemporalBasis)))
    val basisIssues = LnaValidator.validate(missingBasis)
    assert(basisIssues.exists(_.message.contains("explicit latent basis path is undeclared")))
  }

  test("manifest codec roundtrips typed descriptors") {
    val archive =
      LnaPipeline
        .deltaQuantArchive(curvedData, space, quantParams = QuantParams(bits = 16))
        .fold(err => fail(err.message), identity)

    val parsed =
      LnaManifestCodec
        .parse(LnaManifestCodec.render(archive.manifest))
        .fold(err => fail(err.message), identity)

    assertEquals(LnaManifestCodec.render(parsed), LnaManifestCodec.render(archive.manifest))
  }

  test("quant rejects non-finite values") {
    val bad = GaleArchiveTestData.matrixFromRows(Vector(Vector(1.0, Double.NaN, 2.0, 3.0)))
    val result = LnaPipeline.quantArchive(bad, space)
    assert(result.isLeft)
    assert(result.left.toOption.exists(_.message.contains("non-finite")))
  }
