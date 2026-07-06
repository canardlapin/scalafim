package scalafim.archive.io

import io.jhdf.HdfFile
import io.jhdf.api.WritableGroup
import io.jhdf.`object`.datatype.FixedPoint
import scalafim.archive.ArchivePath
import scalafim.archive.lna.{LnaArchive, LnaDType, LnaExplicitLatent, LnaManifestCodec, LnaPipeline, LnaTemporalDct, LnaValidator, Payload, QuantMethod, QuantParams, QuantScaleScope, TemporalDctNorm, TemporalDctParams, TransformKind, TransformParams, TransformReport}
import scalafim.image.{DMat, NeuroSpace}

import java.nio.file.Files

class JhdfLnaHdf5StoreSuite extends munit.FunSuite:
  private val space = NeuroSpace(Vector(2, 2, 1))

  private val data =
    DMat.fromRows(
      Vector(
        Vector(0.0, 1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0, 7.0),
        Vector(8.0, 9.0, 10.0, 11.0)
      )
    )

  private val curvedData =
    DMat.fromRows(
      Vector(
        Vector(0.0, 1.0, 2.0, 3.0),
        Vector(1.0, 3.0, 6.0, 10.0),
        Vector(2.0, 6.0, 12.0, 20.0),
        Vector(4.0, 10.0, 20.0, 35.0)
      )
    )

  private val spikyData =
    DMat.fromRows(
      Vector.tabulate(25) {
        case 23 => Vector(100.0, 0.0, 0.0, 0.0)
        case 24 => Vector(-100.0, 0.0, 0.0, 0.0)
        case _  => Vector(0.0, 0.0, 0.0, 0.0)
      }
    )

  private val explicitLatent =
    LnaExplicitLatent.Response(
      basis = DMat.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.5, 1.0),
          Vector(0.0, 2.0)
        )
      ),
      loadings = DMat.fromRows(
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

  test("jHDF store writes inspectable metadata and roundtrips quant archives") {
    val archive =
      LnaPipeline
        .quantArchive(data, space, params = QuantParams(bits = 16))
        .fold(err => fail(err.message), identity)

    val file = Files.createTempFile("scalafim-lna-quant-", ".h5")
    try
      JhdfLnaHdf5Store.write(file, archive).fold(err => fail(err.message), identity)

      val hdf = new HdfFile(file)
      try
        assertEquals(hdf.getAttribute("scalafim_lna_storage").getData, "scalafim-lna-hdf5-0")
        assertEquals(hdf.getAttribute("lna_checksum_algorithm").getData, "sha256:lna-manifest-null-payloads-v1")
        assertEquals(hdf.getAttribute("lna_checksum").getData.asInstanceOf[String].length, 64)
        assert(hdf.getDatasetByPath("/__lna__/manifest_json").getData.asInstanceOf[String].contains("\"required_transforms\""))
        assertEquals(hdf.getDatasetByPath("/__lna__/payloads/scans/run-01/step_00_quant/values").getDimensions().toVector, Vector(3, 4))
      finally hdf.close()

      val loaded =
        JhdfLnaHdf5Store
          .read(file)
          .fold(err => fail(err.message), identity)

      assertEquals(LnaValidator.validate(loaded), Vector.empty)
      assertEquals(loaded.manifest.copy(checksum = None), archive.manifest)
      assert(loaded.manifest.checksum.exists(_.matches("[A-Fa-f0-9]{64}")))
      assertEquals(loaded.payloads.keySet, archive.payloads.keySet)

      val reconstructed =
        LnaPipeline
          .reconstruct(loaded)
          .fold(err => fail(err.message), identity)

      val actual = reconstructed.toRows.flatten
      val expected = data.toRows.flatten
      actual.zip(expected).foreach { case (a, e) =>
        assert(math.abs(a - e) < 2e-4, s"$a was not close to $e")
      }
    finally Files.deleteIfExists(file)
  }

  test("jHDF store verifies archive checksums on read") {
    val archive =
      LnaPipeline
        .quantArchive(data, space, params = QuantParams(bits = 16), creator = "checksum-good")
        .fold(err => fail(err.message), identity)
    val badChecksum = "0" * 64

    val file = Files.createTempFile("scalafim-lna-checksum-", ".h5")
    try
      writeRawFixture(file, archive.copy(manifest = archive.manifest.copy(checksum = Some(badChecksum))))
      val failed = LnaHdf5Store.default.read(file)
      assert(failed.isLeft)
      val message = failed.left.toOption.map(_.message).getOrElse("")
      assert(message.contains("checksum mismatch"), message)
    finally Files.deleteIfExists(file)
  }

  test("jHDF store exposes logical unsigned dtype attributes for quantized payloads") {
    val archive =
      LnaPipeline
        .quantArchive(data, space, params = QuantParams(bits = 8))
        .fold(err => fail(err.message), identity)

    val file = Files.createTempFile("scalafim-lna-uint8-", ".h5")
    try
      LnaHdf5Store.default.write(file, archive).fold(err => fail(err.message), identity)

      val hdf = new HdfFile(file)
      try
        val dataset = hdf.getDatasetByPath("/__lna__/payloads/scans/run-01/step_00_quant/values")
        assertEquals(dataset.getAttribute("lna_dtype").getData, "uint8")
        assertEquals(dataset.getAttribute("lna_unsigned").getData.asInstanceOf[Boolean], true)
        dataset.getDataType match
          case fixed: FixedPoint =>
            assert(fixed.isSigned, "jHDF 0.12.0 still writes Java byte arrays as signed fixed-point storage")
          case other =>
            fail(s"expected fixed-point integer dataset, found $other")
      finally hdf.close()
    finally Files.deleteIfExists(file)
  }

  test("jHDF store roundtrips sd quant clipping reports") {
    val archive =
      LnaPipeline
        .quantArchive(spikyData, space, params = QuantParams(bits = 8, method = QuantMethod.Sd, allowClip = true))
        .fold(err => fail(err.message), identity)

    val file = Files.createTempFile("scalafim-lna-quant-sd-", ".h5")
    try
      LnaHdf5Store.default.write(file, archive).fold(err => fail(err.message), identity)
      val loaded = LnaHdf5Store.default.read(file).fold(err => fail(err.message), identity)

      assertEquals(LnaValidator.validate(loaded), Vector.empty)
      assertEquals(loaded.manifest.transforms.map(_.kind), Vector(TransformKind.Quant))
      assertEquals(loaded.payloads.keySet, archive.payloads.keySet)

      val report =
        loaded.manifest.transforms.head.report.collect { case TransformReport.Quant(report) => report }.getOrElse(fail("missing quant report"))
      assertEquals(report.method, QuantMethod.Sd)
      assertEquals(report.scaleScope, QuantScaleScope.Global)
      assertEquals(report.nClippedTotal, 2)
      assertEquals(report.clipPct, 2.0)

      val reconstructed =
        LnaPipeline
          .reconstruct(loaded)
          .fold(err => fail(err.message), identity)

      assertEquals(reconstructed.rows, spikyData.rows)
      assertEquals(reconstructed.cols, spikyData.cols)
      assert(reconstructed.toRows.flatten.forall(_.isFinite))
    finally Files.deleteIfExists(file)
  }

  test("jHDF store roundtrips delta/quant transform plans") {
    val archive =
      LnaPipeline
        .deltaQuantArchive(curvedData, space, quantParams = QuantParams(bits = 16, scaleScope = QuantScaleScope.Voxel))
        .fold(err => fail(err.message), identity)

    val file = Files.createTempFile("scalafim-lna-delta-quant-", ".h5")
    try
      LnaHdf5Store.default.write(file, archive).fold(err => fail(err.message), identity)
      val loaded = LnaHdf5Store.default.read(file).fold(err => fail(err.message), identity)

      assertEquals(LnaValidator.validate(loaded), Vector.empty)
      assertEquals(loaded.manifest.transforms.map(_.kind), Vector(TransformKind.Delta, TransformKind.Quant))

      val reconstructed =
        LnaPipeline
          .reconstruct(loaded)
          .fold(err => fail(err.message), identity)

      val actual = reconstructed.toRows.flatten
      val expected = curvedData.toRows.flatten
      actual.zip(expected).foreach { case (a, e) =>
        assert(math.abs(a - e) < 1e-3, s"$a was not close to $e")
      }
    finally Files.deleteIfExists(file)
  }

  test("jHDF store roundtrips basis/embed archives") {
    val identityBasis =
      DMat.fromRows(
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

    val file = Files.createTempFile("scalafim-lna-basis-", ".h5")
    try
      LnaHdf5Store.default.write(file, archive).fold(err => fail(err.message), identity)
      val loaded = LnaHdf5Store.default.read(file).fold(err => fail(err.message), identity)

      assertEquals(loaded.manifest.transforms.map(_.kind), Vector(TransformKind.Basis, TransformKind.Embed))
      assertEquals(LnaPipeline.reconstruct(loaded).fold(err => fail(err.message), identity), data)
    finally Files.deleteIfExists(file)
  }

  test("jHDF store roundtrips explicit latent archives") {
    val archive =
      LnaExplicitLatent
        .archive(explicitLatent, space)
        .fold(err => fail(err.message), identity)

    val file = Files.createTempFile("scalafim-lna-explicit-latent-", ".h5")
    try
      LnaHdf5Store.default.write(file, archive).fold(err => fail(err.message), identity)
      val loaded = LnaHdf5Store.default.read(file).fold(err => fail(err.message), identity)

      assertEquals(LnaValidator.validate(loaded), Vector.empty)
      assertEquals(loaded.manifest.transforms.map(_.kind), Vector(TransformKind.Basis, TransformKind.Embed))
      assertEquals(LnaExplicitLatent.read(loaded).fold(err => fail(err.message), identity), explicitLatent)
      assertEquals(LnaPipeline.reconstruct(loaded).fold(err => fail(err.message), identity), LnaExplicitLatent.dense(explicitLatent))
    finally Files.deleteIfExists(file)
  }

  test("jHDF store roundtrips temporal DCT explicit latent archives") {
    val params = TemporalDctParams(components = 2, norm = TemporalDctNorm.Ortho, center = true, ridge = 0.25)
    val archive =
      LnaTemporalDct
        .archive(explicitLatent, params, space)
        .fold(err => fail(err.message), identity)

    val file = Files.createTempFile("scalafim-lna-temporal-dct-", ".h5")
    try
      LnaHdf5Store.default.write(file, archive).fold(err => fail(err.message), identity)
      val loaded = LnaHdf5Store.default.read(file).fold(err => fail(err.message), identity)

      assertEquals(LnaValidator.validate(loaded), Vector.empty)
      assertEquals(loaded.manifest.transforms.map(_.kind), Vector(TransformKind.Temporal, TransformKind.Embed))
      loaded.manifest.transforms.head.params match
        case TransformParams.TemporalDct(actual) =>
          assertEquals(actual, params)
        case other =>
          fail(s"expected temporal DCT params, found $other")
      assertEquals(LnaExplicitLatent.read(loaded).fold(err => fail(err.message), identity).metadata("basis"), "dct")
      assertEquals(LnaPipeline.reconstruct(loaded).fold(err => fail(err.message), identity), LnaExplicitLatent.dense(explicitLatent))
    finally Files.deleteIfExists(file)
  }

  private def writeRawFixture(path: java.nio.file.Path, archive: LnaArchive): Unit =
    val hdf = HdfFile.write(path)
    try
      hdf.putAttribute("scalafim_lna_storage", "scalafim-lna-hdf5-0")
      archive.manifest.checksum.foreach(hdf.putAttribute("lna_checksum", _))
      val meta = hdf.putGroup("__lna__")
      meta.putDataset("manifest_json", LnaManifestCodec.render(archive.manifest))
      val payloadRoot = meta.putGroup("payloads")
      archive.payloads.toVector.sortBy(_._1.value).foreach { case (path, payload) =>
        writeRawPayload(payloadRoot, path, payload)
      }
    finally hdf.close()

  private def writeRawPayload(root: WritableGroup, path: ArchivePath, payload: Payload): Unit =
    val segments = path.value.stripPrefix("/").split('/').filter(_.nonEmpty).toVector
    val parent = segments.dropRight(1).foldLeft(root) { case (group, segment) =>
      group.getChild(segment) match
        case existing: WritableGroup => existing
        case null                    => group.putGroup(segment)
        case other                   => throw new IllegalArgumentException(s"${other.getPath} is not a group")
    }
    parent.putDataset(segments.last, rawPayloadData(payload))

  private def rawPayloadData(payload: Payload): Object =
    payload match
      case Payload.DoubleMatrix(data, LnaDType.Float64) =>
        Array.tabulate(data.rows, data.cols)((r, c) => data(r, c))
      case Payload.DoubleMatrix(data, LnaDType.Float32) =>
        Array.tabulate(data.rows, data.cols)((r, c) => data(r, c).toFloat)
      case Payload.DoubleVector(values, LnaDType.Float64) =>
        values.toArray
      case Payload.DoubleVector(values, LnaDType.Float32) =>
        values.map(_.toFloat).toArray
      case Payload.IntMatrix(rows, cols, values, LnaDType.UInt8) =>
        Array.tabulate(rows, cols)((r, c) => values(r * cols + c).toByte)
      case Payload.IntMatrix(rows, cols, values, LnaDType.UInt16) =>
        Array.tabulate(rows, cols)((r, c) => values(r * cols + c).toShort)
      case Payload.IntMatrix(rows, cols, values, LnaDType.Int32) =>
        Array.tabulate(rows, cols)((r, c) => values(r * cols + c))
      case other =>
        throw new IllegalArgumentException(s"unsupported fixture payload $other")
