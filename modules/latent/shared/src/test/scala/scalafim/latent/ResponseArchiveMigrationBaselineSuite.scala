package scalafim.latent

import gale.linalg.{DMat, DVec, LinAlgError}
import scalafim.archive.ArchiveError
import scalafim.archive.lna.{
  LnaPipeline,
  QuantParams,
  SharedBasisArtifact,
  SharedBasisId,
  SharedBasisMask
}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}

enum MigrationComparator:
  case RawBits
  case AbsoluteRelative(absolute: Double, relative: Double)

  def id: String =
    this match
      case RawBits                 => "raw-bits"
      case AbsoluteRelative(_, _) => "absolute-relative"

final case class MigrationBaselineCase(
    id: String,
    comparator: MigrationComparator,
    rows: Int,
    columns: Int,
    values: Vector[Double]
):
  require(id.nonEmpty, "baseline case id must be non-empty")
  require(rows > 0, "baseline rows must be positive")
  require(columns > 0, "baseline columns must be positive")
  require(values.length == rows * columns, "baseline values must match rows by columns")
  require(values.forall(_.isFinite), "baseline values must be finite")

  def rawBits: Vector[Long] =
    values.map(java.lang.Double.doubleToRawLongBits)

  def rawHex: Vector[String] =
    rawBits.map: value =>
      val raw = java.lang.Long.toHexString(value)
      "0" * (16 - raw.length) + raw

final case class MigrationGoldenCase(
    id: String,
    comparator: MigrationComparator,
    rows: Int,
    columns: Int,
    rawHex: Vector[String]
):
  require(rawHex.length == rows * columns, "golden values must match rows by columns")

object ResponseArchiveMigrationBaseline:
  val Schema: String = "scalafim-response-archive-migration-baseline-v1"

  val Golden: Vector[MigrationGoldenCase] =
    Vector(
      golden(
        "dense",
        MigrationComparator.RawBits,
        4,
        4,
        "3ff0000000000000 c000000000000000 3fe0000000000000 4010000000000000 " +
          "4008000000000000 3fd0000000000000 bff8000000000000 4000000000000000 " +
          "bfe8000000000000 4004000000000000 4010000000000000 c008000000000000 " +
          "0000000000000000 c008000000000000 3ff4000000000000 4014000000000000"
      ),
      golden(
        "lna-quant",
        MigrationComparator.RawBits,
        4,
        4,
        "3fefff81ff81ff88 c0000015c015c016 3fdfff69ff69ff70 4010000cc00cc00d " +
          "4007fff03ff03ff2 3fd000a800a800b0 bff7ffbf7fbf7fbf 40000009c009c00a " +
          "bfe8005100510050 4003fffcfffcfffe 4010000cc00cc00d c007fffc3ffc3ffc " +
          "bee8001800140000 c007fffc3ffc3ffc 3ff3fff6fff6fff8 4014000000000000"
      ),
      golden(
        "lna-delta",
        MigrationComparator.RawBits,
        4,
        4,
        "3ff0000000000000 c000000000000000 3fe0000000000000 4010000000000000 " +
          "4008000000000000 3fd0000000000000 bff8000000000000 4000000000000000 " +
          "bfe8000000000000 4004000000000000 4010000000000000 c008000000000000 " +
          "0000000000000000 c008000000000000 3ff4000000000000 4014000000000000"
      ),
      golden(
        "lna-delta-quant",
        MigrationComparator.RawBits,
        4,
        4,
        "3ff0000000000000 c000000000000000 3fe0000000000000 4010000000000000 " +
          "4007ffe5553aaa8c 3fd001e001e001c0 bff800035558aab4 3ffffffcaaa7554c " +
          "bfe7ffe5553aaaa8 4004007800780070 4010000f000f000d c0080010aabb556c " +
          "3f1e001e00190000 c007ffc3ffc3ffd0 3ff4003200320024 4013fff7aaa2554a"
      ),
      golden(
        "lna-basis-embed",
        MigrationComparator.RawBits,
        4,
        4,
        "3ff0000000000000 c000000000000000 3fe0000000000000 4010000000000000 " +
          "4008000000000000 3fd0000000000000 bff8000000000000 4000000000000000 " +
          "bfe8000000000000 4004000000000000 4010000000000000 c008000000000000 " +
          "0000000000000000 c008000000000000 3ff4000000000000 4014000000000000"
      ),
      golden(
        "latent-explicit",
        MigrationComparator.RawBits,
        3,
        4,
        "4059400000000000 4069400000000000 4072f00000000000 4079400000000000 " +
          "405b800000000000 406b800000000000 4074a00000000000 407b800000000000 " +
          "405bc00000000000 406bc00000000000 4074d00000000000 407bc00000000000"
      ),
      golden(
        "lna-temporal-dct",
        MigrationComparator.AbsoluteRelative(1e-12, 1e-12),
        4,
        4,
        "3fefffffffffffff bfffffffffffffff 3fdffffffffffffa 4010000000000000 " +
          "4008000000000000 3fd0000000000000 bff7fffffffffffe 3ffffffffffffffe " +
          "bfe8000000000002 4004000000000000 4010000000000000 c008000000000001 " +
          "0000000000000000 c008000000000000 3ff4000000000003 4014000000000001"
      ),
      golden(
        "latent-temporal-dct",
        MigrationComparator.AbsoluteRelative(1e-12, 1e-12),
        4,
        4,
        "3fefffffffffffff bfffffffffffffff 3fdffffffffffffa 4010000000000000 " +
          "4008000000000000 3fd0000000000000 bff7fffffffffffe 3ffffffffffffffe " +
          "bfe8000000000002 4004000000000000 4010000000000000 c008000000000001 " +
          "0000000000000000 c008000000000000 3ff4000000000003 4014000000000001"
      ),
      golden(
        "latent-temporal-haar",
        MigrationComparator.AbsoluteRelative(1e-12, 1e-12),
        4,
        4,
        "3feffffffffffffe c000000000000000 3fe0000000000002 4010000000000000 " +
          "4008000000000000 3fd0000000000004 bff8000000000001 3fffffffffffffff " +
          "bfe8000000000000 4004000000000000 4010000000000000 c008000000000002 " +
          "0000000000000000 c008000000000000 3ff3ffffffffffff 4014000000000001"
      ),
      golden(
        "latent-shared-basis",
        MigrationComparator.AbsoluteRelative(1e-12, 1e-12),
        3,
        3,
        "4026000000000000 3ff0000000000002 401c000000000000 " +
          "402a000000000000 bcb0000000000000 4010000000000000 " +
          "4020000000000000 c00bffffffffffff 4015ffffffffffff"
      ),
      golden(
        "latent-hrbf-shared-basis",
        MigrationComparator.AbsoluteRelative(1e-12, 1e-12),
        3,
        3,
        "4001b4597e37cb06 4002f8e1d192a693 4001152aaa3bf81b " +
          "400325d340e41a7e 3fedfcaed0613ffe bfe3020005305ea5 " +
          "bffb25d340e41a7c bfe8b2b0e6686b04 3fcd5aaab880fc58"
      ),
      golden(
        "latent-transport",
        MigrationComparator.RawBits,
        4,
        4,
        "3ff0000000000000 c000000000000000 3fe0000000000000 4010000000000000 " +
          "4008000000000000 3fd0000000000000 bff8000000000000 4000000000000000 " +
          "bfe8000000000000 4004000000000000 4010000000000000 c008000000000000 " +
          "0000000000000000 c008000000000000 3ff4000000000000 4014000000000000"
      ),
      golden(
        "latent-boldzip",
        MigrationComparator.RawBits,
        4,
        4,
        "3ff0000000000000 c000000000000000 3fe0000000000000 4010000000000000 " +
          "4008000000000000 3fd0000000000000 bff8000000000000 4000000000000000 " +
          "bfe8000000000000 4004000000000000 4010000000000000 c008000000000000 " +
          "0000000000000000 c008000000000000 3ff4000000000000 4014000000000000"
      )
    )

  val ExpectedIds: Vector[String] = Golden.map(_.id)

  def current: Vector[MigrationBaselineCase] =
    Vector(
      matrixCase("dense", MigrationComparator.RawBits, canonicalRows)
    ) ++
      lnaPipelineCases ++
      explicitCase ++
      temporalDctCases ++
      temporalHaarCase ++
      sharedBasisCase ++
      radialBasisCase ++
      transportCase ++
      boldZipCase

  def render(cases: Vector[MigrationBaselineCase] = current): String =
    val renderedCases =
      cases.map: baseline =>
        val comparator =
          baseline.comparator match
            case MigrationComparator.RawBits =>
              """{"kind":"raw-bits"}"""
            case MigrationComparator.AbsoluteRelative(absolute, relative) =>
              s"""{"absolute":${doubleJson(absolute)},"kind":"absolute-relative","relative":${doubleJson(relative)}}"""
        val bits = baseline.rawHex.map(quoted).mkString("[", ",", "]")
        s"""{"columns":${baseline.columns},"comparator":$comparator,"id":${quoted(baseline.id)},"rawBits":$bits,"rows":${baseline.rows}}"""
      .mkString("[", ",", "]")
    s"""{"cases":$renderedCases,"schema":${quoted(Schema)}}"""

  private val canonicalRows: Vector[Vector[Double]] =
    Vector(
      Vector(1.0, -2.0, 0.5, 4.0),
      Vector(3.0, 0.25, -1.5, 2.0),
      Vector(-0.75, 2.5, 4.0, -3.0),
      Vector(0.0, -3.0, 1.25, 5.0)
    )

  private val canonicalSpace = NeuroSpace(Vector(2, 2, 1))

  private def lnaPipelineCases: Vector[MigrationBaselineCase] =
    val data = ImageDMat.fromRows(canonicalRows)
    val quant =
      archiveValue(
        LnaPipeline.quantArchive(
          data,
          canonicalSpace,
          params = QuantParams(bits = 16)
        )
      )
    val delta =
      archiveValue(LnaPipeline.deltaArchive(data, canonicalSpace))
    val deltaQuant =
      archiveValue(
        LnaPipeline.deltaQuantArchive(
          data,
          canonicalSpace,
          quantParams = QuantParams(bits = 16)
        )
      )
    val basis =
      archiveValue(
        LnaPipeline.basisEmbedArchive(
          data,
          canonicalSpace,
          ImageDMat.eye(data.cols)
        )
      )

    Vector(
      imageCase(
        "lna-quant",
        MigrationComparator.RawBits,
        archiveValue(LnaPipeline.reconstruct(quant))
      ),
      imageCase(
        "lna-delta",
        MigrationComparator.RawBits,
        archiveValue(LnaPipeline.reconstruct(delta))
      ),
      imageCase(
        "lna-delta-quant",
        MigrationComparator.RawBits,
        archiveValue(LnaPipeline.reconstruct(deltaQuant))
      ),
      imageCase(
        "lna-basis-embed",
        MigrationComparator.RawBits,
        archiveValue(LnaPipeline.reconstruct(basis))
      )
    )

  private def explicitCase: Vector[MigrationBaselineCase] =
    val source =
      latentValue(
        ExplicitLatentResponse(
          basis = LatentNumerics.matrixFromRows(
            Vector(
              Vector(1.0, 0.0),
              Vector(0.0, 1.0),
              Vector(1.0, 1.0)
            )
          ),
          loadings = LatentNumerics.matrixFromRows(
            Vector(
              Vector(1.0, 10.0),
              Vector(2.0, 20.0),
              Vector(3.0, 30.0),
              Vector(4.0, 40.0)
            )
          ),
          offset = Some(DVec.fromSeq(Vector(100.0, 200.0, 300.0, 400.0))),
          label = "phase-0-explicit"
        )
      )
    val archive =
      archiveValue(ExplicitLatentArchiveCodec.toArchive(source, canonicalSpace))
    val decoded =
      archiveValue(LatentArchiveRegistry.standard.fromArchive(archive)) match
        case LatentArchiveResponse.Explicit(response) => response
        case other => throw IllegalStateException(s"expected explicit response, found $other")

    Vector(
      galeCase(
        "latent-explicit",
        MigrationComparator.RawBits,
        latentValue(decoded.reconstruct())
      )
    )

  private def temporalDctCases: Vector[MigrationBaselineCase] =
    val archive =
      archiveValue(
        ExplicitLatentArchiveCodec.toTemporalDctArchive(
          data = LatentNumerics.matrixFromRows(canonicalRows),
          space = canonicalSpace,
          components = canonicalRows.length,
          norm = DctNorm.Ortho,
          center = true,
          ridge = 0.0,
          label = "phase-0-dct"
        )
      )
    val decoded =
      archiveValue(LatentArchiveRegistry.standard.fromArchive(archive)) match
        case LatentArchiveResponse.TemporalDct(response, _, _, _) => response
        case other => throw IllegalStateException(s"expected temporal DCT response, found $other")
    val comparator = MigrationComparator.AbsoluteRelative(1e-12, 1e-12)

    Vector(
      imageCase(
        "lna-temporal-dct",
        comparator,
        archiveValue(LnaPipeline.reconstruct(archive))
      ),
      galeCase(
        "latent-temporal-dct",
        comparator,
        latentValue(decoded.reconstruct())
      )
    )

  private def temporalHaarCase: Vector[MigrationBaselineCase] =
    val spec =
      latentValue(
        LatentEncodingSpec.haar(
          timepoints = canonicalRows.length,
          components = canonicalRows.length,
          center = true,
          label = "phase-0-haar"
        )
      )
    val archive =
      archiveValue(
        LatentEncoder.toArchive(
          LatentNumerics.matrixFromRows(canonicalRows),
          canonicalSpace,
          spec
        )
      )
    val decoded =
      archiveValue(LatentArchiveRegistry.standard.fromArchive(archive)) match
        case LatentArchiveResponse.TemporalHaar(response, _, _, _) => response
        case other => throw IllegalStateException(s"expected temporal Haar response, found $other")

    Vector(
      galeCase(
        "latent-temporal-haar",
        MigrationComparator.AbsoluteRelative(1e-12, 1e-12),
        latentValue(decoded.reconstruct())
      )
    )

  private def sharedBasisCase: Vector[MigrationBaselineCase] =
    val loadings =
      ImageDMat.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 1.0)
        )
      )
    val artifact =
      SharedBasisArtifact(
        loadings = loadings,
        mask = SharedBasisMask(Vector(3), Vector(true, true, true)),
        kind = "phase-0-nonorthogonal",
        params = Map("fixture" -> "response-archive-phase-0")
      )
    val coefficients =
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, -1.0),
        Vector(-2.0, 0.5)
      )
    val offset = Vector(10.0, -2.0, 5.0)
    val data = denseFromLoadings(coefficients, offset, loadings)
    val archive =
      archiveValue(
        SharedBasisLatentArchiveCodec.toArchive(
          data = data,
          space = NeuroSpace(Vector(3, 1, 1)),
          basis = artifact,
          basisId = SharedBasisId.unsafe("phase_0_shared_basis"),
          center = true
        )
      )
    val decoded =
      archiveValue(LatentArchiveRegistry.standard.fromArchive(archive)) match
        case LatentArchiveResponse.SharedBasis(response) =>
          latentValue(response.materialize(artifact, Some(NeuroSpace(Vector(3, 1, 1)))))
        case other => throw IllegalStateException(s"expected shared-basis response, found $other")

    Vector(
      galeCase(
        "latent-shared-basis",
        MigrationComparator.AbsoluteRelative(1e-12, 1e-12),
        latentValue(decoded.reconstruct())
      )
    )

  private def radialBasisCase: Vector[MigrationBaselineCase] =
    val coordinates =
      Vector(
        WorldCoordinate3D.unsafe(0.0, 0.0, 0.0),
        WorldCoordinate3D.unsafe(1.0, 0.0, 0.0),
        WorldCoordinate3D.unsafe(2.0, 0.0, 0.0)
      )
    val radialSpace = NeuroSpace(Vector(3, 1, 1))
    val radial =
      radialValue(
        RadialBasis.fromSpaceIndices(
          atoms = Vector(
            RadialAtom(coordinates(0), RadialSigmaMm.unsafe(1.0)),
            RadialAtom(coordinates(2), RadialSigmaMm.unsafe(2.0))
          ),
          space = radialSpace,
          activeIndices = Vector(0, 1, 2),
          kernel = RadialKernel.Gaussian
        )
      )
    val coefficients =
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, -1.0),
        Vector(-2.0, 0.5)
      )
    val data =
      LatentNumerics.matrixFromRows(
        coefficients.map: row =>
          Vector.tabulate(radial.loadings.rows): sample =>
            var sum = 0.0
            var atom = 0
            while atom < radial.loadings.cols do
              sum += row(atom) * radial.loadings(sample, atom)
              atom += 1
            sum
      )
    val spec =
      latentValue(
        LatentEncodingSpec.radialBasis(
          radialBasis = radial,
          maskDims = Vector(3),
          basisId = SharedBasisId.unsafe("phase_0_hrbf_basis"),
          center = false,
          metadata = Map("fixture" -> "response-archive-phase-0")
        )
      )
    val archive =
      archiveValue(
        LatentEncoder.toArchive(
          data,
          radialSpace,
          spec
        )
      )
    val artifact =
      radialValue(
        radial.toSharedBasisArtifact(
          maskDims = Vector(3),
          params = Map("fixture" -> "response-archive-phase-0")
        )
      )
    val decoded =
      archiveValue(LatentArchiveRegistry.standard.fromArchive(archive)) match
        case LatentArchiveResponse.SharedBasis(response) =>
          latentValue(response.materialize(artifact, Some(radialSpace)))
        case other => throw IllegalStateException(s"expected HRBF shared-basis response, found $other")

    Vector(
      galeCase(
        "latent-hrbf-shared-basis",
        MigrationComparator.AbsoluteRelative(1e-12, 1e-12),
        latentValue(decoded.reconstruct())
      )
    )

  private def transportCase: Vector[MigrationBaselineCase] =
    val decoder = linearValue(LatentOperators.identity(canonicalRows.head.length))
    val source =
      latentValue(
        TransportLatentResponse.withIdentityTransform(
          coefficientsAnalysis = LatentNumerics.matrixFromRows(canonicalRows),
          nativeDecoder = decoder,
          label = "phase-0-transport"
        )
      )
    val archive =
      archiveValue(TransportLatentArchiveCodec.toArchive(source, canonicalSpace))
    val decoded =
      archiveValue(LatentArchiveRegistry.standard.fromArchive(archive)) match
        case LatentArchiveResponse.Transport(response) => response
        case other => throw IllegalStateException(s"expected transport response, found $other")

    Vector(
      galeCase(
        "latent-transport",
        MigrationComparator.RawBits,
        latentValue(decoded.reconstruct())
      )
    )

  private def boldZipCase: Vector[MigrationBaselineCase] =
    val payload =
      latentValue(
        BoldZipEncoder
          .identityDetail(
            LatentNumerics.matrixFromRows(canonicalRows),
            center = true,
            metadata = Map("fixture" -> "response-archive-phase-0")
          )
      ).payload
    val archive =
      archiveValue(BoldZipLatentArchiveCodec.toArchive(payload, canonicalSpace))
    val decoded =
      archiveValue(LatentArchiveRegistry.standard.fromArchive(archive)) match
        case LatentArchiveResponse.BoldZip(response) => response
        case other => throw IllegalStateException(s"expected BOLDZip response, found $other")

    Vector(
      galeCase(
        "latent-boldzip",
        MigrationComparator.RawBits,
        latentValue(decoded.reconstruct())
      )
    )

  private def denseFromLoadings(
      coefficients: Vector[Vector[Double]],
      offset: Vector[Double],
      loadings: ImageDMat
  ): DMat =
    LatentNumerics.matrixFromRows(
      coefficients.map: row =>
        Vector.tabulate(loadings.rows): sample =>
          var sum = offset(sample)
          var atom = 0
          while atom < loadings.cols do
            sum += row(atom) * loadings(sample, atom)
            atom += 1
          sum
    )

  private def imageCase(
      id: String,
      comparator: MigrationComparator,
      matrix: ImageDMat
  ): MigrationBaselineCase =
    matrixCase(id, comparator, matrix.toRows)

  private def galeCase(
      id: String,
      comparator: MigrationComparator,
      matrix: DMat
  ): MigrationBaselineCase =
    matrixCase(id, comparator, matrix.toRows)

  private def matrixCase(
      id: String,
      comparator: MigrationComparator,
      rows: Vector[Vector[Double]]
  ): MigrationBaselineCase =
    MigrationBaselineCase(
      id = id,
      comparator = comparator,
      rows = rows.length,
      columns = rows.headOption.fold(0)(_.length),
      values = rows.flatten
    )

  private def archiveValue[A](result: Either[ArchiveError, A]): A =
    result.fold(error => throw IllegalStateException(error.message), identity)

  private def latentValue[A](result: Either[LatentError, A]): A =
    result.fold(error => throw IllegalStateException(error.message), identity)

  private def radialValue[A](result: Either[RadialBasisError, A]): A =
    result.fold(error => throw IllegalStateException(error.message), identity)

  private def linearValue[A](result: Either[LinAlgError, A]): A =
    result.fold(error => throw IllegalStateException(error.message), identity)

  private def doubleJson(value: Double): String =
    if value == 0.0 then "0.0" else value.toString

  private def quoted(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def golden(
      id: String,
      comparator: MigrationComparator,
      rows: Int,
      columns: Int,
      rawHex: String
  ): MigrationGoldenCase =
    MigrationGoldenCase(id, comparator, rows, columns, rawHex.split("\\s+").toVector)

object ResponseArchiveMigrationBaselineMain:
  def main(args: Array[String]): Unit =
    println("RRA_PHASE0_BASELINE_BEGIN")
    println(ResponseArchiveMigrationBaseline.render())
    println("RRA_PHASE0_BASELINE_END")

class ResponseArchiveMigrationBaselineSuite extends munit.FunSuite:
  test("phase-zero corpus matches the frozen reconstruction baseline"):
    val cases = ResponseArchiveMigrationBaseline.current
    assertEquals(cases.map(_.id), ResponseArchiveMigrationBaseline.ExpectedIds)
    cases.zip(ResponseArchiveMigrationBaseline.Golden).foreach: (actual, golden) =>
      assertEquals(actual.id, golden.id)
      assertEquals(actual.rows, golden.rows)
      assertEquals(actual.columns, golden.columns)
      assertEquals(actual.comparator, golden.comparator)
      actual.comparator match
        case MigrationComparator.RawBits =>
          assertEquals(actual.rawHex, golden.rawHex, actual.id)
        case MigrationComparator.AbsoluteRelative(absolute, relative) =>
          actual.values.zip(golden.rawHex).zipWithIndex.foreach:
            case ((observed, expectedHex), index) =>
              val expected =
                java.lang.Double.longBitsToDouble(java.lang.Long.parseUnsignedLong(expectedHex, 16))
              val tolerance =
                absolute + relative * math.max(math.abs(expected), math.abs(observed))
              assert(
                math.abs(observed - expected) <= tolerance,
                s"${actual.id}[$index]: observed=$observed expected=$expected tolerance=$tolerance"
              )
