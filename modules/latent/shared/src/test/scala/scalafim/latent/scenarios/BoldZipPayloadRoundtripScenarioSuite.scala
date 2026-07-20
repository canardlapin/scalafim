package scalafim.latent.scenarios

import scalafim.archive.lna.{DatasetRole, TransformParams}
import scalafim.image.NeuroSpace
import scalafim.latent.*
import scalafim.linalg.{DoubleMatrix, DoubleVector}

class BoldZipPayloadRoundtripScenarioSuite extends munit.FunSuite:
  test("latent BOLDZip payload roundtrip scenario receipt passes") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val spatialBasis =
      latentValue(
        BoldZipSpatialBasis(
          sampleCount = 3,
          coarse = BoldZipCoarseBasis.MatrixBasis(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))),
          detail = BoldZipDetailBasis.IdentitySamples,
          label = "scenario-detail"
        )
      )
    val source =
      latentValue(
        BoldZipPayload(
          temporalBasis = DoubleMatrix.eye(4),
          carrierTheta = DoubleMatrix.fromRows(
            Vector(
              Vector(1.0, 2.0, 3.0, 4.0),
              Vector(10.0, 20.0, 30.0, 40.0)
            )
          ),
          carrierLoadings = DoubleMatrix.fromRows(Vector(Vector(2.0, 1.0))),
          spatialBasis = spatialBasis,
          texture = Vector(
            BoldZipTextureEntry.unsafe(atom = 0, carrier = 0, amplitude = 0.5, lag = 0),
            BoldZipTextureEntry.unsafe(atom = 1, carrier = 1, amplitude = 1.0, lag = 1)
          ),
          events = Vector(BoldZipResidualEvent.unsafe(atom = 2, frame = 2, amplitude = 3.0)),
          offset = Some(DoubleVector.fromSeq(Vector(10.0, 20.0, 30.0))),
          label = "scenario-boldzip",
          metadata = Map("scenario" -> "latent.boldzip-payload-roundtrip.v1")
        )
      )
    val archive =
      LatentArchiveCodec
        .toBoldZipArchive(source, NeuroSpace(Vector(3, 1, 1)))
        .fold(err => fail(err.message), identity)
    val decoded =
      LatentArchiveCodec
        .fromArchive(archive)
        .fold(err => fail(err.message), identity) match
        case LatentArchiveResponse.BoldZip(response) => response
        case other => fail(s"expected BOLDZip archive response, found $other")

    val selection =
      LatentSelection(timepoints = Some(Vector(2, 0)), samples = Some(Vector(2, 0)))
    val selected =
      decoded
        .reconstruct(selection)
        .fold(err => fail(err.message), identity)
    val sourceSelected =
      source
        .reconstruct(selection)
        .fold(err => fail(err.message), identity)
    val expected =
      Vector(
        Vector(69.0, 47.5),
        Vector(42.0, 22.5)
      )
    val descriptor = archive.manifest.transforms.head
    val descriptorKind =
      descriptor.params match
        case params: TransformParams.Custom => params.metadata.get("lna.response.kind")
        case _ => None

    ScenarioHarness.result(
      "latent.boldzip-payload-roundtrip.v1",
      Vector(
        ScenarioHarness.fact("archive.variant", LatentArchiveCodec.isBoldZipArchive(archive), "archive is tagged as BOLDZip-SR"),
        ScenarioHarness.fact("descriptor.kind", descriptorKind.contains("boldzip_sr"), s"metadata=${descriptorKind.getOrElse("<missing>")}"),
        ScenarioHarness.fact(
          "descriptor.texture",
          descriptor.datasets.exists(_.role == DatasetRole.Other("boldzip_texture_index")),
          s"roles=${descriptor.datasets.map(_.role.value).mkString(",")}"
        ),
        ScenarioHarness.fact("label", decoded.label == "scenario-boldzip", s"actual=${decoded.label}"),
        ScenarioHarness.fact("metadata.scenario", decoded.metadata.get("scenario").contains("latent.boldzip-payload-roundtrip.v1"), s"metadata=${decoded.metadata}"),
        ScenarioHarness.fact("texture.count", decoded.texture.length == 2, s"actual=${decoded.texture.length} expected=2"),
        ScenarioHarness.fact("event.count", decoded.events.length == 1, s"actual=${decoded.events.length} expected=1"),
        ScenarioHarness.finite("selected.values", selected.dataArray.toIndexedSeq)
      ) ++
        ScenarioHarness.matrix("selected.roundtrip", selected, expected, ScenarioTolerance.absolute(1e-12)) ++
        ScenarioHarness.matrix("selected.source", sourceSelected, expected, ScenarioTolerance.absolute(1e-12))
    )

  private def latentValue[A](result: Either[LatentError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)
