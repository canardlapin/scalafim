package scalafim.latent.scenarios

import scalafim.archive.lna.TransformParams
import scalafim.image.NeuroSpace
import scalafim.latent.*
import gale.linalg.{DMat, DVec, LinAlgError}

class TransportSelectionScenarioSuite extends munit.FunSuite:
  test("latent transport selection scenario receipt passes") {
    val result = runScenario()
    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
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
      latentValue(
        TransportLatentResponse(
          coefficientsAnalysis = LatentNumerics.matrixFromRows(
            Vector(
              Vector(2.0, 2.0),
              Vector(4.0, -1.0)
            )
          ),
          nativeDecoder = decoder,
          transform = latentValue(CoefficientTransform(toAnalysis, toRaw)),
          offset = Some(DVec.fromSeq(Vector(10.0, 20.0, 30.0, 40.0))),
          label = "scenario-transport",
          metadata = Map("scenario" -> "latent.transport-selection.v1")
        )
      )
    val archive =
      LatentArchiveCodec
        .toTransportArchive(source, NeuroSpace(Vector(2, 2, 1)))
        .fold(err => fail(err.message), identity)
    val decoded =
      LatentArchiveCodec
        .fromArchive(archive)
        .fold(err => fail(err.message), identity) match
        case LatentArchiveResponse.Transport(response) => response
        case other => fail(s"expected transport archive response, found $other")

    val selection =
      LatentSelection(timepoints = Some(Vector(1, 0)), samples = Some(Vector(3, 1)))
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
        Vector(49.0, 19.0),
        Vector(42.0, 22.0)
      )
    val descriptor = archive.manifest.transforms.head
    val descriptorKind =
      descriptor.params match
        case params: TransformParams.Embed => params.metadata.get("lna.response.kind")
        case _ => None

    ScenarioHarness.result(
      "latent.transport-selection.v1",
      Vector(
        ScenarioHarness.fact("archive.variant", LatentArchiveCodec.isTransportArchive(archive), "archive is tagged as transport latent"),
        ScenarioHarness.fact("descriptor.kind", descriptorKind.contains("transport_latent"), s"metadata=${descriptorKind.getOrElse("<missing>")}"),
        ScenarioHarness.fact("label", decoded.label == "scenario-transport", s"actual=${decoded.label}"),
        ScenarioHarness.fact("metadata.scenario", decoded.metadata.get("scenario").contains("latent.transport-selection.v1"), s"metadata=${decoded.metadata}"),
        ScenarioHarness.finite("selected.values", selected.copyData.toIndexedSeq)
      ) ++
        ScenarioHarness.matrix("selected.roundtrip", selected, expected, ScenarioTolerance.absolute(1e-12)) ++
        ScenarioHarness.matrix("selected.source", sourceSelected, expected, ScenarioTolerance.absolute(1e-12))
    )

  private def latentValue[A](result: Either[LatentError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)

  private def mapValue[A](result: Either[LinAlgError, A]): A =
    result match
      case Right(value) => value
      case Left(error)  => fail(error.message)
