package scalafim.latent

import scalafim.archive.lna.{SharedBasisArtifact, SharedBasisId, SharedBasisMask}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}
import gale.linalg.DMat

class LatentEncoderSuite extends munit.FunSuite:
  test("temporal DCT specs dispatch through the direct encoder and preserve archive kind") {
    val data =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0, 3.0),
          Vector(2.0, 3.0, 5.0),
          Vector(3.0, 5.0, 8.0),
          Vector(5.0, 8.0, 13.0)
        )
      )
    val dctSpec =
      DctSpec(data.rows, data.rows, DctNorm.Ortho).fold(err => fail(err.message), identity)
    val spec =
      LatentEncodingSpec.dctSpec(
        spec = dctSpec,
        center = true,
        metadata = Map("subject" -> "sub-01")
      ).fold(err => fail(err.message), identity)
    assertEquals(spec.typedMetadata.get("subject"), Some("sub-01"))

    val result =
      LatentEncoder.encode(data, spec).fold(err => fail(err.message), identity)
    val direct =
      TemporalBasisEncoder
        .encodeDctSpec(data, dctSpec, center = true, metadata = Map("subject" -> "sub-01"))
        .fold(err => fail(err.message), identity)

    result match
      case LatentEncodingResult.TemporalDct(response, returnedSpec, center, ridge) =>
        assertEquals(returnedSpec, dctSpec)
        assertEquals(center, true)
        assertEquals(ridge.value, 0.0)
        assertRowsEqual(response.basis.toRows, direct.basis.toRows, 1e-12)
        assertRowsEqual(response.loadings.toRows, direct.loadings.toRows, 1e-12)
        assertEquals(response.metadata("family"), "time_dct")
        assertEquals(response.metadata("subject"), "sub-01")
        assert(response.decodeSemantics.coefficientDecodeIsLinearOnly)
        assert(response.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))
      case other =>
        fail(s"expected temporal DCT result, found $other")

    val archive =
      LatentEncoder
        .toArchive(data, NeuroSpace(Vector(3, 1, 1)), spec)
        .fold(err => fail(err.message), identity)
    val plan =
      LegacyLatentArchiveCodec
        .openPlan(archive)
        .fold(err => fail(err.message), identity)
    assertEquals(plan.kind, LatentArchiveKind.TemporalDct)
    val decoded =
      LegacyLatentArchiveCodec.fromArchive(archive).fold(err => fail(err.message), identity)

    decoded match
      case LatentArchiveResponse.TemporalDct(response, returnedSpec, center, _) =>
        assertEquals(returnedSpec.components, data.rows)
        assertEquals(center, true)
        assertRowsEqual(response.reconstruct().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-10)
      case other =>
        fail(s"expected temporal DCT archive variant, found $other")
  }

  test("temporal Haar specs dispatch through the direct encoder and archive explicitly") {
    val data =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0, 3.0),
          Vector(2.0, 3.0, 5.0),
          Vector(3.0, 5.0, 8.0),
          Vector(5.0, 8.0, 13.0)
        )
      )
    val haarSpec =
      HaarSpec(data.rows, data.rows).fold(err => fail(err.message), identity)
    val spec =
      LatentEncodingSpec.haarSpec(
        spec = haarSpec,
        center = true,
        metadata = Map("subject" -> "sub-01")
      ).fold(err => fail(err.message), identity)
    assertEquals(spec.typedMetadata.get("subject"), Some("sub-01"))

    val result =
      LatentEncoder.encode(data, spec).fold(err => fail(err.message), identity)
    val direct =
      TemporalBasisEncoder
        .encodeHaarSpec(data, haarSpec, center = true, metadata = Map("subject" -> "sub-01"))
        .fold(err => fail(err.message), identity)

    result match
      case LatentEncodingResult.TemporalHaar(response, returnedSpec, center, ridge) =>
        assertEquals(returnedSpec.timepoints, haarSpec.timepoints)
        assertEquals(returnedSpec.components, haarSpec.components)
        assertEquals(center, true)
        assertEquals(ridge.value, 0.0)
        assertRowsEqual(response.basis.toRows, direct.basis.toRows, 1e-12)
        assertRowsEqual(response.loadings.toRows, direct.loadings.toRows, 1e-12)
        assertEquals(response.metadata("family"), "time_haar")
        assertEquals(response.metadata("subject"), "sub-01")
        assert(response.decodeSemantics.coefficientDecodeIsLinearOnly)
        assert(response.decodeSemantics.reconstructionIncludes(LatentMaterializationTerm.SampleOffset))
      case other =>
        fail(s"expected temporal Haar result, found $other")

    val archive =
      LatentEncoder
        .toArchive(data, NeuroSpace(Vector(3, 1, 1)), spec)
        .fold(err => fail(err.message), identity)
    val plan =
      LegacyLatentArchiveCodec
        .openPlan(archive)
        .fold(err => fail(err.message), identity)
    assertEquals(plan.kind, LatentArchiveKind.TemporalHaar)
    plan match
      case LatentArchivePlan.TemporalHaar(_, descriptor, _, returnedSpec, center, ridge) =>
        assertEquals(descriptor.kind, LatentArchiveKind.TemporalHaar)
        assertEquals(returnedSpec.timepoints, haarSpec.timepoints)
        assertEquals(returnedSpec.components, haarSpec.components)
        assertEquals(center, true)
        assertEquals(ridge.value, 0.0)
      case other =>
        fail(s"expected temporal Haar archive plan, found $other")
    val decoded =
      LegacyLatentArchiveCodec.fromArchive(archive).fold(err => fail(err.message), identity)

    decoded match
      case LatentArchiveResponse.TemporalHaar(response, returnedSpec, center, ridge) =>
        assertEquals(returnedSpec.timepoints, haarSpec.timepoints)
        assertEquals(returnedSpec.components, haarSpec.components)
        assertEquals(center, true)
        assertEquals(ridge.value, 0.0)
        assertEquals(response.metadata("basis"), "haar")
        assertEquals(response.metadata("family"), "time_haar")
        assertRowsEqual(response.reconstruct().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-10)
      case other =>
        fail(s"expected temporal Haar archive variant, found $other")
  }

  test("latent typed shape metadata and selections preserve intent") {
    val shape =
      LatentShape
        .checked(timepoints = 4, samples = 3, coefficients = 2)
        .fold(err => fail(err.message), identity)
    assertEquals(shape.timepointCount.value, 4)
    assertEquals(shape.sampleCount.value, 3)
    assertEquals(shape.coefficientCount.value, 2)
    assert(LatentShape.checked(timepoints = 0, samples = 3, coefficients = 2).isLeft)

    assertEquals(LatentMetadata(Map("subject" -> "sub-01")).map(_.get("subject")), Right(Some("sub-01")))
    assert(LatentMetadata(Map("" -> "bad")).isLeft)
    assertEquals(LatentLabel(" analysis ").map(_.value), Right("analysis"))
    assertEquals(LatentLabel.optional(" ").map(_.value), Right(""))
    assert(LatentLabel("").isLeft)

    val annotation =
      LatentAnnotation(" encoded ", Map("subject" -> "sub-01"))
        .fold(err => fail(err.message), identity)
    assertEquals(annotation.labelValue, "encoded")
    assertEquals(annotation.metadata.get("subject"), Some("sub-01"))
    assert(LatentAnnotation("encoded", Map(" " -> "bad")).isLeft)
    assert(LatentEncodingSpec.providedBasis(DMat.eye(2), metadata = Map(" " -> "bad")).isLeft)

    val dctSpec =
      DctSpec(timepoints = 2, components = 2).fold(err => fail(err.message), identity)
    assert(LatentEncodingSpec.dctSpec(dctSpec, metadata = Map("" -> "bad")).isLeft)

    val typedSelection =
      TypedLatentSelection
        .checked(timepoints = Some(Vector(2, 0)), samples = Some(Vector(1)))
        .fold(err => fail(err.message), identity)
    val resolved =
      typedSelection
        .toLatentSelection
        .resolve(timepointCount = 3, sampleCount = 2)
        .fold(err => fail(err.message), identity)

    assertEquals(resolved.timepoints, Vector(2, 0))
    assertEquals(resolved.samples, Vector(1))
    assertEquals(LatentSelection.fromTyped(TypedLatentSelection.All), LatentSelection.All)
    assert(TypedLatentSelection.checked(timepoints = Some(Vector(-1))).isLeft)
  }

  test("provided temporal basis specs use Gram-solve projection") {
    val basis =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 1.0)
        )
      )
    val expectedLoadings =
      Vector(
        Vector(2.0, -1.0),
        Vector(0.5, 3.0)
      )
    val data = temporalDataFrom(basis, expectedLoadings)
    val spec =
      LatentEncodingSpec
        .providedBasis(basis, center = false, label = "provided-demo")
        .fold(err => fail(err.message), identity)

    val response =
      LatentEncoder.encodeResponse(data, spec).fold(err => fail(err.message), identity)

    assertEquals(response.label, "provided-demo")
    assertRowsEqual(response.loadings.toRows, expectedLoadings, 1e-12)
    assert(!rowsClose(response.loadings.toRows, rawTemporalProjection(data, basis), 1e-8))

    val archive =
      LatentEncoder
        .toArchive(data, NeuroSpace(Vector(2, 1, 1)), spec)
        .fold(err => fail(err.message), identity)
    LegacyLatentArchiveCodec.fromArchive(archive).fold(err => fail(err.message), identity) match
      case LatentArchiveResponse.Explicit(decoded) =>
        assertRowsEqual(decoded.reconstruct().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-12)
      case other =>
        fail(s"expected explicit archive variant, found $other")
  }

  test("shared spatial basis specs match direct shared-basis encoder and archive helpers") {
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
        kind = "nonorthogonal",
        params = Map("source" -> "encoder-suite")
      )
    val basisId = SharedBasisId.unsafe("encoder_suite_basis")
    val coefficients =
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, -1.0),
        Vector(-2.0, 0.5)
      )
    val data = spatialDataFrom(loadings, coefficients, Vector(10.0, -2.0, 5.0))
    val spec =
      LatentEncodingSpec
        .sharedBasis(artifact, basisId, center = true, metadata = Map("subject" -> "sub-01"))
        .fold(err => fail(err.message), identity)

    val result =
      LatentEncoder.encode(data, spec).fold(err => fail(err.message), identity)
    val direct =
      SharedBasisEncoder
        .encode(data, artifact, basisId, center = true, metadata = Map("subject" -> "sub-01"))
        .fold(err => fail(err.message), identity)

    result match
      case LatentEncodingResult.SharedBasis(encoding) =>
        assertRowsEqual(encoding.coefficients.toRows, direct.coefficients.toRows, 1e-12)
        assertEquals(encoding.offset.map(_.toVector), direct.offset.map(_.toVector))
        assertEquals(encoding.response.metadata("subject"), "sub-01")
      case other =>
        fail(s"expected shared-basis result, found $other")

    val archive =
      LatentEncoder
        .toArchive(data, NeuroSpace(Vector(3, 1, 1)), spec)
        .fold(err => fail(err.message), identity)
    LegacyLatentArchiveCodec.fromArchive(archive).fold(err => fail(err.message), identity) match
      case LatentArchiveResponse.SharedBasis(decoded) =>
        assertEquals(decoded.basis.basisId, basisId)
        assertRowsEqual(decoded.coefficients.toRows, direct.coefficients.toRows, 1e-12)
        assertEquals(decoded.offset.map(_.toVector), direct.offset.map(_.toVector))
      case other =>
        fail(s"expected shared-basis archive variant, found $other")
  }

  test("radial spatial basis specs dispatch through shared-basis encoding and archives") {
    val space = NeuroSpace(Vector(3, 1, 1))
    val radial =
      RadialBasis
        .fromSpaceIndices(
          atoms = Vector(
            RadialAtom(WorldCoordinate3D.unsafe(0.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0)),
            RadialAtom(WorldCoordinate3D.unsafe(2.0, 0.0, 0.0), RadialSigmaMm.unsafe(1.0))
          ),
          space = space,
          activeIndices = Vector(0, 1, 2),
          kernel = RadialKernel.Gaussian
        )
        .fold(err => fail(err.message), identity)
    val basisId = SharedBasisId.unsafe("encoder_suite_hrbf")
    val coefficients =
      LatentNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(-0.5, 0.25),
          Vector(3.0, -1.0)
        )
      )
    val data = LatentNumerics.multiply(coefficients, radial.loadings.transpose)
    val spec =
      LatentEncodingSpec
        .radialBasis(
          radialBasis = radial,
          maskDims = space.spatialDims,
          basisId = basisId,
          center = false,
          metadata = Map("subject" -> "sub-hrbf"),
          artifactParams = Map("source" -> "encoder-suite")
        )
        .fold(err => fail(err.message), identity)
    val direct =
      RadialBasisEncoder
        .encode(
          data = data,
          radialBasis = radial,
          maskDims = space.spatialDims,
          basisId = basisId,
          center = false,
          metadata = Map("subject" -> "sub-hrbf"),
          artifactParams = Map("source" -> "encoder-suite")
        )
        .fold(err => fail(err.message), identity)

    LatentEncoder.encode(data, spec).fold(err => fail(err.message), identity) match
      case LatentEncodingResult.RadialBasis(encoding) =>
        assertRowsEqual(encoding.coefficients.toRows, direct.coefficients.toRows, 1e-12)
        assertEquals(encoding.artifact.kind, "hrbf")
        assertEquals(encoding.artifact.params("source"), "encoder-suite")
        assertEquals(encoding.response.metadata("radial.kernel"), "gaussian")
        assertEquals(encoding.response.metadata("subject"), "sub-hrbf")
      case other =>
        fail(s"expected radial-basis result, found $other")

    val archive =
      LatentEncoder
        .toArchive(data, space, spec)
        .fold(err => fail(err.message), identity)
    val plan =
      LegacyLatentArchiveCodec
        .openPlan(archive)
        .fold(err => fail(err.message), identity)
    assertEquals(plan.kind, LatentArchiveKind.SharedBasis)

    LegacyLatentArchiveCodec.fromArchive(archive).fold(err => fail(err.message), identity) match
      case LatentArchiveResponse.SharedBasis(decoded) =>
        assertEquals(decoded.basis.basisId, basisId)
        assertEquals(decoded.basis.checksum, direct.artifact.checksum)
        assertRowsEqual(decoded.coefficients.toRows, direct.coefficients.toRows, 1e-12)
        assertEquals(decoded.metadata("radial.kernel"), "gaussian")
        val materialized =
          decoded
            .materialize(direct.artifact, Some(space))
            .fold(err => fail(err.message), identity)
        assertRowsEqual(materialized.reconstruct().fold(err => fail(err.message), identity).toRows, data.toRows, 1e-12)
      case other =>
        fail(s"expected shared-basis archive variant, found $other")
  }

  private def temporalDataFrom(
      basis: DMat,
      loadings: Vector[Vector[Double]]
  ): DMat =
    LatentNumerics.matrixFromRows(
      Vector.tabulate(basis.rows) { time =>
        Vector.tabulate(loadings.length) { sample =>
          var sum = 0.0
          var component = 0
          while component < basis.cols do
            sum += basis(time, component) * loadings(sample)(component)
            component += 1
          sum
        }
      }
    )

  private def spatialDataFrom(
      loadings: ImageDMat,
      coefficients: Vector[Vector[Double]],
      offset: Vector[Double]
  ): DMat =
    LatentNumerics.matrixFromRows(
      coefficients.map { row =>
        Vector.tabulate(loadings.rows) { voxel =>
          var sum = offset(voxel)
          var atom = 0
          while atom < loadings.cols do
            sum += row(atom) * loadings(voxel, atom)
            atom += 1
          sum
        }
      }
    )

  private def rawTemporalProjection(
      data: DMat,
      basis: DMat
  ): Vector[Vector[Double]] =
    Vector.tabulate(data.cols) { sample =>
      Vector.tabulate(basis.cols) { component =>
        var sum = 0.0
        var time = 0
        while time < data.rows do
          sum += data(time, sample) * basis(time, component)
          time += 1
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
