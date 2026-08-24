package scalafim.archive.lna

import scalafim.archive.{
  ArchivePath,
  CanonicalArchiveManifestCodec,
  CanonicalValue,
  RepresentationMetadata,
  RunLabel
}
import scalafim.image.{SampleSpaces, SomeSampleSpace}

class LnaArchiveManifestAdapterSuite extends munit.FunSuite:
  test("LNA manifest adaptation is pure namespaced and canonically readable"):
    val manifest = fixtureManifest()
    val parsedFixture =
      LnaManifestCodec
        .parse(LnaManifestCodec.render(manifest))
        .fold(error => fail(error.message), identity)
    val first =
      LnaArchiveManifestAdapter
        .adapt(parsedFixture)
        .fold(error => fail(error.message), identity)
    val second =
      LnaArchiveManifestAdapter
        .adapt(parsedFixture)
        .fold(error => fail(error.message), identity)

    assertEquals(first, second)
    assertEquals(first.format.value, "lna-hdf5@2")
    assertEquals(first.key.objectType.value, "org.scalafim/fmri-response")
    assertEquals(first.key.schemaMajor, 2)
    assertEquals(
      first.representation.map(_.key.value),
      Some("org.scalafim/lna-pipeline@2")
    )
    assertEquals(
      first.payloads.map(_.role.value),
      Vector("org.scalafim.lna/raw-data")
    )

    val newFixture =
      CanonicalArchiveManifestCodec
        .parse(CanonicalArchiveManifestCodec.render(first))
        .fold(error => fail(error.message), identity)
    assertEquals(newFixture, first)

  test("temporal DCT metadata becomes a narrow descriptor envelope"):
    val manifest =
      fixtureManifest().copy(
        header = Map(
          RepresentationMetadata.DescriptorHeader -> "dct-model-v1",
          RepresentationMetadata.OutputSchemaHeader -> "response-schema-v1"
        )
      )
    val translated =
      LnaArchiveManifestAdapter
        .adapt(manifest)
        .fold(error => fail(error.message), identity)

    translated.representation match
      case Some(value) =>
        assertEquals(
          value.key.value,
          "org.scalafim/temporal-dct@1"
        )
        assertEquals(
          value.descriptor,
          CanonicalValue.string("dct-model-v1")
        )
        assertEquals(
          value.outputSchema,
          CanonicalValue.string("response-schema-v1")
        )
      case None =>
        fail("expected translated representation envelope")

  private def fixtureManifest(): LnaManifest =
    val path = ArchivePath("/runs/0/raw")
    LnaManifest(
      creator = "lna-fixture",
      requiredTransforms = Vector.empty,
      transforms = Vector.empty,
      runs = Vector(
        LnaRun(
          RunLabel.indexed(0),
          LnaShape(SampleSpaces(Vector(1, 1, 1)), 2),
          path
        )
      ),
      datasets = Vector(
        DatasetRef(
          path,
          DatasetRole.RawData,
          Vector(2, 1),
          Some(LnaDType.Float64)
        )
      )
    )
