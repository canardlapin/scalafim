package scalafim.archive.lna

import scalafim.image.DMat

class SharedBasisSuite extends munit.FunSuite:
  private val mask =
    SharedBasisMask(
      dims = Vector(2, 2, 1),
      values = Vector(true, true, true, true)
    )

  private val loadings =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.5, 0.5),
        Vector(0.0, 1.0),
        Vector(-0.5, 0.25)
      )
    )

  test("shared SHA-256 helper matches the empty digest fixture") {
    assertEquals(
      SharedBasisDigest.sha256Hex(_ => ()),
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
  }

  test("shared basis ids and checksums validate safe values") {
    assertEquals(SharedBasisId("schaefer400_slepian-k8").map(_.value), Right("schaefer400_slepian-k8"))
    assert(SharedBasisId("../bad").isLeft)
    assert(SharedBasisId("bad/name").isLeft)

    val checksum = SharedBasisChecksum.fromHex("a" * 64).fold(err => fail(err.message), identity)
    assertEquals(checksum.value, s"sha256:${"a" * 64}")
    assertEquals(checksum.filename, s"sha256-${"a" * 64}.lna_basis.h5")
    assert(SharedBasisChecksum("sha256:abc").isLeft)
  }

  test("artifact checksums are deterministic and ignore param insertion order") {
    val a =
      SharedBasisArtifact(
        loadings = loadings,
        mask = mask,
        kind = "slepian",
        params = Map("radius" -> "8", "space" -> "MNI")
      )
    val b =
      SharedBasisArtifact(
        loadings = loadings,
        mask = mask,
        kind = "slepian",
        params = Map("space" -> "MNI", "radius" -> "8")
      )
    val changed =
      SharedBasisArtifact(
        loadings = DMat.fromRows(
          Vector(
            Vector(2.0, 0.0),
            Vector(0.5, 0.5),
            Vector(0.0, 1.0),
            Vector(-0.5, 0.25)
          )
        ),
        mask = mask,
        kind = "slepian",
        params = Map("radius" -> "8", "space" -> "MNI")
      )

    assertEquals(a.checksum, b.checksum)
    assertNotEquals(a.checksum, changed.checksum)
    assert(a.contentAddressedFilename.matches("sha256-[0-9a-f]{64}\\.lna_basis\\.h5"))
    assertEquals(a.maskChecksum, b.maskChecksum)
  }

  test("artifact validation catches mask cardinality and non-finite loadings") {
    interceptMessage[IllegalArgumentException]("requirement failed: shared basis active mask count must match loading rows") {
      SharedBasisArtifact(
        loadings = loadings,
        mask = mask.copy(values = Vector(true, false, false, false)),
        kind = "slepian"
      )
    }

    val bad =
      SharedBasisArtifact(
        loadings = DMat.fromRows(
          Vector(
            Vector(1.0, 0.0),
            Vector(0.5, Double.NaN),
            Vector(0.0, 1.0),
            Vector(-0.5, 0.25)
          )
        ),
        mask = mask,
        kind = "slepian"
      )
    assert(SharedBasisArtifact.validateFinite(bad).left.toOption.exists(_.message.contains("non-finite")))
  }

  test("registry codec roundtrips aliases deterministically") {
    val artifact =
      SharedBasisArtifact(
        loadings = loadings,
        mask = mask,
        kind = "slepian",
        params = Map("radius" -> "8")
      )
    val id = SharedBasisId.unsafe("schaefer400_slepian-k8")
    val entry = SharedBasisRegistryEntry.fromArtifact(artifact, created = "2026-07-06T19:30:00Z")
    val registry = SharedBasisRegistry().updated(id, entry)

    val rendered = SharedBasisRegistryCodec.render(registry)
    val parsed =
      SharedBasisRegistryCodec
        .parse(rendered)
        .fold(err => fail(err.message), identity)

    assertEquals(parsed, registry)
    assertEquals(parsed.get(id), Some(entry))
    assertEquals(parsed.aliasesFor(artifact.checksum).map(_.value), Vector("schaefer400_slepian-k8"))
    assert(rendered.indexOf("\"bases\"") < rendered.indexOf("schaefer400_slepian-k8"))
  }
