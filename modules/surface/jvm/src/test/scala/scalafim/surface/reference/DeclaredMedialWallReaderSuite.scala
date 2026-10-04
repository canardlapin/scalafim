package scalafim.surface.reference

import scalafim.examples.reference.FslrRouteExample
import scalafim.surface.*
import java.nio.file.{Files, Path}

class DeclaredMedialWallReaderSuite extends munit.FunSuite:
  private val manifest = Path.of(getClass.getResource("/fslr-route-example/manifest.json").toURI)

  test("mask bytes and declared hemisphere must agree before their receipt enters a route"):
    val result = FslrRouteExample.synthetic(manifest)
    val domain = result.mapped.reference.domain
    val path = manifest.getParent.resolve("cortex.label.gii")
    val bytes = Files.readAllBytes(path)
    val asset = DataAsset.make("mask", AssetSha256.of(bytes).value).toOption.get
    assert(DeclaredMedialWallReader.read(path, domain, asset).isRight)
    val wrongDigest = DataAsset.make("mask", "0" * 64).toOption.get
    assert(DeclaredMedialWallReader.read(path, domain, wrongDigest).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    val other = Files.createTempFile("opposite-mask", ".gii")
    try
      val reversed = new String(bytes, "UTF-8").replace("CortexLeft", "CortexRight").getBytes("UTF-8")
      Files.write(other, reversed)
      val ownDigest = DataAsset.make("opposite-mask", AssetSha256.of(reversed).value).toOption.get
      assert(DeclaredMedialWallReader.read(other, domain, ownDigest).left.exists(_.isInstanceOf[ReferenceError.InvalidMedialWall]))
    finally Files.deleteIfExists(other)
