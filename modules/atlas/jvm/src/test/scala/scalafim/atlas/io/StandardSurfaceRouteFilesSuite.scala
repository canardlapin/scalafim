package scalafim.atlas.io

import scalafim.atlas.{Fslr32kFrom2009c, StandardRouteRefusal, StandardSurfaceRoutes}
import scalafim.atlas.reference.{Fslr32kRouteExample, PublicFslrRoute}
import scalafim.surface.reference.*

import java.nio.file.{Files, Path}

/** The JVM entry point's refusals without the real assets, the lock against the reference fixtures it replaces,
  * and (when the locked cache is present) the consumer example end to end.
  */
class StandardSurfaceRouteFilesSuite extends munit.FunSuite, RealAssetGate:
  private val lock = Fslr32kFrom2009c

  private def withRoot(populate: Path => Unit)(body: Path => Unit): Unit =
    val root = Files.createTempDirectory("scalafim-standard-route-")
    try
      populate(root)
      body(root)
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder()).forEach(path => Files.deleteIfExists(path))

  private def write(root: Path, relative: String, bytes: Array[Byte]): Unit =
    val path = root.resolve(relative)
    Files.createDirectories(path.getParent)
    Files.write(path, bytes)

  test("an empty cache is refused with every locked path listed; nothing is downloaded"):
    withRoot(_ => ()): root =>
      StandardSurfaceRouteFiles.fsLR32kFrom2009c(roots = Vector(root)) match
        case Left(StandardRouteRefusal.AssetsMissing(paths, searched)) =>
          assertEquals(paths, lock.requiredPaths)
          assertEquals(searched, Vector(root.toString))
        case other => fail(s"expected AssetsMissing, got $other")
      assertEquals(Files.list(root).count(), 0L)

  test("zero-byte TemplateFlow skeleton placeholders count as missing"):
    withRoot(root => lock.requiredPaths.foreach(write(root, _, Array.emptyByteArray))): root =>
      assert(StandardSurfaceRouteFiles.fsLR32kFrom2009c(roots = Vector(root))
        .left.exists(_.isInstanceOf[StandardRouteRefusal.AssetsMissing]))

  test("bytes that differ from the lock are refused by digest before decoding"):
    withRoot(root => lock.requiredPaths.foreach(write(root, _, "not the locked asset".getBytes("UTF-8")))): root =>
      StandardSurfaceRouteFiles.fsLR32kFrom2009c(roots = Vector(root)) match
        case Left(StandardRouteRefusal.AssetRefused(asset, ReferenceError.DigestMismatch(_, declared, _))) =>
          assertEquals(asset, lock.hemispheres.head.midthickness.archivePath)
          assertEquals(declared, lock.hemispheres.head.midthickness.sha256.value)
        case other => fail(s"expected a digest refusal, got $other")

  test("a point map from another transform is not the locked route input"):
    val directory = Path.of(getClass.getResource("/pointmap-synthetic/manifest.json").toURI).getParent
    val synthetic = DeclaredPointMapReader.read(directory, SyntheticItkOracle.sourceSha256, SyntheticItkOracle.manifestSha256)
      .fold(e => fail(e.message), identity)
    StandardSurfaceRoutes.fsLR32kFrom2009c(synthetic, Vector.empty) match
      case Left(StandardRouteRefusal.NotLocked(role, expected, supplied)) =>
        assertEquals(role, "point-map source")
        assertEquals(expected, lock.transform.sha256)
        assertEquals(supplied, SyntheticItkOracle.sourceSha256)
      case other => fail(s"expected NotLocked, got $other")

  test("the public lock is exactly the one the reference fixtures and the inverse oracle were qualified on"):
    assertEquals(lock.catalogRevision.value, RealAssets.catalogRevision)
    assertEquals(lock.transform.sha256, RealAssets.transformSha256)
    assertEquals(lock.pointMapManifestSha256, RealAssets.manifestSha256)
    assertEquals(lock.sourceFrame, RealAssets.nlin2009c)
    assertEquals(lock.anatomyFrame, RealAssets.nlin6)
    assertEquals(lock.anatomyBasis, RealAssets.fslrBasis)
    for h <- lock.hemispheres do
      val label = PublicFslrRoute.label(h.hemisphere)
      assertEquals(h.midthickness.sha256.value, RealAssets.midthicknessSha256(label))
      assertEquals(h.medialWall.sha256.value, RealAssets.nomedialwallSha256(label))
      assertEquals(RealInverseOracle.manifest("hemispheres")(label)("surface")("sha256").str, h.midthickness.sha256.value)
    assertEquals(RealInverseOracle.manifest("transform")("sha256").str, lock.transform.sha256)

  test("the consumer example maps locked GM through the public route and discloses its identity"):
    requireReal(RealAssets.evidencePresent, s"locked assets under ${RealAssets.root}")
    val receipt = Fslr32kRouteExample.real(RealAssets.root)
    assertEquals(receipt("route").str, PublicFslrRoute.route.identity.token)
    val fields = receipt("disclosure").arr.flatMap(_.obj.toVector).map((k, v) => k -> v.str).toVector
    assert(fields.exists((k, v) => k == "anatomyBasis" && v.contains("matches no single anatomy")))
    assert(fields.exists((k, v) => k == "bridgeExactness" && v.startsWith("approximate: pointwise fixed-point inverse")))
    assert(fields.exists((k, v) => k == "policy" && v.startsWith("frozen")))
    assertEquals(fields.count(_._1 == "limit"), 3)
    assertEquals(receipt("inverse")("converged").num.toInt, 32492)
    assertEquals(receipt("coverage")("BridgeUnavailable").num.toInt, 0)
