package scalafim.surface.reference

import scalafim.surface.*
import scalafim.surface.gifti.*
import scalafim.surface.io.GiftiSurfaceReader

import java.nio.file.{Files, Path}
import java.security.MessageDigest

class DeclaredSurfaceReaderSuite extends munit.FunSuite, RealAssetGate:
  private val nlin6 = TemplateFrame.unsafe("MNI152NLin6Asym", "templateflow-24.2.0")
  private val realName = "tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii"
  private val realDigest = "036a8b6c84fa4b581b7ad7b36d99190b57ad6755c9d7ef7adc3e9ffc6448f1af"

  /** TemplateFlow's fsLR 32k left midthickness, if a local copy exists: under
    * `$TEMPLATEFLOW_HOME/tpl-fsLR`, `~/.cache/templateflow/tpl-fsLR`, or the flat
    * directory named by `SCALAFIM_FSLR_ASSET_DIR`.
    */
  private lazy val realAsset: Option[Path] =
    Vector(
      sys.env.get("TEMPLATEFLOW_HOME").map(home => Path.of(home, "tpl-fsLR", realName)),
      sys.props.get("user.home").map(home => Path.of(home, ".cache", "templateflow", "tpl-fsLR", realName)),
      sys.env.get("SCALAFIM_FSLR_ASSET_DIR").map(dir => Path.of(dir, realName))
    ).flatten.find(Files.isRegularFile(_))

  private def requireRealAsset(): Path =
    requireReal(realAsset.nonEmpty, s"$realName (TEMPLATEFLOW_HOME, ~/.cache/templateflow, SCALAFIM_FSLR_ASSET_DIR)")
    realAsset.get

  private def withFile[A](bytes: Array[Byte])(f: Path => A): A =
    val path = Files.createTempFile("scalafim-declared-", ".surf.gii")
    try
      Files.write(path, bytes)
      f(path)
    finally Files.deleteIfExists(path)

  test("a declared GIFTI loads only when its bytes match the declared digest"):
    val bytes = DeclaredGiftiFixture.bytes(DeclaredGiftiFixture.xml())
    val declaration = DeclaredGiftiFixture.declaration(nlin6, bytes)
    withFile(bytes) { path =>
      val surface = DeclaredSurfaceReader.read(path, declaration, Hemisphere.Left, SurfaceKind.Midthickness)
        .fold(e => fail(e.message), s => s)
      assertEquals(surface.frame, nlin6)
      assertEquals(surface.geometry.vertexCount, 3)
      assertEquals(surface.coordinates.primaryStructure, Some(GiftiPrimaryStructure.CortexLeft))
      assert(DeclaredSurfaceReader.read(path, declaration, Hemisphere.Right, SurfaceKind.Midthickness)
        .left.exists(_.isInstanceOf[ReferenceError.DeclarationConflict]))
    }
    val edited = DeclaredGiftiFixture.bytes(DeclaredGiftiFixture.xml().replace("0 1 0</Data>", "0 2 0</Data>"))
    withFile(edited) { path =>
      assert(DeclaredSurfaceReader.read(path, declaration, Hemisphere.Left, SurfaceKind.Midthickness)
        .left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))
    }
    assert(DeclaredSurfaceReader.read(Path.of("/nonexistent/x.surf.gii"), declaration, Hemisphere.Left,
      SurfaceKind.Midthickness).left.exists(_.isInstanceOf[ReferenceError.AssetReadFailure]))

  test("the declared read path exposes coordinate metadata beside the ordinary geometry"):
    withFile(DeclaredGiftiFixture.bytes(DeclaredGiftiFixture.xml())) { path =>
      val declared = GiftiSurfaceReader.readDeclaredEither(path, Hemisphere.Left, SurfaceKind.Midthickness)
        .fold(e => fail(e.message), d => d)
      assertEquals(declared.geometry.vertexCount, GiftiSurfaceReader.read(path, Hemisphere.Left, SurfaceKind.Midthickness).vertexCount)
      assertEquals(declared.coordinates.geometricType, Some(GiftiGeometricType.Anatomical))
      assertEquals(declared.coordinates.coordinateSystems.size, 2)
    }

  test("TemplateFlow fsLR 32k left midthickness declares Talairach, CortexLeft, 32492 vertices"):
    val path = requireRealAsset()
    val bytes = Files.readAllBytes(path)
    val jdk = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
    assertEquals(jdk, realDigest, "local copy is not the pinned asset")
    assertEquals(AssetSha256.of(bytes).value, jdk, "portable SHA-256 agrees with the JDK")

    val declared = GiftiSurfaceReader.readDeclaredEither(path, Hemisphere.Left, SurfaceKind.Midthickness)
      .fold(e => fail(e.message), d => d)
    assertEquals(declared.geometry.vertexCount, 32492)
    assertEquals(declared.coordinates.primaryStructure, Some(GiftiPrimaryStructure.CortexLeft))
    assertEquals(declared.coordinates.secondaryStructure, Some(GiftiSecondaryStructure.MidThickness))
    assertEquals(declared.coordinates.geometricType, Some(GiftiGeometricType.Anatomical))
    assertEquals(declared.coordinates.declaredSpaces,
      Vector((Some(GiftiDeclaredSpace.Talairach), Some(GiftiDeclaredSpace.Talairach))))

    val declaration = FrameDeclaration.make(nlin6,
      FrameBasis.literature("10.1093/cercor/bhr291", "TemplateFlow tpl-fsLR (HCP Pipelines templates; ReferencesAndLinks doi:10.1093/cercor/bhr291) — surfaces in MNI152NLin6Asym per HCP convention; corroborated by FrameEvidence on the 2009c GM probseg")
        .toOption.get,
      AssetProvenance.make(TemplateId.unsafe("fsLR"), s"tpl-fsLR/$realName", "templateflow-local", realDigest).toOption.get
    ).toOption.get
    val surface = DeclaredSurfaceReader.read(path, declaration, Hemisphere.Left, SurfaceKind.Midthickness)
      .fold(e => fail(e.message), s => s)
    assertEquals(surface.frame, nlin6)
    assertEquals(surface.geometry.vertexCount, 32492)
    val reference = CorticalMeshReference.make(StandardCorticalMesh.FsLR32k, surface.geometry, MedialWallMask
      .fromCortexFlags(surface.geometry.meshDomainEither.toOption.get, Vector.fill(32492)(true)).toOption.get).toOption.get
    val anatomy = SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(surface)).fold(e => fail(e.message), a => a)
    assertEquals(anatomy.frame, nlin6)
    assertEquals(anatomy.declarations, Vector(declaration))
