package scalafim.surface.reference

import scalafim.surface.*
import scalafim.surface.gifti.*

class FrameDeclarationSuite extends munit.FunSuite:
  private val fsLR = TemplateId.unsafe("fsLR")
  private val nlin6 = TemplateFrame.unsafe("MNI152NLin6Asym", "templateflow-24.2.0")
  private val nlin2009c = TemplateFrame.unsafe("MNI152NLin2009cAsym", "templateflow-24.2.0")
  private val digest = "036a8b6c84fa4b581b7ad7b36d99190b57ad6755c9d7ef7adc3e9ffc6448f1af"
  private val path = "tpl-fsLR/tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii"
  private val asset = AssetProvenance.make(fsLR, path, "templateflow-24.2.0", digest).toOption.get
  private val conte69 = FrameBasis.literature("10.1093/cercor/bhr291",
    "Conte69 surfaces registered to FSL MNI152 nonlinear 6th generation").toOption.get

  private val geometry = SurfaceGeometry(
    TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0)), Vector((0, 1, 2))),
    Hemisphere.Left, SurfaceKind.Midthickness)

  private def coordinates(xml: String): GiftiCoordinateDeclaration =
    GiftiXmlParser.parseString(xml).flatMap(GiftiCoordinateDeclaration.fromDocument).fold(e => fail(e.message), c => c)

  test("portable SHA-256 matches the FIPS 180-2 vectors on this platform"):
    assertEquals(AssetSha256.of(Array.emptyByteArray).value,
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    assertEquals(AssetSha256.of("abc".getBytes("UTF-8")).value,
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")

  test("asset provenance requires 64 lowercase hex, a tpl-<template>/ path and a revision"):
    assertEquals(asset.sha256.value, digest)
    assert(AssetProvenance.make(fsLR, path, "r", digest.toUpperCase).isLeft, "uppercase hex")
    assert(AssetProvenance.make(fsLR, path, "r", digest.drop(1)).isLeft, "63 digits")
    assert(AssetProvenance.make(fsLR, path, "r", digest.updated(0, 'g')).isLeft, "non-hex")
    assert(AssetProvenance.make(fsLR, path, " ", digest).isLeft, "blank revision")
    assert(AssetProvenance.make(fsLR, path, " r", digest).isLeft, "untrimmed revision")
    for bad <- Vector("", "tpl-fsLR/", "tpl-MNI152NLin6Asym/x.nii.gz", "fsLR/x.gii", "/tpl-fsLR/x.gii",
        "tpl-fsLR/../tpl-MNI152NLin6Asym/x.gii", "tpl-fsLR//x.gii", "tpl-fsLR\\x.gii", " tpl-fsLR/x.gii") do
      assert(AssetProvenance.make(fsLR, bad, "r", digest).isLeft, s"archive path '$bad'")

  test("frame bases are validated; a derivation cannot consume its own asset"):
    assert(FrameBasis.literature("doi:10.1093/cercor/bhr291", "x").isLeft)
    assert(FrameBasis.literature("10.1093/cercor/bhr291", " ").isLeft)
    assert(FrameBasis.literature("10.12/x", "x").isLeft)
    assert(FrameBasis.derived(" ", Vector(asset)).isLeft)
    assert(FrameBasis.derived("recipe", Vector.empty).isLeft)
    val circular = FrameBasis.derived("recipe", Vector(asset)).toOption.get
    assert(FrameDeclaration.make(nlin6, circular, asset).isLeft)
    val declaration = FrameDeclaration.make(nlin6, conte69, asset).toOption.get
    assertEquals(declaration.frame, nlin6)
    assert(declaration.display.contains("10.1093/cercor/bhr291"))

  test("digest binding refuses bytes that are not the declared asset"):
    val xml = DeclaredGiftiFixture.xml()
    val bytes = DeclaredGiftiFixture.bytes(xml)
    val declaration = DeclaredGiftiFixture.declaration(nlin6, bytes)
    val decoded = DeclaredGiftiSurface(geometry, coordinates(xml))
    val surface = DeclaredSurface.verified(declaration, bytes, decoded).fold(e => fail(e.message), s => s)
    assertEquals(surface.frame, nlin6)
    assertEquals(surface.coordinates.declaredSpaces,
      Vector((Some(GiftiDeclaredSpace.Talairach), Some(GiftiDeclaredSpace.Talairach))))
    val tampered = bytes.clone()
    tampered(tampered.length - 2) = ' '.toByte
    assert(DeclaredSurface.verified(declaration, tampered, decoded).left.exists(_.isInstanceOf[ReferenceError.DigestMismatch]))

  test("a file's own hemisphere and anatomical type must not contradict the requested reading"):
    val xml = DeclaredGiftiFixture.xml()
    val bytes = DeclaredGiftiFixture.bytes(xml)
    val declaration = DeclaredGiftiFixture.declaration(nlin6, bytes)
    def readAs(hemisphere: Hemisphere, kind: SurfaceKind, source: String = xml) =
      DeclaredSurface.verified(declaration, bytes, DeclaredGiftiSurface(
        SurfaceGeometry(geometry.mesh, hemisphere, kind), coordinates(source)))
    assert(readAs(Hemisphere.Left, SurfaceKind.Midthickness).isRight)
    assert(readAs(Hemisphere.Right, SurfaceKind.Midthickness).left.exists(_.isInstanceOf[ReferenceError.DeclarationConflict]))
    assert(readAs(Hemisphere.Left, SurfaceKind.Pial).left.exists(_.isInstanceOf[ReferenceError.DeclarationConflict]))
    assert(readAs(Hemisphere.Left, SurfaceKind.Inflated).left.exists(_.isInstanceOf[ReferenceError.DeclarationConflict]))
    // An inflated file may keep MidThickness as its secondary structure.
    assert(readAs(Hemisphere.Left, SurfaceKind.Inflated, DeclaredGiftiFixture.xml(geometricType = "Inflated")).isRight)
    assert(readAs(Hemisphere.Left, SurfaceKind.Pial, DeclaredGiftiFixture.xml(secondary = "Pial")).isRight)

  test("the anatomy frame comes only from declarations, which must agree"):
    val white = SurfaceGeometry(geometry.mesh, Hemisphere.Left, SurfaceKind.White)
    val pial = SurfaceGeometry(geometry.mesh, Hemisphere.Left, SurfaceKind.Pial)
    val mesh = StandardCorticalMesh.declare(CorticalMeshFamily.FsLR, "test3", 3).toOption.get
    val reference = CorticalMeshReference.make(mesh, white, MedialWallMask.fromCortexFlags(
      white.meshDomainEither.toOption.get, Vector(true, true, true)).toOption.get).toOption.get
    def on(g: SurfaceGeometry, frame: TemplateFrame) =
      DeclaredSurface.unsafeAssumeVerified(FrameDeclaration.make(frame, conte69, asset).toOption.get, g)
    val agreed = SamplingAnatomy.make(reference, AnatomicalGeometry.WhitePial(on(white, nlin6), on(pial, nlin6)))
    assertEquals(agreed.map(_.frame), Right(nlin6))
    assertEquals(agreed.map(_.declarations.map(_.frame)), Right(Vector(nlin6, nlin6)))
    assert(SamplingAnatomy.make(reference, AnatomicalGeometry.WhitePial(on(white, nlin6), on(pial, nlin2009c)))
      .left.exists(_.isInstanceOf[ReferenceError.InvalidAnatomy]))
