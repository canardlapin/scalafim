package scalafim.surface

class SurfaceKindSuite extends munit.FunSuite:

  // Independent oracle for the canonical saved-identity spelling of every kind.
  // The match has no wildcard: under -Werror a new SurfaceKind case that is not
  // given a canonical spelling here fails compilation of this suite.
  private def canonicalSpelling(kind: SurfaceKind): String =
    kind match
      case SurfaceKind.White => "white"
      case SurfaceKind.Pial => "pial"
      case SurfaceKind.Inflated => "inflated"
      case SurfaceKind.VeryInflated => "veryinflated"
      case SurfaceKind.Sphere => "sphere"
      case SurfaceKind.SmoothWm => "smoothwm"
      case SurfaceKind.Midthickness => "midthickness"
      case SurfaceKind.Custom(value) => value

  private val namedKinds = Vector(
    SurfaceKind.White,
    SurfaceKind.Pial,
    SurfaceKind.Inflated,
    SurfaceKind.VeryInflated,
    SurfaceKind.Sphere,
    SurfaceKind.SmoothWm,
    SurfaceKind.Midthickness
  )

  test("every named kind has its canonical label and round trips through fromString"):
    namedKinds.foreach: kind =>
      assertEquals(kind.label, canonicalSpelling(kind))
      assertEquals(SurfaceKind.fromString(kind.label), kind)
      assertEquals(SurfaceKind.named(kind.label), Some(kind))
    assertEquals(namedKinds.map(_.label).distinct.size, namedKinds.size)

  test("VeryInflated is a distinct display kind whose canonical label is the TemplateFlow spelling"):
    assertEquals(SurfaceKind.VeryInflated.label, "veryinflated")
    assertNotEquals(SurfaceKind.VeryInflated, SurfaceKind.Inflated)
    assertNotEquals(SurfaceKind.fromString("veryinflated"), SurfaceKind.fromString("inflated"))

  test("GIFTI, TemplateFlow and HCP spellings of the very-inflated surface parse to VeryInflated"):
    Vector(
      "VeryInflated", // GIFTI GeometricType
      "veryinflated", // TemplateFlow / BIDS suffix
      "very_inflated", // HCP file-name spelling
      "very-inflated",
      "  VERYINFLATED  "
    ).foreach: spelling =>
      assertEquals(SurfaceKind.fromString(spelling), SurfaceKind.VeryInflated, spelling)

  test("a legacy Custom very-inflated label is restored as the named kind, not a parallel identity"):
    Vector("veryinflated", "very_inflated", "VeryInflated").foreach: legacy =>
      assertEquals(SurfaceKind.fromString(SurfaceKind.Custom(legacy).label), SurfaceKind.VeryInflated)

  test("unnamed kinds remain Custom and preserve their spelling; blank kinds are rejected"):
    assertEquals(SurfaceKind.fromString(" Flat "), SurfaceKind.Custom("Flat"))
    assertEquals(SurfaceKind.named("flat"), None)
    assertEquals(SurfaceKind.named("very"), None)
    intercept[IllegalArgumentException](SurfaceKind.fromString("   "))

  test("file-name inference recognizes TemplateFlow fsLR 32k surface names"):
    val cases = Vector(
      "tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii" -> SurfaceKind.Midthickness,
      "tpl-fsLR_den-32k_hemi-R_inflated.surf.gii" -> SurfaceKind.Inflated,
      "tpl-fsLR_den-32k_hemi-L_veryinflated.surf.gii" -> SurfaceKind.VeryInflated,
      "tpl-fsLR_den-32k_hemi-R_veryinflated.surf.gii" -> SurfaceKind.VeryInflated,
      "tpl-fsLR_den-32k_hemi-L_sphere.surf.gii" -> SurfaceKind.Sphere
    )
    cases.foreach: (name, expected) =>
      assertEquals(SurfaceKind.fromFileName(name), expected, name)

  test("file-name inference reads HCP very_inflated as VeryInflated, never Inflated"):
    val cases = Vector(
      "Q1-Q6_R440.L.very_inflated.32k_fs_LR.surf.gii" -> SurfaceKind.VeryInflated,
      "S1200.R.very_inflated_MSMAll.32k_fs_LR.surf.gii" -> SurfaceKind.VeryInflated,
      "fsaverage.L.very-inflated.164k_fs_L.surf.gii" -> SurfaceKind.VeryInflated,
      "S1200.R.inflated_MSMAll.32k_fs_LR.surf.gii" -> SurfaceKind.Inflated,
      "Q1-Q6_R440.L.midthickness.32k_fs_LR.surf.gii" -> SurfaceKind.Midthickness
    )
    cases.foreach: (name, expected) =>
      assertEquals(SurfaceKind.fromFileName(name), expected, name)

  test("file-name inference keeps FreeSurfer conventions and the Custom fallback"):
    assertEquals(SurfaceKind.fromFileName("lh.inflated"), SurfaceKind.Inflated)
    assertEquals(SurfaceKind.fromFileName("rh.white"), SurfaceKind.White)
    assertEquals(SurfaceKind.fromFileName("sub-01_hemi-R_pial123.asc"), SurfaceKind.Pial)
    assertEquals(SurfaceKind.fromFileName("lh.smoothwm.asc"), SurfaceKind.SmoothWm)
    // "very" only qualifies an immediately following "inflated" token.
    assertEquals(SurfaceKind.fromFileName("lh.very.white"), SurfaceKind.White)
    assertEquals(SurfaceKind.fromFileName("lh.very"), SurfaceKind.Custom("very"))
    assertEquals(SurfaceKind.fromFileName("lh.flat7"), SurfaceKind.Custom("flat7"))
    assertEquals(SurfaceKind.fromFileName(".gii"), SurfaceKind.Custom("surface"))
