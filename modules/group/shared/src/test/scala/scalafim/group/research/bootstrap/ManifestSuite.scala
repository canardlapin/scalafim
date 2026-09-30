package scalafim.group.research.bootstrap

class ManifestSuite extends munit.FunSuite:
  /** SHA-256 of the canonical serialization, frozen with tools/group-bootstrap-research/cells.json. */
  val FrozenSha256 = "76e6785bf77a6971d542a7de0601d6b0ca9b3c12d791a2a5b26756154f03bc73"

  test("SHA-256 matches the FIPS 180-4 test vectors"):
    assertEquals(Sha256.hex(""), "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
    assertEquals(Sha256.hex("abc"), "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    assertEquals(
      Sha256.hex("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
      "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
    )
    assertEquals(Sha256.hex("a" * 1000), "41edece42d63e8d9bf515a9ba6932e1c20cbc9f5a5d134645adb5db1b9737ea3")

  test("the declared families have 90 + 24 + 3 = 117 cells with unique, parseable, immutable IDs"):
    assertEquals(Cell.core.length, 90)
    assertEquals(Cell.stress.length, 24)
    assertEquals(Cell.hierarchy.length, 3)
    val ids = Cell.all.map(_.id.value)
    assertEquals(ids.distinct.length, 117)
    ids.foreach(id => assertEquals(CellId.parse(id).map(_.value), Right(id)))
    assertEquals(Cell.all.map(_.id.value), ids, "IDs are pure functions of the cell fields")

  test("the six fixed confirmation cells and the four power cells are core cells"):
    CellManifest.FixedConfirmation.foreach(id => assert(Cell.core.exists(_.id == id), s"${id.value} missing"))
    assertEquals(CellManifest.PowerCells.map(_.value), Vector("C-n8-DG-Vrev-T2-N8", "C-n20-DI-Vspread-T0-N8", "C-n20-DG-Vspread-T2-N40", "C-n80-DI-Vspread-T2-N8"))

  test("variance patterns follow §5: flat .52, spread .04 * 25^((i-1)/(n-1)), reversed on group and high z"):
    val spread = ResearchTestSupport.cell("C-n20-DI-Vspread-T0-N8").sigma2.get
    assertEqualsDouble(spread.head, 0.04, 1e-15)
    assertEqualsDouble(spread.last, 1.0, 1e-14)
    assert(spread.sliding(2).forall(w => w(0) < w(1)))
    val rev = ResearchTestSupport.cell("C-n20-DG-Vrev-T0-N8")
    val s = rev.sigma2.get
    assertEquals(s.sorted.toVector, spread.sorted.toVector)
    val group = (0 until rev.quarter).map(s)
    val rest = (rev.quarter until rev.n).map(s)
    assert(group.min > rest.max, "the quarter group gets the largest variances")
    assert(rest.sliding(2).forall(w => w(0) < w(1)), "outside the group sigma^2 grows with z")
    assertEquals(ResearchTestSupport.cell("C-n8-DI-Vflat-T0-Ninf").sigma2.get.toVector, Vector.fill(8)(0.52))
    assert(Cell.of(Family.Core, None, 8, DesignKind.Intercept, VariancePattern.Reversed, Tau2Level.Zero, NuLevel.Infinite).isLeft)

  test("the power alternative gives the oracle z a power of about one half"):
    Cell.core.foreach { c =>
      val delta = c.powerDelta.get
      val d = c.researchDesign
      val sigma2 = c.sigma2.get.map(_ + c.tau2.value)
      val fitter = new StudyFitter(d)
      assert(fitter.fitFull(new Array[Double](c.n), sigma2, TauPolicy.Fixed(0.0)).ok)
      assertEqualsDouble(delta / math.sqrt(fitter.contrastVariance), 1.96, 1e-12, c.id.value)
    }
    assert(Cell.stress.forall(_.powerDelta.isEmpty) && Cell.hierarchy.forall(_.powerDelta.isEmpty))

  test("the manifest serialization is canonical and its SHA-256 is frozen"):
    val text = CellManifest.canonical
    assert(text.startsWith("{\"schema\":\"scalafim-group-bootstrap-cells/v1\""))
    assertEquals(text, CellManifest.json.render + "\n")
    assert(!text.contains("NaN") && !text.contains("Infinity"))
    println(s"BOOTSTRAP_MANIFEST_SHA256,${CellManifest.sha256},bytes=${text.length}")
    assertEquals(CellManifest.sha256, FrozenSha256)

  test("every cell draws a finite study on the null stream, and core cells on the power stream"):
    Cell.all.foreach { c =>
      val sim = ModelJ.draw(c, Phase.Harness, StudyPurpose.Null, 0, c.researchDesign)
      assert(sim.data.y.forall(_.isFinite) && sim.data.v.forall(v => v > 0.0 && v.isFinite), c.id.value)
      assertEquals(sim.data.nu.toVector, Vector.fill(c.n)(c.declaredNu.value))
    }
    Cell.core.take(10).foreach { c =>
      val sim = ModelJ.draw(c, Phase.Harness, StudyPurpose.Power, 0, c.researchDesign)
      assertEqualsDouble(c.contrast.zip(sim.truth.beta).map(_ * _).sum, c.powerDelta.get, 1e-15)
    }

  test("the null and power streams of a study are distinct and reproducible"):
    val c = ResearchTestSupport.cell("C-n20-DG-Vspread-T2-N40")
    val a = ModelJ.draw(c, Phase.Pilot, StudyPurpose.Null, 5, c.researchDesign)
    val b = ModelJ.draw(c, Phase.Pilot, StudyPurpose.Null, 5, c.researchDesign)
    val p = ModelJ.draw(c, Phase.Pilot, StudyPurpose.Power, 5, c.researchDesign)
    assertEquals(a.data.y.toVector, b.data.y.toVector)
    assertEquals(a.data.v.toVector, b.data.v.toVector)
    assertNotEquals(a.data.v.toVector, p.data.v.toVector)
