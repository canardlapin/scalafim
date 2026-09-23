package scalafim.surface.reference

class FrameEvidenceSuite extends munit.FunSuite:
  private val fsLR = TemplateId.unsafe("fsLR")
  private val nlin2009c = TemplateId.unsafe("MNI152NLin2009cAsym")
  private val digest = "0" * 64
  private val declaration = FrameDeclaration.make(
    TemplateFrame.unsafe("MNI152NLin6Asym", "templateflow-24.2.0"),
    FrameBasis.literature("10.1093/cercor/bhr291", "TemplateFlow tpl-fsLR (HCP Pipelines templates; ReferencesAndLinks doi:10.1093/cercor/bhr291) — surfaces in MNI152NLin6Asym per HCP convention; corroborated by FrameEvidence on the 2009c GM probseg").toOption.get,
    AssetProvenance.make(fsLR, "tpl-fsLR/midthickness.surf.gii", "r", digest).toOption.get).toOption.get
  private val gmMap = AssetProvenance.make(nlin2009c,
    "tpl-MNI152NLin2009cAsym/tpl-MNI152NLin2009cAsym_res-01_label-GM_probseg.nii.gz", "r", digest).toOption.get
  private val thresholds = FrameEvidenceThresholds.Default

  private def score(placement: Placement, mean: Double, scored: Int = 29000, excluded: Int = 0) =
    PlacementScore.make(placement, mean, math.min(1.0, mean + 0.1), scored, excluded).toOption.get

  /** Bridged 0.60, raw 0.55, reversed 0.50, every shift 0.58. */
  private def passing: Vector[PlacementScore] =
    Vector(score(Placement.Bridged, 0.60), score(Placement.Raw, 0.55), score(Placement.Reversed, 0.50)) ++
      thresholds.requiredPlacements.collect { case p: Placement.Shifted => score(p, 0.58) }

  private def verdict(scores: Vector[PlacementScore]) =
    FrameEvidence.make(declaration, gmMap, thresholds, scores).toOption.get.verdict

  test("default thresholds are the plan's frozen margins and require nine placements"):
    assertEquals((thresholds.minimumGainOverRaw, thresholds.minimumGainOverReversed, thresholds.shiftMillimetres),
      (0.03, 0.05, 3.0))
    assertEquals(thresholds.requiredPlacements.size, 9)
    assert(thresholds.requiredPlacements.contains(Placement.Shifted(EvidenceAxis.Y, -3.0)))
    assertEquals(Placement.Shifted(EvidenceAxis.Z, 3.0).name, "shift-z+3mm")
    assertEquals(Placement.Shifted(EvidenceAxis.X, -1.5).name, "shift-x-1.5mm")

  test("a receipt clearing every margin passes"):
    assertEquals(verdict(passing), FrameEvidenceVerdict.Pass)

  test("each margin fails on its own and every failure is reported"):
    def replaced(p: Placement, mean: Double) = passing.map(s => if s.placement == p then score(p, mean) else s)
    assertEquals(verdict(replaced(Placement.Raw, 0.575)),
      FrameEvidenceVerdict.Fail(Vector(FrameEvidenceFailure.InsufficientGainOverRaw(0.60 - 0.575, 0.03))))
    assertEquals(verdict(replaced(Placement.Reversed, 0.56)),
      FrameEvidenceVerdict.Fail(Vector(FrameEvidenceFailure.InsufficientGainOverReversed(0.60 - 0.56, 0.05))))
    val shift = Placement.Shifted(EvidenceAxis.X, -3.0)
    assertEquals(verdict(replaced(shift, 0.60)),
      FrameEvidenceVerdict.Fail(Vector(FrameEvidenceFailure.ShiftNotWorse(shift, 0.60, 0.60))))
    val several = replaced(Placement.Raw, 0.60).map(s => if s.placement == shift then score(shift, 0.7) else s)
    assertEquals(verdict(several) match { case FrameEvidenceVerdict.Fail(f) => f.size; case _ => 0 }, 2)

  test("missing placements and differing vertex populations fail; no bridged score cannot pass"):
    val shift = Placement.Shifted(EvidenceAxis.Z, 3.0)
    assertEquals(verdict(passing.filterNot(_.placement == shift)),
      FrameEvidenceVerdict.Fail(Vector(FrameEvidenceFailure.MissingPlacement(shift))))
    assertEquals(verdict(passing.map(s => if s.placement == Placement.Raw then score(Placement.Raw, 0.55, 28000, 900) else s)),
      FrameEvidenceVerdict.Fail(Vector(FrameEvidenceFailure.VertexPopulationDiffers(Placement.Raw, 29000, 28900))))
    assertEquals(verdict(passing.filterNot(_.placement == Placement.Bridged)),
      FrameEvidenceVerdict.Fail(Vector(FrameEvidenceFailure.MissingPlacement(Placement.Bridged))))
    assertEquals(verdict(Vector.empty), FrameEvidenceVerdict.Fail(thresholds.requiredPlacements.map(FrameEvidenceFailure.MissingPlacement.apply)))

  test("scores, thresholds and receipts validate their inputs"):
    assert(PlacementScore.make(Placement.Raw, 1.1, 0.5, 10, 0).isLeft)
    assert(PlacementScore.make(Placement.Raw, Double.NaN, 0.5, 10, 0).isLeft)
    assert(PlacementScore.make(Placement.Raw, 0.5, -0.1, 10, 0).isLeft)
    assert(PlacementScore.make(Placement.Raw, 0.5, 0.5, 0, 0).isLeft)
    assert(PlacementScore.make(Placement.Raw, 0.5, 0.5, 10, -1).isLeft)
    assert(FrameEvidenceThresholds.make(-0.01, 0.05, 3.0).isLeft)
    assert(FrameEvidenceThresholds.make(0.03, 0.05, 0.0).isLeft)
    assert(FrameEvidenceThresholds.make(0.03, Double.PositiveInfinity, 3.0).isLeft)
    assert(FrameEvidence.make(declaration, gmMap, thresholds, passing :+ score(Placement.Raw, 0.1)).isLeft)
