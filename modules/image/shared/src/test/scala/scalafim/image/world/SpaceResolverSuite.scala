package scalafim.image.world

class SpaceResolverSuite extends munit.FunSuite:
  private def ok[A](result: Either[SpaceError, A]): A =
    result.fold(error => fail(error.message), identity)

  private val context = NativeContext(
    ok(DatasetNamespace("ds")),
    ok(SubjectId("sub-01")),
    Some(ok(SessionId("01"))),
    ReferenceAcquisition(Map("task" -> "rest"), ok(GeometryDigest(Vector(4, 4, 4), Vector.fill(12)(0.5), 1, 1)))
  )
  private val native = WorldSpace.SubjectNative(context.namespace, context.subject, context.session, context.reference)
  private val mni2009c = ok(WorldSpace.template("MNI152NLin2009cAsym"))

  private def resolve(e: SpaceEvidence) = SpaceResolver.resolve(e)

  test("every NIfTI xform code is recognised; others are rejected"):
    assertEquals((0 to 5).map(c => XformCode.fromNifti(c).isRight).toVector, Vector.fill(6)(true))
    assertEquals(XformCode.fromNifti(7), Left(SpaceError.UnknownXformCode("7")))

  test("every GIFTI coordinate-system name maps onto the NIfTI codes"):
    val names = Vector("UNKNOWN", "SCANNER_ANAT", "ALIGNED_ANAT", "TALAIRACH", "MNI_152", "TEMPLATE_OTHER")
    assertEquals(names.map(n => XformCode.fromGifti(s"NIFTI_XFORM_$n")), (0 to 5).map(c => XformCode.fromNifti(c)).toVector)
    assert(XformCode.fromGifti("NIFTI_XFORM_BOGUS").isLeft)

  test("no evidence, or only unknown/scanner codes without context, is the unresolved space"):
    assertEquals(resolve(SpaceEvidence()), Right(WorldSpace.Unresolved))
    assertEquals(resolve(SpaceEvidence(xform = Some(XformCode.Unknown))), Right(WorldSpace.Unresolved))
    assertEquals(resolve(SpaceEvidence(xform = Some(XformCode.ScannerAnatomical))), Right(WorldSpace.Unresolved))

  test("scanner and aligned codes with a native context resolve to that native space"):
    assertEquals(resolve(SpaceEvidence(xform = Some(XformCode.ScannerAnatomical), native = Some(context))), Right(native))
    assertEquals(resolve(SpaceEvidence(xform = Some(XformCode.AlignedAnatomical), native = Some(context))), Right(native))

  test("template codes alone are ambiguous, never guessed"):
    Vector(XformCode.Mni152, XformCode.Talairach, XformCode.TemplateOther).foreach: code =>
      resolve(SpaceEvidence(xform = Some(code))) match
        case Left(SpaceError.AmbiguousTemplate(_)) => ()
        case other                                 => fail(s"$code resolved to $other")

  test("the BIDS space entity names the template and settles an MNI152 code"):
    assertEquals(resolve(SpaceEvidence(bidsSpace = Some("MNI152NLin2009cAsym"))), Right(mni2009c))
    assertEquals(resolve(SpaceEvidence(xform = Some(XformCode.Mni152), bidsSpace = Some("MNI152NLin2009cAsym"))), Right(mni2009c))
    assertEquals(resolve(SpaceEvidence(xform = Some(XformCode.AlignedAnatomical), bidsSpace = Some("MNI152NLin6Asym"))), Right(ok(WorldSpace.template("MNI152NLin6Asym"))))

  test("native BIDS labels need a native context"):
    assertEquals(resolve(SpaceEvidence(bidsSpace = Some("T1w"), native = Some(context))), Right(native))
    resolve(SpaceEvidence(bidsSpace = Some("T1w"))) match
      case Left(SpaceError.MissingNativeContext(_)) => ()
      case other                                    => fail(s"expected missing context, got $other")

  test("contradictory evidence is a typed conflict"):
    val conflicts = Vector(
      SpaceEvidence(xform = Some(XformCode.ScannerAnatomical), bidsSpace = Some("MNI152NLin2009cAsym")),
      SpaceEvidence(xform = Some(XformCode.Mni152), bidsSpace = Some("fsaverage")),
      SpaceEvidence(xform = Some(XformCode.Talairach), bidsSpace = Some("MNI152NLin2009cAsym")),
      SpaceEvidence(xform = Some(XformCode.Mni152), bidsSpace = Some("T1w"), native = Some(context)),
      SpaceEvidence(bidsSpace = Some("MNI152NLin6Asym"), assertion = Some(mni2009c))
    )
    conflicts.foreach: evidence =>
      resolve(evidence) match
        case Left(SpaceError.ConflictingEvidence(_, _)) => ()
        case other                                      => fail(s"$evidence resolved to $other")

  test("an explicit assertion wins over codes and agrees with native BIDS labels"):
    assertEquals(resolve(SpaceEvidence(xform = Some(XformCode.Mni152), assertion = Some(mni2009c))), Right(mni2009c))
    assertEquals(resolve(SpaceEvidence(bidsSpace = Some("MNI152NLin2009cAsym"), assertion = Some(mni2009c))), Right(mni2009c))
    val tk = WorldSpace.SubjectTkRas(context.namespace, context.subject, context.reference)
    assertEquals(resolve(SpaceEvidence(bidsSpace = Some("T1w"), native = Some(context), assertion = Some(tk))), Right(tk))
