package scalafim.spatial

import scalafim.image.world.{
  DatasetNamespace,
  GeometryDigest,
  NativeContext,
  ReferenceAcquisition,
  SessionId,
  SpaceError,
  SubjectId,
  TemplateName,
  WorldSpace
}
import scalafim.surface.{Hemisphere, SurfaceKind}

class SpaceRefWorldSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val subject = ok(SubjectId("sub-01"))
  private val session = ok(SessionId("01"))
  private val context =
    NativeContext(
      ok(DatasetNamespace("ds")),
      subject,
      Some(session),
      ok(ReferenceAcquisition(Map("suffix" -> "T1w"), ok(GeometryDigest(Vector(4, 4, 4), Vector.fill(12)(1.0), 1, 1))))
    )
  private val native = WorldSpace.SubjectNative(context.namespace, context.subject, context.session, context.reference)

  private val volume = SpaceRef.Volume(subject, Some(session), ok(Modality("bold")))
  private val surface = SpaceRef.Surface(subject, Hemisphere.Left, SurfaceKind.White)
  private val template = SpaceRef.Template(TemplateName.unsafe("MNI152NLin2009cAsym"), None, TemplateKind.Volume)
  private val latent = ok(SpaceRef.latent(8))

  test("a template reference names its template world; many domains share it"):
    assertEquals(template.world, Right(WorldSpace.Template(TemplateName.unsafe("MNI152NLin2009cAsym"))))
    val hybrid = SpaceRef.Template(TemplateName.unsafe("MNI152NLin2009cAsym"), None, TemplateKind.Hybrid)
    assertEquals(hybrid.world, template.world)
    assertEquals(template.worldIn(context), template.world)

  test("subject references need a native context, and then live in that subject's native world"):
    Vector(volume, surface).foreach: ref =>
      ref.world match
        case Left(SpaceError.MissingNativeContext(_)) => ()
        case other                                    => fail(s"$ref resolved to $other without a context")
      assertEquals(ref.worldIn(context), Right(native))

  test("a native context for another subject or session is a conflict, not a relabel"):
    val other = context.copy(subject = ok(SubjectId("sub-02")))
    assert(volume.worldIn(other).isLeft)
    assert(surface.worldIn(other).isLeft)
    assert(volume.worldIn(context.copy(session = None)).isLeft)
    assert(volume.worldIn(context.copy(session = Some(ok(SessionId("02"))))).isLeft)
    val sessionless = SpaceRef.Volume(subject, None, ok(Modality("bold")))
    assertEquals(sessionless.worldIn(context), Right(native))

  test("only anatomical surfaces are in the subject's scanner-native world"):
    Vector(SurfaceKind.Pial, SurfaceKind.SmoothWm, SurfaceKind.Midthickness).foreach: kind =>
      assertEquals(SpaceRef.Surface(subject, Hemisphere.Right, kind).worldIn(context), Right(native))
    Vector(SurfaceKind.Inflated, SurfaceKind.VeryInflated, SurfaceKind.Sphere, SurfaceKind.Custom("flat")).foreach: kind =>
      SpaceRef.Surface(subject, Hemisphere.Right, kind).worldIn(context) match
        case Left(SpaceError.NoWorldSpace(_)) => ()
        case other                            => fail(s"a $kind surface resolved to $other")

  test("latent domains have no world space"):
    latent.world match
      case Left(SpaceError.NoWorldSpace(_)) => ()
      case other                            => fail(s"latent resolved to $other")
    assert(latent.worldIn(context).isLeft)
