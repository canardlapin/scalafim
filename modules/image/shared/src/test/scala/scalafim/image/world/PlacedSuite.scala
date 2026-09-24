package scalafim.image.world

import image4s.geometry.{D3, Frame, Point}
import reframe4s.lie.FramedAffine
import scalafim.image.world.Rebind.PointIn

class PlacedSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val subject =
    WorldSpace.SubjectNative(
      ok(DatasetNamespace("ds")),
      ok(SubjectId("sub-01")),
      None,
      ReferenceAcquisition(Map("task" -> "rest"), ok(GeometryDigest(Vector(2, 2, 2), Vector.fill(12)(1.0), 1, 1)))
    )

  test("a value decoded in a fresh MNI frame binds to the static template frame"):
    val decoded = FrameCatalog.frame(WorldSpace.Template(TemplateName.unsafe("MNI152NLin2009cAsym")))
    val point = ok(Point.in(decoded)(10.0, -20.0, 30.0))
    val placed = ok(Placed[PointIn](decoded)(point))
    assertEquals(placed.world, WorldSpace.Template(TemplateName.unsafe("MNI152NLin2009cAsym")))
    val bound: Point[Spaces.Mni2009c, D3] = ok(placed.bindTo(Spaces.MNI152NLin2009cAsym))
    assertEquals(bound.coordinates, Vector(10.0, -20.0, 30.0))

  test("binding to a template the value does not live in fails"):
    val native = FrameCatalog.frame(subject)
    val placed = ok(Placed[PointIn](native)(ok(Point.in(native)(1.0, 2.0, 3.0))))
    assert(placed.bindTo(Spaces.MNI152NLin2009cAsym).isLeft)
    assert(placed.bindTo(Spaces.MNI152NLin6Asym).isLeft)

  test("only world frames can be packaged"):
    val scratch = ok(Frame.named[D3]("scratch"))
    assert(Placed[PointIn](scratch)(ok(Point.in(scratch)(0.0, 0.0, 0.0))).isLeft)

  test("a value cannot be packaged with a frame it does not belong to"):
    val native = FrameCatalog.frame(subject)
    val subjectPoint = ok(Point.in(native)(1.0, 2.0, 3.0))
    val errors = compileErrors("Placed[PointIn](Spaces.MNI152NLin2009cAsym)(subjectPoint)")
    assert(errors.contains("Found:") && errors.contains("Required:"), errors)
    assertNoDiff(compileErrors("Placed[PointIn](native)(subjectPoint)"), "")

  test("a packaged value is not a template point until it is bound"):
    val decoded = FrameCatalog.frame(WorldSpace.Template(TemplateName.unsafe("MNI152NLin2009cAsym")))
    val placed = ok(Placed[PointIn](decoded)(ok(Point.in(decoded)(0.0, 0.0, 0.0))))
    val errors = compileErrors("val p: Point[Spaces.Mni2009c, D3] = placed.value")
    assert(errors.contains("Found:"), errors)

  test("maps between different world frames cannot be composed out of order"):
    val native = FrameCatalog.frame(subject)
    val toMni = FramedAffine.identity(native)
    val mniToMni6 = FramedAffine.between(Spaces.MNI152NLin2009cAsym, Spaces.MNI152NLin6Asym)(image4s.geometry.Affine.identity[D3])
    val errors = compileErrors("toMni.andThen(mniToMni6)")
    assert(errors.nonEmpty, "composing native->native with MNI2009c->MNI6 must not compile")
    assertNoDiff(compileErrors("mniToMni6.andThen(FramedAffine.identity(Spaces.MNI152NLin6Asym))"), "")
