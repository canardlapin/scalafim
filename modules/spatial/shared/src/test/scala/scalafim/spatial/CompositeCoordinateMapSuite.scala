package scalafim.spatial

import ravel.NDArray as RavelArray
import scalafim.image.{DMat, DenseFieldMorphism, GridSpec, NeuroSpace, Resample, SpatialDomainId}

class CompositeCoordinateMapSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def imageValue[A](result: Either[scalafim.image.MorphismError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def domain(name: String): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(
      SamplingGeometry.volume(NeuroSpace(Vector(4, 1, 1), trans = Some(DMat.eye(4))))
    )
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def affine(rows: Vector[Vector[Double]]): CoordinateMap =
    spatialValue(CoordinateMap.affine3D(DMat.fromRows(rows)))

  private def scaleX(value: Double): CoordinateMap =
    affine(
      Vector(
        Vector(value, 0.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def translateX(value: Double): CoordinateMap =
    affine(
      Vector(
        Vector(1.0, 0.0, 0.0, value),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def displacement(source: Domain, target: Domain, x: Double): CoordinateMap =
    val grid = GridSpec.identity(Vector(4, 1, 1))
    val values =
      RavelArray.tabulate[Double](
        grid.shape.x,
        grid.shape.y,
        grid.shape.z,
        3
      ) { (_, _, _, component) =>
        if component == 0 then x else 0.0
      }
    val dense = imageValue(
      DenseFieldMorphism.displacement(
        SpatialDomainId(source.id.value),
        SpatialDomainId(target.id.value),
        grid,
        values,
        Resample.Method.Linear
      )
    )
    spatialValue(CoordinateMap.dense3D(dense))

  test("composite maps are non-empty and contain only geometric 3D components"):
    assertEquals(
      CoordinateMap.composite3D(Vector.empty).left.toOption,
      Some(SpatialError.InvalidCompositeCoordinateMap("at least one component is required"))
    )
    CoordinateMap.composite3D(Vector(CoordinateMap.Unspecified)).left.toOption match
      case Some(SpatialError.InvalidCompositeCoordinateMap(reason)) =>
        assert(reason.contains("component 0"))
      case other => fail(s"expected invalid component failure, got $other")

  test("ITK component order is preserved while point evaluation runs last component first"):
    val source = domain("source")
    val target = domain("target")
    val composite = spatialValue(
      CoordinateMap.composite3D(Vector(scaleX(2.0), displacement(source, target, 1.0)))
    )
    val actual = spatialValue(composite.transform(Vector(0.25, 0.0, 0.0)))

    assertEqualsDouble(actual(0), 2.5, 1e-12)
    assertEqualsDouble(actual(1), 0.0, 1e-12)
    assertEqualsDouble(actual(2), 0.0, 1e-12)

  test("component content and order participate in the composite fingerprint"):
    val scale = scaleX(2.0)
    val shift = translateX(1.0)
    val first = spatialValue(CoordinateMap.composite3D(Vector(scale, shift)))
    val reordered = spatialValue(CoordinateMap.composite3D(Vector(shift, scale)))
    val changed = spatialValue(CoordinateMap.composite3D(Vector(scaleX(3.0), shift)))

    assertNotEquals(first.fingerprint, reordered.fingerprint)
    assertNotEquals(first.fingerprint, changed.fingerprint)

  test("an affine-only composite derives an exact reversed-order inverse"):
    val composite = spatialValue(CoordinateMap.composite3D(Vector(scaleX(2.0), translateX(1.0))))
    val inverse = spatialValue(composite.inverted)
    val transformed = spatialValue(composite.transform(Vector(0.5, 0.0, 0.0)))
    val recovered = spatialValue(inverse.transform(transformed))

    assertEqualsDouble(transformed(0), 3.0, 1e-12)
    assertEqualsDouble(recovered(0), 0.5, 1e-12)

  test("a supplied inverse component chain makes a dense composite executable in reverse"):
    val source = domain("source")
    val target = domain("target")
    val forward = displacement(source, target, 1.0)
    val reverse = displacement(target, source, -1.0)
    val composite = spatialValue(
      CoordinateMap.composite3D(Vector(forward), inverseComponents = Some(Vector(reverse)))
    )
    val inverse = spatialValue(composite.inverted)
    val transformed = spatialValue(composite.transform(Vector(1.0, 0.0, 0.0)))
    val recovered = spatialValue(inverse.transform(transformed))

    assertEqualsDouble(transformed(0), 2.0, 1e-12)
    assertEqualsDouble(recovered(0), 1.0, 1e-12)

  test("composite maps remain executable through the image morphism bridge"):
    val source = domain("source")
    val target = domain("target")
    val coordinateMap = spatialValue(
      CoordinateMap.composite3D(Vector(scaleX(2.0), displacement(source, target, 1.0)))
    )
    val morphism = spatialValue(
      Morphism.between(
        spatialValue(MorphismId("itk-composite")),
        source,
        target,
        MorphismKind.Warp3D,
        RouteTag.Anatomical,
        coordinateMap = coordinateMap
      )
    )
    val lowered = spatialValue(ImageMorphismBridge.lower(morphism))
    val actual = lowered.transform(Vector(0.25, 0.0, 0.0))

    assertEquals(lowered.source.value, source.id.value)
    assertEquals(lowered.target.value, target.id.value)
    assertEqualsDouble(actual(0), 2.5, 1e-12)
