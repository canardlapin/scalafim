package scalafim.spatial

import ravel.NDArray as RavelArray
import scalafim.image.{GridSpec, SampleSpaces, SpatialPullbacks}
import scalafim.image.SampleSpaces.*

class ProviderMapCompositionSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def domain(name: String): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(
      SamplingGeometry.volume(SampleSpaces(Vector(4, 1, 1), affine = Some(ProviderAffines.identity)))
    )
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def grid(domain: Domain): GridSpec =
    domain.geometry match
      case SamplingGeometry.Volume(space, _) => GridSpec.fromSpace(space)
      case _ => fail(s"domain ${domain.id.value} is not volumetric")

  private def affine(
      source: Domain,
      target: Domain,
      rows: Vector[Vector[Double]]
  ): CoordinateMap =
    spatialValue(CoordinateMap.affine(source, target, ProviderAffines.fromRows(rows)))

  private def scaleX(source: Domain, target: Domain, value: Double): CoordinateMap =
    affine(
      source,
      target,
      Vector(
        Vector(value, 0.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def translateX(source: Domain, target: Domain, value: Double): CoordinateMap =
    affine(
      source,
      target,
      Vector(
        Vector(1.0, 0.0, 0.0, value),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def displacement(source: Domain, target: Domain, x: Double): CoordinateMap =
    val sourceGrid = grid(source)
    val targetGrid = grid(target)
    val values =
      RavelArray.tabulate[Double](
        targetGrid.shape.x,
        targetGrid.shape.y,
        targetGrid.shape.z,
        3
      ) { (_, _, _, component) =>
        if component == 0 then x else 0.0
      }
    val pullback =
      SpatialPullbacks
        .displacement(sourceGrid, targetGrid, values)
        .fold(error => fail(error.message), identity)
    spatialValue(CoordinateMap.dense(pullback))

  test("provider composition rejects empty and non-geometric component lists"):
    assertEquals(
      CoordinateMap.compose(Vector.empty).left.toOption,
      Some(SpatialError.InvalidProviderCoordinateMap("at least one provider map is required"))
    )
    CoordinateMap.compose(Vector(CoordinateMap.Unspecified)).left.toOption match
      case Some(SpatialError.InvalidProviderCoordinateMap(reason)) =>
        assert(reason.contains("component 0"))
      case other => fail(s"expected invalid provider component failure, got $other")

  test("composition delegates ordered point evaluation to provider SpatialMap"):
    val source = domain("source")
    val intermediate = domain("intermediate")
    val target = domain("target")
    val composite = spatialValue(
      CoordinateMap.compose(
        Vector(
          displacement(intermediate, target, 1.0),
          scaleX(source, intermediate, 2.0)
        )
      )
    )
    val actual = spatialValue(composite.transform(Vector(0.25, 0.0, 0.0)))

    assertEqualsDouble(actual(0), 2.5, 1e-12)
    assertEqualsDouble(actual(1), 0.0, 1e-12)
    assertEqualsDouble(actual(2), 0.0, 1e-12)

  test("provider component content and order participate in the routing fingerprint"):
    val owner = domain("owner")
    val scale = scaleX(owner, owner, 2.0)
    val shift = translateX(owner, owner, 1.0)
    val first = spatialValue(CoordinateMap.compose(Vector(scale, shift)))
    val reordered = spatialValue(CoordinateMap.compose(Vector(shift, scale)))
    val changed = spatialValue(CoordinateMap.compose(Vector(scaleX(owner, owner, 3.0), shift)))

    assertNotEquals(first.fingerprint, reordered.fingerprint)
    assertNotEquals(first.fingerprint, changed.fingerprint)

  test("an affine-only provider composition derives an exact inverse"):
    val owner = domain("owner")
    val composite = spatialValue(
      CoordinateMap.compose(
        Vector(scaleX(owner, owner, 2.0), translateX(owner, owner, 1.0))
      )
    )
    val inverse = spatialValue(composite.inverted)
    val roundTrip = spatialValue(inverse.inverted)
    val transformed = spatialValue(composite.transform(Vector(0.5, 0.0, 0.0)))
    val recovered = spatialValue(inverse.transform(transformed))

    assertNotEquals(inverse.fingerprint, composite.fingerprint)
    assertEquals(roundTrip.fingerprint, composite.fingerprint)
    assertEqualsDouble(transformed(0), 2.0, 1e-12)
    assertEqualsDouble(recovered(0), 0.5, 1e-12)

  test("a supplied provider inverse makes a dense route executable in reverse"):
    val source = domain("source")
    val target = domain("target")
    val forward = displacement(source, target, 1.0)
    val reverse = displacement(target, source, -1.0)
    val composite = spatialValue(
      CoordinateMap.compose(
        Vector(forward),
        inverseApplicationOrder = Some(Vector(reverse))
      )
    )
    val inverse = spatialValue(composite.inverted)
    val roundTrip = spatialValue(inverse.inverted)
    val transformed = spatialValue(composite.transform(Vector(1.0, 0.0, 0.0)))
    val recovered = spatialValue(inverse.transform(transformed))

    assertNotEquals(inverse.fingerprint, composite.fingerprint)
    assertEquals(roundTrip.fingerprint, composite.fingerprint)
    assertEqualsDouble(transformed(0), 2.0, 1e-12)
    assertEqualsDouble(recovered(0), 1.0, 1e-12)

  test("a composed provider map remains executable through spatial routing"):
    val source = domain("source")
    val intermediate = domain("intermediate")
    val target = domain("target")
    val coordinateMap = spatialValue(
      CoordinateMap.compose(
        Vector(
          displacement(intermediate, target, 1.0),
          scaleX(source, intermediate, 2.0)
        )
      )
    )
    val morphism = spatialValue(
      Morphism.between(
        spatialValue(MorphismId("itk-provider-composition")),
        source,
        target,
        MorphismKind.Warp3D,
        RouteTag.Anatomical,
        coordinateMap = coordinateMap
      )
    )
    val actual = spatialValue(morphism.coordinateMap.transform(Vector(0.25, 0.0, 0.0)))

    assertEquals(morphism.source, source.id)
    assertEquals(morphism.target, target.id)
    assertEqualsDouble(actual(0), 2.5, 1e-12)
