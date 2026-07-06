package scalafim.spatial

import scalafim.image.{DMat, Mask, NeuroSpace}
import scalafim.surface.{Hemisphere, SurfaceGeometry, SurfaceKind, TriangleMesh}

class DomainSuite extends munit.FunSuite:

  private def value[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def volumeDomain(idValue: String, dims: Vector[Int] = Vector(2, 2, 1)): Domain =
    val id = value(DomainId(idValue))
    val subject = value(SubjectId("sub-01"))
    val modality = value(Modality("bold"))
    val space = NeuroSpace(dims, trans = Some(DMat.eye(4)))
    val geometry = value(SamplingGeometry.volume(space))
    value(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def surfaceGeometry: SurfaceGeometry =
    val mesh =
      TriangleMesh.fromRows(
        vertices = Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0)
        ),
        faces = Vector((0, 1, 2))
      )
    SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Midthickness)

  test("opaque ids trim valid input and reject empty identifiers"):
    val id = value(DomainId("  epi-native  "))
    assertEquals(id.value, "epi-native")
    assertEquals(DomainId("  ").left.toOption, Some(SpatialError.EmptyIdentifier("domain")))
    assertEquals(PartName("").left.toOption, Some(SpatialError.EmptyIdentifier("part")))

  test("volume geometry counts sampled voxels and validates masks"):
    val space = NeuroSpace(Vector(2, 2, 2), trans = Some(DMat.eye(4)))
    val geometry = value(SamplingGeometry.volume(space))
    assertEquals(geometry.nElements, 8)

    val mask = Mask.fromIndices(space, scalafim.image.NArrayUtil.fromArray(Array(0, 3, 7)), "mask")
    val masked = value(SamplingGeometry.volume(space, Some(mask)))
    assertEquals(masked.nElements, 8)

    val other = NeuroSpace(Vector(2, 2, 1), trans = Some(DMat.eye(4)))
    val badMask = Mask.fromIndices(other, scalafim.image.NArrayUtil.fromArray(Array(0)), "bad")
    assertEquals(
      SamplingGeometry.volume(space, Some(badMask)).left.toOption,
      Some(SpatialError.MaskSpaceMismatch("volume"))
    )

  test("surface geometry counts vertices"):
    val geometry = value(SamplingGeometry.surface(surfaceGeometry))
    assertEquals(geometry.nElements, 3)

  test("latent space smart constructor rejects invalid dimensions"):
    val latent = SpaceRef.latent(0)
    assertEquals(latent.left.toOption, Some(SpatialError.NonPositiveDimension("latent", 0)))

  test("hybrid geometry computes stable offsets and rejects duplicate part names"):
    val left = volumeDomain("left", Vector(2, 1, 1))
    val right = volumeDomain("right", Vector(3, 1, 1))
    val leftName = value(PartName("left"))
    val rightName = value(PartName("right"))

    val hybrid = value(SamplingGeometry.hybrid(Vector(leftName -> left, rightName -> right)))
    assertEquals(hybrid.nElements, 5)

    hybrid match
      case SamplingGeometry.Hybrid(parts) =>
        assertEquals(parts.map(_.name.value), Vector("left", "right"))
        assertEquals(parts.map(_.offset), Vector(0, 2))
        assertEquals(parts.map(_.endExclusive), Vector(2, 5))
      case _ =>
        fail("expected hybrid geometry")

    val duplicate = SamplingGeometry.hybrid(Vector(leftName -> left, leftName -> right))
    assertEquals(duplicate.left.toOption, Some(SpatialError.DuplicatePartName(leftName)))
