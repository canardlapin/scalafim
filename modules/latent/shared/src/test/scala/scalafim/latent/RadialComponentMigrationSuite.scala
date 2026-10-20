package scalafim.latent

import neuroim4s.image.SampleSpaces

/** Recorded against the original private six-face traversal before provider migration. */
class RadialComponentMigrationSuite extends munit.FunSuite:
  test("fragmented asymmetric mask preserves seeded atom order at every level"):
    val space = SampleSpaces(Vector(3, 4, 5))
    val active = Vector(45, 19, 1, 24, 6, 40, 18, 0, 41, 5)
    val spec = RadialBasisSpec(
      sigma0 = 1.5,
      levels = 1,
      extraFineLevels = 1,
      radiusFactor = 1.2,
      seed = 731L,
      tinyMaskIdentity = false
    ).fold(error => fail(error.message), identity)
    val basis = RadialBasis.fromSpec(space, active, spec).fold(error => fail(error.message), identity)
    // Last-axis-fastest ordinals of the centers, including component and greedy sampling order.
    val expected = Vector(
      Vector(5, 19, 24, 45),
      Vector(1, 0, 5, 6, 19, 18, 24, 41, 45, 40),
      Vector(5, 6, 0, 1, 18, 19, 24, 40, 45, 41)
    )
    assertEquals(basis.activeIndices, Some(active))
    assertEquals(basis.atomLevels, expected.zipWithIndex.flatMap((ordinals, level) => Vector.fill(ordinals.size)(level)))
    expected.zipWithIndex.foreach: (ordinals, level) =>
      val atoms = basis.atoms.filter(_.level == level)
      assertEquals(atoms.map(_.center.toVector), ordinals.map: ordinal =>
        Vector((ordinal / 20).toDouble, ((ordinal % 20) / 5).toDouble, (ordinal % 5).toDouble)
      )
      atoms.foreach(atom => assertEqualsDouble(atom.sigma.value, 1.5 / (1 << level), 0.0))
    val reordered = RadialBasis
      .fromSpec(space, active.reverse, spec)
      .fold(error => fail(error.message), identity)
    assertEquals(reordered.atoms, basis.atoms)
    assertEquals(reordered.activeIndices, Some(active.reverse))
