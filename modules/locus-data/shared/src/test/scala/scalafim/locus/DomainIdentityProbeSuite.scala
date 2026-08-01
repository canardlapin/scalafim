package scalafim.locus

/** What a [[SpaceKey]] guarantees about domain identity.
  *
  * locus4s distinguishes two notions:
  *
  *   - '''runtime owner''' — physical identity of the live `FiniteDomain`
  *     object. This is what the phantom `S` tracks and what every checked
  *     operation compares.
  *   - '''persistent identity''' — the `DomainRecord` (id, key, size) that a
  *     [[SpaceKey]] names, and the only notion that can survive serialization.
  *
  * [[DomainFactory]] canonicalizes the first through the second: one key, one
  * owner, process-wide. Without that a second construction of the same keyed
  * domain would be rejected against the first by every checked operation,
  * despite their persistent identities agreeing.
  */
class DomainIdentityProbeSuite extends munit.FunSuite:

  private val key = SpaceKey.unsafe("probe:same-key")

  test("a SpaceKey names a stable persistent identity"):
    val a = DomainFactory.unsafeRestore(key, 4).space
    val b = DomainFactory.unsafeRestore(key, 4).space
    assertEquals(a.id, b.id)
    assertEquals(a.key, b.key)
    assert(a.samePersistentIdentityAs(b), "same key must mean same persistent identity")

  test("restoring the same key twice yields the same runtime owner"):
    val a = DomainFactory.unsafeRestore(key, 4).space
    val b = DomainFactory.unsafeRestore(key, 4).space
    assert(a.sameRuntimeOwnerAs(b), "one key must name one live domain")
    assert(a eq b, "canonicalization should return the identical owner, not a copy")

  test("structures built on separate restores of one key interoperate"):
    // The property the canonical registry exists to provide: a region built
    // against one view of the domain is accepted against another.
    val a = DomainFactory.unsafeRestore(key, 4).space
    val b = DomainFactory.unsafeRestore(key, 4).space
    val regionA = Region.whole(a)
    val fieldB = IndexedField.tabulate(b)(point => point.ordinal)
    fieldB.restrictChecked(regionA) match
      case Left(error) => fail(s"same-key restores should realign: ${error.message}")
      case Right(section) =>
        assertEquals(section.valuesInDomainOrder.toVector, Vector(0, 1, 2, 3))

  test("distinct keys stay distinct owners"):
    val left = DomainFactory.unsafeRestore(SpaceKey.unsafe("probe:left"), 4).space
    val right = DomainFactory.unsafeRestore(SpaceKey.unsafe("probe:right"), 4).space
    assert(!left.sameRuntimeOwnerAs(right), "different keys must not share an owner")
    val fieldRight = IndexedField.tabulate(right)(point => point.ordinal)
    assert(
      fieldRight.restrictChecked(Region.whole(left)).isLeft,
      "a region from a different domain must still be rejected"
    )

  test("a conflicting record for a known id is rejected, not silently shared"):
    // Same id, different size => different DomainKey => the registry must
    // refuse rather than hand back the incompatible existing owner.
    val conflictKey = SpaceKey.unsafe("probe:conflict")
    assert(DomainFactory.restore(conflictKey, 4).isRight)
    DomainFactory.restore(conflictKey, 9) match
      case Left(DomainFactoryError.RestoreFailed(_)) => ()
      case Left(other) => fail(s"expected a restore conflict, got ${other.message}")
      case Right(_) => fail("a different size for a known id must not resolve")

  test("an ephemeral domain is derived: no record, and never registry-retained"):
    // Derived selection-position domains must not accumulate in the canonical
    // registry, and must not need a key proportional to their own content.
    val before = DomainFactory.registeredCount
    val supports =
      (0 until 50).map(i => DomainFactory.unsafeEphemeral("derived-support", 10 + i))
    assertEquals(
      DomainFactory.registeredCount,
      before,
      "ephemeral domains must not enter the canonical registry"
    )

    val head = supports.head.value
    assert(!head.isPersistable, "a derived support has no persistent record")
    assertEquals(head.persistentRecord, None)
    assertEquals(head.persistentKey, None)

    // Each is its own owner, so they cannot be confused with one another.
    val other = DomainFactory.unsafeEphemeral("derived-support", 10).value
    assert(!head.sameRuntimeOwnerAs(other))

  test("within a single restore the domain is its own owner"):
    val space = DomainFactory.unsafeRestore(SpaceKey.unsafe("probe:single"), 5).space
    val field = IndexedField.tabulate(space)(point => point.ordinal)
    assert(field.restrictChecked(Region.whole(space)).isRight)
    assertEquals(
      field.restrict(Region.whole(space)).valuesInDomainOrder.toVector,
      Vector(0, 1, 2, 3, 4)
    )
