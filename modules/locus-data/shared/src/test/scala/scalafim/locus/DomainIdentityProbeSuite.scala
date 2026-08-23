package scalafim.locus

import locus4s.DomainRegistry

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
  * [[DomainFactory]] canonicalizes the first through the second only inside an
  * explicitly owned [[DomainRegistry]]. Independent scopes retain the same
  * persistent meaning without accidentally sharing live owners.
  */
class DomainIdentityProbeSuite extends munit.FunSuite:

  private val key = SpaceKey.unsafe("probe:same-key")

  test("a SpaceKey names a stable persistent identity"):
    val a = DomainFactory.unsafeRestore(DomainRegistry.empty, key, 4).space
    val b = DomainFactory.unsafeRestore(DomainRegistry.empty, key, 4).space
    assertEquals(a.id, b.id)
    assertEquals(a.key, b.key)
    assert(a.samePersistentIdentityAs(b), "same key must mean same persistent identity")
    assert(!a.sameRuntimeOwnerAs(b), "independent scopes must not share live owners")

  test("threading one registry canonicalizes the same key to one runtime owner"):
    val first = DomainFactory.unsafeRestore(DomainRegistry.empty, key, 4)
    val second = DomainFactory.unsafeRestore(first.registry, key, 4)
    val a = first.space
    val b = second.space
    assert(a.sameRuntimeOwnerAs(b), "one scoped registry must retain one live domain")
    assert(a eq b, "canonicalization should return the identical owner, not a copy")

  test("structures built on scoped restores of one key interoperate"):
    val first = DomainFactory.unsafeRestore(DomainRegistry.empty, key, 4)
    val a = first.space
    val b = DomainFactory.unsafeRestore(first.registry, key, 4).space
    val regionA = Region.whole(a)
    val fieldB = IndexedField.tabulate(b)(point => point.ordinal)
    fieldB.restrictChecked(regionA) match
      case Left(error) => fail(s"same-key restores should realign: ${error.message}")
      case Right(section) =>
        assertEquals(section.valuesInDomainOrder.toVector, Vector(0, 1, 2, 3))

  test("distinct keys stay distinct owners"):
    val left =
      DomainFactory.unsafeRestore(
        DomainRegistry.empty,
        SpaceKey.unsafe("probe:left"),
        4
      )
    val right =
      DomainFactory.unsafeRestore(
        left.registry,
        SpaceKey.unsafe("probe:right"),
        4
      )
    assert(!left.space.sameRuntimeOwnerAs(right.space), "different keys must not share an owner")
    val fieldRight = IndexedField.tabulate(right.space)(point => point.ordinal)
    assert(
      fieldRight.restrictChecked(Region.whole(left.space)).isLeft,
      "a region from a different domain must still be rejected"
    )

  test("a conflicting record for a known id is rejected, not silently shared"):
    // Same id, different size => different DomainKey => the registry must
    // refuse rather than hand back the incompatible existing owner.
    val conflictKey = SpaceKey.unsafe("probe:conflict")
    val first =
      DomainFactory.restore(DomainRegistry.empty, conflictKey, 4).toOption.get
    DomainFactory.restore(first.registry, conflictKey, 9) match
      case Left(DomainFactoryError.RestoreFailed(_)) => ()
      case Left(other) => fail(s"expected a restore conflict, got ${other.message}")
      case Right(_) => fail("a different size for a known id must not resolve")

  test("immutable registry snapshots isolate concurrent branches and cleanup"):
    val root = DomainRegistry.empty
    val branchA = DomainFactory.unsafeRestore(root, key, 4)
    val branchB = DomainFactory.unsafeRestore(root, key, 4)

    assertEquals(root.size, 0, "the input snapshot must remain unchanged")
    assertEquals(branchA.registry.size, 1)
    assertEquals(branchB.registry.size, 1)
    assert(!branchA.space.sameRuntimeOwnerAs(branchB.space))
    assert(
      DomainFactory
        .unsafeRestore(branchA.registry, key, 4)
        .space
        .sameRuntimeOwnerAs(branchA.space),
      "the owner choosing a branch can resume canonicalization in that branch"
    )

  test("an ephemeral domain is derived: no record, and never registry-retained"):
    val scoped =
      DomainFactory.unsafeRestore(
        DomainRegistry.empty,
        SpaceKey.unsafe("probe:retained"),
        4
      )
    val supports =
      (0 until 50).map(i => DomainFactory.unsafeEphemeral("derived-support", 10 + i))
    assertEquals(
      scoped.registry.size,
      1,
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
    val space =
      DomainFactory
        .unsafeRestore(
          DomainRegistry.empty,
          SpaceKey.unsafe("probe:single"),
          5
        )
        .space
    val field = IndexedField.tabulate(space)(point => point.ordinal)
    assert(field.restrictChecked(Region.whole(space)).isRight)
    assertEquals(
      field.restrict(Region.whole(space)).valuesInDomainOrder.toVector,
      Vector(0, 1, 2, 3, 4)
    )
