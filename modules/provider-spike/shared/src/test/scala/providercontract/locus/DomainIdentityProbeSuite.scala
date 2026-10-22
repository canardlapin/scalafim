package providercontract.locus

import locus4s.{DomainId, DomainRecord, DomainRegistry, FiniteDomain, Region, Selection, SelectionError}
import locus4s.data.VectorField

/** The domain-identity contract ScalaFIM relies on.
  *
  * locus4s distinguishes two notions:
  *
  *   - '''runtime owner''' — physical identity of the live `FiniteDomain`
  *     object. This is what the phantom `S` tracks and what every checked
  *     operation compares.
  *   - '''persistent identity''' — the `DomainKey` (id, size, fingerprint), the
  *     only notion that survives serialization.
  *
  * There is no process-wide registry. Each independent registry restore mints an
  * independent owner; consumers that combine owners with equal persistent keys
  * check a `DomainAlignment` and transport through it. A composition that needs
  * one shared live owner threads its own `DomainRegistry` value.
  */
class DomainIdentityProbeSuite extends munit.FunSuite:

  private val key = "probe:same-key"

  test("independent restores share persistent identity but not a runtime owner"):
    val a = DomainFixtures.restore(key, 4).space
    val b = DomainFixtures.restore(key, 4).space
    assertEquals(a.key, b.key)
    assert(a.samePersistentIdentityAs(b))
    assert(!a.sameRuntimeOwnerAs(b), "no process-wide canonicalization remains")

  test("separate restores interoperate only through a checked alignment"):
    val a = DomainFixtures.restore(key, 4).space
    val b = DomainFixtures.restore(key, 4).space
    val field = VectorField.tabulate(b)(index => index.ordinal)
    assert(field.restrictChecked(Region.whole(a)).isLeft, "unaligned owners are rejected")
    val alignment = a.align(b).fold(error => fail(error.message), identity)
    val region = alignment.transport(Region.fromOrdinals(a, Vector(3, 1)).toOption.get)
    assertEquals(field.restrict(region).valuesInDomainOrder.toVector, Vector(1, 3))

  test("transport preserves selection order and ownership"):
    val a = DomainFixtures.restore(key, 4).space
    val b = DomainFixtures.restore(key, 4).space
    val alignment = a.align(b).fold(error => fail(error.message), identity)
    val moved = alignment.transport(Selection.fromOrdinals(a, Vector(3, 1)).toOption.get)
    assertEquals(moved.ordinals.toVector, Vector(3, 1))
    assert(moved.space.sameRuntimeOwnerAs(b))

  test("a unique selection is not a repeated draw"):
    val space = DomainFixtures.restore(key, 4).space
    assertEquals(
      Selection.fromOrdinals(space, Vector(2, 0, 2)).left.toOption,
      Some(SelectionError.DuplicateOrdinal(2))
    )

  test("different ids or sizes never align"):
    val left = DomainFixtures.restore("probe:left", 4).space
    val right = DomainFixtures.restore("probe:right", 4).space
    val resized = DomainFixtures.restore("probe:left", 5).space
    assert(left.align(right).isLeft, "equal size is not identity")
    assert(left.align(resized).isLeft, "an id restored at another size is another domain")
    val field = VectorField.tabulate(right)(index => index.ordinal)
    assert(field.restrictChecked(Region.whole(left)).isLeft)

  test("an explicit registry scope shares one owner and rejects conflicting keys"):
    val record = DomainRecord.make(DomainId.parse(key).toOption.get, "probe", 4).toOption.get
    val first = DomainRegistry.empty.restore(record).toOption.get
    val second = first.registry.restore(record).toOption.get
    assert(first.space.sameRuntimeOwnerAs(second.space))
    val conflicting = DomainRecord.make(DomainId.parse(key).toOption.get, "probe", 9).toOption.get
    assert(first.registry.restore(conflicting).isLeft)

  test("an ephemeral domain is derived: no record, and aligns only with itself"):
    val head: FiniteDomain[?] = DomainFixtures.ephemeral("derived-support", 10).value
    val other: FiniteDomain[?] = DomainFixtures.ephemeral("derived-support", 10).value
    assert(!head.isPersistable)
    assertEquals(head.persistentKey, None)
    assert(head.align(head).isRight)
    assert(head.align(other).isLeft, "equal name and size do not make derived domains equal")
