package scalafim.fmri.design

import scalafim.fmri.design.data.{Column, DataTable}

/** What a validated design id does to the string it was given.
  *
  * Parsing an id and formatting a name are different jobs. Parsing must be
  * injective and rejecting so that an id still names what the caller named;
  * `Names.sanitize` is lossy by design and belongs where *generated* output
  * column names are produced.
  *
  * These began as characterization tests pinning the sanitize-on-parse
  * behaviour. Phase 1 of docs/plans/design-hardening.md flipped them.
  */
class IdRoundTripProbeSuite extends munit.FunSuite:

  /** Accepted by every design id: interior spaces, dots, digits, punctuation. */
  private val accepted = Vector(
    "onset",
    "my col",
    "a.b",
    "a b",
    "a_b",
    "trial_type",
    "task#1",
    "2back",
    ".hidden",
    "condition-1",
    "Ω"
  )

  private def parsed[A](name: String, id: Either[DesignError, A]): A =
    id.fold(err => fail(s"$name rejected an acceptable id: ${err.message}"), identity)

  test("a validated ColumnId round-trips its input"):
    val raw = "my col"
    val id = parsed("ColumnId", ColumnId(raw))
    assertEquals(id.value, raw)

  test("the typed lookup path finds what the untyped one finds"):
    val raw = "my col"
    val id = parsed("ColumnId", ColumnId(raw))
    val table = DataTable.fromColumns(raw -> Column.Doubles(Vector(1.0, 2.0)))

    assert(table.names.contains(raw), "the column is present under the name it was given")
    assert(table.contains(id), "and the id it was parsed into names it")
    assertEquals(table.column(id), Right(Column.Doubles(Vector(1.0, 2.0))))

  test("ColumnId is injective, so distinct columns stay distinct"):
    val dotted = parsed("ColumnId", ColumnId("a.b"))
    val spaced = parsed("ColumnId", ColumnId("a b"))
    assertNotEquals(dotted.value, spaced.value)
    assertEquals(dotted.value, "a.b")
    assertEquals(spaced.value, "a b")

  test("the round-trip law holds for every id type"):
    accepted.foreach { s =>
      assertEquals(ColumnId(s).map(_.value), Right(s), s"ColumnId('$s')")
      assertEquals(TermId(s).map(_.value), Right(s), s"TermId('$s')")
      assertEquals(EventId(s).map(_.value), Right(s), s"EventId('$s')")
      assertEquals(ConditionId(s).map(_.value), Right(s), s"ConditionId('$s')")
      assertEquals(FactorId(s).map(_.value), Right(s), s"FactorId('$s')")
    }

  test("ids do reject the genuinely invalid"):
    assert(ColumnId("").isLeft)
    assert(ColumnId("   ").isLeft)
    assert(TermId("").isLeft)

  test("ids reject what they would otherwise have to rewrite"):
    // Rejecting is how parsing stays both total on what it accepts and
    // injective: a trimming parser maps " x " and "x" onto one id.
    assert(ColumnId(" onset").isLeft, "leading whitespace")
    assert(ColumnId("onset ").isLeft, "trailing whitespace")
    assert(ColumnId("on\tset").isLeft, "embedded control character")
    assert(TermId("task\n").isLeft, "trailing newline")
