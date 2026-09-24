package scalafim.image.world

class IdentifiersSuite extends munit.FunSuite:
  test("identifiers trim surrounding whitespace"):
    assertEquals(SubjectId("  sub-01 ").map(_.value), Right("sub-01"))
    assertEquals(SessionId("ses-02").map(_.value), Right("ses-02"))
    assertEquals(TemplateName("MNI152NLin2009cAsym").map(_.value), Right("MNI152NLin2009cAsym"))

  test("blank identifiers are rejected with their label"):
    assertEquals(SubjectId("   "), Left(SpaceError.EmptyIdentifier("subject")))
    assertEquals(SessionId(""), Left(SpaceError.EmptyIdentifier("session")))
    assertEquals(TemplateName("\t"), Left(SpaceError.EmptyIdentifier("template")))
    assertEquals(SpaceError.EmptyIdentifier("subject").message, "subject identifier must be non-empty")
