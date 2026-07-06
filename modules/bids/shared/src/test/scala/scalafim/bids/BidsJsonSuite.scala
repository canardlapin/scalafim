package scalafim.bids

class BidsJsonSuite extends munit.FunSuite:
  private def value[A](e: Either[BidsError, A]): A =
    e.fold(err => fail(err.message), identity)

  test("JSON parser reads nested values and escapes"):
    val json =
      value(BidsJson.parseObject("""{"Name":"Fixture\nStudy","Nested":{"A":1.5},"Flags":[true,false,null],"Unicode":"\u03b2"}"""))

    assertEquals(json.fields("Name").asString, Some("Fixture\nStudy"))
    assertEquals(json.fields("Nested").asObject.flatMap(_("A").asNumber), Some(1.5))
    assertEquals(json.fields("Unicode").asString, Some("\u03b2"))

  test("JSON parser rejects invalid numbers and unescaped control characters"):
    assert(BidsJson.parse("""{"x":1.}""").isLeft)
    assert(BidsJson.parse("""{"x":1e}""").isLeft)
    assert(BidsJson.parse("""{"x":01}""").isLeft)
    assert(BidsJson.parse("{\"x\":\"line\nbreak\"}").isLeft)

  test("JSON parser keeps last duplicate object key"):
    val json = value(BidsJson.parseObject("""{"x":1,"x":2}"""))

    assertEquals(json.fields("x").asNumber, Some(2.0))
