package scalafim.dataset

class SubjectIdSuite extends munit.FunSuite:
  test("shared and dataset subject ids preserve the literal normalization grammar"):
    val cases = Vector(
      "sub-01" -> "sub-01",
      " \tsub-01\r\n" -> "sub-01",
      "study/participant" -> "study/participant",
      "study A/participant 1" -> "study A/participant 1",
      "\u00a0" -> "\u00a0",
      "\u2003subject\u2003" -> "\u2003subject\u2003",
      "研究/参加者" -> "研究/参加者",
      "sub\u0000ject" -> "sub\u0000ject"
    )
    cases.foreach: (raw, expected) =>
      val dataset = SubjectId.make(raw).toOption.get
      val shared = neuroim4s.spaces.SubjectId(raw).toOption.get
      assertEquals(dataset.value, expected)
      assertEquals(shared.value, expected)
      assertEquals(SubjectId.value(dataset), expected)
      assertEquals(SubjectId(raw).value, expected)

  test("every ASCII trim character preserves both empty rejection and interior text"):
    (0 to 32).foreach: code =>
      val padding = code.toChar.toString
      val empty = padding + padding
      assertEquals(SubjectId.make(empty),
        Left(DatasetError.InvalidLabel("SubjectId", empty, "must be non-empty")))
      assert(neuroim4s.spaces.SubjectId(empty).isLeft)
      val value = padding + "subject" + padding
      assertEquals(SubjectId.make(value).toOption.get.value, "subject")
      assertEquals(neuroim4s.spaces.SubjectId(value).toOption.get.value, "subject")

  test("dataset ingress preserves empty-label errors and throwing constructor message"):
    Vector("", " ", "\t\r\n").foreach: raw =>
      assertEquals(SubjectId.make(raw),
        Left(DatasetError.InvalidLabel("SubjectId", raw, "must be non-empty")))
      val error = intercept[IllegalArgumentException](SubjectId(raw))
      assertEquals(error.getMessage, "requirement failed: SubjectId must be non-empty")

  test("dataset and shared subjects are the same type in both directions"):
    val shared: neuroim4s.spaces.SubjectId = SubjectId(" study/participant ")
    val dataset: SubjectId = shared
    assertEquals(dataset.value, "study/participant")
    assertEquals(SubjectId.make("study/participant"), Right(shared))
    assertEquals(Set(dataset, shared).size, 1)
