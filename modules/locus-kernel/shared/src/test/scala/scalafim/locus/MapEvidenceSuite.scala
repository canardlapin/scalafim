package scalafim.locus

class MapEvidenceSuite extends munit.FunSuite:
  private sealed trait X
  private sealed trait Y

  test("injection, surjection, and bijection are validated evidence"):
    val two = FiniteSpace.make[X](SpaceKey.unsafe("evidence:two"), 2).toOption.get
    val three = FiniteSpace.make[Y](SpaceKey.unsafe("evidence:three"), 3).toOption.get
    val injection = TotalMap.fromTargetOrdinals(two, three, Array(2, 0)).toOption.get

    assert(Injection.validate(injection).isRight)
    assertEquals(Surjection.validate(injection), Left(MapEvidenceError.MissingTarget(1)))
    assert(Bijection.validate(injection).isLeft)

    val threeSource = FiniteSpace.make[X](SpaceKey.unsafe("evidence:three-source"), 3).toOption.get
    val twoTarget = FiniteSpace.make[Y](SpaceKey.unsafe("evidence:two-target"), 2).toOption.get
    val surjection = TotalMap.fromTargetOrdinals(threeSource, twoTarget, Array(0, 1, 0)).toOption.get

    assert(Surjection.validate(surjection).isRight)
    assertEquals(
      Injection.validate(surjection),
      Left(MapEvidenceError.DuplicateTarget(0, 0, 2))
    )

    val twoTargetAgain = FiniteSpace.make[Y](SpaceKey.unsafe("evidence:two-again"), 2).toOption.get
    val bijection = TotalMap.fromTargetOrdinals(two, twoTargetAgain, Array(1, 0)).toOption.get
    assert(Bijection.validate(bijection).isRight)

  test("the empty map is bijective exactly when both spaces are empty"):
    val emptySource = FiniteSpace.make[X](SpaceKey.unsafe("evidence:empty-source"), 0).toOption.get
    val emptyTarget = FiniteSpace.make[Y](SpaceKey.unsafe("evidence:empty-target"), 0).toOption.get
    val nonEmptyTarget = FiniteSpace.make[Y](SpaceKey.unsafe("evidence:nonempty-target"), 1).toOption.get

    val emptyBijection =
      TotalMap.fromTargetOrdinals(emptySource, emptyTarget, Array.emptyIntArray).toOption.get
    val emptyInjection =
      TotalMap.fromTargetOrdinals(emptySource, nonEmptyTarget, Array.emptyIntArray).toOption.get

    assert(Bijection.validate(emptyBijection).isRight)
    assert(Injection.validate(emptyInjection).isRight)
    assertEquals(
      Surjection.validate(emptyInjection),
      Left(MapEvidenceError.MissingTarget(0))
    )
