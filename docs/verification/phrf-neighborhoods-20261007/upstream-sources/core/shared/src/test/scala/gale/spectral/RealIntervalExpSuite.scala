package gale.spectral

class RealIntervalExpSuite extends munit.FunSuite:
  test("Taylor exponential encloses finite values and underflow on both runtimes"):
    Vector(-1000.0, -745.0, -700.0, -100.0, -10.0, -1.0, -0.1, 0.0, 0.1, 1.0, 10.0, 100.0, 700.0).foreach: x =>
      val e = RealInterval.exact(x).flatMap(_.exp).fold(error => fail(error.toString), identity)
      val reference = math.exp(x)
      assert(e.lower <= reference && reference <= e.upper, s"$x: [${e.lower}, ${e.upper}], $reference")
      assert(e.lower >= 0.0)
      if reference > 1e-300 then assert((e.upper - e.lower) / reference < 1e-10)
    val zero = RealInterval.exact(0.0).flatMap(_.exp).toOption.get
    assertEqualsDouble(zero.lower, 1.0, 0.0)
    assertEqualsDouble(zero.upper, 1.0, 0.0)
    assert(RealInterval.exact(1000.0).flatMap(_.exp).isLeft)

  test("interval exponential covers interior points and preserves endpoint order"):
    val e = RealInterval.checked(-0.4, 0.7).flatMap(_.exp).toOption.get
    (0 to 100).foreach: i =>
      val value = math.exp(-0.4 + 1.1 * i / 100.0)
      assert(e.lower <= value && value <= e.upper)

  test("exponential encloses independent 80-digit decimal brackets"):
    // Python decimal exp at 110 digits, bracketed outward to 80 significant
    // digits; endpoint conversion below is exact, not a second math.exp call.
    Vector(
      (-10.0, "0.000045399929762484851535591515560550610237918088866564969259071305650999421614302281", "0.000045399929762484851535591515560550610237918088866564969259071305650999421614302282"),
      (-1.0, "0.36787944117144232159552377016146086744581113103176783450783680169746149574489980", "0.36787944117144232159552377016146086744581113103176783450783680169746149574489981"),
      (1.0, "2.7182818284590452353602874713526624977572470936999595749669676277240766303535475", "2.7182818284590452353602874713526624977572470936999595749669676277240766303535476"),
      (10.0, "22026.465794806716516957900645284244366353512618556781074235426355225202818570792", "22026.465794806716516957900645284244366353512618556781074235426355225202818570793"),
      (700.0, "1.0142320547350045094553295952312676152046795722430733487805362812493517025075236E+304", "1.0142320547350045094553295952312676152046795722430733487805362812493517025075237E+304")
    ).foreach: (x, lower, upper) =>
      val enclosed = RealInterval.exact(x).flatMap(_.exp).toOption.get
      assert(BigDecimal.exact(enclosed.lower).bigDecimal.compareTo(new java.math.BigDecimal(lower)) <= 0)
      assert(BigDecimal.exact(enclosed.upper).bigDecimal.compareTo(new java.math.BigDecimal(upper)) >= 0)
