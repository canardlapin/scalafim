package scalafim.fmri.mvpa

class FeatureModelSuite extends munit.FunSuite:
  test("feature model design validates labels, dimensions, and finite values") {
    val duplicate = FeatureModelDesign(
      Vector("a", "a"),
      GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0)))
    )
    val nonFinite = FeatureModelDesign(
      Vector("a", "b"),
      GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(Double.NaN)))
    )

    assert(duplicate.swap.toOption.get.message.contains("unique"))
    assert(nonFinite.swap.toOption.get.message.contains("non-finite"))
  }


  test("constant standardized source predicts the training target mean"):
    val x = GaleTestMatrix.fromRows(Vector.fill(3)(Vector(0.0)))
    val y = GaleTestMatrix.fromRows(Vector(Vector(10.0), Vector(20.0), Vector(30.0)))
    val model = StandardizedRidgeMap.fit(x, y, 1.0).toOption.get
    val prediction = model.predict(GaleTestMatrix.fromRows(Vector(Vector(0.0), Vector(0.0)))).toOption.get
    assertEqualsDouble(model.coefficients(0, 0), 0.0, 1e-12)
    assertEqualsDouble(prediction(0, 0), 20.0, 1e-12)
    assertEqualsDouble(prediction(1, 0), 20.0, 1e-12)
