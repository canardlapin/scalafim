package scalafim.atlas

import image4s.Continuous
import locus4s.data.Field
import scalafim.atlas.syntax.*
import scalafim.image.*

class AtlasReductionSuite extends munit.FunSuite:
  private def atlas(): VolumeAtlas =
    VolumeAtlas.fromLabelVolume(AtlasRef.volume("reduction", "Policy", SpaceId.Custom, SpaceId.Custom),
      RegionIndex(Vector(AtlasRegionMetadata.fromStrings(RegionId(9), "Second"), AtlasRegionMetadata.fromStrings(RegionId(2), "First"))),
      AtlasTestImages.labelVolume(SampleSpaces(Vector(5, 1, 1)), Array(2, 2, 9, 9, 0)))

  private def assertSame(actual: Double, expected: Double): Unit =
    if expected.isNaN then assert(actual.isNaN)
    else if expected.isInfinity then assertEquals(actual, expected)
    else assertEqualsDouble(actual, expected, 0.0)

  test("scalar and one-timepoint series share every missing and empty policy"):
    val a = atlas()
    val r: a.realization.type = a.realization
    val scenarios = Vector(
      Array(1.0, 3.0, 10.0, 20.0, Double.NaN),
      Array(1.0, Double.NaN, Double.NaN, Double.NaN, Double.NaN),
      Array(Double.PositiveInfinity, 3.0, 10.0, 20.0, Double.NaN)
    )
    val masks = Vector(None, Some(Array(true, false, false, false, true)), Some(Array.fill(5)(false)))
    val reducers = Vector(ParcelReducer.Mean, ParcelReducer.Sum, ParcelReducer.Custom(_.sum))
    val missing = Vector(MissingValuePolicy.SkipNaN, MissingValuePolicy.PropagateNaN, MissingValuePolicy.RejectNaN)
    val empty = Vector(EmptyParcelPolicy.Reject, EmptyParcelPolicy.Fill(-77.0))
    for values <- scenarios; flags <- masks; reducer <- reducers; nan <- missing; noSamples <- empty do
      val mask = flags.map(AtlasTestImages.maskVolume(a, _))
      val policy = ParcelReductionPolicy(nan, noSamples)
      val scalar: Either[AtlasError, Field[r.P, Double]] = a.reduceEither(AtlasTestImages.scalarVolume(a, values), reducer, mask, policy)
      val series: Either[AtlasError, ParcelSeries[r.F, r.X, r.P, Double, Continuous]] =
        a.reduceSeriesEither(AtlasTestImages.scalarSeries(a, values, 1), reducer, mask, policy)
      (scalar, series) match
        case (Right(field), Right(result)) =>
          assert(result.parcellation.parcels.sameRuntimeOwnerAs(r.parcelDomain))
          val slice: Field[r.P, Double] = result.fieldAt(0).toOption.get
          field.toVector.zip(slice.toVector).foreach((actual, expected) => assertSame(actual, expected))
        case (Left(first), Left(second)) => assertEquals(first, second)
        case other => fail(s"scalar/series policy mismatch: $other")

  test("all-NaN is not empty support and custom skip receives an empty array"):
    val a = atlas()
    val r: a.realization.type = a.realization
    val data = AtlasTestImages.scalarVolume(a, Array(Double.NaN, Double.NaN, 10.0, 20.0, Double.NaN))
    val mean = a.reduce(data, policy = ParcelReductionPolicy(empty = EmptyParcelPolicy.Reject))
    val sum = a.reduce(data, ParcelReducer.Sum, policy = ParcelReductionPolicy(empty = EmptyParcelPolicy.Fill(-99.0)))
    assert(mean(r.parcelPoint(RegionId(2)).get).isNaN)
    assertEqualsDouble(sum(r.parcelPoint(RegionId(2)).get), 0.0, 0.0)
    val lengths = scala.collection.mutable.ArrayBuffer.empty[Int]
    val reducer = ParcelReducer.Custom: values =>
      lengths += values.length
      values.length.toDouble
    val custom = a.reduce(data, reducer, policy = ParcelReductionPolicy(empty = EmptyParcelPolicy.Fill(-99.0)))
    assertEqualsDouble(custom(r.parcelPoint(RegionId(2)).get), 0.0, 0.0)
    assertEquals(lengths.toVector.sorted, Vector(0, 2))

  test("mean and sum have explicit independent expectations under every NaN policy"):
    val a = atlas()
    val r: a.realization.type = a.realization
    val data = AtlasTestImages.scalarVolume(a, Array(1.0, Double.NaN, 10.0, 20.0, Double.NaN))
    Vector(ParcelReducer.Mean -> 15.0, ParcelReducer.Sum -> 30.0).foreach: (reducer, expectedSecond) =>
      val skipped = a.reduce(data, reducer)
      assertEqualsDouble(skipped(r.parcelPoint(RegionId(2)).get), 1.0, 0.0)
      assertEqualsDouble(skipped(r.parcelPoint(RegionId(9)).get), expectedSecond, 0.0)
      val propagated = a.reduce(data, reducer, policy = ParcelReductionPolicy(MissingValuePolicy.PropagateNaN))
      assert(propagated(r.parcelPoint(RegionId(2)).get).isNaN)
      assertEqualsDouble(propagated(r.parcelPoint(RegionId(9)).get), expectedSecond, 0.0)
      assertEquals(a.reduceEither(data, reducer, policy = ParcelReductionPolicy(MissingValuePolicy.RejectNaN)),
        Left(AtlasError.Reduction(AtlasReductionError.MissingValue(RegionId(2), 0))))

  test("propagation bypasses callbacks and reject reports the selected parcel and time"):
    val a = atlas()
    val r: a.realization.type = a.realization
    var calls = 0
    val data = AtlasTestImages.scalarVolume(a, Array(Double.NaN, 3.0, 10.0, 20.0, Double.NaN))
    val reducer = ParcelReducer.Custom: values =>
      calls += 1
      values.sum
    val result = a.reduce(data, reducer, policy = ParcelReductionPolicy(MissingValuePolicy.PropagateNaN))
    assert(result(r.parcelPoint(RegionId(2)).get).isNaN)
    assertEquals(calls, 1)
    val series = AtlasTestImages.scalarSeries(a, Array(1.0, Double.NaN, 3.0, 4.0, 10.0, 20.0, 30.0, 40.0, Double.NaN, Double.NaN), 2)
    Vector(ParcelReducer.Mean, ParcelReducer.Sum, ParcelReducer.Custom(_.sum)).foreach: reducer =>
      assertEquals(a.reduceSeriesEither(series, reducer, policy = ParcelReductionPolicy(MissingValuePolicy.RejectNaN)),
        Left(AtlasError.Reduction(AtlasReductionError.MissingValue(RegionId(2), 1))))
    val masked = AtlasTestImages.maskVolume(a, Array(false, true, true, true, true))
    assert(a.reduceEither(data, mask = Some(masked), policy = ParcelReductionPolicy(MissingValuePolicy.RejectNaN)).isRight)

  test("empty support fills or rejects before callbacks"):
    val a = atlas()
    val r: a.realization.type = a.realization
    var calls = 0
    val reducer = ParcelReducer.Custom: values =>
      calls += 1
      values.sum
    val mask = AtlasTestImages.maskVolume(a, Array.fill(5)(false))
    val data = AtlasTestImages.scalarVolume(a, Array.fill(5)(Double.NaN))
    assertEquals(a.reduce(data, reducer, Some(mask), ParcelReductionPolicy(empty = EmptyParcelPolicy.Fill(-42.0))).toVector, Vector(-42.0, -42.0))
    assertEquals(calls, 0)
    val expected = AtlasReductionError.EmptyParcel(r.metadata(r.parcelDomain.indexAtValidatedOrdinal(0)).id)
    assertEquals(a.reduceEither(data, reducer, Some(mask), ParcelReductionPolicy(empty = EmptyParcelPolicy.Reject)), Left(AtlasError.Reduction(expected)))

  test("custom arrays are isolated across samples and lookups never rerun callbacks"):
    val a = atlas()
    val retained = scala.collection.mutable.ArrayBuffer.empty[Array[Double]]
    val reducer = ParcelReducer.Custom: values =>
      retained += values
      val sum = values.sum
      values(0) = -999.0
      sum
    val series = a.reduceSeries(AtlasTestImages.scalarSeries(a, Array(1.0, 2.0, 3.0, 4.0, 10.0, 20.0, 30.0, 40.0, 100.0, 200.0), 2), reducer)
    assertEquals(retained.size, 4)
    assertEquals(retained.map(_.last).toVector, Vector(30.0, 40.0, 3.0, 4.0))
    retained(0)(1) = -888.0
    assertEqualsDouble(series.data(0, 0), 40.0, 0.0)
    assertEqualsDouble(series.fieldAt(0).toOption.get.toVector.head, 40.0, 0.0)
    assertEquals(retained.size, 4)

  test("field results snapshot input values and custom failures remain typed"):
    val a = atlas()
    val r: a.realization.type = a.realization
    val values = Array(1.0, 3.0, 10.0, 20.0, 0.0)
    val input = Field.view(r.parcelAssignment.from)(p => values(p.ordinal))
    val reduced: Field[r.P, Double] = right(AtlasReduce.reduceField(r)(input))
    values(0) = 999.0
    assertEqualsDouble(reduced(r.parcelPoint(RegionId(2)).get), 2.0, 0.0)
    val failing = ParcelReducer.Custom(_ => throw new IllegalStateException("callback exploded"))
    AtlasReduce.reduceField(r)(input, failing) match
      case Left(AtlasReductionError.CallbackFailed(id, sample, detail)) =>
        assertEquals(id, RegionId(9))
        assertEquals(sample, 0)
        assertEquals(detail, "callback exploded")
      case other => fail(s"expected typed callback error, got $other")

  test("scalar and series masks reject an independent equal-shaped grid owner"):
    val a = atlas()
    val foreign = atlas()
    val mask = AtlasTestImages.maskVolume(foreign, Array.fill(5)(true))
    val scalar = a.reduceEither(AtlasTestImages.scalarVolume(a, Array.fill(5)(1.0)), mask = Some(mask))
    val series = a.reduceSeriesEither(AtlasTestImages.scalarSeries(a, Array.fill(5)(1.0), 1), mask = Some(mask))
    assert(scalar.swap.toOption.get.isInstanceOf[AtlasError.ExactGridRequired])
    assert(series.swap.toOption.get.isInstanceOf[AtlasError.ExactGridRequired])

  private def right[A](result: Either[?, A]): A = result.fold(error => fail(error.toString), identity)
