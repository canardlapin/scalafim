package scalafim.inference

import gale.linalg.DVec
import scalafim.multivar.Spectrum

class CoreTypesSuite extends munit.FunSuite:

  private def accepted[A](value: Either[InferenceError, A]): A =
    value.fold(error => fail(error.message), identity)

  test("validated scalar types reject invalid boundary values") {
    assert(UnitId("").isLeft)
    assert(UnitId(" padded ").isLeft)
    assert(ComponentIx(-1).isLeft)
    assert(RowCount(0).isLeft)
    assert(MonteCarloDraws(0).isLeft)
    assert(Alpha(0.0).isLeft)
    assert(Alpha(1.0).isLeft)
    assert(PValue(-0.01).isLeft)
    assert(PValue(1.01).isLeft)
    assert(PValue(Double.NaN).isLeft)

    assertEquals(accepted(UnitId("axis-1")).value, "axis-1")
    assertEquals(accepted(ComponentIx(0)).value, 0)
    assertEqualsDouble(accepted(PValue(1.0)).value, 1.0, 0.0)
  }

  test("component sets are non-empty, ordered, unique, and rank checked") {
    val zero = accepted(ComponentIx(0))
    val one = accepted(ComponentIx(1))
    val two = accepted(ComponentIx(2))

    assert(ComponentSet.from(Vector.empty).isLeft)
    assert(ComponentSet.from(Vector(one, zero)).isLeft)
    assert(ComponentSet.from(Vector(one, one)).isLeft)

    val components = accepted(ComponentSet.from(Vector(zero, one)))
    assertEquals(components.components.map(_.value), Vector(0, 1))
    assert(components.requireWithin(2).isRight)
    assert(accepted(ComponentSet.from(Vector(zero, two))).requireWithin(2).isLeft)
    assert(SubspaceComponents.from(ComponentSet.one(zero)).isLeft)
    assert(SubspaceComponents.from(components).isRight)
  }

  test("latent units expose identifiability instead of pretending tied axes are unique") {
    val zero = accepted(ComponentIx(0))
    val one = accepted(ComponentIx(1))
    val axis = LatentUnit.Axis(accepted(UnitId("axis-1")), zero)
    val subspace = LatentUnit.Subspace(
      accepted(UnitId("plane-1")),
      accepted(SubspaceComponents.from(accepted(ComponentSet.from(Vector(zero, one)))))
    )

    assertEquals(axis.identifiability, Identifiability.OrientableAxis)
    assertEquals(subspace.identifiability, Identifiability.UnorientedSubspace)
    assertEquals(subspace.componentSet.components.map(_.value), Vector(0, 1))
  }

  test("row partitions must cover each row exactly once") {
    val rows = accepted(RowCount(4))
    val partition = accepted(RowPartition.from(rows, Vector(Vector(0, 2), Vector(1, 3))))

    assertEquals(partition.groups.map(_.map(_.value)), Vector(Vector(0, 2), Vector(1, 3)))
    assert(RowPartition.from(rows, Vector(Vector(0, 1), Vector(1, 2, 3))).isLeft)
    assert(RowPartition.from(rows, Vector(Vector(0, 1), Vector(2))).isLeft)
    assert(RowPartition.from(rows, Vector(Vector(0, 1), Vector(2, 4))).isLeft)
  }

  test("ordered spectra retain their mathematical meaning") {
    val covariance = accepted(OrderedSpectrum.from(
      Spectrum.Covariance(InferenceNumerics.vectorFromSeq(Vector(3.0, 1.0, 0.0)))
    ))
    val correlations = accepted(OrderedSpectrum.from(
      Spectrum.CanonicalCorrelations(InferenceNumerics.vectorFromSeq(Vector(0.9, 0.4)))
    ))

    assertEquals(covariance.kind, OrderedSpectrumKind.Covariance)
    assertEquals(correlations.kind, OrderedSpectrumKind.CanonicalCorrelations)
    assert(OrderedSpectrum.from(Spectrum.Eigenvalues(InferenceNumerics.vectorFromSeq(Vector(1.0, 2.0)))).isLeft)
    assert(OrderedSpectrum.from(Spectrum.SingularValues(InferenceNumerics.vectorFromSeq(Vector(1.0, -0.1)))).isLeft)
  }

  test("unrequested and unavailable evidence are distinct states") {
    val unrequested: Evidence[Double] = Evidence.NotRequested
    val unavailable: Evidence[Double] = Evidence.Unavailable(
      UnavailableReason.InsufficientRank(expected = 3, actual = 2)
    )

    assertNotEquals(unrequested, unavailable)
    assertEquals(Evidence.Computed(0.25), Evidence.Computed(0.25))
  }

  test("invalid validity declarations return typed errors") {
    val id = accepted(AssumptionId("exchangeability"))
    assert(DeclaredAssumption.from(id, "").isLeft)
    assert(DeclaredAssumption.from(id, " rows are exchangeable ").isLeft)
    assert(DeclaredAssumption.from(id, "rows are exchangeable").isRight)

    assert(ValidityDowngrade.from(
      ValidityClaim.Exact,
      ValidityClaim.Exact,
      "unchecked dependence"
    ).isLeft)
    assert(ValidityDowngrade.from(
      ValidityClaim.Exact,
      ValidityClaim.Conditional,
      "unchecked dependence"
    ).isRight)
  }

  test("latent unit formation matches singleton and near-tie reference policies") {
    InferenceRReferenceFixtures.unitFormation.foreach { fixture =>
      val policy =
        if fixture.groupNearTies then
          UnitPolicy.GroupNearTies(accepted(RelativeGap(fixture.tieThreshold)))
        else UnitPolicy.SingleAxes
      val units = accepted(LatentUnitFormation.form(fixture.roots, fixture.selectedInput, policy))

      assertEquals(units.map(_.unit.id.value), fixture.units.map(_.id))
      assertEquals(
        units.map(_.unit.componentSet.components.map(_.value + 1)),
        fixture.units.map(_.membersOneBased)
      )
      assertEquals(units.map(_.selected), fixture.units.map(_.selected))
      assertEquals(
        units.map(_.unit.identifiability == Identifiability.OrientableAxis),
        fixture.units.map(_.identifiable)
      )
    }
  }
