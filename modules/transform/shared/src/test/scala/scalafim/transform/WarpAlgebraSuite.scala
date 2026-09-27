package scalafim.transform

import image4s.{BoundaryPolicy, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, D3, Frame, Grid, GridId, LatticeIndex, Point}
import ravel.DType.given
import ravel.NDArray
import reframe4s.core.{InverseDirection, MapError}
import reframe4s.field.{
  CoordinateBoundaryPolicy,
  CompositionError,
  DeterminantDirection,
  DeterminantValue,
  InversePointStatus,
  InversionError,
  InversionGateFailure,
  LogDeterminant,
  MaterializedOutcome
}
import reframe4s.lie.FramedAffine
import reframe4s.resample.VolumeModulation
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.field.DenseLattice

/** STP Phase 6: typed world-transform adapters over reframe4s materialization, numerical inversion, Jacobian
  * determinants and modulated resampling, checked against closed forms.
  */
class WarpAlgebraSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("warp algebra source")))
  private val middle: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("warp algebra middle")))
  private val target: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("warp algebra target")))

  private def axisAligned(origin: Vector[Double], spacing: Double): Affine[D3] =
    ok(Affine.fromOriginSpacingDirection[D3](origin, Vector.fill(3)(spacing), Vector(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)))

  private def grid[F <: Frame[D3] & Singleton](frame: F, shape: Vector[Int], affine: Affine[D3]): Grid[F, D3] =
    ok(Grid.forFrame[D3, F](frame)(shape, affine))

  private def persistentGrid[F <: Frame[D3] & Singleton](frame: F, id: String, shape: Vector[Int], affine: Affine[D3]): Grid[F, D3] =
    ok(GridId.parse(id).flatMap(key => Grid.createPersistent[D3, F](key, frame)(shape, affine)))

  private def indices(shape: Vector[Int]): Vector[Vector[Int]] =
    for i <- (0 until shape(0)).toVector; j <- 0 until shape(1); k <- 0 until shape(2) yield Vector(i, j, k)

  private def pointAt[F <: Frame[D3]](lattice: Grid[F, D3], index: Vector[Int]): Point[F, D3] =
    ok(LatticeIndex.fromVector[D3](index).flatMap(lattice.pointAt))

  private def world(affine: Affine[D3], x: Int, y: Int, z: Int): Vector[Double] =
    val m = affine.rowMajor
    Vector.tabulate(3)(r => m(4 * r) * x + m(4 * r + 1) * y + m(4 * r + 2) * z + m(4 * r + 3))

  /** A dense warp `target -> source` sampling `pull` on the lattice `(shape, affine)` of the target frame. */
  private def dense(
      shape: Vector[Int],
      affine: Affine[D3],
      boundary: CoordinateBoundaryPolicy = CoordinateBoundaryPolicy.Reject
  )(pull: Vector[Double] => Vector[Double]): WorldTransform.Mapped[source.type, target.type] =
    val field = ok(DenseLattice.pullback[source.type, target.type](target, source, shape, affine, boundary)((x, y, z) => pull(world(affine, x, y, z))))
    WorldTransform.Mapped(field, PushAvailability.Unavailable(), TransformProvenance.constructed("analytic dense warp"))

  private def linear[S <: Frame[D3] & Singleton, T <: Frame[D3] & Singleton](s: S, t: T, rowMajor: Double*): WorldTransform.Linear[S, T] =
    WorldTransform.Linear(FramedAffine.betweenFrames[T, S, D3](t, s)(ok(Affine.fromRowMajor[D3](rowMajor.toVector))), TransformProvenance.constructed("test affine"))

  private def close(actual: Vector[Double], expected: Vector[Double], tol: Double, clue: => String)(using munit.Location): Unit =
    actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, tol, clue))

  // ---------------------------------------------------------------- P6.01 materialize

  private val oblique = linear(source, target, 1.1, 0.05, -0.02, 3.0, -0.03, 0.95, 0.1, -12.5, 0.02, -0.08, 1.2, 7.25, 0, 0, 0, 1)

  test("an affine materialized on a lattice samples its pullback exactly, fully covered"):
    val lattice = grid(target, Vector(6, 5, 4), ok(Affine.fromRowMajor[D3](Vector(1.5, 0.2, 0.0, -4.0, 0.0, 1.25, 0.3, 2.0, 0.1, 0.0, 2.0, -1.0, 0, 0, 0, 1))))
    val materialized = ok(oblique.materialize(lattice))
    assertEquals(materialized.coverage.counts.covered, 120L)
    assertEquals(materialized.coverage.counts.total, 120L)
    indices(lattice.shape).foreach: index =>
      val y = pointAt(lattice, index)
      val sampled = Vector.tabulate(3)(c => ok(materialized.field.coordinates.valueAt(index, Vector(c))))
      close(sampled, ok(oblique.pullPoint(y)).coordinates, 1e-12, s"at $index")
    // the interpolant has no forward map of its own until it is inverted; the operation is recorded
    assert(materialized.transform.push.isEmpty)
    assert(materialized.transform.provenance.describe.contains("materialized on a 6x5x4 lattice"))

  test("a typed composition S -> M -> T materializes to one field equal to the lazy chain"):
    val shape = Vector(12, 10, 8)
    val affine = axisAligned(Vector(-6.0, -5.0, -4.0), 1.0)
    // M -> T: a dense warp on the target lattice; S -> M: an affine
    val warpMT: WorldTransform.Mapped[middle.type, target.type] =
      WorldTransform.Mapped(
        ok(DenseLattice.pullback[middle.type, target.type](target, middle, shape, affine, CoordinateBoundaryPolicy.Reject): (x, y, z) =>
          val p = world(affine, x, y, z)
          Vector(p(0) + 0.4 * math.sin(0.3 * p(1)), p(1) + 0.2 * math.cos(0.2 * p(2)), p(2) - 0.3 * math.sin(0.25 * p(0)))
        ),
        PushAvailability.Unavailable(),
        TransformProvenance.constructed("dense M -> T")
      )
    val affineSM = linear(source, middle, 0.98, 0.02, 0.0, 1.5, -0.01, 1.03, 0.0, -0.75, 0.0, 0.015, 0.97, 0.5, 0, 0, 0, 1)
    val composite: WorldTransform[source.type, target.type] = affineSM.andThen(warpMT)
    val lattice = grid(target, Vector(10, 8, 6), axisAligned(Vector(-4.5, -3.5, -2.5), 1.0))
    val field = ok(composite.materialize(lattice))
    assertEquals(field.coverage.counts.covered, 480L)
    indices(lattice.shape).foreach: index =>
      val sampled = Vector.tabulate(3)(c => ok(field.field.coordinates.valueAt(index, Vector(c))))
      close(sampled, ok(composite.pullPoint(pointAt(lattice, index))).coordinates, 1e-12, s"at $index")
    assert(compileErrors("warpMT.andThen(affineSM)").nonEmpty, "a middle-to-target warp must not compose with a source-to-middle affine")

  test("points that leave a stage's support are rejected by default and reported, never silently filled"):
    val warp = dense(Vector(8, 8, 8), axisAligned(Vector(0.0, 0.0, 0.0), 1.0))(y => y.map(_ + 0.25))
    // a lattice that reaches two samples past the warp's lattice on every axis
    val wide = grid(target, Vector(12, 12, 12), axisAligned(Vector(-2.0, -2.0, -2.0), 1.0))
    warp.materialize(wide) match
      case Left(TransformError.Composition(CompositionError.RejectedPoints(report))) =>
        assertEquals(report.counts.covered, 512L)
        assertEquals(report.counts.rejected, 1728L - 512L)
        assertEquals(report.outcomeAt(Vector(0, 0, 0)), Some(MaterializedOutcome.Rejected))
        assertEquals(report.coverageMask.valueAt(Vector(0, 0, 0)).toOption, Some(false))
        assertEquals(report.coverageMask.valueAt(Vector(5, 5, 5)).toOption, Some(true))
      case other => fail(s"expected a rejected-points report, got $other")
    val filled = ok(warp.materialize(wide, CoordinateBoundaryPolicy.Constant(Vector(0.0, 0.0, 0.0))))
    assertEquals(filled.coverage.counts.rejected, 1728L - 512L)
    assertEquals(filled.coverage.rejectedFill, CoordinateBoundaryPolicy.Constant(Vector(0.0, 0.0, 0.0)))

  test("coverage is attributed to the stage whose boundary policy supplied the value"):
    // ITK-style zero-displacement extension on the warp, then an affine: points outside the warp's lattice are
    // SourcePreserved by the warp stage, not reported as covered.
    val warpMT: WorldTransform.Mapped[middle.type, target.type] =
      WorldTransform.Mapped(
        ok(DenseLattice.pullback[middle.type, target.type](target, middle, Vector(8, 8, 8), axisAligned(Vector(0.0, 0.0, 0.0), 1.0), CoordinateBoundaryPolicy.PreserveSource)(
          (x, y, z) => Vector(x + 0.25, y - 0.25, z + 0.5)
        )),
        PushAvailability.Unavailable(),
        TransformProvenance.constructed("extended warp")
      )
    val shift = linear(source, middle, 1, 0, 0, 1.0, 0, 1, 0, 0.0, 0, 0, 1, 0.0, 0, 0, 0, 1)
    val lattice = grid(target, Vector(10, 8, 8), axisAligned(Vector(0.0, 0.0, 0.0), 1.0))
    val field = ok(shift.andThen(warpMT).materialize(lattice))
    assertEquals(field.coverage.counts.covered, 512L)
    assertEquals(field.coverage.counts.sourcePreserved, 128L)
    assertEquals(field.coverage.counts.rejected, 0L)
    assertEquals(field.coverage.outcomeAt(Vector(9, 0, 0)), Some(MaterializedOutcome.SourcePreserved))
    // x = 9 lies outside the warp lattice, so its displacement is zero and only the affine applies
    close(Vector.tabulate(3)(c => ok(field.field.coordinates.valueAt(Vector(9, 2, 3), Vector(c)))), Vector(10.0, 2.0, 3.0), 1e-12, "preserved point")

  // ---------------------------------------------------------------- P6.02 invertNumerically

  // pull(y) = y + a sin(w y) per axis; a w = 0.4 < 1, so the fixed-point iteration contracts and the inverse is unique.
  private val (amplitude, omega, spacing) = (1.0, 0.4, 0.5)
  private def sinusoid(y: Vector[Double]): Vector[Double] = y.map(v => v + amplitude * math.sin(omega * v))
  private def exactInverse(x: Double): Double =
    var y = x
    var step = 0
    while step < 60 do
      y -= (y + amplitude * math.sin(omega * y) - x) / (1.0 + amplitude * omega * math.cos(omega * y))
      step += 1
    y
  // Linear interpolation of the pull errs by at most a w^2 h^2 / 8, amplified by at most 1 / (1 - a w) in the inverse:
  // the bound on every lattice sample of the estimate. Between samples, linear interpolation of the inverse g adds at
  // most |g''| h^2 / 8, with |g''| = |f''(g)| / f'(g)^3 <= a w^2 / (1 - a w)^3.
  private val sampleBound = amplitude * omega * omega * spacing * spacing / 8.0 / (1.0 - amplitude * omega)
  private val betweenSamplesBound = amplitude * omega * omega / math.pow(1.0 - amplitude * omega, 3) * spacing * spacing / 8.0
  private lazy val sinusoidWarp = dense(Vector(24, 20, 16), axisAligned(Vector(0.0, 0.0, 0.0), spacing))(sinusoid)
  private lazy val inverseLattice = persistentGrid(source, "warp-algebra-inverse", Vector(20, 16, 12), axisAligned(Vector(1.0, 1.0, 1.0), spacing))
  private lazy val qualifying = ok(InversionPolicy.create(minimumCoverage = 1.0, maximumResidual = 0.05, p99Residual = 0.05))

  test("a dense warp gains a forward map only through a qualified numerical estimate"):
    val probe = ok(Point.fromVector(source, Vector(5.3, 4.1, 3.7)))
    assert(sinusoidWarp.mapPoint(probe).left.exists(_.isInstanceOf[TransformError.NoForwardMap]))
    val estimated = ok(sinusoidWarp.invertNumerically(inverseLattice, qualifying))
    val inverse = estimated.availability match
      case PushAvailability.Estimated(inverse) => inverse
      case other                               => fail(s"expected an estimated forward map, got $other")
    val bound = sampleBound
    close(ok(estimated.mapPoint(probe)).coordinates, Vector(5.3, 4.1, 3.7).map(exactInverse), sampleBound + betweenSamplesBound, "forward map at a probe")
    var worst = 0.0
    indices(inverseLattice.shape).foreach: index =>
      assertEquals(inverse.evidence.statusAt(index), Some(InversePointStatus.Converged))
      val estimate = Vector.tabulate(3)(c => ok(inverse.samples.coordinates.valueAt(index, Vector(c))))
      val exact = pointAt(inverseLattice, index).coordinates.map(exactInverse)
      worst = math.max(worst, estimate.zip(exact).map((a, e) => math.abs(a - e)).max)
    assert(worst <= bound + 1e-9, s"worst inverse sample error $worst exceeds the interpolation bound $bound")
    // evidence: full coverage, both residual directions within the gates, and the record in the provenance
    assertEqualsDouble(inverse.evidence.coveredFraction, 1.0, 0.0)
    val reverse = inverse.evidence.reverseResidual.getOrElse(fail("no reverse residual"))
    assert(reverse.maximum <= 0.05 && reverse.p99 <= reverse.maximum, s"$reverse")
    assert(inverse.evidence.forwardResidual.exists(_.maximum <= 1e-7))
    assertEqualsDouble(inverse.estimate.residual.coveredFraction, 1.0, 0.0)
    assert(estimated.provenance.describe.contains("numerical inverse on a 20x16x12 lattice"))
    // outside its evaluation domain the estimate refuses instead of extrapolating
    estimated.mapPoint(ok(Point.fromVector(source, Vector(-5.0, -5.0, -5.0)))) match
      case Left(TransformError.Map(MapError.OutsideDomain(_))) => ()
      case other                                               => fail(s"expected OutsideDomain, got $other")
    // the estimated pair can be swapped, which is exactly what the evidence licenses
    assert(estimated.invert.isRight)
    // the estimate is bound to the pullback it was qualified against: it cannot be re-paired with another one
    intercept[IllegalArgumentException](estimated.copy(pull = sinusoidWarp.andThen(oblique.inverse.andThen(oblique)).pull))
    // a forward map read from an inverse asset is never silently replaced by an estimate
    val paired = WorldTransform.Mapped(sinusoidWarp.pull, PushAvailability.FromAsset(inverse.estimate.value, AssetRef("invwarp", None)), sinusoidWarp.provenance)
    assert(paired.invertNumerically(inverseLattice, qualifying).left.exists(_.isInstanceOf[TransformError.Invalid]))

  test("an estimate whose evidence fails the declared tolerance is refused with the evidence"):
    val strict = ok(InversionPolicy.create(minimumCoverage = 1.0, maximumResidual = 1e-6, p99Residual = 1e-6))
    sinusoidWarp.invertNumerically(inverseLattice, strict) match
      case Left(TransformError.Inversion(InversionError.GatesFailed(failures, evidence))) =>
        assert(
          failures.exists:
            case InversionGateFailure.MaximumResidualExceeded(InverseDirection.Reverse, observed, 1e-6) => observed > 1e-6
            case _                                                                                    => false
          ,
          failures.toString
        )
        assertEqualsDouble(evidence.coveredFraction, 1.0, 0.0)
      case other => fail(s"expected a gate failure, got $other")
    // a source lattice reaching past the pull's support fails the coverage gate
    val wide = persistentGrid(source, "warp-algebra-inverse-wide", Vector(18, 16, 14), axisAligned(Vector(-2.0, -2.0, -2.0), 0.75))
    sinusoidWarp.invertNumerically(wide, qualifying) match
      case Left(TransformError.Inversion(InversionError.GatesFailed(failures, evidence))) =>
        assert(failures.exists(_.isInstanceOf[InversionGateFailure.CoverageBelowMinimum]), failures.toString)
        assert(evidence.statusCounts.outsideCoverage > 0L)
      case other => fail(s"expected a coverage gate failure, got $other")
    // a folding warp diverges in the interior and fails the divergence gate
    // (a w = 2: the map folds and the fixed-point iteration is expansive wherever |cos(y)| > 1/2)
    val folding = dense(Vector(40, 16, 16), axisAligned(Vector(-5.0, -2.0, -2.0), spacing))(y => Vector(y(0) + 2.0 * math.sin(y(0)), y(1), y(2)))
    val foldLattice = persistentGrid(source, "warp-algebra-inverse-fold", Vector(20, 4, 4), axisAligned(Vector(0.0, 1.0, 1.0), spacing))
    val lenient = ok(InversionPolicy.create(minimumCoverage = 0.0, maximumResidual = 1.0, p99Residual = 1.0))
    folding.invertNumerically(foldLattice, lenient) match
      case Left(TransformError.Inversion(InversionError.GatesFailed(failures, evidence))) =>
        assert(evidence.interiorStatusCounts.diverged > 0L)
        assert(failures.contains(InversionGateFailure.InteriorDivergence(evidence.interiorStatusCounts.diverged)), failures.toString)
      case other => fail(s"expected a divergence gate failure, got $other")

  test("invalid policies and non-persistent lattices are typed errors; composites must be materialized first"):
    assert(InversionPolicy.create(minimumCoverage = 1.5, maximumResidual = 0.1, p99Residual = 0.1).left.exists(_.isInstanceOf[TransformError.Inversion]))
    assert(InversionPolicy.create(minimumCoverage = 0.9, maximumResidual = 0.1, p99Residual = 0.1, maximumIterations = 0).isLeft)
    val transient = grid(source, Vector(20, 16, 12), axisAligned(Vector(1.0, 1.0, 1.0), spacing))
    assert(sinusoidWarp.invertNumerically(transient, qualifying).left.exists(_.isInstanceOf[TransformError.Inversion]))
    val bridge = linear(target, middle, 1, 0, 0, 0.5, 0, 1, 0, -0.25, 0, 0, 1, 0.0, 0, 0, 0, 1)
    val chained = sinusoidWarp.andThen(bridge)
    val inverseOnChain = persistentGrid(source, "warp-algebra-inverse-chain", Vector(16, 12, 10), axisAligned(Vector(1.5, 1.5, 1.5), spacing))
    assert(chained.invertNumerically(inverseOnChain, qualifying).left.exists(_.isInstanceOf[TransformError.NeedsMaterialization]))
    val lattice = grid(middle, Vector(22, 18, 14), axisAligned(Vector(-0.25, 0.5, 0.25), spacing))
    val estimated = ok(ok(chained.materialize(lattice)).invertNumerically(inverseOnChain, qualifying))
    val x = Vector(4.0, 3.5, 3.0)
    val y = ok(estimated.mapPoint(ok(Point.fromVector(source, x)))).coordinates
    // the materialized chain interpolates the same sinusoid on a lattice of the same spacing: the sinusoid bounds apply
    close(ok(chained.pullPoint(ok(Point.fromVector(middle, y)))).coordinates, x, sampleBound + betweenSamplesBound, "pull(push(x)) = x through the materialized chain")
    // a field whose rejected points were filled is not the transform, and is not inverted
    val filled = ok(chained.materialize(grid(middle, Vector(26, 22, 18), axisAligned(Vector(-1.25, -0.5, -0.75), spacing)), CoordinateBoundaryPolicy.Constant(Vector(0.0, 0.0, 0.0))))
    assert(filled.coverage.counts.rejected > 0L)
    filled.invertNumerically(inverseOnChain, qualifying) match
      case Left(TransformError.Composition(CompositionError.RejectedPoints(_))) => ()
      case other                                                                => fail(s"expected the filled field to be refused, got $other")

  // ---------------------------------------------------------------- P6.03 Jacobian determinants

  private def determinantAt[S <: Frame[D3], T <: Frame[D3]](field: JacobianDeterminant[S, T], index: Vector[Int])(using munit.Location): Double =
    field.at(index) match
      case Some(DeterminantValue.Regular(value)) => value
      case other                                 => fail(s"expected a regular determinant at $index, got $other")

  test("an affine has the constant determinant det(A); Push is its reciprocal and the log is finite"):
    val lattice = grid(target, Vector(5, 6, 4), ok(Affine.fromRowMajor[D3](Vector(1.2, 0.3, 0.0, -2.0, 0.0, 0.9, 0.2, 1.0, 0.1, 0.0, 1.7, 0.5, 0, 0, 0, 1))))
    val m = Vector(1.1, 0.05, -0.02, -0.03, 0.95, 0.1, 0.02, -0.08, 1.2)
    val detA = m(0) * (m(4) * m(8) - m(5) * m(7)) - m(1) * (m(3) * m(8) - m(5) * m(6)) + m(2) * (m(3) * m(7) - m(4) * m(6))
    val pull = ok(oblique.jacobianDeterminant(lattice))
    val push = ok(oblique.jacobianDeterminant(lattice, DeterminantDirection.Push))
    val logs = ok(oblique.logJacobian(lattice))
    assertEquals(pull.foldCount, 0L)
    indices(lattice.shape).foreach: index =>
      assertEqualsDouble(determinantAt(pull, index), detA, 1e-10)
      assertEqualsDouble(determinantAt(push, index), 1.0 / detA, 1e-10)
      logs.at(index) match
        case Some(LogDeterminant.Finite(value)) => assertEqualsDouble(value, math.log(detA), 1e-10)
        case other                              => fail(s"expected a finite log-determinant, got $other")

  // radial map pull(y) = c + (1 + k r^2)(y - c): det = (1 + k r^2)^2 (1 + 3 k r^2)
  private val (radialCentre, radialK) = (Vector(6.0, 5.5, 5.0), 2e-3)
  private def radial(y: Vector[Double]): Vector[Double] =
    val d = y.zip(radialCentre).map(_ - _)
    val scale = 1.0 + radialK * d.map(v => v * v).sum
    radialCentre.zip(d).map((c, v) => c + scale * v)
  private def radialDeterminant(y: Vector[Double]): Double =
    val r2 = y.zip(radialCentre).map((a, c) => (a - c) * (a - c)).sum
    math.pow(1.0 + radialK * r2, 2) * (1.0 + 3.0 * radialK * r2)

  test("radial scaling matches its closed-form determinant and the chain rule holds through a composition"):
    val shape = Vector(13, 12, 11)
    val affine = axisAligned(Vector(0.0, 0.0, 0.0), 1.0)
    val warp = dense(shape, affine)(radial)
    val lattice = grid(target, shape, affine)
    val determinant = ok(warp.jacobianDeterminant(lattice))
    // With J = s I + 2k d d^T (s = 1 + k r^2), a central difference of step h adds exactly k h^2 to each diagonal entry
    // (the only non-quadratic term along an axis is k x^3), so the discrete determinant is (s + k h^2)^2 (s + 2k r^2 + k h^2)
    // and the continuous one (1 + k r^2)^2 (1 + 3k r^2) is reached as h -> 0.
    var worst = 0.0
    indices(shape).filter(i => i.zip(shape).forall((v, n) => v > 0 && v < n - 1)).foreach: index =>
      val y = pointAt(lattice, index).coordinates
      val r2 = y.zip(radialCentre).map((a, c) => (a - c) * (a - c)).sum
      val s = 1.0 + radialK * r2
      val discrete = math.pow(s + radialK, 2) * (s + 2.0 * radialK * r2 + radialK)
      assertEqualsDouble(determinantAt(determinant, index), discrete, 1e-10, s"discrete closed form at $index")
      worst = math.max(worst, math.abs(determinantAt(determinant, index) - radialDeterminant(y)) / radialDeterminant(y))
    assert(worst < 1e-2, s"worst interior relative error against the continuous determinant: $worst")
    // chain rule: pull = radial after a scaling affine T -> M, det = det(A) * det_radial(A y)
    val scaling = linear(middle, target, 0.9, 0.0, 0.0, 0.6, 0.0, 0.9, 0.0, 0.55, 0.0, 0.0, 0.9, 0.5, 0, 0, 0, 1)
    val warpSM: WorldTransform.Mapped[source.type, middle.type] =
      WorldTransform.Mapped(
        ok(DenseLattice.pullback[source.type, middle.type](middle, source, shape, affine, CoordinateBoundaryPolicy.Reject)((x, y, z) => radial(world(affine, x, y, z)))),
        PushAvailability.Unavailable(),
        TransformProvenance.constructed("radial")
      )
    val composite = warpSM.andThen(scaling)
    val inner = grid(target, Vector(10, 10, 10), axisAligned(Vector(1.0, 1.0, 1.0), 1.0))
    val chained = ok(composite.jacobianDeterminant(inner))
    indices(inner.shape).filter(_.forall(v => v > 0 && v < 9)).foreach: index =>
      val y = pointAt(inner, index).coordinates
      val ay = Vector(0.9 * y(0) + 0.6, 0.9 * y(1) + 0.55, 0.9 * y(2) + 0.5)
      val expected = 0.729 * radialDeterminant(ay)
      // the inner lattice samples the dense radial field between its nodes, so trilinear interpolation error of the
      // field (O(k h^2) in its derivative) enters on top of the central-difference error: measured below 1%
      assertEqualsDouble(determinantAt(chained, index) / expected, 1.0, 2e-2, s"chain rule at $index")

  test("a folded warp is reported by mask and count, with NonPositive log-determinants and no NaN"):
    val shape = Vector(16, 6, 6)
    val affine = axisAligned(Vector(0.0, 0.0, 0.0), 0.5)
    // d/dx (x + 2 sin(x)) = 1 + 2 cos(x) <= 0 near x = pi
    val folding = dense(shape, affine)(y => Vector(y(0) + 2.0 * math.sin(y(0)), y(1), y(2)))
    val lattice = grid(target, shape, affine)
    val field = ok(folding.jacobianDeterminant(lattice))
    assert(field.foldCount > 0L)
    val foldIndex = Vector(6, 3, 3) // x = 3.0
    field.at(foldIndex) match
      case Some(DeterminantValue.Folded(pull)) => assert(pull <= 0.0)
      case other                               => fail(s"expected a fold at x = 3, got $other")
    assertEquals(field.determinant.foldMask.valueAt(foldIndex).toOption, Some(true))
    assertEquals(field.determinant.foldMask.valueAt(Vector(1, 3, 3)).toOption, Some(false))
    ok(folding.jacobianDeterminant(lattice, DeterminantDirection.Push)).at(foldIndex) match
      case Some(DeterminantValue.Folded(pull)) => assert(pull <= 0.0) // a fold has no push determinant
      case other                               => fail(s"expected a fold under Push, got $other")
    field.logJacobian.at(foldIndex) match
      case Some(LogDeterminant.NonPositive(pull)) => assert(pull <= 0.0)
      case other                                  => fail(s"expected NonPositive, got $other")
    field.determinant.values.data.foreachElement(value => assert(value.isFinite))
    field.logJacobian.values.data.foreachElement(value => assert(value.isFinite))

  // ---------------------------------------------------------------- P6.04 modulated resampling

  // A Gaussian density (sigma 3 mm) on a 0.5 mm source grid reaching 4.25 sigma; source and target voxels are isotropic.
  private val (sigma, sourceSpacing, targetSpacing) = (3.0, 0.5, 0.9)
  private val sourceShape = Vector(52, 52, 52)
  private val sourceOrigin = -12.75
  private lazy val sourceImage =
    ok(
      Sampled.continuous(
        grid(source, sourceShape, axisAligned(Vector.fill(3)(sourceOrigin), sourceSpacing)),
        NonSpatialAxes.empty,
        NDArray.tabulate[Double](52, 52, 52): (i, j, k) =>
          val r2 = Vector(i, j, k).map(v => math.pow(sourceOrigin + sourceSpacing * v, 2)).sum
          math.exp(-r2 / (2.0 * sigma * sigma))
      )
    )
  private def total(data: NDArray[Double, ?], term: Double => Double): Double =
    var sum = 0.0
    data.foreachElement(value => sum += term(value))
    sum
  private lazy val sourceMass = total(sourceImage.data, identity) * math.pow(sourceSpacing, 3)
  private lazy val sourceEnergy = total(sourceImage.data, v => v * v) * math.pow(sourceSpacing, 3)
  private val targetGrid = grid(target, Vector(40, 40, 40), axisAligned(Vector.fill(3)(-17.55), targetSpacing))

  /** A compressive (s < 1) or expansive (s > 1) pull, as an exact affine and as a sampled dense warp. */
  private def pulls(s: Double): Vector[(String, WorldTransform[source.type, target.type])] =
    Vector(
      "affine" -> linear(source, target, s, 0, 0, 0, 0, s, 0, 0, 0, 0, s, 0, 0, 0, 0, 1),
      "dense" -> dense(Vector(40, 40, 40), axisAligned(Vector.fill(3)(-17.55), targetSpacing), CoordinateBoundaryPolicy.PreserveSource)(y =>
        Vector.tabulate(3)(i => s * y(i) + 0.3 * math.sin(0.12 * y((i + 1) % 3)))
      )
    )

  private def laws(transform: WorldTransform[source.type, target.type], modulation: VolumeModulation): (Double, Double) =
    val resampled = ok(transform.resampleModulated(sourceImage, targetGrid, modulation, boundary = BoundaryPolicy.Constant(0.0)))
    assertEquals(resampled.modulation, modulation)
    val voxel = math.pow(targetSpacing, 3)
    (total(resampled.result.image.data, identity) * voxel / sourceMass, total(resampled.result.image.data, v => v * v) * voxel / sourceEnergy)

  for s <- Vector(0.7, 1.3) do
    val kind = if s < 1.0 then "compressive" else "expansive"

    test(s"jacobian modulation preserves a density's integral under a $kind pull, and not its squared norm"):
      pulls(s).foreach: (name, transform) =>
        val (mass, energy) = laws(transform, VolumeModulation.Jacobian)
        assertEqualsDouble(mass, 1.0, 1e-3, s"$name mass") // measured <= 2.1e-4
        assert(math.abs(energy - 1.0) > 0.2, s"$name energy ratio $energy")

    test(s"sqrt-jacobian modulation preserves the squared L2 norm under a $kind pull, and not the integral"):
      pulls(s).foreach: (name, transform) =>
        val (mass, energy) = laws(transform, VolumeModulation.SqrtJacobian)
        // Trilinear interpolation of the source smooths it, lowering the squared norm by O((h / sigma)^2): measured
        // 0.63-0.72% at h / sigma = 1/6. The other law moves by more than 30%, so the modes stay an order apart.
        assertEqualsDouble(energy, 1.0, 1e-2, s"$name energy")
        assert(math.abs(mass - 1.0) > 0.1, s"$name mass ratio $mass")

    test(s"unmodulated resampling preserves neither law under a $kind pull"):
      pulls(s).foreach: (name, transform) =>
        val (mass, energy) = laws(transform, VolumeModulation.Unmodulated)
        assert(math.abs(mass - 1.0) > 0.2 && math.abs(energy - 1.0) > 0.2, s"$name mass $mass energy $energy")

  test("modulation diagnostics report the exact affine determinant; a single-sample axis is a typed error"):
    val (_, affine) = pulls(0.7).head
    val resampled = ok(affine.resampleModulated(sourceImage, targetGrid, VolumeModulation.Jacobian, boundary = BoundaryPolicy.Constant(0.0)))
    assertEqualsDouble(resampled.diagnostics.minimumDeterminant, 0.343, 1e-12)
    assertEqualsDouble(resampled.diagnostics.maximumDeterminant, 0.343, 1e-12)
    assertEquals(resampled.diagnostics.orientationReversingPoints, 0L)
    val (_, warp) = pulls(1.3)(1)
    val thin = grid(target, Vector(1, 8, 8), axisAligned(Vector(0.0, 0.0, 0.0), 1.0))
    assert(warp.resampleModulated(sourceImage, thin, VolumeModulation.Jacobian, boundary = BoundaryPolicy.Constant(0.0)).left.exists(_.isInstanceOf[TransformError.Resampling]))

  test("resample samples the source through the pullback and rejects target points outside it by default"):
    val gaussian = (y: Vector[Double]) => math.exp(-y.map(v => v * v).sum / (2.0 * sigma * sigma))
    pulls(0.7).foreach: (name, transform) =>
      val plain = ok(transform.resample(sourceImage, targetGrid, boundary = BoundaryPolicy.Constant(0.0))).image.data
      val unmodulated = ok(transform.resampleModulated(sourceImage, targetGrid, VolumeModulation.Unmodulated, boundary = BoundaryPolicy.Constant(0.0))).result.image.data
      for i <- 0 until 40 by 3; j <- 0 until 40 by 5; k <- 0 until 40 by 7 do
        val y = Vector(i, j, k).map(v => -17.55 + targetSpacing * v)
        val pulled = if name == "affine" then y.map(_ * 0.7) else Vector.tabulate(3)(a => 0.7 * y(a) + 0.3 * math.sin(0.12 * y((a + 1) % 3)))
        assertEqualsDouble(plain.at(IArray(i, j, k)), unmodulated.at(IArray(i, j, k)), 1e-12, s"$name matches unmodulated at ($i,$j,$k)")
        // Trilinear error is at most h^2/8 * sum_a max|d2f/dy_a^2| = 3 * 0.25/8 / sigma^2 ~ 1.04e-2 (h = 0.5, sigma = 3).
        assertEqualsDouble(plain.at(IArray(i, j, k)), gaussian(pulled), 3 * sourceSpacing * sourceSpacing / 8.0 / (sigma * sigma), s"$name value at ($i,$j,$k)")
    // An expansive pull reaches beyond the source image (|1.3 y| up to 22.8 mm against 12.75 mm): Reject fails the plan.
    val (_, expansive) = pulls(1.3).head
    assert(expansive.resample(sourceImage, targetGrid).left.exists(_.isInstanceOf[TransformError.Resampling]))
