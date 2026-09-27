package scalafim.transform.field

import image4s.geometry.{Affine, D3, Frame, Grid, GridId, LatticeIndex, Point}
import reframe4s.field.{DenseInverseEvidence, DenseMap, InversePointStatus, InversionError, InversionGateFailure, InversionStartKind}
import scalafim.image.world.{FrameCatalog, FslVolumeGeometry, ToolCoordinates, WorldSpace}
import scalafim.transform.*
import scalafim.transform.fsl.{FlirtInterpretation, FlirtMatrix, FslHeaderGeometry}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** STP P6.02: numerical inverses of real toolkit fields, qualified by their gates and checked against independent
  * references.
  *
  *   - An ITK displacement field against ITK's own `InvertDisplacementFieldImageFilter` (native oracle `itk_inverse`),
  *     point by point, within a stability bound derived from both residuals and the field's own Lipschitz bound.
  *   - FNIRT dense fields between differently placed volumes (FSL 5.0.9 `fsl_dense_oracle`) and FNIRT `--cout --aff`
  *     coefficient files (`fsl_coef_oracle`), inverted from an affine guess, with coverage gates derived from the guess
  *     and the field's largest displacement, round trips through the exact spline, and round trips through FSL
  *     `applywarp`'s own coordinate ramps.
  *
  * FSL `invwarp` and ANTs `InverseWarp` outputs for these fixtures do not exist yet; those comparisons are pending.
  */
class NumericalInverseParitySuite extends munit.FunSuite:
  import NumericalInverseParitySuite.*

  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def raw(path: String): NiftiRaw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(path))))
  private def field(path: String): VectorFieldNifti =
    ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(path)))))

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("inverse parity source")))
  private val target: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("inverse parity target")))
  private val frames = Frames[source.type, target.type](source, target)

  private def lattice(shape: Vector[Int]): Vector[Vector[Int]] =
    for i <- (0 until shape(0)).toVector; j <- 0 until shape(1); k <- 0 until shape(2) yield Vector(i, j, k)

  private def pointAt[F <: Frame[D3]](grid: Grid[F, D3], index: Vector[Int]): Point[F, D3] =
    ok(LatticeIndex.fromVector[D3](index).flatMap(grid.pointAt))

  private def sourcePoint(coordinates: Vector[Double]): Point[source.type, D3] = ok(Point.fromVector(source, coordinates))
  private def targetPoint(coordinates: Vector[Double]): Point[target.type, D3] = ok(Point.fromVector(target, coordinates))

  private def continuousIndex(grid: Grid[target.type, D3], coordinates: Vector[Double]): Vector[Double] =
    ok(grid.continuousIndexOf(targetPoint(coordinates))).values

  private def persistent(id: String, shape: Vector[Int], affine: Affine[D3]): Grid[source.type, D3] =
    ok(GridId.parse(id).flatMap(key => Grid.createPersistent[D3, source.type](key, source)(shape, affine)))

  private def evidenceOf(estimated: WorldTransform.Mapped[source.type, target.type]): DenseInverseEvidence[source.type, D3] =
    estimated.availability match
      case PushAvailability.Estimated(inverse) => inverse.evidence
      case other                               => fail(s"expected an estimated forward map, got $other")

  private def denseSamples(map: DenseMap[?, ?, D3, ?]): Vector[Int] => Vector[Double] =
    index => Vector.tabulate(3)(c => ok(map.coordinates.valueAt(index, Vector(c))))

  /** The estimate's own sample at a lattice node of its evaluation lattice. */
  private def nodeEstimate(estimated: WorldTransform.Mapped[source.type, target.type], index: Vector[Int]): Vector[Double] =
    estimated.availability match
      case PushAvailability.Estimated(inverse) => denseSamples(inverse.samples)(index)
      case other                               => fail(s"expected an estimated forward map, got $other")

  private def reverseMaximum(evidence: DenseInverseEvidence[source.type, D3]): Double =
    evidence.reverseResidual.fold(fail("no reverse residual samples"))(_.maximum)

  /** The coverage an affine-guess start must reach, and can at most reach, on `inverse`.
    *
    * The guess pull `pull0(y) = L y + b` differs from the field's trilinear pull by a trilinear displacement, whose norm
    * is largest at a lattice node: `displacement`. With the preconditioner `P = L^-1` (the linear part of the guess's
    * push), the iterates `y_k+1 = y0 - P (pull(y_k) - pull0(y_k))` all lie within `D = ||P|| displacement` of the guess
    * image `y0`, and so does the solution. A source point whose `y0` lies at least `D` inside the field lattice
    * therefore keeps every iterate inside it (and converges when the iteration contracts); one whose `y0` lies more
    * than `D` outside cannot be covered. Distances to the faces use the lattice spacing, so the lattice must be
    * orthogonal.
    */
  private def coverageBounds(
      inverse: Grid[source.type, D3],
      guess: WorldTransform.Linear[source.type, target.type],
      fieldLattice: Grid[target.type, D3],
      samples: Vector[Int] => Vector[Double]
  ): (Double, Double) =
    val displacement = lattice(fieldLattice.shape).map(i => distance(samples(i), ok(guess.pullPoint(pointAt(fieldLattice, i))).coordinates)).max
    requireOrthogonal(fieldLattice.indexToFrame)
    val reach = spectralBound(linearPart(guess.framed.inverse.operator)) * displacement
    val spacing = columnNorms(linearPart(fieldLattice.indexToFrame))
    val shape = fieldLattice.shape
    val depths = lattice(inverse.shape).map: index =>
      val y0 = ok(guess.mapPoint(pointAt(inverse, index))).coordinates
      val u = continuousIndex(fieldLattice, y0)
      (0 until 3).map(a => math.min(u(a), shape(a) - 1 - u(a)) * spacing(a)).min
    val total = depths.size.toDouble
    (depths.count(_ >= reach + 1e-6) / total, depths.count(_ >= -reach - 1e-6) / total)

  // --------------------------------------------------------------------------------------------- ITK native inverse

  private def itkWarp: (WorldTransform.Mapped[source.type, target.type], Grid[source.type, D3], Grid[target.type, D3]) =
    val warpRaw = raw("neurotransform/itk_oracle/warp.nii.gz")
    val warp = ok(LpsDisplacementInterpretation.Ants.interpret(field("neurotransform/itk_oracle/warp.nii.gz"), DenseContext.itk(frames)))
    val affine = ok(LatticeAffine.of(warpRaw, LatticeAffine.Itk))
    (warp, persistent("itk-inverse-parity", warpRaw.spatialShape, affine), ok(Grid.forFrame[D3, target.type](target)(warpRaw.spatialShape, affine)))

  test("an ITK displacement field's numerical inverse matches ITK InvertDisplacementField within the stability bound"):
    val (warp, inverseLattice, fieldLattice) = itkWarp
    val rows = OracleTable.load("itk_inverse/inverse.tsv").rows
    assertEquals(rows.size, inverseLattice.shape.product)
    // Which lattice points have their preimage inside the field's sampled support follows from ITK's inverse alone.
    // The evaluation domain is exactly that support: values the border hold supplies are not inverted.
    val placement = rows.map: row =>
      val preimage = flipLps(row.slice(3, 6).zip(row.slice(6, 9)).map(_ + _))
      val u = continuousIndex(fieldLattice, preimage)
      val shape = fieldLattice.shape
      if (0 until 3).forall(a => u(a) >= 1e-6 && u(a) <= shape(a) - 1 - 1e-6) then Some(true)
      else if (0 until 3).exists(a => u(a) < -1e-6 || u(a) > shape(a) - 1 + 1e-6) then Some(false)
      else None
    val inside = placement.count(_.contains(true))
    val policy = ok(InversionPolicy.create(minimumCoverage = inside.toDouble / rows.size, maximumResidual = 1e-4, p99Residual = 1e-4, tolerance = 1e-11))
    val estimated = ok(warp.invertNumerically(inverseLattice, policy))
    val evidence = evidenceOf(estimated)
    assertEquals((evidence.start, evidence.preconditioned, evidence.startRejections), (InversionStartKind.Identity, false, 0L))
    assertEquals(evidence.statusCounts.diverged, 0L)
    assert(evidence.coveredFraction <= (rows.size - placement.count(_.contains(false))).toDouble / rows.size, evidence.coveredFraction.toString)
    // Lipschitz bound of the pull's inverse over the lattice and ITK's half-voxel band (L = I: a displacement field).
    val stability = inverseLipschitz(denseSamples(warp.pull.asInstanceOf[DenseMap[?, ?, D3, ?]]), fieldLattice.shape, fieldLattice.indexToFrame, Identity3)
      .getOrElse(fail("the field's pull is not bi-Lipschitz"))
    assert(stability < 1.2, s"stability bound $stability") // measured 1.134
    var compared = 0
    var worst = 0.0
    rows.zip(placement).foreach: (row, expected) =>
      val index = row.take(3).map(_.toInt)
      val x = pointAt(inverseLattice, index)
      evidence.statusAt(index) match
        case Some(InversePointStatus.Converged) =>
          assert(!expected.contains(false), s"$index: converged although ITK's preimage lies outside the field lattice")
          // With our pull, pull(ours) = x + r1 and pull(itk) = x + r2, so |ours - itk| <= K (|r1| + |r2|): both points
          // lie in the lattice and band where K bounds the pull's inverse.
          val ours = nodeEstimate(estimated, index)
          val itk = flipLps(row.slice(3, 6).zip(row.slice(6, 9)).map(_ + _))
          assert(row(9) <= 1e-9, s"$index: ITK's own residual ${row(9)}") // the oracle converged
          val residual = distance(ok(warp.pullPoint(targetPoint(ours))).coordinates, x.coordinates)
          val itkResidual = distance(ok(warp.pullPoint(targetPoint(itk))).coordinates, x.coordinates)
          val error = distance(ours, itk)
          // 1e-12 mm: double rounding of coordinates near 20 mm, well above their ulp (3.6e-15)
          assert(error <= stability * (residual + itkResidual) + 1e-12, s"$index: |ours - ITK| = $error, residuals $residual and $itkResidual")
          worst = math.max(worst, error)
          compared += 1
        case Some(InversePointStatus.OutsideCoverage) =>
          assert(!expected.contains(true), s"$index: outside coverage although ITK's preimage lies inside the field lattice")
        case other => fail(s"$index: status $other")
    assert(compared >= inside, s"$compared compared, $inside preimages inside the lattice")
    // measured: 336 of 504 points in the domain, K 1.134, largest difference 9.5e-12 mm. The comparison reads the
    // estimate's lattice samples: reframe4s' mapPoint also refuses 56 of these nodes, whose continuous index rounds to
    // 2.2e-16 below its integer value, so an in-lattice neighbour outside the domain gets a weight of that size
    // (an upstream follow-up).
    assert(worst < 1e-9, s"largest difference from ITK $worst mm")

  test("a continuation start is recorded in the evidence and qualifies like the identity start"):
    val (warp, inverseLattice, _) = itkWarp
    val policy = ok(InversionPolicy.create(minimumCoverage = 0.5, maximumResidual = 1e-4, p99Residual = 1e-4, tolerance = 1e-11))
    val identity = evidenceOf(ok(warp.invertNumerically(inverseLattice, policy)))
    val continued = ok(warp.invertNumerically(inverseLattice, policy, InversionStart.Continuation[source.type, target.type]()))
    val evidence = evidenceOf(continued)
    assertEquals((evidence.start, evidence.preconditioned), (InversionStartKind.Continuation, false))
    assertEquals(evidence.statusCounts, identity.statusCounts)
    assert(continued.provenance.describe.contains("continuation start"), continued.provenance.describe)

  test("an affine guess is typed by the transform's own frames"):
    val errors = compileErrors(
      "InversionStart.AffineGuess[source.type, target.type](??? : WorldTransform.Linear[target.type, source.type])"
    )
    assert(errors.contains("Required: scalafim.transform.WorldTransform.Linear["), errors)

  // ----------------------------------------------------------------------------------- FNIRT dense fields, FSL 5.0.9

  private val handedness = Vector("left_left", "left_right", "right_left", "right_right")

  /** The FLIRT identity between two volumes: FSL's own placement of each, which FNIRT fields displace from. */
  private val FlirtIdentity = FlirtMatrix(Vector(1.0, 0, 0, 0, 0, 1.0, 0, 0, 0, 0, 1.0, 0, 0, 0, 0, 1.0))

  /** `applywarp` coordinate ramps agree with our pull within 3e-5 mm per component on their exact support
    * (`DenseFieldOracleSuite`); checked again per voxel below.
    */
  private val DenseRampTolerance = math.sqrt(3.0) * 3e-5

  /** The solver's convergence tolerance on `|pull(push(x)) - x|` (the `InversionPolicy` default). */
  private val SolverTolerance = 1e-8

  test("FNIRT fields between differently placed volumes invert from an affine guess; the identity start is refused"):
    handedness.foreach: pair =>
      val dir = s"neurotransform/fsl_dense_oracle/${pair}_relative"
      val sourceRaw = raw(s"$dir/source.nii.gz")
      val sourceGeometry = ok(FslHeaderGeometry(sourceRaw))
      val warpField = field(s"$dir/warp.nii.gz")
      val reference = ok(FslHeaderGeometry(warpField.raw))
      val warp = ok(FnirtFieldInterpretation.interpret(warpField, FnirtContext(frames, sourceGeometry, Some(FnirtDefinition.Relative))))
      val guess = ok(FlirtInterpretation.interpret(FlirtIdentity, FslGrids[source.type, target.type](source, sourceGeometry, target, reference)))
      val fieldLattice = ok(Grid.forFrame[D3, target.type](target)(warpField.spatialDims, reference.voxelToWorld))
      val inverseLattice = persistent(s"fsl-dense-guess-$pair", sourceRaw.spatialShape, sourceGeometry.voxelToWorld)
      val samples = denseSamples(warp.pull.asInstanceOf[DenseMap[?, ?, D3, ?]])
      val (lower, upper) = coverageBounds(inverseLattice, guess, fieldLattice, samples)
      assert(lower > 0.05, s"$pair: coverage lower bound $lower")
      // Residual gates (mm): the forward residual is the solver tolerance; the reverse one is the trilinear
      // interpolation error of the estimate on the 1.6-1.8 mm source lattice (measured max 5.35e-3, p99 5.11e-3).
      // Measured coverage 0.0822 within the derived [0.0647, 0.1247]; 248 native voxels round-trip within 5.5e-3 mm.
      val policy = ok(InversionPolicy.create(minimumCoverage = lower, maximumResidual = 1e-2, p99Residual = 1e-2))
      warp.invertNumerically(inverseLattice, policy) match
        case Left(TransformError.Inversion(InversionError.GatesFailed(failures, evidence))) =>
          assert(failures.exists(_.isInstanceOf[InversionGateFailure.CoverageBelowMinimum]), s"$pair: $failures")
          assertEquals(evidence.start, InversionStartKind.Identity)
        case other => fail(s"$pair: the identity start must fail its coverage gate, got $other")
      val estimated = ok(warp.invertNumerically(inverseLattice, policy, InversionStart.AffineGuess(guess)))
      val evidence = evidenceOf(estimated)
      // a FramedAffine guess is total, so it never rejects a start
      assertEquals((evidence.start, evidence.preconditioned, evidence.startRejections), (InversionStartKind.Guess, true, 0L), pair)
      assertEquals(evidence.interiorStatusCounts.diverged, 0L, pair)
      assert(evidence.coveredFraction >= lower && evidence.coveredFraction <= upper, s"$pair: coverage ${evidence.coveredFraction} outside [$lower, $upper]")
      assert(estimated.provenance.describe.contains("affine-guess start"), estimated.provenance.describe)
      val stability = cellwiseInverseLipschitz(samples, fieldLattice.shape, fieldLattice.indexToFrame)
        .getOrElse(fail(s"$pair: the field's pull is not bi-Lipschitz"))
      val checked = nativeRoundTrip(dir, estimated, warp, fieldLattice, inverseLattice, NativeSupport(margin = 2, threshold = 0.5), reverseMaximum(evidence), stability, DenseRampTolerance, pair)
      assert(checked >= 55, s"$pair: only $checked voxels round-tripped") // measured 62 per case

  /** Which reference voxels of an `applywarp` fixture are exact: `native_support` above `threshold` over a
    * `(2 margin + 1)^3` neighbourhood.
    */
  private final case class NativeSupport(margin: Int, threshold: Double)

  /** Send FSL `applywarp`'s coordinate ramps (source RAS `n` at each reference voxel `y`) through the estimated forward
    * map and back onto the voxel. Voxels are interior (the reverse-residual margin) and exactly supported.
    *
    * With `p = pull(y)` (our pull, which the estimate inverts), `|push(n) - y| <= |push(p) - y| + Lip(push) |n - p|`.
    * The first term is the gated reverse residual. `push` interpolates trilinearly, on the orthogonal source lattice,
    * samples of `pull^-1` that are `K`-Lipschitz up to the solver tolerance, so `Lip(push) <= sqrt(3) K` and each
    * sample adds at most `K * SolverTolerance`. `|n - p|` is measured per voxel and must itself stay within the ramps'
    * float32 tolerance. Returns the number of voxels checked.
    */
  private def nativeRoundTrip(
      dir: String,
      estimated: WorldTransform.Mapped[source.type, target.type],
      pull: WorldTransform[source.type, target.type],
      fieldLattice: Grid[target.type, D3],
      inverseLattice: Grid[source.type, D3],
      support: NativeSupport,
      reverse: Double,
      stability: Double,
      rampTolerance: Double,
      label: String
  ): Int =
    requireOrthogonal(inverseLattice.indexToFrame)
    val ramps = Vector(0, 1, 2).map(i => raw(s"$dir/native_coord$i.nii.gz"))
    val supportImage = raw(s"$dir/native_support.nii.gz")
    val shape = fieldLattice.shape
    def supported(i: Vector[Int]): Boolean =
      val offsets = -support.margin to support.margin
      offsets.forall(dx => offsets.forall(dy => offsets.forall { dz =>
        val (x, y, z) = (i(0) + dx, i(1) + dy, i(2) + dz)
        x >= 0 && y >= 0 && z >= 0 && x < shape(0) && y < shape(1) && z < shape(2) && supportImage.value(x, y, z) > support.threshold
      }))
    var checked = 0
    lattice(shape).filter(i => i.zip(shape).forall((v, n) => v >= 1 && v <= n - 2) && supported(i)).foreach: index =>
      val y = pointAt(fieldLattice, index)
      val native = Vector.tabulate(3)(c => ramps(c).value(index(0), index(1), index(2)))
      val ramp = distance(native, ok(pull.pullPoint(y)).coordinates)
      assert(ramp <= rampTolerance, s"$label $index: applywarp differs from our pull by $ramp")
      estimated.mapPoint(sourcePoint(native)) match
        case Right(back) =>
          val tolerance = reverse + math.sqrt(3.0) * stability * ramp + stability * SolverTolerance
          val error = distance(back.coordinates, y.coordinates)
          assert(error <= tolerance, s"$label $index: |push(applywarp) - y| = $error > $tolerance")
          checked += 1
        case Left(_) => () // outside the estimate's evaluation domain; the caller's floor bounds how many
    checked

  // ------------------------------------------------------------------------------ FNIRT --cout --aff, FSL 5.0.9

  private val CoefRoot = "neurotransform/fsl_coef_oracle"

  /** applywarp --warp=coef ramps agree with the exact spline within 2e-5 mm per component on their exact support
    * (`FnirtCoefficientOracleSuite`); at lattice nodes the materialized field equals the spline.
    */
  private val CoefRampTolerance = math.sqrt(3.0) * 2e-5

  test("FNIRT --aff coefficient fields invert from their FLIRT matrix and round-trip through the exact spline"):
    val withAff = Vector("srcleft_refleft_aff", "srcleft_refright_aff", "srcright_refleft_aff", "srcright_refright_aff", "srcleft_refright_aff_quad")
    withAff.foreach: name =>
      val file = ok(FnirtCoefficientsCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(s"$CoefRoot/$name/coef.nii.gz")))))
      val sourceRaw = raw(s"$CoefRoot/$name/source.nii.gz")
      val sourceGeometry = ok(FslHeaderGeometry(sourceRaw))
      val reference = ok(FslHeaderGeometry(raw(s"$CoefRoot/$name/target.nii.gz")))
      val grids = FslGrids[source.type, target.type](source, sourceGeometry, target, reference)
      val exact = ok(FnirtCoefficientInterpretation.interpret(file, FnirtCoefficientContext(grids)))
      val fieldLattice = ok(Grid.forFrame[D3, target.type](target)(reference.dims, reference.voxelToWorld))
      // the spline sampled on the reference lattice under the default Reject: every point, faces included, is covered
      val materialized = ok(exact.materialize(fieldLattice))
      assertEquals(materialized.coverage.counts.covered, fieldLattice.shape.product.toLong, name)
      val guess = ok(FlirtInterpretation.interpret(file.premat, grids))
      val inverseLattice = persistent(s"fnirt-coef-guess-$name", sourceRaw.spatialShape, sourceGeometry.voxelToWorld)
      val samples = denseSamples(materialized.field)
      val (lower, upper) = coverageBounds(inverseLattice, guess, fieldLattice, samples)
      assert(lower > 0.05, s"$name: coverage lower bound $lower")
      // The reverse residual is the trilinear error of the estimate on the 2.1-2.7 mm source lattice of a spline with
      // 2-5 voxel knots (measured max 0.108-0.191 mm, p99 0.095-0.161 mm); the forward residual is the solver tolerance.
      val policy = ok(InversionPolicy.create(minimumCoverage = lower, maximumResidual = 0.25, p99Residual = 0.2))
      val estimated = ok(materialized.invertNumerically(inverseLattice, policy, InversionStart.AffineGuess(guess)))
      val evidence = evidenceOf(estimated)
      assertEquals((evidence.start, evidence.preconditioned, evidence.startRejections), (InversionStartKind.Guess, true, 0L), name)
      assertEquals(evidence.interiorStatusCounts.diverged, 0L, name)
      assert(evidence.coveredFraction >= lower && evidence.coveredFraction <= upper, s"$name: coverage ${evidence.coveredFraction} outside [$lower, $upper]")
      // Analytic round trip: at every converged source node x the estimate y satisfies |materialized(y) - x| <= the
      // solver tolerance, so the exact spline returns the node to within that tolerance plus the materialization's
      // interpolation error |exact - materialized|. Trilinear interpolation on the unit reference lattice errs by at most (1/8) sum_a sup|d2f/du_a^2|; a quadratic
      // or cubic B-spline's second derivative along u_a is a convex combination of the coefficients' second differences
      // divided by the knot spacing squared. The displacement is in FSL mm, scaled to world by the source FSL map.
      val interpolation = spectralBound(linearPart(fslToWorld(sourceGeometry))) * math.sqrt((0 until 3).map { c =>
        val perComponent = (0 until 3).map(axis => secondDifference(file, c, axis) / (8.0 * file.knotSpacing(axis) * file.knotSpacing(axis))).sum
        perComponent * perComponent
      }.sum)
      var converged = 0
      lattice(inverseLattice.shape).filter(i => evidence.statusAt(i).contains(InversePointStatus.Converged)).foreach: index =>
        val x = pointAt(inverseLattice, index)
        val y = targetPoint(nodeEstimate(estimated, index))
        val error = distance(ok(exact.pullPoint(y)).coordinates, x.coordinates)
        assert(error <= SolverTolerance + interpolation, s"$name $index: |exact(push(x)) - x| = $error > $interpolation")
        converged += 1
      assertEquals(converged.toLong, evidence.statusCounts.converged, name)
      // Round trip through applywarp --warp=coef, as for the dense fields.
      val stability = cellwiseInverseLipschitz(samples, fieldLattice.shape, fieldLattice.indexToFrame)
        .getOrElse(fail(s"$name: the field's pull is not bi-Lipschitz"))
      val checked = nativeRoundTrip(s"$CoefRoot/$name", estimated, materialized.transform, fieldLattice, inverseLattice, NativeSupport(margin = 0, threshold = 0.999), reverseMaximum(evidence), stability, CoefRampTolerance, name)
      assert(checked >= 270, s"$name: only $checked voxels round-tripped") // measured 290-473

  private def fslToWorld(geometry: FslVolumeGeometry): Affine[D3] = ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(geometry))

  /** Largest second difference of coefficient component `c` along `axis` (FSL mm). reframe4s treats coefficients
    * outside the stored grid as zero, and FNIRT omits the outermost knots, so the grid is padded with zeros: second
    * differences are taken at every index from -1 to n along `axis`.
    */
  private def secondDifference(file: FnirtCoefficientFile, c: Int, axis: Int): Double =
    val dims = file.coefficientDims
    def value(j: Vector[Int]): Double =
      if j.zip(dims).forall((v, n) => v >= 0 && v < n) then file.raw.value(j(0), j(1), j(2), c) else 0.0
    val padded = for i <- lattice(dims.updated(axis, 1)); k <- -1 to dims(axis) yield i.updated(axis, k)
    padded.map { i =>
      def at(offset: Int) = value(i.updated(axis, i(axis) + offset))
      math.abs(at(1) - 2.0 * at(0) + at(-1))
    }.max

object NumericalInverseParitySuite:
  val Identity3: Vector[Double] = Vector(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)

  def flipLps(v: Vector[Double]): Vector[Double] = Vector(-v(0), -v(1), v(2))

  def distance(a: Vector[Double], b: Vector[Double]): Double =
    math.sqrt(a.zip(b).map((x, y) => (x - y) * (x - y)).sum)

  /** `sqrt(||m||_1 ||m||_inf)`, an upper bound on the spectral norm of a row-major 3x3 matrix. */
  def spectralBound(m: Vector[Double]): Double =
    val columns = (0 until 3).map(c => (0 until 3).map(r => math.abs(m(3 * r + c))).sum).max
    val rows = (0 until 3).map(r => (0 until 3).map(c => math.abs(m(3 * r + c))).sum).max
    math.sqrt(columns * rows)

  def linearPart(affine: Affine[D3]): Vector[Double] =
    val m = affine.rowMajor
    Vector(m(0), m(1), m(2), m(4), m(5), m(6), m(8), m(9), m(10))

  /** The derived bounds use per-axis lattice spacings: the lattice axes must be orthogonal. */
  def requireOrthogonal(indexToWorld: Affine[D3]): Unit =
    val m = linearPart(indexToWorld)
    val norms = columnNorms(m)
    for a <- 0 until 3; b <- a + 1 until 3 do
      val cosine = (0 until 3).map(r => m(3 * r + a) * m(3 * r + b)).sum / (norms(a) * norms(b))
      require(math.abs(cosine) <= 1e-6, s"lattice axes $a and $b are not orthogonal (cosine $cosine)")

  def columnNorms(m: Vector[Double]): Vector[Double] =
    Vector.tabulate(3)(c => math.sqrt((0 until 3).map(r => m(3 * r + c) * m(3 * r + c)).sum))

  def inverse3(m: Vector[Double]): Vector[Double] =
    val det = m(0) * (m(4) * m(8) - m(5) * m(7)) - m(1) * (m(3) * m(8) - m(5) * m(6)) + m(2) * (m(3) * m(7) - m(4) * m(6))
    Vector(
      m(4) * m(8) - m(5) * m(7), m(2) * m(7) - m(1) * m(8), m(1) * m(5) - m(2) * m(4),
      m(5) * m(6) - m(3) * m(8), m(0) * m(8) - m(2) * m(6), m(2) * m(3) - m(0) * m(5),
      m(3) * m(7) - m(4) * m(6), m(1) * m(6) - m(0) * m(7), m(0) * m(4) - m(1) * m(3)
    ).map(_ / det)

  /** An upper bound on `||(D pull)^-1||_2` wherever the trilinear interpolant of `samples` (absolute source coordinates
    * at the lattice nodes, `indexToWorld` placing the lattice) is evaluated. With `linear = I` (a displacement field,
    * `samples = x + d`) the bound also holds in ITK's clamped border band.
    *
    * Write `D pull = L + E` with `L` a chosen 3x3 linear part. Within a cell the interpolant is multilinear, so each
    * entry of its index-space Jacobian is multilinear in the other coordinates and bounded by the largest lattice edge
    * difference along its axis: `|dg_c/du_a| <= M(c, a)` for `g = pull - L`. Hence `||E||_2 <= ||M||_F ||A^-1||_2`, and
    * `||(L + E)^-1||_2 <= ||L^-1|| / (1 - ||L^-1|| ||E||)` when the denominator is positive. In the border band
    * `g = d(clamp(u))`, and clamping only zeroes columns of the index-space Jacobian. `None` when the bound does not
    * exist.
    */
  def inverseLipschitz(samples: Vector[Int] => Vector[Double], shape: Vector[Int], indexToWorld: Affine[D3], linear: Vector[Double]): Option[Double] =
    val a = linearPart(indexToWorld)
    val step = Vector.tabulate(3)(axis => Vector.tabulate(3)(c => (0 until 3).map(k => linear(3 * c + k) * a(3 * k + axis)).sum))
    val bound = Array.ofDim[Double](3, 3)
    for
      i <- 0 until shape(0); j <- 0 until shape(1); k <- 0 until shape(2)
      axis <- 0 until 3
      node = Vector(i, j, k)
      if node(axis) + 1 < shape(axis)
    do
      val here = samples(node)
      val next = samples(node.updated(axis, node(axis) + 1))
      (0 until 3).foreach(c => bound(c)(axis) = math.max(bound(c)(axis), math.abs(next(c) - here(c) - step(axis)(c))))
    val e = math.sqrt(bound.map(_.map(v => v * v).sum).sum) * spectralBound(inverse3(a))
    val inverseNorm = spectralBound(inverse3(linear))
    Option.when(inverseNorm * e < 1.0)(inverseNorm / (1.0 - inverseNorm * e))

  /** As [[inverseLipschitz]] over the lattice hull only (no border band), bounding each cell separately around its own
    * centre Jacobian `L_cell` (the mean of the cell's four edge differences per axis). Within the cell each index-space
    * Jacobian entry stays between its edge extremes, so it deviates from `L_cell` by at most the largest deviation of an
    * edge difference from their mean. The bound over the hull is the largest cell bound, since `pull^-1` is Lipschitz
    * cell by cell along any segment. `None` when some cell has no bound.
    */
  def cellwiseInverseLipschitz(samples: Vector[Int] => Vector[Double], shape: Vector[Int], indexToWorld: Affine[D3]): Option[Double] =
    val inverseA = inverse3(linearPart(indexToWorld))
    val inverseANorm = spectralBound(inverseA)
    val cells = for i <- 0 until shape(0) - 1; j <- 0 until shape(1) - 1; k <- 0 until shape(2) - 1 yield Vector(i, j, k)
    val bounds = cells.map: cell =>
      // edges(axis): the four edge differences along `axis`, each a 3-vector
      val edges = Vector.tabulate(3): axis =>
        val others = (0 until 3).filterNot(_ == axis)
        for o1 <- 0 to 1; o2 <- 0 to 1 yield
          val base = cell.updated(others(0), cell(others(0)) + o1).updated(others(1), cell(others(1)) + o2)
          val here = samples(base)
          val next = samples(base.updated(axis, base(axis) + 1))
          Vector.tabulate(3)(c => next(c) - here(c))
      val centre = Vector.tabulate(3, 3)((c, axis) => edges(axis).map(_(c)).sum / 4.0)
      val deviation = Vector.tabulate(3, 3)((c, axis) => edges(axis).map(e => math.abs(e(c) - centre(c)(axis))).max)
      // world Jacobian at the centre: J_idx A^-1 (row-major 3x3)
      val jacobian = Vector.tabulate(9)(entry => (0 until 3).map(k => centre(entry / 3)(k) * inverseA(3 * k + entry % 3)).sum)
      val e = math.sqrt(deviation.flatten.map(v => v * v).sum) * inverseANorm
      val inverseNorm = spectralBound(inverse3(jacobian))
      Option.when(inverseNorm * e < 1.0)(inverseNorm / (1.0 - inverseNorm * e))
    Option.when(bounds.forall(_.isDefined))(bounds.flatten.max)
