package scalafim.surface.scenarios

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.lie.FramedAffine
import scalafim.image.{GridSpec, SpatialDims}
import scalafim.image.world.*
import scalafim.scenarios.{
  CaveatKind,
  CaveatSeverity,
  ScenarioCaveat,
  ScenarioHarness,
  ScenarioObservation,
  ScenarioPolicy,
  ScenarioResult,
  ScenarioStatus,
  ScenarioTolerance
}
import scalafim.surface.*
import scalafim.surface.fixtures.SphereMeshes

/** Scenario `surface.volume-to-template-mesh-chain.v1`: the surface leg of an fMRIPrep-style workflow. A volume in the
  * subject's scanner RAS is sampled through the cortical ribbon of FreeSurfer surfaces stored in tkRAS, and the
  * per-vertex values are resampled from the subject's registered sphere onto template meshes, both through an
  * intermediate (fsaverage-like) mesh and directly (fsLR-like), which must commute.
  *
  * Workflow risk protected: a FreeSurfer surface sits in tkRAS, not in the scanner RAS the volume lives in; tkRAS ->
  * scanner is `Norig * inverse(Torig)`, which for a conformed volume is a shift by `c_ras`. The scenario fails if the
  * tkRAS coordinates are taken as scanner coordinates, if the tkRAS link is applied backwards, if the volume's RAS
  * axes are read as LPS, or if a resampling plan is applied against its direction; each mutation is run below.
  *
  * References are mathematical: the volume is a linear field, so trilinear ribbon sampling returns the field at the
  * mean of each white -> pial segment exactly, and the scanner position is the documented FreeSurfer relation
  * `scanner = tkRAS + c_ras` with `c_ras = Norig * (dims / 2)` (the Norig of `conventions/freesurfer_conformed.tsv`
  * case 1, a nibabel reference for a conformed 256^3 LIA volume). Barycentric resampling of a field linear on the
  * sphere errs by at most the field gradient times the moving mesh's sagitta, which is computed from the meshes.
  */
class SurfaceChainScenarioSuite extends munit.FunSuite:
  import SurfaceChainScenarioSuite.{Caveats, Mutation, Policy}

  private val Id = "surface.volume-to-template-mesh-chain.v1"

  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def affine(rowMajor: Vector[Double]): Affine[D3] = ok(Affine.fromRowMajor[D3](rowMajor))

  // ------------------------------------------------------------------------------------------------ subject spaces

  private val namespace = ok(DatasetNamespace("ds-surface-chain"))
  private val subject = ok(SubjectId("01"))
  private val reference = ok(ReferenceAcquisition(Map("acq" -> "mprage"), ok(GeometryDigest(Vector(256, 256, 256), Vector.fill(12)(1.0), 1, 1))))
  private val scanner: Frame[D3] = FrameCatalog.frame(WorldSpace.SubjectNative(namespace, subject, None, reference))
  private val tkRas: Frame[D3] = FrameCatalog.frame(WorldSpace.SubjectTkRas(namespace, subject, reference))

  /** orig.mgz: conformed 256^3, 1 mm, LIA, centred at c_ras (nibabel oracle case 1). */
  private val orig = FreeSurferVolumeGeometry(
    Vector(256, 256, 256),
    affine(Vector(-1.0, 0.0, 0.0, 140.3000030517578, 0.0, 0.0, 1.0, -173.60000610351562, 0.0, -1.0, 0.0, 135.8000030517578, 0.0, 0.0, 0.0, 1.0))
  )

  /** FreeSurfer's c_ras: the scanner position of the conformed volume's centre voxel. */
  private val cRas: Vector[Double] =
    val m = orig.norig.rowMajor
    Vector.tabulate(3)(r => (0 until 3).map(c => m(4 * r + c) * 128.0).sum + m(4 * r + 3))

  // ----------------------------------------------------------------------------------------------------- surfaces

  private val centreTk = (-24.5, -12.0, 18.25)
  private val (whiteRadius, pialRadius) = (30.0, 33.0)
  private val whiteMesh = SphereMeshes.icosphere(3, whiteRadius, centreTk)
  private val pialMesh = SphereMeshes.icosphere(3, pialRadius, centreTk)
  private val whiteTk = ok(FramedSurface.in(tkRas)(SurfaceGeometry(whiteMesh, Hemisphere.Left, SurfaceKind.White), SurfacePlacement.StoredCoordinates))
  private val pialTk = ok(FramedSurface.in(tkRas)(SurfaceGeometry(pialMesh, Hemisphere.Left, SurfaceKind.Pial), SurfacePlacement.StoredCoordinates))

  /** Rotation about z by `a`, then about x by `0.7 a` (applied to row vectors as in the resampling suite). */
  private def rotation(a: Double): Vector[Vector[Double]] =
    val (cz, sz, cx, sx) = (math.cos(a), math.sin(a), math.cos(0.7 * a), math.sin(0.7 * a))
    val rz = Vector(Vector(cz, -sz, 0.0), Vector(sz, cz, 0.0), Vector(0.0, 0.0, 1.0))
    val rx = Vector(Vector(1.0, 0.0, 0.0), Vector(0.0, cx, -sx), Vector(0.0, sx, cx))
    Vector.tabulate(3, 3)((i, j) => (0 until 3).map(k => rx(i)(k) * rz(k)(j)).sum)

  private def rotate(r: Vector[Vector[Double]], v: Vector[Double]): Vector[Double] =
    Vector.tabulate(3)(i => (0 until 3).map(j => r(i)(j) * v(j)).sum)

  private def rotatedSphere(levels: Int, a: Double): TriangleMesh =
    val mesh = SphereMeshes.icosphere(levels, 100.0)
    val r = rotation(a)
    TriangleMesh.fromArrays((0 until mesh.vertexCount).toArray.flatMap(i => rotate(r, Vector(mesh.coordinates(3 * i), mesh.coordinates(3 * i + 1), mesh.coordinates(3 * i + 2)))), mesh.faceIndices.clone())

  /** The subject's `?h.sphere.reg`: white-surface vertex directions carried by the registration rotation `Q`. */
  private val registrationAngle = 0.13
  private val sphereReg = rotatedSphere(3, registrationAngle)
  private val fsaverageLike = rotatedSphere(4, 0.37)
  private val fsLrLike = rotatedSphere(2, 0.91)

  // ------------------------------------------------------------------------------------------------------- volume

  /** The linear field in scanner RAS mm. */
  private val gradient = Vector(0.8, -0.5, 1.2)
  private def field(x: Vector[Double]): Double = 100.0 + x.zip(gradient).map(_ * _).sum

  private val centreScanner = Vector(centreTk._1, centreTk._2, centreTk._3).zip(cRas).map(_ + _)

  /** An oblique, anisotropic BOLD-like grid in scanner RAS covering the ribbon with a two-voxel margin. */
  private val (volumeDims, volumeAffine) =
    val spacing = Vector(2.4, 2.5, 2.7)
    val r = rotation(0.2)
    val half = pialRadius + 6.0
    val dims = spacing.map(s => (math.ceil(2.0 * half / s) + 1.0).toInt)
    // voxel (dims - 1) / 2 sits at the ribbon centre
    val axes = Vector.tabulate(3, 3)((row, col) => r(row)(col) * spacing(col))
    val origin = Vector.tabulate(3)(row => centreScanner(row) - (0 until 3).map(col => axes(row)(col) * (dims(col) - 1) / 2.0).sum)
    (dims, Vector.tabulate(3)(row => axes(row) :+ origin(row)).flatten ++ Vector(0.0, 0.0, 0.0, 1.0))

  private val volume: Array[Double] =
    val (nx, ny, nz) = (volumeDims(0), volumeDims(1), volumeDims(2))
    val m = volumeAffine
    val out = new Array[Double](nx * ny * nz)
    for i <- 0 until nx; j <- 0 until ny; k <- 0 until nz do
      out((i * ny + j) * nz + k) = field(Vector.tabulate(3)(r => m(4 * r) * i + m(4 * r + 1) * j + m(4 * r + 2) * k + m(4 * r + 3)))
    out

  // ------------------------------------------------------------------------------------------------------ scenario

  /** How far a barycentric hit on the chord triangles of a radius-100 sphere mesh can sit from the sphere. */
  private def sagitta(mesh: TriangleMesh): Double =
    val c = ok(SphereMesh.withRadius(mesh, 100.0)).coordinates
    val f = mesh.faceIndices
    val distances = (0 until mesh.faceCount).map: face =>
      def corner(k: Int) = Vector(c(3 * f(3 * face + k)), c(3 * f(3 * face + k) + 1), c(3 * f(3 * face + k) + 2))
      val (a, b, d) = (corner(0), corner(1), corner(2))
      val (e1, e2) = (b.zip(a).map(_ - _), d.zip(a).map(_ - _))
      val n = Vector(e1(1) * e2(2) - e1(2) * e2(1), e1(2) * e2(0) - e1(0) * e2(2), e1(0) * e2(1) - e1(1) * e2(0))
      math.abs(n.zip(a).map(_ * _).sum) / math.sqrt(n.map(v => v * v).sum)
    100.0 - distances.min

  private def surfacesInScanner(mutation: Mutation): Either[SurfaceFrameError, (FramedSurface[scanner.type], FramedSurface[scanner.type])] =
    mutation match
      case Mutation.TkRasTakenAsScanner =>
        for
          w <- FramedSurface.in(scanner)(SurfaceGeometry(whiteMesh, Hemisphere.Left, SurfaceKind.White), SurfacePlacement.StoredCoordinates)
          p <- FramedSurface.in(scanner)(SurfaceGeometry(pialMesh, Hemisphere.Left, SurfaceKind.Pial), SurfacePlacement.StoredCoordinates)
        yield (w, p)
      case Mutation.TkRasLinkReversed =>
        val backwards = FramedAffine.betweenFrames[tkRas.type, scanner.type, D3](tkRas, scanner)(orig.tkrToScanner.inverse)
        for
          w <- whiteTk.transport(backwards)
          p <- pialTk.transport(backwards)
        yield (w, p)
      case _ =>
        for
          w <- whiteTk.toScanner(scanner, orig)
          p <- pialTk.toScanner(scanner, orig)
        yield (w, p)

  private def runScenario(mutation: Mutation): ScenarioResult =
    val gridAffine =
      if mutation == Mutation.VolumeAxesReadAsLps then volumeAffine.zipWithIndex.map((v, i) => if i < 8 then -v else v)
      else volumeAffine
    val grid = ok(GridSpec.in(scanner)(SpatialDims(volumeDims(0), volumeDims(1), volumeDims(2)), affine(gridAffine)))

    // (1) tkRAS -> scanner, (2) ribbon sampling of the scanner-space volume
    val sampled =
      for
        surfaces <- surfacesInScanner(mutation).left.map(_.message)
        operator <- RibbonOperator.compile(surfaces._1, surfaces._2, grid, ok(RibbonSteps(6))).left.map(_.message)
        values <- operator.sample(volume).left.map(_.message)
      yield (surfaces._1, values)

    // reference: the field at the mean of each segment's samples, which is the scanner position of the mid-thickness
    // point `(white + pial) / 2 + c_ras`
    val expectedVertex = Array.tabulate(whiteMesh.vertexCount): v =>
      field(Vector.tabulate(3)(a => (whiteMesh.coordinates(3 * v + a) + pialMesh.coordinates(3 * v + a)) / 2.0 + cRas(a)))

    // (3) resampling from the registered sphere: the per-vertex values are a field linear on the sphere,
    // g(x) = field(centre + midRadius Q^-1 x / 100), whose gradient norm is |gradient| midRadius / 100
    val midRadius = (whiteRadius + pialRadius) / 2.0
    val inverseQ = rotation(registrationAngle).transpose
    def onSphere(mesh: TriangleMesh, i: Int): Double =
      val x = Vector(mesh.coordinates(3 * i), mesh.coordinates(3 * i + 1), mesh.coordinates(3 * i + 2))
      val unit = x.map(_ / math.sqrt(x.map(v => v * v).sum))
      field(rotate(inverseQ, unit).zip(centreScanner).map((u, c) => c + midRadius * u))
    val gradientOnSphere = math.sqrt(gradient.map(g => g * g).sum) * midRadius / 100.0
    val (subjectSag, fsaverageSag) = (sagitta(sphereReg), sagitta(fsaverageLike))

    def resample(target: TriangleMesh, moving: TriangleMesh, values: Array[Double]): Either[String, Array[Double]] =
      val (ref, mov) = if mutation == Mutation.ResamplingDirectionSwapped then (moving, target) else (target, moving)
      SurfaceResampling.plan(ref, mov).flatMap(plan => SurfaceResampling.apply(plan, values)).left.map(_.message)

    def errorTo(target: TriangleMesh, values: Array[Double]): Double =
      if values.length != target.vertexCount then Double.PositiveInfinity
      else (0 until target.vertexCount).map(i => math.abs(values(i) - onSphere(target, i))).max

    val observations: Vector[ScenarioObservation] = sampled match
      case Left(error) => Vector(ScenarioHarness.fact("ribbon.sample", false, error))
      case Right((white, values)) =>
        val vertexErrors = values.indices.map(v => math.abs(values(v) - expectedVertex(v)))
        // where the scanner-space white surface landed: its centre is the tkRAS centre shifted by c_ras
        val whiteCentre = Vector.tabulate(3)(a => (0 until white.vertexCount).map(v => white.coordinates(3 * v + a)).sum / white.vertexCount)
        val chain =
          for
            fsaverage <- resample(fsaverageLike, sphereReg, values)
            twoStep <- resample(fsLrLike, fsaverageLike, fsaverage)
            direct <- resample(fsLrLike, sphereReg, values)
          yield (fsaverage, twoStep, direct)
        val resampling = chain match
          case Left(error) => Vector(ScenarioHarness.fact("resampling.plans", false, error))
          case Right((fsaverage, twoStep, direct)) =>
            val gap = if twoStep.length == direct.length then twoStep.indices.map(i => math.abs(twoStep(i) - direct(i))).max else Double.PositiveInfinity
            Vector(
              ScenarioHarness.fact("resampling.sagitta", subjectSag > 0.0 && fsaverageSag > 0.0, f"subject $subjectSag%.4f mm, fsaverage-like $fsaverageSag%.4f mm at radius 100"),
              ScenarioHarness.scalar("resampling.subject-to-fsaverage.max-abs", errorTo(fsaverageLike, fsaverage), 0.0, ScenarioTolerance.absolute(gradientOnSphere * subjectSag)),
              ScenarioHarness.scalar("resampling.subject-to-fslr-direct.max-abs", errorTo(fsLrLike, direct), 0.0, ScenarioTolerance.absolute(gradientOnSphere * subjectSag)),
              ScenarioHarness.scalar("resampling.subject-to-fsaverage-to-fslr.max-abs", errorTo(fsLrLike, twoStep), 0.0, ScenarioTolerance.absolute(gradientOnSphere * (subjectSag + fsaverageSag))),
              // the two routes commute up to the sum of their interpolation bounds
              ScenarioHarness.scalar("resampling.commutativity.max-abs", gap, 0.0, ScenarioTolerance.absolute(gradientOnSphere * (2.0 * subjectSag + fsaverageSag)))
            )
        Vector(
          ScenarioHarness.scalar("tkras-to-scanner.white-centre.max-abs-mm", whiteCentre.zip(centreScanner).map((a, e) => math.abs(a - e)).max, 0.0, ScenarioTolerance.absolute(1e-9)),
          ScenarioHarness.finite("ribbon.sampled-values", values),
          // trilinear interpolation of a linear field is exact; the residual is floating-point rounding
          ScenarioHarness.scalar("ribbon.sample-vs-mid-thickness-field.max-abs", vertexErrors.max, 0.0, ScenarioTolerance.absolute(1e-9))
        ) ++ resampling
    ScenarioHarness.result(Id, observations, Caveats)

  // ----------------------------------------------------------------------------------------------------- tests

  test("surface chain: scanner volume -> tkRAS ribbon -> registered sphere -> template meshes, commuting routes"):
    val result = runScenario(Mutation.Faithful)
    assertEquals(result.status, ScenarioStatus.PassWithCaveats, result.render)
    assert(!result.ciPass, "the declared caveats must not pass the default clean-Pass policy")
    if !result.ciPass(Policy) then fail(result.render)

  test("every convention mutation of the surface chain fails the scenario"):
    val guards = Map(
      Mutation.TkRasTakenAsScanner -> "tkras-to-scanner.white-centre.max-abs-mm",
      Mutation.TkRasLinkReversed -> "tkras-to-scanner.white-centre.max-abs-mm",
      Mutation.VolumeAxesReadAsLps -> "ribbon.sample", // the sampling step, its finiteness, or its values
      Mutation.ResamplingDirectionSwapped -> "resampling.plans"
    )
    assertEquals(guards.keySet, Mutation.values.toSet - Mutation.Faithful)
    guards.foreach: (mutation, guard) =>
      val result = runScenario(mutation)
      assertEquals(result.status, ScenarioStatus.Fail, s"$mutation must fail:\n${result.render}")
      assert(result.failures.exists(_.render.startsWith(guard)), s"$mutation must fail $guard:\n${result.render}")

  test("a tkRAS surface cannot be sampled on a scanner-space grid without moving it first"):
    val errors = compileErrors("RibbonOperator.compile(whiteTk, pialTk, GridSpec.in(scanner)(SpatialDims(2, 2, 2), affine(volumeAffine)).toOption.get)")
    assert(errors.contains("Found:") || errors.contains("Required:"), errors)

object SurfaceChainScenarioSuite:
  /** Convention errors a surface chain can make; `Faithful` is the correct reading. */
  enum Mutation derives CanEqual:
    case Faithful, TkRasTakenAsScanner, TkRasLinkReversed, VolumeAxesReadAsLps, ResamplingDirectionSwapped

  /** The template-volume leg of this workflow (a MNI152NLin2009cAsym volume brought to fsLR, whose volumetric companion
    * is MNI152NLin6Asym) needs the nonlinear template bridge, which is not executable yet. The leg is declared, not run,
    * and never approximated with an affine.
    */
  val TemplateBridge: ScenarioCaveat = ScenarioCaveat(
    id = "surface.mni152-nlin6-nlin2009c-bridge",
    kind = CaveatKind.PublicApiGap,
    severity = CaveatSeverity.Actionable,
    owner = "spatial",
    followUp = Some("bd-01M37FQFRRF1TW2REJPWS8BRM8: executable MNI152NLin6Asym <-> MNI152NLin2009cAsym TemplateFlow H5 steps"),
    detail = "the MNI152NLin2009cAsym volume -> fsLR leg is not executed; it needs the MNI152NLin6Asym <-> 2009cAsym nonlinear bridge"
  )

  /** Ribbon QC by overlap (Dice/Jaccard of the operator's support against the geometric ribbon mask) belongs to locus4s
    * region algebra and is not gated here.
    */
  val RibbonOverlap: ScenarioCaveat = ScenarioCaveat(
    id = "surface.ribbon-overlap-metrics",
    kind = CaveatKind.DiagnosticsGap,
    severity = CaveatSeverity.Actionable,
    owner = "surface",
    followUp = Some("STP P7.05: overlap metrics in locus4s, blocked on the upstream locus4s push"),
    detail = "Dice/Jaccard agreement between the ribbon operator's support and RibbonMask is not gated"
  )

  val Caveats: Vector[ScenarioCaveat] = Vector(TemplateBridge, RibbonOverlap)

  /** The manifest's policy for this scenario: pass with exactly the two declared caveats. */
  val Policy: ScenarioPolicy = ScenarioPolicy.allowCaveats(Caveats.map(_.id)*)
