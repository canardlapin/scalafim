package scalafim.atlas.scenarios

import image4s.{BoundaryPolicy, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, D3, Frame}
import ravel.NDArray
import reframe4s.lie.FramedAffine
import scalafim.atlas.{DataKind, MniTemplateBridge, Point3D, SpaceId, SpaceTransforms, TransformBackend}
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

/** Scenario `surface.volume-to-template-mesh-chain.v1`: the surface leg of an fMRIPrep-style workflow, in two parts.
  *
  * Subject leg (every platform). A volume in the subject's scanner RAS is sampled through the cortical ribbon of
  * FreeSurfer surfaces stored in tkRAS, and the per-vertex values are resampled from the subject's registered sphere
  * onto template meshes, both through an intermediate (fsaverage-like) mesh and directly (fsLR-like), which must
  * commute.
  *
  * Template-volume leg (JVM, TemplateFlow assets). A group volume in MNI152NLin2009cAsym is brought to fsLR 32k, whose
  * anatomy (the Conte69 midthickness) is in MNI152NLin6Asym: each fsLR vertex is carried to 2009c by the TemplateFlow
  * bridge's forward map (`MniTemplateBridge` with its qualified numerical inverse), the volume is sampled there
  * (`FramedSurface.transport`, `RibbonOperator` on the midthickness), and the fsLR values are routed on to fsaverage
  * through the transform graph's sphere-resampling operator. The bridge is anchored to SimpleITK: the graph's
  * 6Asym -> 2009c route pulls 2009c points as `TransformPoint` does and pushes 6Asym points to its fixed-point inverse.
  *
  * Workflow risk protected: a FreeSurfer surface sits in tkRAS, not in the scanner RAS the volume lives in; tkRAS ->
  * scanner is `Norig * inverse(Torig)`, which for a conformed volume is a shift by `c_ras`. HCP's fsLR anatomy sits in
  * MNI152NLin6Asym, not in the MNI152NLin2009cAsym of fMRIPrep's group outputs. The scenario fails if the tkRAS
  * coordinates are taken as scanner coordinates, if the tkRAS link is applied backwards, if the volume's RAS axes are
  * read as LPS, if a resampling plan is applied against its direction, if the two MNI templates are taken as one, or if
  * the bridge is run backwards; each mutation is run below.
  *
  * References are mathematical. Subject leg: the volume is a linear field, so trilinear ribbon sampling returns the
  * field at the mean of each white -> pial segment exactly, and the scanner position is the documented FreeSurfer
  * relation `scanner = tkRAS + c_ras` with `c_ras = Norig * (dims / 2)` (the Norig of
  * `conventions/freesurfer_conformed.tsv` case 1, a nibabel reference for a conformed 256^3 LIA volume). Barycentric
  * resampling of a field linear on the sphere errs by at most the field gradient times the moving mesh's sagitta,
  * which is computed from the meshes. Template leg: the 2009c volume is a field `h` linear in 6Asym millimetres pulled
  * back through the composite onto the composite's own 1 mm lattice, `g(q) = h(pull(q))`; the composite is trilinear
  * on that lattice, so trilinear sampling of `g` at a vertex's 2009c image `push(p)` is exactly `h(pull(push(p)))`, and
  * it differs from `h(p)` by at most `|grad h|` times the round-trip residual the inverse was qualified with.
  */
class SurfaceChainScenarioSuite extends munit.FunSuite:
  import SurfaceChainScenarioSuite.{Mutation, Policy, RibbonOverlap, TemplateAssetsAbsent}

  // Qualifying the bridge's numerical inverse (shared with the bridge's own suite) takes over a minute on the JVM.
  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(10, "min")

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
  // the same resolution as the subject sphere, so a plan applied against its direction still type-checks by length
  private val fsLrLike = rotatedSphere(3, 0.91)

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

    /** The largest entry; NaN if empty or if any entry is NaN. */
    def worst(values: Iterable[Double]): Double =
      values.foldLeft(Option.empty[Double])((acc, v) => Some(acc.fold(v)(a => if a.isNaN || v.isNaN then Double.NaN else math.max(a, v)))).getOrElse(Double.NaN)

    def errorTo(target: TriangleMesh, values: Array[Double]): Double =
      if values.length != target.vertexCount then Double.PositiveInfinity
      else worst((0 until target.vertexCount).map(i => math.abs(values(i) - onSphere(target, i))))

    def routeError(name: String, route: Either[String, Array[Double]], target: TriangleMesh, bound: Double): ScenarioObservation =
      route.fold(error => ScenarioHarness.fact(name, false, error), values => ScenarioHarness.scalar(name, errorTo(target, values), 0.0, ScenarioTolerance.absolute(bound)))

    val observations: Vector[ScenarioObservation] = sampled match
      case Left(error) => Vector(ScenarioHarness.fact("ribbon.sample", false, error))
      case Right((white, values)) =>
        val vertexErrors =
          if values.length != expectedVertex.length then Vector(Double.PositiveInfinity)
          else values.indices.map(v => math.abs(values(v) - expectedVertex(v)))
        // where the scanner-space white surface landed: its centre is the tkRAS centre shifted by c_ras
        val whiteCentre = Vector.tabulate(3)(a => (0 until white.vertexCount).map(v => white.coordinates(3 * v + a)).sum / white.vertexCount)
        // each route is judged on its own, so a failure names the route that broke
        val fsaverage = resample(fsaverageLike, sphereReg, values)
        val twoStep = fsaverage.flatMap(resample(fsLrLike, fsaverageLike, _))
        val direct = resample(fsLrLike, sphereReg, values)
        val gap =
          for
            a <- twoStep
            b <- direct
          yield if a.length == b.length then worst(a.indices.map(i => math.abs(a(i) - b(i)))) else Double.PositiveInfinity
        val resampling = Vector(
          ScenarioHarness.fact("resampling.sagitta", subjectSag > 0.0 && fsaverageSag > 0.0, f"subject $subjectSag%.4f mm, fsaverage-like $fsaverageSag%.4f mm at radius 100"),
          routeError("resampling.subject-to-fsaverage.max-abs", fsaverage, fsaverageLike, gradientOnSphere * subjectSag),
          routeError("resampling.subject-to-fslr-direct.max-abs", direct, fsLrLike, gradientOnSphere * subjectSag),
          routeError("resampling.subject-to-fsaverage-to-fslr.max-abs", twoStep, fsLrLike, gradientOnSphere * (subjectSag + fsaverageSag)),
          // the two routes commute up to the sum of their interpolation bounds
          gap.fold(
            error => ScenarioHarness.fact("resampling.commutativity.max-abs", false, error),
            g => ScenarioHarness.scalar("resampling.commutativity.max-abs", g, 0.0, ScenarioTolerance.absolute(gradientOnSphere * (2.0 * subjectSag + fsaverageSag)))
          )
        )
        Vector(
          ScenarioHarness.scalar("tkras-to-scanner.white-centre.max-abs-mm", whiteCentre.zip(centreScanner).map((a, e) => math.abs(a - e)).max, 0.0, ScenarioTolerance.absolute(1e-9)),
          ScenarioHarness.fact("ribbon.sampled-vertices", values.length == whiteMesh.vertexCount, s"${values.length} of ${whiteMesh.vertexCount} vertices"),
          ScenarioHarness.finite("ribbon.sampled-values", values),
          // trilinear interpolation of a linear field is exact; the residual is floating-point rounding
          ScenarioHarness.scalar("ribbon.sample-vs-mid-thickness-field.max-abs", worst(vertexErrors), 0.0, ScenarioTolerance.absolute(1e-9))
        ) ++ resampling
    val (template, templateCaveats) = templateLeg(mutation)
    ScenarioHarness.result(Id, observations ++ template, RibbonOverlap +: templateCaveats)

  // ------------------------------------------------------------------------------------------ template-volume leg

  private val templateAssets: TemplateLegAssets = TemplateLegPlatform.assets

  /** The 6Asym field the group volume is made of: `h(y) = 100 + a . y`, `a` per 6Asym millimetre. */
  private val templateGradient = Vector(0.8, -0.5, 1.2)
  private def templateField(x: Double, y: Double, z: Double): Double =
    100.0 + templateGradient(0) * x + templateGradient(1) * y + templateGradient(2) * z
  private val templateGradientNorm = math.sqrt(templateGradient.map(g => g * g).sum)

  /** The group volume `g(q) = h(pull(q))` on a crop of the composite's own MNI152NLin2009cAsym 1 mm lattice (res-01,
    * first voxel (-96, -132, -78)) around the left fsLR hemisphere, with a margin of over 10 mm for the bridge's shift.
    * `h` is sampled on a generous 4 mm 6Asym grid, which trilinear interpolation of a linear field reproduces exactly.
    */
  private final class TemplateVolume(val grid: GridSpec[Spaces.Mni2009c], val values: Array[Double])

  private val templateVolumes = scala.collection.mutable.Map.empty[MniTemplateBridge, Either[String, TemplateVolume]]

  private def templateVolume(bridge: MniTemplateBridge): Either[String, TemplateVolume] =
    templateVolumes.getOrElseUpdate(
      bridge, {
        val (nx, ny, nz) = (64, 76, 60)
        val (cx, cy, cz) = (93, 201, 153)
        for
          source <- GridSpec.in(Spaces.MNI152NLin6Asym)(SpatialDims(nx, ny, nz), cardinal(4.0, -120.0, -160.0, -110.0)).left.map(_.message)
          image <- Sampled
            .continuous(source.grid, NonSpatialAxes.empty, NDArray.tabulate[Double](nx, ny, nz)((i, j, k) => templateField(-120.0 + 4.0 * i, -160.0 + 4.0 * j, -110.0 + 4.0 * k)))
            .left
            .map(_.toString)
          crop <- GridSpec.in(Spaces.MNI152NLin2009cAsym)(SpatialDims(cx, cy, cz), cardinal(1.0, -80.0, -118.0, -60.0)).left.map(_.message)
          resampled <- bridge.transform.resample(image, crop.grid, boundary = BoundaryPolicy.Reject).left.map(_.message)
        yield
          val data = resampled.image.data
          val values = new Array[Double](cx * cy * cz)
          for i <- 0 until cx; j <- 0 until cy; k <- 0 until cz do values((i * cy + j) * cz + k) = data.at(IArray(i, j, k))
          TemplateVolume(crop, values)
      }
    )

  private def cardinal(spacing: Double, x: Double, y: Double, z: Double): Affine[D3] =
    affine(Vector(spacing, 0.0, 0.0, x, 0.0, spacing, 0.0, y, 0.0, 0.0, spacing, z, 0.0, 0.0, 0.0, 1.0))

  /** The standard manifest with the bridge installed and the left fsaverage and fsLR 32k spaces sampled; under
    * [[Mutation.SphereResampledByNearestVertex]] its fsaverage <-> fsLR steps resample by nearest vertex instead.
    */
  private val templateGraphs = scala.collection.mutable.Map.empty[(TemplateLegInputs, Boolean), Either[String, scalafim.atlas.SpaceTransformGraph]]

  private def templateGraph(inputs: TemplateLegInputs, mutation: Mutation): Either[String, scalafim.atlas.SpaceTransformGraph] =
    val nearest = mutation == Mutation.SphereResampledByNearestVertex
    templateGraphs.getOrElseUpdate(
      (inputs, nearest), {
        val manifest = inputs.bridge.install(SpaceTransforms.manifest)
        val registry =
          if !nearest then manifest
          else manifest.map(step => if step.backend == TransformBackend.Workbench then step.copy(backend = TransformBackend.SphereNearest) else step)
        SpaceTransforms.graph(registry, sampling = inputs.sampling).left.map(_.message)
      }
    )

  /** The fsLR midthickness where the volume is sampled, in 2009c: through the bridge's forward map, or as a mutation. */
  private def midthicknessIn2009c(inputs: TemplateLegInputs, mutation: Mutation): Either[String, FramedSurface[Spaces.Mni2009c]] =
    val placement = SurfacePlacement.StoredCoordinates
    mutation match
      case Mutation.MniTemplatesTakenAsOne =>
        FramedSurface.in(Spaces.MNI152NLin2009cAsym)(inputs.midthickness, placement).left.map(_.message)
      case Mutation.TemplateBridgeRunBackwards =>
        // the pullback (2009c points to 6Asym) used as if it carried 6Asym points to 2009c
        for
          misread <- FramedSurface.in(Spaces.MNI152NLin2009cAsym)(inputs.midthickness, placement).left.map(_.message)
          moved <- misread.transport(inputs.bridge.transform.pull).left.map(_.message)
          relabelled <- FramedSurface.in(Spaces.MNI152NLin2009cAsym)(moved.geometry, placement).left.map(_.message)
        yield relabelled
      case _ =>
        for
          anatomy <- FramedSurface.in(Spaces.MNI152NLin6Asym)(inputs.midthickness, placement).left.map(_.message)
          push <- inputs.bridge.transform.push.toRight("the bridge has no forward map: no numerical inverse is attached")
          moved <- anatomy.transport(push).left.map(_.message)
        yield moved

  /** The leg's observations and caveats: executed with its assets, a declared caveat without them, a failure when an
    * asset is present but unusable.
    */
  private def templateLeg(mutation: Mutation): (Vector[ScenarioObservation], Vector[ScenarioCaveat]) =
    templateAssets match
      case TemplateLegAssets.Absent(missing) =>
        (Vector.empty, Vector(TemplateAssetsAbsent.copy(detail = s"${TemplateAssetsAbsent.detail}; absent: ${missing.mkString(", ")}")))
      case TemplateLegAssets.Unusable(reason) =>
        (Vector(ScenarioHarness.fact("template.assets", false, reason)), Vector.empty)
      case TemplateLegAssets.Ready(inputs) =>
        (templateObservations(inputs, mutation), Vector.empty)

  private def distance(a: Vector[Double], b: Vector[Double]): Double = math.sqrt(a.zip(b).map((x, y) => (x - y) * (x - y)).sum)

  /** The largest entry; NaN if empty or if any entry is NaN, so an empty comparison never passes. */
  private def worstOf(values: Iterable[Double]): Double =
    values.foldLeft(Option.empty[Double])((acc, v) => Some(acc.fold(v)(a => if a.isNaN || v.isNaN then Double.NaN else math.max(a, v)))).getOrElse(Double.NaN)

  /** The 99th percentile; NaN if empty or if any entry is NaN. */
  private def p99Of(values: Array[Double]): Double =
    if values.isEmpty || values.exists(_.isNaN) then Double.NaN
    else
      val sorted = values.sorted
      sorted(math.min(sorted.length - 1, math.ceil(0.99 * sorted.length).toInt - 1))

  private def templateObservations(inputs: TemplateLegInputs, mutation: Mutation): Vector[ScenarioObservation] =
    // (1) the bridge in the transform graph, anchored to SimpleITK in both directions
    val anchors =
      templateGraph(inputs, mutation).flatMap(_.plan(SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym, DataKind.Voxel).left.map(_.message)) match
        case Left(error) => Vector(ScenarioHarness.fact("template.bridge-route", false, error))
        case Right(route) =>
          val pulled = route.pullPoints(inputs.pullOracle.map(row => Point3D.fromVector(row._1))).left.map(_.message)
          val pushed = route.transform(inputs.pushOracle.map(row => Point3D.fromVector(row._1))).left.map(_.message)
          Vector(
            pulled.fold(
              error => ScenarioHarness.fact("template.itk-pull-2009c-to-6asym.max-mm", false, error),
              points => ScenarioHarness.scalar("template.itk-pull-2009c-to-6asym.max-mm", worstOf(points.zip(inputs.pullOracle).map((p, row) => distance(p.toVector, row._2))), 0.0, ScenarioTolerance.absolute(1e-8))
            ),
            // the forward map interpolates inverse samples on the 1 mm 6Asym lattice (MniTemplateBridgeFilesSuite)
            pushed.fold(
              error => ScenarioHarness.fact("template.itk-push-6asym-to-2009c.max-mm", false, error),
              points => ScenarioHarness.scalar("template.itk-push-6asym-to-2009c.max-mm", worstOf(points.zip(inputs.pushOracle).map((p, row) => distance(p.toVector, row._2))), 0.0, ScenarioTolerance.absolute(0.05))
            )
          )

    // (2) the 2009c volume sampled at every fsLR vertex's 2009c position
    val sampled =
      for
        volume <- templateVolume(inputs.bridge)
        surface <- midthicknessIn2009c(inputs, mutation)
        operator <- RibbonOperator.compile(surface, surface, volume.grid, ok(RibbonSteps(1))).left.map(_.message)
        values <- operator.sample(volume.values).left.map(_.message)
      yield values

    // references, per vertex p: the exact value h(pull(push(p))) the chain computes (the composite is trilinear on the
    // volume's lattice), the round trip |pull(push(p)) - p|, and the 6Asym field h(p) itself
    val gates = MniTemplateBridge.defaultInversionPolicy.gates
    val c = inputs.midthickness.mesh.coordinates
    val n = inputs.midthickness.vertexCount
    val sixAsym = Vector.tabulate(n)(v => Vector(c(3 * v), c(3 * v + 1), c(3 * v + 2)))
    val expected = sixAsym.map(p => templateField(p(0), p(1), p(2))).toArray
    val roundTrips: Either[String, Vector[Vector[Double]]] =
      sixAsym.foldLeft[Either[String, Vector[Vector[Double]]]](Right(Vector.empty)): (acc, p) =>
        for
          out <- acc
          point <- image4s.geometry.Point.in(Spaces.MNI152NLin6Asym)(p(0), p(1), p(2)).left.map(_.message)
          pushed <- inputs.bridge.transform.mapPoint(point).left.map(_.message)
          back <- inputs.bridge.transform.pullPoint(pushed).left.map(_.message)
        yield out :+ back.coordinates

    val topology =
      val sphere = inputs.sampling.geometry(SpaceId.FsLR32k)
      ScenarioHarness.fact(
        "template.midthickness-matches-fslr-sphere",
        sphere.exists(_.mesh.hasSameTopology(inputs.midthickness.mesh)),
        "the midthickness and the fsLR 32k sphere must share vertex count and ordered faces"
      )

    val values = (sampled, roundTrips) match
      case (Left(error), _) => Vector(ScenarioHarness.fact("template.fslr-sample", false, error))
      case (_, Left(error)) => Vector(ScenarioHarness.fact("template.fslr-round-trip", false, error))
      case (Right(values), Right(backs)) =>
        val errors = values.indices.map(v => math.abs(values(v) - expected(v))).toArray
        val exact = values.indices.map(v => math.abs(values(v) - templateField(backs(v)(0), backs(v)(1), backs(v)(2)))).toArray
        val trips = sixAsym.indices.map(v => distance(backs(v), sixAsym(v))).toArray
        Vector(
          ScenarioHarness.fact("template.fslr-sampled-vertices", values.length == n, s"${values.length} of $n fsLR 32k vertices (${inputs.midthicknessAsset})"),
          ScenarioHarness.finite("template.fslr-sampled-values", values),
          // transport + trilinear sampling reproduce h(pull(push(p))) up to rounding (values are about 10..200)
          ScenarioHarness.scalar("template.fslr-value-vs-exact-chain.max-abs", worstOf(exact), 0.0, ScenarioTolerance.absolute(1e-9)),
          // the forward map's round trip at the vertices, against the gates it was qualified with on its lattice
          ScenarioHarness.scalar("template.fslr-vertex-round-trip.max-mm", worstOf(trips), 0.0, ScenarioTolerance.absolute(gates.maximumResidual)),
          ScenarioHarness.scalar("template.fslr-vertex-round-trip.p99-mm", p99Of(trips), 0.0, ScenarioTolerance.absolute(gates.p99Residual)),
          // hence the workflow's value: h(p) within |grad h| times those gates
          ScenarioHarness.scalar("template.fslr-value-vs-6asym-field.max-abs", worstOf(errors), 0.0, ScenarioTolerance.absolute(templateGradientNorm * gates.maximumResidual)),
          ScenarioHarness.scalar("template.fslr-value-vs-6asym-field.p99-abs", p99Of(errors), 0.0, ScenarioTolerance.absolute(templateGradientNorm * gates.p99Residual))
        )

    anchors ++ Vector(topology) ++ values ++ onward(inputs, mutation)

  /** (3) fsLR 32k vertex data on to fsaverage through the graph's sphere-resampling operator. The probe is a field
    * linear in fsaverage-sphere coordinates, `s(x) = 5 + b . x / 100`, given at the fsLR 32k sphere's vertices: a
    * barycentric row evaluates it at the radial hit on the fsLR sphere's chord triangle, so each fsaverage vertex's
    * value is within `|b|` times the fsLR sphere's sagitta of `s` there.
    */
  private def onward(inputs: TemplateLegInputs, mutation: Mutation): Vector[ScenarioObservation] =
    val b = Vector(1.3, -0.7, 0.9)
    def probe(geometry: SurfaceGeometry): Array[Double] =
      val x = geometry.mesh.coordinates
      Array.tabulate(geometry.vertexCount)(i => 5.0 + (b(0) * x(3 * i) + b(1) * x(3 * i + 1) + b(2) * x(3 * i + 2)) / 100.0)
    val routed =
      for
        fsLR <- inputs.sampling.geometry(SpaceId.FsLR32k).toRight("fsLR 32k is not sampled")
        fsAverage <- inputs.sampling.geometry(SpaceId.FsAverage).toRight("fsaverage is not sampled")
        graph <- templateGraph(inputs, mutation)
        operator <- graph.vertexOperator(SpaceId.FsLR32k, SpaceId.FsAverage).left.map(_.message)
        out <- operator.forward(column(probe(fsLR))).left.map(_.toString)
      yield (operator, Array.tabulate(out.rows)(out(_, 0)), probe(fsAverage), sagitta(fsLR.mesh))
    routed match
      case Left(error) => Vector(ScenarioHarness.fact("template.fslr-to-fsaverage", false, error))
      case Right((operator, out, expected, sag)) =>
        val bound = math.sqrt(b.map(v => v * v).sum) / 100.0 * sag + 1e-12
        Vector(
          ScenarioHarness.fact(
            "template.fslr-to-fsaverage.rows",
            out.length == expected.length && operator.qc.coverage.rowCoverage.forall(c => math.abs(c - 1.0) <= 1e-12),
            s"${out.length} fsaverage vertices; path ${operator.path.ids.map(_.value).mkString(" -> ")}"
          ),
          if out.length != expected.length then ScenarioHarness.fact("template.fslr-to-fsaverage.sphere-field.max-abs", false, s"${out.length} values for ${expected.length} vertices")
          else ScenarioHarness.scalar("template.fslr-to-fsaverage.sphere-field.max-abs", worstOf(out.indices.map(i => math.abs(out(i) - expected(i)))), 0.0, ScenarioTolerance.absolute(bound))
        )

  private def column(values: Array[Double]): gale.linalg.DMat =
    val builder = gale.linalg.DMat.newBuilder(values.length, 1)
    values.indices.foreach(i => builder.writeLinear(i, values(i)))
    builder.result()

  // ----------------------------------------------------------------------------------------------------- tests

  test("surface chain: scanner volume -> tkRAS ribbon -> registered sphere -> template meshes, commuting routes"):
    val result = runScenario(Mutation.Faithful)
    assertEquals(result.status, ScenarioStatus.PassWithCaveats, result.render)
    assert(!result.ciPass, "the declared caveats must not pass the default clean-Pass policy")
    if !result.ciPass(Policy) then fail(result.render)
    // the absence caveat stands exactly for an unexecuted template leg: it never hides a leg that ran
    val templateRan = result.observations.exists(_.render.startsWith("template."))
    assertEquals(result.caveats.exists(_.id == TemplateAssetsAbsent.id), !templateRan, result.render)
    templateAssets match
      case TemplateLegAssets.Absent(_) => assert(!templateRan)
      case _                           => assert(templateRan, "assets are present: the template leg must run")
    println(s"surface chain scenario: ${result.status}; caveats ${result.caveats.map(_.id).mkString(", ")}")
    result.observations.map(_.render).filter(_.startsWith("template.")).foreach(line => println(s"  $line"))

  test("every convention mutation of the subject leg fails the scenario"):
    val guards = Map(
      Mutation.TkRasTakenAsScanner -> "tkras-to-scanner.white-centre.max-abs-mm:",
      Mutation.TkRasLinkReversed -> "tkras-to-scanner.white-centre.max-abs-mm:",
      // the mirrored grid no longer covers the ribbon: every segment samples outside it
      Mutation.VolumeAxesReadAsLps -> "ribbon.sampled-values:",
      // same-resolution meshes: the swapped plan runs, and its values are wrong
      Mutation.ResamplingDirectionSwapped -> "resampling.subject-to-fslr-direct.max-abs:"
    )
    assertEquals(guards.keySet ++ Mutation.template, Mutation.values.toSet - Mutation.Faithful)
    guards.foreach: (mutation, guard) =>
      val result = runScenario(mutation)
      assertEquals(result.status, ScenarioStatus.Fail, s"$mutation must fail:\n${result.render}")
      assert(result.failures.exists(_.render.startsWith(guard)), s"$mutation must fail $guard:\n${result.render}")

  test("every convention mutation of the template leg fails the scenario"):
    assume(templateAssets.isInstanceOf[TemplateLegAssets.Ready], s"template leg not executed here: $templateAssets")
    val guards = Map(
      Mutation.MniTemplatesTakenAsOne -> "template.fslr-value-vs-6asym-field.max-abs:",
      Mutation.TemplateBridgeRunBackwards -> "template.fslr-value-vs-6asym-field.max-abs:",
      Mutation.SphereResampledByNearestVertex -> "template.fslr-to-fsaverage.sphere-field.max-abs:"
    )
    assertEquals(guards.keySet, Mutation.template)
    guards.foreach: (mutation, guard) =>
      val result = runScenario(mutation)
      assertEquals(result.status, ScenarioStatus.Fail, s"$mutation must fail:\n${result.render}")
      assert(result.failures.exists(_.render.startsWith(guard)), s"$mutation must fail $guard:\n${result.render}")
      result.failures.map(_.render).filter(_.startsWith("template.")).foreach(line => println(s"  $mutation: $line"))

  test("a tkRAS surface cannot be sampled on a scanner-space grid without moving it first"):
    val errors = compileErrors("RibbonOperator.compile(whiteTk, pialTk, GridSpec.in(scanner)(SpatialDims(2, 2, 2), affine(volumeAffine)).toOption.get)")
    assert(errors.contains("Required:") && errors.contains("this.tkRas") && errors.contains("this.scanner"), errors)

object SurfaceChainScenarioSuite:
  /** Convention errors a surface chain can make; `Faithful` is the correct reading. */
  enum Mutation derives CanEqual:
    case Faithful, TkRasTakenAsScanner, TkRasLinkReversed, VolumeAxesReadAsLps, ResamplingDirectionSwapped

    /** MNI152NLin6Asym anatomy read as MNI152NLin2009cAsym: a generic "MNI" alias instead of the bridge. */
    case MniTemplatesTakenAsOne

    /** The bridge's pullback (2009c points to 6Asym) applied to 6Asym vertices as if it pushed them to 2009c. */
    case TemplateBridgeRunBackwards

    /** fsLR 32k -> fsaverage resampled by nearest vertex instead of Workbench's barycentric interpolation. */
    case SphereResampledByNearestVertex

  object Mutation:
    val template: Set[Mutation] =
      Set(Mutation.MniTemplatesTakenAsOne, Mutation.TemplateBridgeRunBackwards, Mutation.SphereResampledByNearestVertex)

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

  /** The template-volume leg needs TemplateFlow assets that are never committed; where they are absent (every Scala.js
    * run, and a JVM without a TemplateFlow cache) the leg is not executed and this caveat says so. It is emitted only
    * then: an asset that is present but refused fails the scenario instead.
    */
  val TemplateAssetsAbsent: ScenarioCaveat = ScenarioCaveat(
    id = "surface.template-leg-assets-absent",
    kind = CaveatKind.FixtureFreshness,
    severity = CaveatSeverity.Actionable,
    owner = "atlas",
    followUp = Some(
      "run atlasJVM/test with a TemplateFlow cache holding tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5, " +
        "the fsLR 32k left midthickness and the left fsaverage-sphere spheres of fsaverage and fsLR 32k"
    ),
    detail = "the MNI152NLin2009cAsym volume -> fsLR 32k -> fsaverage leg was not executed: its TemplateFlow assets are absent"
  )

  /** The manifest's policy for this scenario: pass with the ribbon-overlap caveat, plus the absence caveat when the
    * template leg's assets are absent.
    */
  val Policy: ScenarioPolicy =
    ScenarioPolicy(Set(ScenarioStatus.PassWithCaveats), Set(RibbonOverlap.id, TemplateAssetsAbsent.id))
