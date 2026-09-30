package scalafim.transform.scenarios

import image4s.{BoundaryPolicy, NonSpatialAxes, Sampled}
import image4s.geometry.{D3, Frame, Grid, GridId, LatticeIndex, Point}
import ravel.DType.given
import ravel.NDArray
import reframe4s.core.MapError
import reframe4s.field.CoordinateBoundaryPolicy
import reframe4s.resample.Interpolation
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.scenarios.{CaveatKind, CaveatSeverity, ScenarioCaveat, ScenarioHarness, ScenarioObservation, ScenarioPolicy, ScenarioResult, ScenarioStatus, ScenarioTolerance}
import scalafim.transform.*
import scalafim.transform.field.{DenseContext, LatticeAffine}
import scalafim.transform.itk.{ItkHdf5Component, ItkHdf5Dumps, ItkHdf5File, ItkHdf5Interpretation, ItkLinearInterpretation, ItkTextCodec}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}
import ChainScenarioSupport.*

/** Scenario `transform.fmriprep-chain.v1`: the fMRIPrep registration chain, boldref -> T1w (an ITK rigid text
  * transform, as `from-boldref_to-T1w_mode-image_xfm.txt`) followed by T1w -> template (an ANTs/ITK HDF5 composite of an
  * affine and a displacement field, as `from-T1w_to-<template>_mode-image_xfm.h5`).
  *
  * Workflow risk protected: a template-space point or voxel must pull back to the right subject location through two
  * toolkit files whose conventions all differ from RAS world space. The chain is wrong if ITK's pullback is read as a
  * forward map, if ITK's LPS parameters are taken for RAS, or if the composite's last-component-first order is reversed; each of those
  * mutations is run below and must fail.
  *
  * References are native SimpleITK 2.5.6 outputs only, never this module:
  *   - `itk_hdf5/points.tsv`: ITK `TransformPoint` of both composite orders at 12 template points;
  *   - `neurotransform/itk_oracle/{affine_warp,warp_affine}_resampled.nii.gz`: ITK `Resample` (linear) of the T1w ramp through each composite;
  *   - `itk_linear/points.tsv`: ITK `TransformPoint` of `euler.tfm` at 12 points. The rigid leg's reference at the
  *     composite's output points is the exact affine through those 12 native pairs (residual reported below).
  *
  * The composites are small synthetic SimpleITK files on an oblique, anisotropic lattice, not fMRIPrep demo outputs.
  */
class FmriprepChainScenarioSuite extends munit.FunSuite:
  import FmriprepChainScenarioSuite.{ItkReference, Mutation}

  private val Id = "transform.fmriprep-chain.v1"

  // Declared spaces: the fixtures are synthetic, so they are not claimed to be a real subject or MNI template.
  private val boldref: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("itk_linear euler.tfm moving (boldref)")))
  private val t1w: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("itk_oracle composite moving (T1w)")))
  private val template: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("itk_oracle composite fixed (template)")))

  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val EulerPath = "itk_linear/euler.tfm"
  private val CompositeH5 = "neurotransform/itk_oracle/affine_warp.h5"

  // ----------------------------------------------------------------------------------------------- scenario inputs

  private def boldToT1w(mutation: Mutation): WorldTransform.Linear[boldref.type, t1w.type] =
    val file = ok(ItkTextCodec.decode(TransformSource.Text(OracleFixtures.text(EulerPath))))
    val asset = AssetRef(EulerPath, Some(OracleFixtures.sha256Hex(EulerPath)))
    mutation match
      case Mutation.BoldLegReadAsForwardMap =>
        // the classic confusion: ITK's TransformPoint (fixed -> moving) taken as the moving -> fixed forward map
        ok(ItkLinearInterpretation.linear(file, Frames[t1w.type, boldref.type](t1w, boldref), asset, TransformFormat.ItkText)).inverse
      case _ => ok(ItkLinearInterpretation.linear(file, Frames[boldref.type, t1w.type](boldref, t1w), asset, TransformFormat.ItkText))

  private def composite(name: String, mutation: Mutation): WorldTransform[t1w.type, template.type] =
    val dump = ItkHdf5Dumps.parse(OracleFixtures.text(s"itk_hdf5/$name.components.txt"))
    val file = mutation match
      case Mutation.CompositeStagesReversed => ItkHdf5File(dump.components.filter(_.isComposite) ++ dump.stages.reverse)
      case Mutation.ItkLpsReadAsRas         => ItkHdf5File(dump.components.map(lpsReadAsRas))
      case _                                => dump
    val h5 = s"neurotransform/itk_oracle/$name.h5"
    // ITK uses zero displacement outside a field's lattice; PreserveSource agrees except in the border band (BorderBand).
    val context = DenseContext(Frames[t1w.type, template.type](t1w, template), CoordinateBoundaryPolicy.PreserveSource)
    ok(ItkHdf5Interpretation.interpretWith(file, context, AssetRef(h5, Some(OracleFixtures.sha256Hex(h5))))).composed

  /** The composite a reader would build if it took ITK's LPS parameters for RAS: every stored matrix, offset, centre,
    * lattice origin and direction, and displacement vector is conjugated by the LPS flip, so the module's own (correct)
    * LPS -> RAS conjugation turns it back into the raw LPS numbers applied to RAS points.
    */
  private def lpsReadAsRas(component: ItkHdf5Component): ItkHdf5Component =
    val f = Vector(-1.0, -1.0, 1.0)
    val p = component.parameters
    val fixed = component.fixedParameters
    if component.isComposite then component
    else if component.isDisplacementField then
      val direction = (0 until 9).map(i => fixed(9 + i) * f(i / 3) * f(i % 3))
      val geometry = (0 until 3).map(i => fixed(i)) ++ (0 until 3).map(i => fixed(3 + i) * f(i)) ++ (0 until 3).map(i => fixed(6 + i)) ++ direction
      component.copy(parameters = IArray.tabulate(p.length)(i => p(i) * f(i % 3)), fixedParameters = IArray.from(geometry))
    else
      // AffineTransform_double_3_3: nine row-major matrix entries, three translations; fixed = centre
      val matrix = (0 until 9).map(i => p(i) * f(i / 3) * f(i % 3))
      component.copy(parameters = IArray.from(matrix ++ (0 until 3).map(i => p(9 + i) * f(i))), fixedParameters = IArray.tabulate(3)(i => fixed(i) * f(i)))

  private def templatePoint(ras: Vector[Double]): Point[template.type, D3] = ok(Point.fromVector(template, ras))
  private def t1wPoint(ras: Vector[Double]): Point[t1w.type, D3] = ok(Point.fromVector(t1w, ras))
  private def boldPoint(ras: Vector[Double]): Point[boldref.type, D3] = ok(Point.fromVector(boldref, ras))

  /** Oracle rows of one transform: (fixed point, ITK TransformPoint), both LPS. */
  private def itkRows(table: OracleTable, key: String, skip: Int): Vector[(Vector[Double], Vector[Double])] =
    table.keyed.filter(_._1 == key).map((_, row) => (row.slice(skip, skip + 3), row.slice(skip + 3, skip + 6)))

  // ------------------------------------------------------------------------------------------------------ scenario

  private def runScenario(mutation: Mutation): ScenarioResult =
    val pointTable = OracleTable.load("itk_hdf5/points.tsv")
    def asRas(lps: Vector[Double]) = flipLps(lps)

    // (1) the rigid leg's native reference, fitted exactly through ITK's own 12 point pairs
    val eulerPairs = itkRows(OracleTable.load("itk_linear/points.tsv"), "euler", skip = 1)
    val (eulerLps, eulerFitResidual) = fitAffine(eulerPairs)
    def eulerReference(t1wRas: Vector[Double]): Vector[Double] =
      val lps = flipLps(t1wRas)
      flipLps(Vector.tabulate(3)(r => (0 until 3).map(c => eulerLps(4 * r + c) * lps(c)).sum + eulerLps(4 * r + 3)))

    val bold = boldToT1w(mutation)
    val composites = Vector("affine_warp", "warp_affine").map(name => name -> composite(name, mutation)).toMap
    val chain: WorldTransform.Mapped[boldref.type, template.type] = bold.andThen(composites("affine_warp"))

    // (2) composite leg against ITK TransformPoint, both component orders
    val compositePointErrors = composites.toVector.sortBy(_._1).map: (name, transform) =>
      val errors = itkRows(pointTable, s"$name.h5", skip = 0).map: (fixedLps, movingLps) =>
        transform.pullPoint(templatePoint(asRas(fixedLps))).fold(_ => Double.PositiveInfinity, p => maxAbsDifference(p.coordinates, flipLps(movingLps)))
      ScenarioHarness.scalar(s"composite.$name.transform-point.max-abs-mm", worst(errors), 0.0, ScenarioTolerance.absolute(1e-9))

    // (3) the whole chain: template point -> boldref point, against TransformPoint then the fitted rigid reference
    val chainRows = itkRows(pointTable, "affine_warp.h5", skip = 0)
    val chainErrors = chainRows.map: (fixedLps, movingLps) =>
      val expected = eulerReference(flipLps(movingLps))
      chain.pullPoint(templatePoint(asRas(fixedLps))).fold(_ => Double.PositiveInfinity, p => maxAbsDifference(p.coordinates, expected))

    // (4) volumes: the T1w ramp resampled through each composite onto the template lattice, against ITK Resample
    val sourceRaw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded("neurotransform/itk_oracle/source.nii.gz"))))
    val latticeAffine = ok(LatticeAffine.of(sourceRaw, LatticeAffine.Itk))
    val shape = sourceRaw.spatialShape
    val sourceGrid = ok(Grid.forFrame[D3, t1w.type](t1w)(shape, latticeAffine))
    val templateGrid = ok(Grid.forFrame[D3, template.type](template)(shape, latticeAffine))
    val sourceImage = ok(Sampled.continuous(sourceGrid, NonSpatialAxes.empty, NDArray.tabulate[Double](shape(0), shape(1), shape(2))((i, j, k) => sourceRaw.value(i, j, k))))
    val voxels = for i <- (0 until shape(0)).toVector; j <- 0 until shape(1); k <- 0 until shape(2) yield Vector(i, j, k)
    // The comparison set is chosen from the native data alone: the affine fitted through ITK's own TransformPoint of
    // affine.h5 and the displacement samples as stored, evaluated in LPS exactly as ITK composes them.
    val itk = ItkReference(itkRows(pointTable, "affine.h5", skip = 0), ItkHdf5Dumps.parse(OracleFixtures.text("itk_hdf5/affine_warp.components.txt")))
    val volumeObservations = composites.toVector.sortBy(_._1).flatMap: (name, transform) =>
      val native = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"neurotransform/itk_oracle/${name}_resampled.nii.gz"))))
      transform.resample(sourceImage, templateGrid, Interpolation.Linear, BoundaryPolicy.Constant(0.0)) match
        case Left(error) => Vector(ScenarioHarness.fact(s"volume.$name.resample", false, error.toString))
        case Right(resampled) =>
          // Every voxel is compared except those whose field query or final pullback lands in the one-voxel band just
          // outside a lattice, where ITK and reframe4s extend differently (caveat BorderBand).
          val (compared, band) = voxels.partition: index =>
            val y = flipLps(ok(LatticeIndex.fromVector[D3](index).flatMap(templateGrid.pointAt)).coordinates)
            !itk.inBand(y, affineFirst = name == "warp_affine")
          val errors = compared.map(index => math.abs(resampled.image.data.at(IArray(index(0), index(1), index(2))) - native.value(index(0), index(1), index(2))))
          Vector(
            ScenarioHarness.fact(
              s"volume.$name.compared-voxels",
              compared.size >= voxels.size / 2,
              s"${compared.size} of ${voxels.size} voxels compared; ${band.size} in the border band"
            ),
            // float64 on both sides; the residual is the float32 geometry SimpleITK wrote to the NIfTI header
            // (measured <= 6.6e-7; the STP Phase 4 intensity tolerance for itk_oracle is 2e-5)
            ScenarioHarness.scalar(s"volume.$name.linear-resample.max-abs", worst(errors), 0.0, ScenarioTolerance.absolute(2e-5))
          )

    // (5) direction: no forward map without an inverse; a qualified numerical inverse closes the round trip
    val noForward = chain.mapPoint(boldPoint(Vector(1.0, 2.0, 3.0))).left.exists(_.isInstanceOf[TransformError.NoForwardMap])
    val roundTrip = roundTripObservations(composites("affine_warp"), templateGrid, chainRows, asRas, bold)

    // (6) provenance: one Read step per file, in application order, each with the hash its producer recorded
    val provenanceSteps = chain.provenance.steps
    val expectedProvenance = Vector(
      TransformProvenance.Step.Read(TransformFormat.ItkText, AssetRef(EulerPath, recordedSha256("itk_linear/manifest.json", "euler.tfm"))),
      TransformProvenance.Step.Read(TransformFormat.ItkHdf5, AssetRef(CompositeH5, recordedSha256("neurotransform/itk_oracle/evidence.json", "affine_warp.h5")))
    )

    val observations =
      Vector(
        ScenarioHarness.scalar("rigid-leg.affine-fit.residual-mm", eulerFitResidual, 0.0, ScenarioTolerance.absolute(1e-10)),
        ScenarioHarness.fact("chain.points", chainRows.size == 12, s"${chainRows.size} TransformPoint oracle rows"),
        ScenarioHarness.scalar("chain.boldref-point.max-abs-mm", worst(chainErrors), 0.0, ScenarioTolerance.absolute(1e-8)),
        ScenarioHarness.fact("chain.push-unavailable", chain.push.isEmpty && noForward, "a dense chain maps forward only through an inverse"),
        ScenarioHarness.fact(
          "chain.provenance",
          provenanceSteps == expectedProvenance && expectedProvenance.forall:
            case TransformProvenance.Step.Read(_, asset) => asset.sha256.isDefined
            case _                                       => false
          ,
          provenanceSteps.map(_.toString).mkString(" then ")
        )
      ) ++ compositePointErrors ++ volumeObservations ++ roundTrip
    ScenarioHarness.result(Id, observations, Vector(FmriprepChainScenarioSuite.BorderBand))

  /** Materialize the composite on the template lattice, invert it numerically on a persistent T1w lattice, and send the
    * native moving points (and, through the exactly invertible rigid leg, the boldref points) forward again.
    */
  private def roundTripObservations(
      transform: WorldTransform[t1w.type, template.type],
      templateGrid: Grid[template.type, D3],
      rows: Vector[(Vector[Double], Vector[Double])],
      asRas: Vector[Double] => Vector[Double],
      bold: WorldTransform.Linear[boldref.type, t1w.type]
  ): Vector[ScenarioObservation] =
    val inverted =
      for
        field <- transform.materialize(templateGrid)
        lattice <- GridId.parse("fmriprep-chain-t1w").flatMap(id => Grid.createPersistent[D3, t1w.type](id, t1w)(templateGrid.shape, templateGrid.indexToFrame)).left.map(TransformError.Geometry(_))
        policy <- InversionPolicy.create(minimumCoverage = 0.5, maximumResidual = 1e-3, p99Residual = 1e-3)
        estimated <- field.invertNumerically(lattice, policy)
      yield estimated
    inverted match
      case Left(error) => Vector(ScenarioHarness.fact("round-trip.inverse", false, error.toString))
      case Right(estimated) =>
        val forward = bold.andThen(estimated)
        // The estimate answers only inside its evaluation domain (moving lattice points whose image stays inside the
        // template lattice); elsewhere it must refuse with OutsideDomain rather than extrapolate.
        val t1wResults = rows.map: (fixedLps, movingLps) =>
          estimated.mapPoint(t1wPoint(flipLps(movingLps))).map(p => maxAbsDifference(p.coordinates, asRas(fixedLps)))
        val boldResults = rows.map: (fixedLps, movingLps) =>
          bold.pullPoint(t1wPoint(flipLps(movingLps))).flatMap(b => forward.mapPoint(boldPoint(b.coordinates))).map(p => maxAbsDifference(p.coordinates, asRas(fixedLps)))
        def refusedOutsideDomain(results: Vector[Either[TransformError, Double]]): Boolean =
          results.forall:
            case Left(TransformError.Map(MapError.OutsideDomain(_))) => true
            case Left(_)                                            => false
            case Right(_)                                           => true
        val estimatedKind = estimated.availability match
          case PushAvailability.Estimated(_) => true
          case _                             => false
        val answered = t1wResults.count(_.isRight)
        Vector(
          ScenarioHarness.fact("round-trip.estimated-inverse", estimatedKind && forward.push.isDefined, estimated.provenance.describe),
          ScenarioHarness.fact(
            "round-trip.evaluation-domain",
            answered >= rows.size / 2 && boldResults.count(_.isRight) == answered && refusedOutsideDomain(t1wResults) && refusedOutsideDomain(boldResults),
            s"$answered of ${rows.size} native points inside the estimate's evaluation domain; the rest refused as OutsideDomain"
          ),
          // native moving point -> estimated forward map -> native fixed point
          // The estimate is trilinear on the 1.3-2.1 mm lattice: its lattice residual is gated by the policy (1e-3 mm);
          // between samples the curvature of the inverse adds interpolation error (measured 1.05e-4 mm).
          ScenarioHarness.scalar("round-trip.t1w-to-template.max-abs-mm", worst(t1wResults.collect { case Right(e) => e }), 0.0, ScenarioTolerance.absolute(3e-4)),
          ScenarioHarness.scalar("round-trip.boldref-to-template.max-abs-mm", worst(boldResults.collect { case Right(e) => e }), 0.0, ScenarioTolerance.absolute(3e-4))
        )

  // ----------------------------------------------------------------------------------------------------- tests

  test("fMRIPrep chain: boldref -> T1w (ITK rigid) -> template (ITK HDF5 composite) matches native ITK points and volumes"):
    val result = runScenario(Mutation.Faithful)
    assertEquals(result.status, ScenarioStatus.PassWithCaveats, result.render)
    if !result.ciPass(FmriprepChainScenarioSuite.Policy) then fail(result.render)

  test("every convention mutation of the fMRIPrep chain fails the scenario"):
    // each mutation must fail on the observation that guards its convention, not merely somewhere
    val guards = Map(
      Mutation.BoldLegReadAsForwardMap -> "chain.boldref-point.max-abs-mm",
      Mutation.CompositeStagesReversed -> "volume.affine_warp.linear-resample.max-abs",
      Mutation.ItkLpsReadAsRas -> "composite.affine_warp.transform-point.max-abs-mm"
    )
    assertEquals(guards.keySet, Mutation.values.toSet - Mutation.Faithful)
    guards.foreach: (mutation, guard) =>
      val result = runScenario(mutation)
      assertEquals(result.status, ScenarioStatus.Fail, s"$mutation must fail:\n${result.render}")
      assert(result.failures.exists(_.render.startsWith(s"$guard:")), s"$mutation must fail $guard:\n${result.render}")

  test("a template -> T1w warp cannot stand in for the T1w -> template composite: the direction is a type"):
    val errors = compileErrors("val swapped: WorldTransform[t1w.type, template.type] = composite(\"affine_warp\", Mutation.Faithful).materialize(???).toOption.get.transform.invert.toOption.get")
    // the mismatch is between the frames, reversed
    assert(errors.contains("Required: scalafim.transform.WorldTransform[") && errors.contains("t1w.type") && errors.contains("template.type"), errors)

object FmriprepChainScenarioSuite:
  /** Where a continuous lattice index falls: on the lattice, in the one-voxel band just outside it, or beyond. */
  private enum LatticePlacement derives CanEqual:
    case Inside, Band, Beyond

  /** ITK's composite evaluated from native data only, in LPS: `affine` fitted through TransformPoint pairs of the affine
    * alone, `field` the stored displacement component. Used to decide which voxels touch the border band.
    */
  private final case class ItkReference(affinePairs: Vector[(Vector[Double], Vector[Double])], dump: ItkHdf5File):
    private val (affine, _) = ChainScenarioSupport.fitAffine(affinePairs)
    private val field = dump.stages.find(_.isDisplacementField).getOrElse(throw new IllegalArgumentException("no displacement field"))
    private val f = field.fixedParameters
    private val size = Vector(0, 1, 2).map(i => math.rint(f(i)).toInt)
    private val origin = Vector(f(3), f(4), f(5))
    private val spacing = Vector(f(6), f(7), f(8))
    private def direction(r: Int, c: Int): Double = f(9 + 3 * r + c)

    private def applyAffine(p: Vector[Double]): Vector[Double] =
      Vector.tabulate(3)(r => (0 until 3).map(c => affine(4 * r + c) * p(c)).sum + affine(4 * r + 3))

    /** Continuous index of an LPS point on the field lattice (orthonormal direction). */
    private def index(p: Vector[Double]): Vector[Double] =
      Vector.tabulate(3)(c => (0 until 3).map(r => direction(r, c) * (p(r) - origin(r))).sum / spacing(c))

    private def placement(p: Vector[Double]): LatticePlacement =
      val c = index(p)
      if c.zip(size).exists((v, n) => v <= -1.0 || v >= n.toDouble) then LatticePlacement.Beyond
      // template lattice points come from the float32 NIfTI header: they land within 1e-6 of the faces
      else if c.zip(size).forall((v, n) => v >= -1e-6 && v <= n - 1.0 + 1e-6) then LatticePlacement.Inside
      else LatticePlacement.Band

    /** Trilinear displacement at an LPS point inside the lattice; zero beyond it (ITK). */
    private def displacement(p: Vector[Double]): Vector[Double] =
      if placement(p) != LatticePlacement.Inside then Vector(0.0, 0.0, 0.0)
      else
        val c = index(p)
        val lo = c.zip(size).map((v, n) => math.max(0, math.min(math.floor(v).toInt, n - 2)))
        val t = c.zip(lo).map(_ - _)
        Vector.tabulate(3): component =>
          (for dx <- 0 to 1; dy <- 0 to 1; dz <- 0 to 1 yield
            val w = (if dx == 1 then t(0) else 1 - t(0)) * (if dy == 1 then t(1) else 1 - t(1)) * (if dz == 1 then t(2) else 1 - t(2))
            val v = (lo(0) + dx) + size(0) * ((lo(1) + dy) + size(1) * (lo(2) + dz))
            w * field.parameters(3 * v + component)
          ).sum

    /** Whether ITK's field query or final pullback of the LPS template point lies in the border band. The image lattice
      * is the field lattice (the oracle's source copies the field's geometry).
      */
    def inBand(y: Vector[Double], affineFirst: Boolean): Boolean =
      val query = if affineFirst then applyAffine(y) else y
      val moved = query.zip(displacement(query)).map(_ + _)
      val last = if affineFirst then moved else applyAffine(moved)
      placement(query) == LatticePlacement.Band || placement(last) == LatticePlacement.Band

  /** ITK extends a displacement field (and clamps an image) by its border sample up to half a voxel outside the
    * lattice, then uses zero displacement (and zero padding). reframe4s' `PreserveSource` (and `Constant`) blend towards
    * the identity (or the constant) across the whole first voxel outside. The two agree on the lattice and beyond that
    * band, and the volume comparison skips the band.
    */
  val BorderBand: ScenarioCaveat = ScenarioCaveat(
    id = "transform.itk-border-band",
    kind = CaveatKind.AlgorithmDivergence,
    severity = CaveatSeverity.Actionable,
    owner = "transform",
    followUp = Some("a reframe4s CoordinateBoundaryPolicy (and image BoundaryPolicy) reproducing ITK's half-voxel border extension"),
    detail = "voxels whose displacement-field query or final pullback lies in the one-voxel band outside a lattice are not compared with ITK Resample"
  )

  /** The manifest's policy for this scenario: pass, or pass with exactly the declared border-band caveat. */
  val Policy: ScenarioPolicy = ScenarioPolicy(Set(ScenarioStatus.PassWithCaveats), Set(BorderBand.id))

  /** Convention errors a registration chain can make; `Faithful` is the correct reading. */
  enum Mutation derives CanEqual:
    case Faithful, BoldLegReadAsForwardMap, CompositeStagesReversed, ItkLpsReadAsRas
