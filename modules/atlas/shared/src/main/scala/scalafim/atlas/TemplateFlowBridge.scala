package scalafim.atlas

import image4s.geometry.{Affine, D3, Grid}
import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.world.{Spaces as WorldFrames, ToolCoordinates}
import scalafim.transform.{AssetRef, Frames, InversionPolicy, PushAvailability, TransformError, WorldTransform}
import scalafim.transform.field.DenseContext
import scalafim.transform.itk.{ItkHdf5File, ItkHdf5Interpretation}

/** Which way a transform file pulls points: the resampling direction, fixed (target) point to moving (source) point. */
enum TemplatePull derives CanEqual:
  /** An MNI152NLin2009cAsym point to its MNI152NLin6Asym correspondent: resamples 6Asym data onto 2009c grids. */
  case PointsFrom2009cTo6Asym

  /** An MNI152NLin6Asym point to its MNI152NLin2009cAsym correspondent: resamples 2009c data onto 6Asym grids. */
  case PointsFrom6AsymTo2009c

/** A TemplateFlow inter-template composite whose bytes and pull direction ScalaFIM has inspected.
  *
  * TemplateFlow names `mode-image` transforms `tpl-X_from-Y`: the transform an image in `Y` is resampled onto `X`'s
  * grid with, so ITK `TransformPoint` should take an `X` point to a `Y` point. `measured` is what the file actually
  * does, established by resampling each template's brain T1w and mask through it under both hypotheses (evidence and
  * generator: `tools/transform/generate_templateflow_mni_bridge_oracle.py`). Only a file whose measurement agrees with
  * its name is admitted. Cases are named by the data direction their file name declares (`Mni6ToMni2009c` moves 6Asym
  * data onto 2009c grids); [[TemplatePull]] names point directions, which run the other way.
  */
enum TemplateFlowXfm(
    val template: String,
    val fileName: String,
    val sha256: String,
    val bytes: Long,
    val declared: TemplatePull,
    val measured: TemplatePull
) derives CanEqual:
  /** Pulls 2009c points to 6Asym, as its name says. Measured: 6Asym T1w resampled onto 2009c correlates r = 0.9852
    * with 2009c (identity 0.9743, reversed use 0.9437); mask Dice 0.9736 (identity 0.9677, reversed 0.9429).
    */
  case Mni6ToMni2009c
      extends TemplateFlowXfm(
        "MNI152NLin2009cAsym",
        "tpl-MNI152NLin2009cAsym_from-MNI152NLin6Asym_mode-image_xfm.h5",
        "2e3869a07b96aec406e0419ca2e434afc54882d37cc212b933b139d1b63a4dfe",
        204735104L,
        TemplatePull.PointsFrom2009cTo6Asym,
        TemplatePull.PointsFrom2009cTo6Asym
      )

  /** Named as the reverse, but it pulls 2009c points to 6Asym too: used as named it worsens the templates' agreement
    * (T1w r = 0.9433, Dice 0.9427, below identity), used the other way it matches the forward file (r = 0.9850). Its
    * displacement lattice is 2009c's res-01 grid, as the forward file's is. It is not an inverse and is refused.
    */
  case Mni2009cToMni6
      extends TemplateFlowXfm(
        "MNI152NLin6Asym",
        "tpl-MNI152NLin6Asym_from-MNI152NLin2009cAsym_mode-image_xfm.h5",
        "2a19853bc99ebd1d711d230ea4a4dc4db3b9a60f8b50b9181f93b44036dfbd0f",
        204735104L,
        TemplatePull.PointsFrom6AsymTo2009c,
        TemplatePull.PointsFrom2009cTo6Asym
      )

  /** The path under a TemplateFlow home, e.g. `tpl-MNI152NLin2009cAsym/tpl-..._xfm.h5`. */
  def relativePath: String = s"tpl-$template/$fileName"

  def agreesWithName: Boolean = declared == measured

object TemplateFlowXfm:
  def bySha256(digest: String): Option[TemplateFlowXfm] =
    values.find(_.sha256.equalsIgnoreCase(digest.trim))

/** The MNI152NLin6Asym -> MNI152NLin2009cAsym edge, executed by TemplateFlow's exact nonlinear composite (an affine and
  * a 193 x 229 x 193 displacement field on the 2009c res-01 lattice, applied field first as ITK does).
  *
  * `transform` pulls 2009c points to 6Asym: resampling 6Asym data onto a 2009c grid, and carrying 2009c coordinates to
  * 6Asym, are exact. Points outside the field lattice are rejected, never passed through unwarped. The forward map
  * (6Asym points to 2009c) exists only after [[withNumericalInverse]] qualifies one: TemplateFlow's reverse file is not
  * an inverse (see [[TemplateFlowXfm.Mni2009cToMni6]]). No affine or generic MNI alias ever stands in for the warp.
  */
final class MniTemplateBridge private (
    val transform: WorldTransform.Mapped[WorldFrames.Mni6, WorldFrames.Mni2009c],
    val asset: AssetRef
):
  val xfm: TemplateFlowXfm = TemplateFlowXfm.Mni6ToMni2009c

  /** Whether 6Asym points can be carried to 2009c (a qualified numerical inverse is attached). */
  def hasForwardMap: Boolean = transform.push.nonEmpty

  /** What determines the edge's values: the composite's bytes, the interpretation and, once attached, the inverse. */
  def identity: String =
    val inverse =
      transform.availability match
        case PushAvailability.Estimated(inverse) =>
          val (gates, settings) = (inverse.policy.gates, inverse.policy.settings)
          val lattice = inverse.evidence.lattice
          val latticeId = lattice.persistentId.fold(lattice.shape.mkString("x"))(_.value)
          s"|inverse=numerical(lattice=$latticeId,iterations<=${settings.maximumIterations},tolerance=${settings.tolerance}," +
            s"divergence=${settings.divergenceRatio},coverage>=${gates.minimumCoverage},max<=${gates.maximumResidual}mm," +
            s"p99<=${gates.p99Residual}mm,margin=${gates.interiorMargin})"
        case _ => ""
    s"templateflow:${xfm.relativePath}|sha256=${xfm.sha256}|itk-composite|outside-lattice=reject$inverse"

  /** The manifest step `MNI152NLin6Asym -> MNI152NLin2009cAsym`, available and backed by this bridge. */
  def forwardStep: TransformStep =
    TransformStep(
      SpaceId.MNI152NLin6Asym,
      SpaceId.MNI152NLin2009cAsym,
      TransformKind.NonlinearWarp,
      TransformBackend.TemplateFlowAnts,
      Confidence.High,
      reversible = hasForwardMap,
      dataFiles = Vector(xfm.relativePath),
      TransformStatus.Available,
      notes = Some(
        if hasForwardMap then "TemplateFlow composite warp; forward map from a qualified numerical inverse"
        else "TemplateFlow composite warp; pullback only (2009c points to 6Asym, 6Asym data onto 2009c grids)"
      ),
      asset = Some(TransformAsset(transform, identity))
    )

  /** The manifest step `MNI152NLin2009cAsym -> MNI152NLin6Asym`: executable only with a qualified numerical inverse,
    * whose estimate is then its pullback and whose forward map is the exact composite; otherwise a planned, typed
    * non-executable edge naming why TemplateFlow's reverse file does not implement it.
    */
  def reverseStep: TransformStep =
    val base =
      TransformStep(
        SpaceId.MNI152NLin2009cAsym,
        SpaceId.MNI152NLin6Asym,
        TransformKind.NonlinearWarp,
        TransformBackend.TemplateFlowAnts,
        Confidence.High,
        reversible = true,
        dataFiles = Vector(xfm.relativePath),
        TransformStatus.Planned,
        notes = Some(MniTemplateBridge.reverseFileRefusal)
      )
    // `invert` exists only once an estimate is attached. The swapped step's pullback is that estimate, so the step is
    // Approximate even though its forward map (the composite) is exact.
    transform.invert.toOption.fold(base): inverted =>
      base.withAsset(TransformAsset(inverted, s"$identity|swapped")).copy(
        confidence = Confidence.Approximate,
        notes = Some(
          "Inverse of the TemplateFlow composite: pullback is the qualified numerical inverse (approximate), " +
            "forward map is the exact composite"
        )
      )

  /** `manifest` with its TemplateFlow 6Asym <-> 2009c steps replaced by this bridge's (added when absent). */
  def install(manifest: Vector[TransformStep]): Vector[TransformStep] =
    def isStep(step: TransformStep, from: SpaceId, to: SpaceId): Boolean =
      step.backend == TransformBackend.TemplateFlowAnts &&
        SpaceId.normalize(step.from) == from && SpaceId.normalize(step.to) == to
    val kept =
      manifest.filterNot(step =>
        isStep(step, SpaceId.MNI152NLin6Asym, SpaceId.MNI152NLin2009cAsym) ||
          isStep(step, SpaceId.MNI152NLin2009cAsym, SpaceId.MNI152NLin6Asym)
      )
    kept ++ Vector(forwardStep, reverseStep)

  /** Attach a forward map estimated on the 6Asym lattice `on` that passes every gate of `policy`.
    *
    * The composite is first materialized on its own displacement lattice (2009c res-01). There the result is exact:
    * the composite's affine applied to a trilinearly interpolated displacement is the trilinear interpolant of its
    * lattice values. The bridge's pullback becomes that field, since an estimate belongs to the pullback it was
    * qualified against. Gate failures are [[AtlasError.Transform]] with the complete residual evidence.
    */
  def withNumericalInverse(
      policy: InversionPolicy = MniTemplateBridge.defaultInversionPolicy,
      on: Grid[WorldFrames.Mni6, D3] = TemplateGrids.mni6Res1.grid
  ): Either[AtlasError, MniTemplateBridge] =
    for
      field    <- transform.materialize(TemplateGrids.mni2009cRes1.grid).left.map(AtlasError.Transform.apply)
      inverted <- field.invertNumerically(on, policy).left.map(AtlasError.Transform.apply)
    yield new MniTemplateBridge(inverted, asset)

object MniTemplateBridge:
  /** Gates fixed before the inverse was first evaluated: every 6Asym res-01 point's image must be found (coverage at
    * least 0.99 of the lattice), with round-trip residuals `|pull(push(x)) - x|` and `|push(pull(y)) - y|` of at most
    * 0.5 mm anywhere and 0.05 mm at the 99th percentile, one voxel inside the lattices.
    */
  val defaultInversionPolicy: InversionPolicy =
    InversionPolicy
      .create(minimumCoverage = 0.99, maximumResidual = 0.5, p99Residual = 0.05)
      .fold(error => throw new IllegalStateException(error.message), identity)

  private val reverseFileRefusal: String =
    s"Not implemented by TemplateFlow's ${TemplateFlowXfm.Mni2009cToMni6.relativePath}: it pulls 2009c points to 6Asym, " +
      "the same way as the forward composite, so it is not an inverse. Carry 2009c points to 6Asym through the " +
      "6Asym -> 2009c route's pullback, or qualify a numerical inverse."

  /** Admit a decoded ITK composite as the bridge. `asset` must carry the SHA-256 of the file's bytes, which only the
    * loaders that hashed those bytes can vouch for (`scalafim.atlas.io.MniTemplateBridgeFiles` on the JVM), hence
    * package-private: only the inspected TemplateFlow composite is admitted, and its layout and displacement lattice are
    * checked before it is interpreted.
    */
  private[atlas] def fromItk(file: ItkHdf5File, asset: AssetRef): Either[AtlasError, MniTemplateBridge] =
    for
      xfm <- admitted(asset)
      _   <- checkLayout(file, xfm)
      chain <- ItkHdf5Interpretation
        .interpretWith(
          file,
          DenseContext(
            Frames[WorldFrames.Mni6, WorldFrames.Mni2009c](WorldFrames.MNI152NLin6Asym, WorldFrames.MNI152NLin2009cAsym),
            CoordinateBoundaryPolicy.Reject
          ),
          asset
        )
        .left
        .map(AtlasError.Transform.apply)
      mapped <- chain.composed match
        case mapped: WorldTransform.Mapped[WorldFrames.Mni6, WorldFrames.Mni2009c] @unchecked => Right(mapped)
        case other => Left(refused(xfm, s"expected a dense composite, the file interpreted as ${other.getClass.getSimpleName}"))
    yield new MniTemplateBridge(mapped, asset)

  /** The inspected composite `asset`'s digest names, if it is admitted; a loader checks this before decoding. */
  private[atlas] def admitted(asset: AssetRef): Either[AtlasError, TemplateFlowXfm] =
    asset.sha256 match
      case None => Left(AtlasError.TemplateAssetRefused(asset.label, "no SHA-256 was recorded for the file's bytes"))
      case Some(digest) =>
        TemplateFlowXfm.bySha256(digest) match
          case None =>
            Left(
              AtlasError.TemplateAssetRefused(
                asset.label,
                s"SHA-256 $digest is not an inspected TemplateFlow composite; expected ${TemplateFlowXfm.Mni6ToMni2009c.sha256}"
              )
            )
          case Some(xfm) if !xfm.agreesWithName =>
            Left(
              refused(
                xfm,
                s"named to pull ${xfm.declared} but measured to pull ${xfm.measured}; it is not the inverse of " +
                  s"${TemplateFlowXfm.Mni6ToMni2009c.fileName}"
              )
            )
          case Some(xfm) => Right(xfm)

  /** Composite marker, affine, displacement field on exactly the 2009c res-01 lattice (LPS size/origin/spacing/direction). */
  private[atlas] def checkLayout(file: ItkHdf5File, xfm: TemplateFlowXfm): Either[AtlasError, Unit] =
    val kinds = file.components.map(_.kind)
    if kinds != Vector("CompositeTransform", "AffineTransform", "DisplacementFieldTransform") then
      Left(refused(xfm, s"expected a composite of one affine and one displacement field, got ${kinds.mkString(",")}"))
    else
      val fixed = file.components(2).fixedParameters
      if fixed.length != 18 then Left(refused(xfm, s"displacement field has ${fixed.length} fixed parameters, expected 18"))
      else
        val lattice = TemplateGrids.mni2009cRes1
        val dims = Vector(fixed(0), fixed(1), fixed(2))
        val lpsIndexToWorld =
          Vector.tabulate(3)(r => Vector.tabulate(3)(c => fixed(9 + 3 * r + c) * fixed(6 + c)) :+ fixed(3 + r)).flatten ++
            Vector(0.0, 0.0, 0.0, 1.0)
        val rasIndexToWorld =
          Affine
            .fromRowMajor[D3](lpsIndexToWorld)
            .flatMap(_.andThen(ToolCoordinates.LpsToRas))
            .map(_.rowMajor)
        val expected = lattice.affine.rowMajor
        rasIndexToWorld match
          case Right(actual)
              if dims == lattice.dims.map(_.toDouble) && actual.zip(expected).forall((a, e) => math.abs(a - e) <= 1e-9) =>
            Right(())
          case Right(actual) =>
            Left(
              refused(
                xfm,
                s"displacement lattice ${dims.mkString("x")} ${actual.mkString(",")} is not MNI152NLin2009cAsym res-01 " +
                  s"${lattice.dims.mkString("x")} ${expected.mkString(",")}"
              )
            )
          case Left(error) => Left(refused(xfm, s"displacement lattice is not an affine: ${error.message}"))

  private def refused(xfm: TemplateFlowXfm, reason: String): AtlasError =
    AtlasError.TemplateAssetRefused(xfm.relativePath, reason)
