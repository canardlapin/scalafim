package scalafim.atlas

import scalafim.surface.{CorticalHemisphere, VertexId}
import scalafim.surface.reference.*

import java.nio.charset.StandardCharsets

/** Why a standard surface route was not assembled or admitted. Nothing is downloaded, substituted or approximated in
  * place of a refused input.
  */
enum StandardRouteRefusal:
  /** Locked assets found in none of the searched caches (relative TemplateFlow paths). */
  case AssetsMissing(relativePaths: Vector[String], searched: Vector[String])

  /** A locked asset was found but its bytes, digest, declaration or decoding were refused. */
  case AssetRefused(asset: String, cause: ReferenceError)

  /** A supplied input is not the locked asset this route was qualified on. */
  case NotLocked(role: String, expected: String, supplied: String)

  /** The requested policy is not admissible (for example an override without a reason). */
  case PolicyRefused(reason: String)

  /** The surface route itself refused the request (frame, mesh, hemisphere or bridge). */
  case Route(cause: RouteRefusal)

  /** The policy gate failed: some cortical vertices were not placed by the pointwise inverse. */
  case PlacementGate(hemisphere: CorticalHemisphere, unplacedCortical: Int, summary: InversePlacementSummary)

  def message: String =
    this match
      case AssetsMissing(paths, searched) =>
        s"locked assets missing: ${paths.mkString(", ")} (searched ${searched.mkString(", ")}; nothing is downloaded)"
      case AssetRefused(asset, cause) => s"locked asset $asset refused: ${cause.message}"
      case NotLocked(role, expected, supplied) => s"$role is not the locked asset: expected $expected; supplied $supplied"
      case PolicyRefused(reason) => s"route policy refused: $reason"
      case Route(cause) => s"surface route refused: ${cause.message}"
      case PlacementGate(hemisphere, unplaced, summary) =>
        s"${hemisphere.code}: $unplaced cortical vertices were not placed by the pointwise inverse " +
          s"(converged ${summary.converged}, unplaced ${summary.unplaced.map((k, v) => s"${k.label}=$v").mkString(", ")})"

/** What a route policy is backed by. */
enum PolicyQualification:
  /** The policy frozen before the P1-P6 budgets were measured on the locked assets (ticket `ticket`). */
  case FrozenBudgets(ticket: String)

  /** A caller-chosen policy. The frozen budgets were not measured under it; `reason` is the caller's justification. */
  case Override(reason: String)

  def label: String =
    this match
      case FrozenBudgets(ticket) => s"frozen (P1-P6 budgets, $ticket)"
      case Override(reason) => s"override, not budget-qualified: $reason"

/** The pointwise-inverse policy a standard route places vertices with, and what backs it. The default is
  * [[StandardRoutePolicy.Frozen]]; any other inverse policy is an explicit, reasoned [[StandardRoutePolicy.overriding]].
  */
final case class StandardRoutePolicy private (inverse: InversePolicy, qualification: PolicyQualification):
  def isFrozen: Boolean =
    qualification match
      case PolicyQualification.FrozenBudgets(_) => true
      case PolicyQualification.Override(_) => false

  def label: String =
    s"${qualification.label}; tolerance ${inverse.toleranceMm} mm, at most ${inverse.maxIterations} iterations, " +
      s"divergence ratio ${inverse.divergenceRatio}"

object StandardRoutePolicy:
  /** reframe4s defaults (tolerance 1e-8 mm, 100 iterations, divergence ratio 4), frozen on native ticket
    * bd-01M3WCQD1MFW1WRTJP6C6A2ZFS before the bridge was built; P1-P6 passed under it.
    */
  val Frozen: StandardRoutePolicy =
    StandardRoutePolicy(InversePolicy.Default, PolicyQualification.FrozenBudgets("bd-01M3WCQD1MFW1WRTJP6C6A2ZFS"))

  /** A different inverse policy, with the caller's reason. Restating the frozen policy is refused so that a route's
    * identity never carries an override label for qualified settings.
    */
  def overriding(inverse: InversePolicy, reason: String): Either[StandardRouteRefusal, StandardRoutePolicy] =
    if reason.trim.isEmpty then Left(StandardRouteRefusal.PolicyRefused("an override must state its reason"))
    else if inverse == Frozen.inverse then
      Left(StandardRouteRefusal.PolicyRefused("the override equals the frozen policy; use StandardRoutePolicy.Frozen"))
    else Right(StandardRoutePolicy(inverse, PolicyQualification.Override(reason.trim)))

/** The digest-bound identity of an assembled standard route: a canonical description of everything that determines
  * its placements (locked asset digests, anatomy basis, bridge method and policy, mapping method) and its SHA-256.
  * Persist [[token]] (or the whole canonical text) with every result and export made through the route.
  */
final case class StandardRouteIdentity private[atlas] (canonical: String, sha256: AssetSha256):
  def token: String = s"scalafim-route:sha256:${sha256.value}"

object StandardRouteIdentity:
  private[atlas] def of(lines: Vector[String]): StandardRouteIdentity =
    val canonical = lines.mkString("", "\n", "\n")
    StandardRouteIdentity(canonical, AssetSha256.of(canonical.getBytes(StandardCharsets.UTF_8)))

/** What a consumer must show next to results made through a standard route. Per-call details (source grid,
  * hemisphere, value semantics, lookup and per-vertex placement evidence) are in each admitted route's
  * `disclosure` and `bridgePlacement`.
  */
final case class StandardRouteDisclosure(
  identity: StandardRouteIdentity,
  route: String,
  sourceFrame: TemplateFrame,
  anatomyFrame: TemplateFrame,
  anatomyBasis: FrameBasis,
  bridge: String,
  bridgeExactness: BridgeExactness,
  policy: StandardRoutePolicy,
  method: MappingMethod,
  assets: Vector[DeclaredAsset],
  remainingLimits: Vector[String]
):
  /** Ordered label/value pairs for a report or an export sidecar. */
  def fields: Vector[(String, String)] =
    Vector(
      "route" -> route,
      "identity" -> identity.token,
      "sourceFrame" -> sourceFrame.display,
      "anatomyFrame" -> anatomyFrame.display,
      "anatomyBasis" -> anatomyBasis.display,
      "bridge" -> bridge,
      "bridgeExactness" -> bridgeExactness.label,
      "policy" -> policy.label,
      "method" -> method.label
    ) ++ assets.map(asset => "asset" -> asset.display) ++ remainingLimits.map(limit => "limit" -> limit)

/** The locked inputs of the qualified MNI152NLin2009cAsym -> fsLR 32k route (TemplateFlow catalog
  * `templateflow@d79aacb1`): fsLR 32k midthickness and medial-wall labels for both hemispheres, declared in
  * MNI152NLin6Asym on the publisher's methods, and the templateflow4s point map derived from the admitted
  * 6Asym -> 2009c composite ([[TemplateFlowXfm.Mni6ToMni2009c]]).
  */
object Fslr32kFrom2009c:
  val name: String = "fsLR32k-from-MNI152NLin2009cAsym (midthickness, pointwise-inverse TemplateFlow bridge)"

  val catalogRevision: TemplateRelease = TemplateRelease.unsafe("templateflow@d79aacb1ad7d1c52e5d10ad88f48fd8af6e5ae56")

  /** The composite the point map was converted from; ScalaFIM admits it only because its measured pull agrees with
    * its name.
    */
  val transform: TemplateFlowXfm = TemplateFlowXfm.Mni6ToMni2009c

  val pointMapManifestSha256: String = "34bdcea2dab7c0fccde6e6607cd080fbcea087abaf19721925b5b8fb61d85318"

  /** The templateflow4s point-map directory under a TemplateFlow home. */
  val pointMapDirectory: String = s".templateflow4s/derived/point-map/${transform.sha256}"

  private def frame(template: String): TemplateFrame =
    TemplateFrame.make(TemplateId.unsafe(template), catalogRevision).fold(e => throw new IllegalStateException(e.message), identity)

  /** The frame source volumes must be declared in. */
  val sourceFrame: TemplateFrame = frame("MNI152NLin2009cAsym")

  /** The frame the fsLR 32k midthickness is declared in. */
  val anatomyFrame: TemplateFrame = frame("MNI152NLin6Asym")

  /** The publisher's methods for TemplateFlow tpl-fsLR midthickness: the bytes equal the Conte69 v2 32k_fs_LR
    * midthickness, an affine-aligned average of 69 subjects in FSL 4.1.7 MNI152_T1_1mm (TemplateFlow
    * MNI152NLin6Asym). It is not an asset-specific registration proof and matches no single anatomy.
    */
  val anatomyBasis: FrameBasis = FrameBasis.publisherMethods("10.1093/cercor/bhr291",
    "Van Essen et al. 2012, Cereb Cortex 22:2241, Materials and Methods p. 2245",
    PublishedRegistration.Affine, PublishedAggregate.GroupAverage(69),
    "To create population-average surfaces for the Conte-69 data set, linear volumetric registration between the " +
      "individual subject and the MNI152_T1_1mm.nii.gz was performed using ... (FLIRT). (This target is the " +
      "nonlinearly derived template distributed with FSL version 4.1.7.) The resultant affine transform was applied " +
      "to the FreeSurfer white, pial, and midthickness surfaces before they had been resampled to the fs_LR mesh. " +
      "The individual-subject midthickness surfaces were averaged separately for the left and right hemispheres.")
    .fold(e => throw new IllegalStateException(e.message), identity)

  val method: MappingMethod = MappingMethod.MidthicknessNearest

  /** The locked per-hemisphere assets. */
  final case class HemisphereLock(hemisphere: CorticalHemisphere, midthickness: AssetProvenance, medialWall: AssetProvenance):
    def declaration: FrameDeclaration =
      FrameDeclaration.make(anatomyFrame, anatomyBasis, midthickness).fold(e => throw new IllegalStateException(e.message), identity)

  private def provenance(path: String, sha256: String): AssetProvenance =
    AssetProvenance.make(TemplateId.unsafe("fsLR"), path, catalogRevision.value, sha256)
      .fold(e => throw new IllegalStateException(e.message), identity)

  private def lock(hemisphere: CorticalHemisphere, h: String, midthickness: String, medialWall: String): HemisphereLock =
    HemisphereLock(hemisphere,
      provenance(s"tpl-fsLR/tpl-fsLR_den-32k_hemi-${h}_midthickness.surf.gii", midthickness),
      provenance(s"tpl-fsLR/tpl-fsLR_hemi-${h}_den-32k_desc-nomedialwall_dparc.label.gii", medialWall))

  val hemispheres: Vector[HemisphereLock] = Vector(
    lock(CorticalHemisphere.Left, "L", "036a8b6c84fa4b581b7ad7b36d99190b57ad6755c9d7ef7adc3e9ffc6448f1af",
      "4ac9199dab151ccdc2a35bdddb5bac4f4907da7dfebd90807c09b82e2eb9d512"),
    lock(CorticalHemisphere.Right, "R", "9d2cef05096c433b134870456abebe7ff201cdda60ce5741d7b794018eeccda7",
      "698f46b399f5a89829f83cc697e32dc8841031fbf31ffef75aaaa0c4a16c3015"))

  def lockFor(hemisphere: CorticalHemisphere): HemisphereLock = hemispheres.find(_.hemisphere == hemisphere).get

  /** Every locked file a loader must find, as paths relative to a TemplateFlow home. */
  val requiredPaths: Vector[String] =
    hemispheres.flatMap(h => Vector(h.midthickness.archivePath, h.medialWall.archivePath)) :+ s"$pointMapDirectory/manifest.json"

  val remainingLimits: Vector[String] = Vector(
    "anatomy frame is the publisher's declaration (affine-aligned 69-subject Conte69 average); it is not an " +
      "asset-specific registration proof and matches no single anatomy",
    "placements are numerical pointwise-inverse estimates with per-vertex residuals, not an exact inverse",
    "source-volume release, cohort and statistic provenance, and consumer qualification, belong to the consumer")

/** One hemisphere's loaded anatomy: a midthickness decoded under the locked declaration and its medial wall. */
final case class Fslr32kHemisphereInput(surface: DeclaredSurface, medialWall: MedialWallMask)

/** The qualified MNI152NLin2009cAsym -> fsLR 32k route, assembled from verified locked assets. It owns both
  * hemispheres' sampling anatomy and the pointwise-inverse bridge; [[admit]] binds it to one source grid and
  * hemisphere. Platform loaders (`scalafim.atlas.io.StandardSurfaceRouteFiles` on the JVM) build it through
  * [[StandardSurfaceRoutes.fsLR32kFrom2009c]].
  */
final class Fslr32kRoute private[atlas] (
  val policy: StandardRoutePolicy,
  val pointMap: DeclaredPointMap,
  val bridge: FrameBridge,
  anatomies: Map[CorticalHemisphere, SamplingAnatomy]
):
  def anatomy(hemisphere: CorticalHemisphere): SamplingAnatomy = anatomies(hemisphere)

  val identity: StandardRouteIdentity =
    val locked = Fslr32kFrom2009c.hemispheres.flatMap: h =>
      Vector(s"midthickness.${h.hemisphere.code}=${h.midthickness.display}", s"medialWall.${h.hemisphere.code}=${h.medialWall.display}")
    StandardRouteIdentity.of(Vector(
      "schema=scalafim.standard-surface-route/1",
      s"route=${Fslr32kFrom2009c.name}",
      s"sourceFrame=${Fslr32kFrom2009c.sourceFrame.display}",
      s"anatomyFrame=${Fslr32kFrom2009c.anatomyFrame.display}",
      s"anatomyBasis=${Fslr32kFrom2009c.anatomyBasis.display}",
      s"transform=${pointMap.source.display}",
      s"pointMapManifest=${pointMap.manifest.sha256.value}") ++
      pointMap.stageFiles.map(stage => s"pointMapStage=${stage.display}") ++ locked ++ Vector(
      s"bridgeExactness=${bridge.exactness.label}",
      s"policy=${policy.label}",
      s"method=${Fslr32kFrom2009c.method.label}",
      s"targetMesh=${StandardCorticalMesh.FsLR32k.display}"))

  val disclosure: StandardRouteDisclosure = StandardRouteDisclosure(
    identity, Fslr32kFrom2009c.name, Fslr32kFrom2009c.sourceFrame, Fslr32kFrom2009c.anatomyFrame,
    Fslr32kFrom2009c.anatomyBasis, bridge.display, bridge.exactness, policy, Fslr32kFrom2009c.method,
    Fslr32kFrom2009c.hemispheres.flatMap(h => Vector(h.midthickness, h.medialWall)) ++
      Vector(pointMap.source, pointMap.manifest) ++ pointMap.stageFiles,
    Fslr32kFrom2009c.remainingLimits)

  /** The request this route answers for `source` and `hemisphere`. */
  def request(source: VolumeReference, hemisphere: CorticalHemisphere, semantics: ValueSemantics): RouteRequest =
    RouteRequest(source, StandardCorticalMesh.FsLR32k, hemisphere, Fslr32kFrom2009c.method, semantics)

  /** Admit the route for one source grid declared in [[Fslr32kFrom2009c.sourceFrame]], placing every vertex of
    * `hemisphere` through the pointwise inverse. Refused when the surface route refuses, or when the placement gate
    * fails: every cortical vertex must be placed.
    */
  def admit(source: VolumeReference, hemisphere: CorticalHemisphere,
      semantics: ValueSemantics = ValueSemantics.Continuous): Either[StandardRouteRefusal, AdmittedSurfaceRoute] =
    val sampling = anatomy(hemisphere)
    for
      route <- SurfaceRoute.admit(request(source, hemisphere, semantics), sampling, Some(bridge))
        .left.map(StandardRouteRefusal.Route.apply)
      placement <- route.bridgePlacement.toRight(StandardRouteRefusal.Route(RouteRefusal.BridgeComposition(
        "the pointwise-inverse bridge produced no placement evidence")))
      wall = sampling.reference.medialWall
      unplaced = (0 until sampling.reference.vertexCount).count(v =>
        wall.cortexAt(VertexId(v)).contains(true) && !placement.isAvailable(v))
      _ <- Either.cond(unplaced == 0, (), StandardRouteRefusal.PlacementGate(hemisphere, unplaced,
        placement.inverseSummary.getOrElse(InversePlacementSummary(0, Map.empty, None, None))))
    yield route

  /** [[admit]] for both hemispheres, left then right. */
  def admitBoth(source: VolumeReference, semantics: ValueSemantics = ValueSemantics.Continuous)
      : Either[StandardRouteRefusal, Vector[AdmittedSurfaceRoute]] =
    for
      left <- admit(source, CorticalHemisphere.Left, semantics)
      right <- admit(source, CorticalHemisphere.Right, semantics)
    yield Vector(left, right)

/** Public, consumer-agnostic standard volume-to-surface routes, composed from surface primitives. */
object StandardSurfaceRoutes:
  /** Assemble the qualified MNI152NLin2009cAsym -> fsLR 32k route from loaded inputs, refusing anything that is not
    * exactly the locked asset: the point map must come from [[Fslr32kFrom2009c.transform]] under the locked manifest,
    * and each hemisphere's midthickness must carry the locked declaration (frame, publisher-methods basis, digest)
    * with the locked medial wall. The bridge is the map's pointwise inverse under `policy`.
    */
  def fsLR32kFrom2009c(
      pointMap: DeclaredPointMap,
      hemispheres: Vector[Fslr32kHemisphereInput],
      policy: StandardRoutePolicy = StandardRoutePolicy.Frozen
  ): Either[StandardRouteRefusal, Fslr32kRoute] =
    val lock = Fslr32kFrom2009c
    def locked(role: String, expected: String, supplied: String): Either[StandardRouteRefusal, Unit] =
      Either.cond(expected == supplied, (), StandardRouteRefusal.NotLocked(role, expected, supplied))
    for
      _ <- locked("point-map source", lock.transform.sha256, pointMap.source.sha256.value)
      _ <- locked("point-map manifest", lock.pointMapManifestSha256, pointMap.manifest.sha256.value)
      _ <- locked("point-map catalog revision", lock.catalogRevision.value, pointMap.catalogRevision.fold("none")(_.value))
      _ <- Either.cond(lock.transform.agreesWithName, (), StandardRouteRefusal.NotLocked("composite direction",
        lock.transform.declared.toString, lock.transform.measured.toString))
      _ <- locked("hemispheres", "lh,rh", hemispheres.map(_.surface.geometry.hemisphere.code).mkString(","))
      anatomies <- sequence(lock.hemispheres.zip(hemispheres).map((h, input) => anatomy(h, input).map(h.hemisphere -> _)))
      bridge <- FrameBridge.displacement(pointMap, PointMapUse.Inverse(policy.inverse),
          evidence = s"templateflow4s point map of ${lock.transform.relativePath} (admitted composite)")
        .left.map(StandardRouteRefusal.AssetRefused(lock.pointMapDirectory, _))
    yield Fslr32kRoute(policy, pointMap, bridge, anatomies.toMap)

  private def anatomy(h: Fslr32kFrom2009c.HemisphereLock, input: Fslr32kHemisphereInput)
      : Either[StandardRouteRefusal, SamplingAnatomy] =
    val declared = input.surface.declaration
    val wallAsset = input.medialWall.asset.fold("undeclared")(_.display)
    for
      _ <- Either.cond(declared == h.declaration, (),
        StandardRouteRefusal.NotLocked(s"${h.hemisphere.code} midthickness", h.declaration.display, declared.display))
      _ <- Either.cond(input.medialWall.asset.contains(h.medialWall), (),
        StandardRouteRefusal.NotLocked(s"${h.hemisphere.code} medial wall", h.medialWall.display, wallAsset))
      reference <- CorticalMeshReference.make(StandardCorticalMesh.FsLR32k, input.surface.geometry, input.medialWall)
        .left.map(StandardRouteRefusal.AssetRefused(h.medialWall.archivePath, _))
      sampling <- SamplingAnatomy.make(reference, AnatomicalGeometry.Midthickness(input.surface))
        .left.map(StandardRouteRefusal.AssetRefused(h.midthickness.archivePath, _))
    yield sampling

  private def sequence[A](items: Vector[Either[StandardRouteRefusal, A]]): Either[StandardRouteRefusal, Vector[A]] =
    items.foldLeft[Either[StandardRouteRefusal, Vector[A]]](Right(Vector.empty)): (acc, item) =>
      acc.flatMap(done => item.map(done :+ _))
