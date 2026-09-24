package scalafim.surface.reference

import image4s.geometry.{Affine, D3}
import scalafim.image.*
import scalafim.surface.*
import scala.util.control.NonFatal

/** How a displacement point map is used by a bridge. */
enum PointMapUse:
  /** Apply the map: its input frame to its output frame. */
  case Forward
  /** Solve the map per point: its output frame to its input frame. */
  case Inverse(policy: InversePolicy)

  def label: String =
    this match
      case Forward => "forward"
      case Inverse(policy) => s"inverse (tolerance ${policy.toleranceMm} mm, at most ${policy.maxIterations} iterations)"

/** The transform a bridge applies to world coordinates. */
enum BridgeTransform:
  /** One exact affine. */
  case AffineMap(matrix: Affine[D3])
  /** A digest-bound composite point map, applied per vertex. */
  case Displacement(pointMap: DeclaredPointMap, use: PointMapUse)

/** Explicit transform taking world coordinates in `from` to world coordinates
  * in `to`. Either an exact affine, or a digest-bound displacement point map
  * whose endpoints follow from its manifest frames and its declared use. A
  * route across any other gap is refused rather than approximated. `Affine`
  * values are immutable, finite, homogeneous and invertible by construction.
  */
final case class FrameBridge private (from: TemplateFrame, to: TemplateFrame, transform: BridgeTransform, evidence: String):
  def display: String =
    transform match
      case BridgeTransform.AffineMap(_) => s"affine ${from.display} -> ${to.display} ($evidence)"
      case BridgeTransform.Displacement(map, use) =>
        s"point map ${from.display} -> ${to.display}, ${use.label}, source ${map.source.display}, " +
          s"manifest sha256 ${map.manifest.sha256.value}" +
          s"${map.stageFiles.map(f => s", stage ${f.display}").mkString} ($evidence)"

object FrameBridge:
  def affine(from: TemplateFrame, to: TemplateFrame, matrix: Affine[D3], evidence: String): Either[ReferenceError, FrameBridge] =
    if from == to then Left(ReferenceError.InvalidBridge("source and target frames are identical"))
    else if evidence.trim.isEmpty then Left(ReferenceError.InvalidBridge("bridge evidence must be declared"))
    else Right(FrameBridge(from, to, BridgeTransform.AffineMap(matrix), evidence))

  /** A point-map bridge. Forward use runs from the map's input frame to its
    * output frame; inverse use runs the other way. Frame releases are the
    * map's catalog revision; `release` is required when the map has none and
    * must agree with it when both are present.
    */
  def displacement(pointMap: DeclaredPointMap, use: PointMapUse, release: Option[TemplateRelease] = None,
      evidence: String = "digest-bound point map"): Either[ReferenceError, FrameBridge] =
    val resolved = (pointMap.catalogRevision, release) match
      case (Some(declared), Some(requested)) if declared != requested =>
        Left(ReferenceError.InvalidBridge(s"release ${requested.value} differs from the map's catalog revision ${declared.value}"))
      case (Some(declared), _) => Right(declared)
      case (None, Some(requested)) => Right(requested)
      case (None, None) => Left(ReferenceError.InvalidBridge("point map has no catalog revision; declare the frame release"))
    for
      value <- resolved
      _ <- Either.cond(evidence.trim.nonEmpty, (), ReferenceError.InvalidBridge("bridge evidence must be declared"))
      input <- TemplateFrame.make(pointMap.input, value)
      output <- TemplateFrame.make(pointMap.output, value)
    yield use match
      case PointMapUse.Forward => FrameBridge(input, output, BridgeTransform.Displacement(pointMap, use), evidence)
      case PointMapUse.Inverse(_) => FrameBridge(output, input, BridgeTransform.Displacement(pointMap, use), evidence)

/** How volume values reach a vertex. Both methods use nearest-voxel lookup
  * (ties round up; per-axis support [-0.5, dim - 0.5)). Neither is a
  * voxel-overlap ribbon method: `DepthNearest` combines point lookups at
  * declared white-to-pial fractions without volume weighting.
  */
enum MappingMethod:
  /** One lookup at the midthickness vertex, or at the white/pial midpoint. */
  case MidthicknessNearest
  /** Lookups at fractions of white-to-pial depth; requires white and pial anatomy. */
  case DepthNearest(fractions: Vector[Double])

  def label: String =
    this match
      case MidthicknessNearest => "midthickness-nearest"
      case DepthNearest(fractions) => fractions.mkString("depth-nearest[", ",", "]")

/** Continuous values average accepted lookups; categorical values must be
  * integral and take the modal label (ties toward the smallest label).
  *
  * Semantics are declared on the request rather than carried by a
  * `SomeLabelVolume`: the shared sampling kernel reads `Double` values, and
  * categorical routes still admit integral-valued floating-point atlases, whose
  * non-integral voxels are refused with a typed error at execution.
  */
enum ValueSemantics:
  case Continuous, Categorical

/** What a consumer asks for: map this exact source volume onto this exact
  * mesh hemisphere with this method and value semantics.
  */
final case class RouteRequest(
  source: VolumeReference,
  targetMesh: StandardCorticalMesh,
  hemisphere: CorticalHemisphere,
  method: MappingMethod,
  semantics: ValueSemantics
)

enum RouteRefusal:
  case TargetMeshMismatch(requested: StandardCorticalMesh, available: StandardCorticalMesh)
  case HemisphereMismatch(requested: CorticalHemisphere, available: CorticalHemisphere)
  case FrameMismatch(source: TemplateFrame, anatomy: TemplateFrame)
  case ReversedBridge(required: (TemplateFrame, TemplateFrame))
  case BridgeMismatch(required: (TemplateFrame, TemplateFrame), supplied: (TemplateFrame, TemplateFrame))
  case UnexpectedBridge(frame: TemplateFrame)
  case BridgeComposition(reason: String)
  case DepthThroughPointMap(anatomy: String)
  case WhitePialRequired(method: MappingMethod)
  case InvalidMethod(reason: String)

  def message: String =
    this match
      case TargetMeshMismatch(requested, available) => s"requested ${requested.display}; anatomy is ${available.display}"
      case HemisphereMismatch(requested, available) => s"requested ${requested.code}; anatomy is ${available.code}"
      case FrameMismatch(source, anatomy) =>
        s"source frame ${source.display} differs from anatomy frame ${anatomy.display} and no exact bridge was supplied"
      case ReversedBridge((from, to)) => s"bridge runs ${to.display} -> ${from.display}; required ${from.display} -> ${to.display}"
      case BridgeMismatch((from, to), (suppliedFrom, suppliedTo)) =>
        s"required bridge ${from.display} -> ${to.display}; supplied ${suppliedFrom.display} -> ${suppliedTo.display}"
      case UnexpectedBridge(frame) => s"anatomy is already in ${frame.display}; a bridge would be applied twice"
      case BridgeComposition(reason) => s"bridge cannot be composed with the anatomy's surface-to-world affine: $reason"
      case DepthThroughPointMap(anatomy) =>
        s"$anatomy anatomy through a point-map bridge would interpolate depth points between warped endpoints " +
          "instead of warping each depth point; refused until per-depth warping exists"
      case WhitePialRequired(method) => s"${method.label} requires white and pial anatomy"
      case InvalidMethod(reason) => s"invalid mapping method: $reason"

enum RouteError:
  case SourceGridMismatch(expected: Vector[Int], actual: Vector[Int])
  case SourceFrameMismatch(required: TemplateFrame, declared: TemplateFrame)
  case ForeignPreparation
  case NonIntegralLabel(voxel: VoxelCoord, value: Double)
  case VertexOutOfRange(vertex: Int, vertexCount: Int)
  case DisplayMismatch(reason: String)
  case SamplingFailure(reason: String)

  def message: String =
    this match
      case SourceGridMismatch(expected, actual) =>
        s"volume grid differs from the admitted source reference (dims $expected vs $actual, or affine differs)"
      case SourceFrameMismatch(required, declared) =>
        s"volume is declared in ${declared.display}; the route requires ${required.display}"
      case ForeignPreparation => "prepared source belongs to another route"
      case NonIntegralLabel(voxel, value) => s"categorical volume holds non-integral value $value at voxel $voxel"
      case VertexOutOfRange(vertex, count) => s"vertex $vertex is outside [0, $count)"
      case DisplayMismatch(reason) => s"display surface rejected: $reason"
      case SamplingFailure(reason) => s"sampling failed: $reason"

/** Status of the evidence behind an admitted route. Real-data qualification
  * is recorded separately per template route; until then only the numerical
  * kernel contract is claimed.
  */
enum RouteQualification:
  case NumericalContract

/** Everything a consumer must disclose about the route it used. */
final case class RouteDisclosure(
  sourceFrame: TemplateFrame,
  sourceDims: Vector[Int],
  sourceVoxelToWorld: Affine[D3],
  targetMesh: StandardCorticalMesh,
  hemisphere: CorticalHemisphere,
  anatomyFrame: TemplateFrame,
  anatomy: String,
  anatomyDeclarations: Vector[FrameDeclaration],
  bridge: Option[FrameBridge],
  method: MappingMethod,
  semantics: ValueSemantics,
  qualification: RouteQualification
):
  def lookup: String = "nearest voxel, ties round up, support [-0.5, dim - 0.5) per axis"
  def nonFinite: String = "nonfinite and out-of-support voxels are excluded before aggregation"

enum VertexCoverage:
  /** Mapped from at least one accepted lookup. */
  case Mapped
  /** Medial wall of the admitted mesh; values are withheld. */
  case MedialWall
  /** Placed in the source frame, but no lookup was accepted. */
  case NoSupport
  /** The bridge could not place the vertex (not converged, or outside the displacement support); never sampled. */
  case BridgeUnavailable

/** One requested lookup for a vertex, in source-volume world millimetres. */
enum Contribution:
  case OutsideGrid
  case OutsideSupport(voxel: VoxelCoord)
  case NonFinite(voxel: VoxelCoord, value: Double)
  /** `weight` is this lookup's share of the mapped value (duplicates count
    * separately): 1/n of the mean for continuous values; for categorical
    * values 1/k on each of the k lookups carrying the modal label, else 0.
    * In both cases the mapped value is the weighted sum.
    */
  case Included(voxel: VoxelCoord, value: Double, weight: Double)

final case class ContributionSample(world: WorldPoint, contribution: Contribution)

/** Per-vertex evidence sufficient to reconstruct the mapped value and to link
  * a pick back to the contributing source voxels.
  */
final case class VertexMappingEvidence(
  vertex: VertexId,
  coverage: VertexCoverage,
  value: Option[Double],
  samples: Vector[ContributionSample],
  /** Point-map outcome per anatomical surface (midthickness, or white then pial); empty without a point-map bridge. */
  bridge: Vector[PointMapOutcome] = Vector.empty
)

/** Values mapped onto one hemisphere's ordered vertex domain. Non-mapped
  * vertices hold NaN and say why through `coverageAt`.
  */
final class MappedSurfaceValues private[reference] (
  val disclosure: RouteDisclosure,
  val source: FrameDeclaration,
  val reference: CorticalMeshReference,
  values: Array[Double],
  coverage: Array[VertexCoverage],
  counts: Array[Int]
):
  def vertexCount: Int = values.length

  def valueAt(vertex: VertexId): Option[Double] =
    coverageAt(vertex).filter(_ == VertexCoverage.Mapped).map(_ => values(vertex.index))

  /** None when the vertex is outside this domain. */
  def coverageAt(vertex: VertexId): Option[VertexCoverage] = coverage.lift(vertex.index)

  /** Accepted lookups, reported for medial-wall vertices too although their value is withheld. */
  def acceptedLookupsAt(vertex: VertexId): Option[Int] = counts.lift(vertex.index)

  /** Dense copy with NaN at every vertex that is not `Mapped`. */
  def valuesCopy: Array[Double] = values.clone()

  def count(kind: VertexCoverage): Int = coverage.count(_ == kind)

  /** Carry these exact values and coverage onto a display shape of the same
    * ordered mesh. No resampling occurs; vertex ids are preserved.
    */
  def onDisplay(display: DisplaySurface): Either[RouteError, DisplayedSurfaceValues] =
    if display.reference.sameAs(reference) then Right(DisplayedSurfaceValues(this, display))
    else Left(RouteError.DisplayMismatch(s"${display.reference.display} is not ${reference.display} (mesh, domain or medial wall)"))

final case class DisplayedSurfaceValues private[reference] (mapped: MappedSurfaceValues, display: DisplaySurface)

/** A route admitted for an exact source reference and sampling anatomy. It
  * executes through the shared `VolumeSurfaceSampler` kernel.
  */
final class AdmittedSurfaceRoute private[reference] (
  val request: RouteRequest,
  val anatomy: SamplingAnatomy,
  val bridge: Option[FrameBridge],
  sampler: VolumeSurfaceSampler,
  placement: Option[BridgePlacement]
):
  /** Per-vertex point-map outcomes, when the bridge is a point map. */
  def bridgePlacement: Option[BridgePlacement] = placement

  private def unavailable(index: Int): Boolean = placement.exists(p => !p.available(index))

  val disclosure: RouteDisclosure = RouteDisclosure(
    request.source.frame, request.source.dims, request.source.voxelToWorld, anatomy.reference.mesh,
    anatomy.reference.hemisphere, anatomy.frame, anatomy.geometry.label, anatomy.declarations, bridge, request.method, request.semantics,
    RouteQualification.NumericalContract)

  /** Check a declared source volume once: declared frame, grid, support and
    * value semantics. The result is reused by `map` and every `inspect`.
    */
  def prepare(source: DeclaredVolume): Either[RouteError, PreparedSource] =
    if source.frame != request.source.frame then
      Left(RouteError.SourceFrameMismatch(request.source.frame, source.frame))
    else admissionMask(source.volume).map(PreparedSource(this, source, _))

  def map(source: DeclaredVolume): Either[RouteError, MappedSurfaceValues] =
    prepare(source).flatMap(map)

  def map(prepared: PreparedSource): Either[RouteError, MappedSurfaceValues] =
    owned(prepared).flatMap: _ =>
      guarded:
        val sampled = sampler.sampleSelected(prepared.source.volume, Some(prepared.admission), placement.map(_.available).orNull)
        val medialWall = anatomy.reference.medialWall
        val n = anatomy.reference.vertexCount
        val values = Array.fill(n)(Double.NaN)
        val coverage = new Array[VertexCoverage](n)
        val counts = new Array[Int](n)
        var i = 0
        while i < n do
          val vertex = VertexId(i)
          val accepted = sampled.sampleCounts.valueAt(vertex).getOrElse(0)
          counts(i) = accepted
          coverage(i) =
            if !medialWall.isCortex(vertex) then VertexCoverage.MedialWall
            else if unavailable(i) then VertexCoverage.BridgeUnavailable
            else if accepted == 0 then VertexCoverage.NoSupport
            else VertexCoverage.Mapped
          if coverage(i) == VertexCoverage.Mapped then values(i) = sampled.values.valueAt(vertex).get
          i += 1
        MappedSurfaceValues(disclosure, prepared.source.declaration, anatomy.reference, values, coverage, counts)

  def inspect(source: DeclaredVolume, vertex: VertexId): Either[RouteError, VertexMappingEvidence] =
    prepare(source).flatMap(inspect(_, vertex))

  /** Per-vertex evidence against a prepared source; no whole-volume work. */
  def inspect(prepared: PreparedSource, vertex: VertexId): Either[RouteError, VertexMappingEvidence] =
    val n = anatomy.reference.vertexCount
    if vertex.index >= n then Left(RouteError.VertexOutOfRange(vertex.index, n))
    else
      val outcomes = placement.fold(Vector.empty[PointMapOutcome])(_.outcomesAt(vertex.index))
      inspectPlaced(prepared, vertex, outcomes)

  private def inspectPlaced(prepared: PreparedSource, vertex: VertexId,
      outcomes: Vector[PointMapOutcome]): Either[RouteError, VertexMappingEvidence] =
    val volume = prepared.source.volume
    if unavailable(vertex.index) then
      owned(prepared).map: _ =>
        val coverage =
          if !anatomy.reference.medialWall.isCortex(vertex) then VertexCoverage.MedialWall else VertexCoverage.BridgeUnavailable
        VertexMappingEvidence(vertex, coverage, None, Vector.empty, outcomes)
    else
      owned(prepared).flatMap: _ =>
        sampler.inspectVertexEither(volume, vertex, Some(prepared.admission)).left.map(error => RouteError.SamplingFailure(error.message))
          .map: receipt =>
            val accepted = receipt.acceptedCount
            val winners = receipt.samples.count:
              case SurfacePointSample(_, SurfaceSampleOutcome.Included(_, value)) => value == receipt.value
              case _ => false
            def weight(value: Double): Double = request.semantics match
              case ValueSemantics.Continuous => 1.0 / accepted
              case ValueSemantics.Categorical => if value == receipt.value then 1.0 / winners else 0.0
            val support = request.source.support
            val samples = receipt.samples.map: sample =>
              val contribution = sample.outcome match
                case SurfaceSampleOutcome.OutsideVolume => Contribution.OutsideGrid
                case SurfaceSampleOutcome.Included(voxel, value) => Contribution.Included(voxel, value, weight(value))
                case SurfaceSampleOutcome.Masked(voxel) =>
                  val value = volume(voxel)
                  val supported = support.forall(mask => mask(voxel))
                  if supported && !value.isFinite then Contribution.NonFinite(voxel, value)
                  else Contribution.OutsideSupport(voxel)
              ContributionSample(sample.world, contribution)
            val coverage =
              if !anatomy.reference.medialWall.isCortex(vertex) then VertexCoverage.MedialWall
              else if accepted == 0 then VertexCoverage.NoSupport
              else VertexCoverage.Mapped
            VertexMappingEvidence(vertex, coverage, Option.when(coverage == VertexCoverage.Mapped)(receipt.value), samples, outcomes)

  /** Support-and-finite mask on the source grid; categorical values must be integral. */
  private def admissionMask(volume: SomeScalarVolume[Double]): Either[RouteError, SomeMaskVolume] =
    val source = request.source
    if !source.sharesGrid(volume.grid) then Left(RouteError.SourceGridMismatch(source.dims, volume.grid.shape))
    else
      val size = source.dims.product
      val flags = new Array[Boolean](size)
      var i = 0
      var failure: Option[RouteError] = None
      while i < size && failure.isEmpty do
        val value = volume.valueAtCanonicalOrdinal(i)
        val supported = source.support.forall(_.valueAtCanonicalOrdinal(i))
        if supported && value.isFinite then
          if request.semantics == ValueSemantics.Categorical && value != math.rint(value) then
            failure = Some(RouteError.NonIntegralLabel(volume.indexToVoxel(i), value))
          flags(i) = true
        i += 1
      failure.toLeft(()).flatMap: _ =>
        guarded(SomeMaskVolume.unsafeCopyFromCanonicalArray(flags, volume.space, "route-admission"))

  private def owned(prepared: PreparedSource): Either[RouteError, Unit] =
    Either.cond(prepared.route eq this, (), RouteError.ForeignPreparation)

  private def guarded[A](body: => A): Either[RouteError, A] =
    try Right(body)
    catch case NonFatal(error) => Left(RouteError.SamplingFailure(SurfaceError.reason(error)))

object SurfaceRoute:
  /** Admit one candidate anatomy for a request, or say exactly why not. The
    * anatomy frame must equal the source frame, or an affine bridge must run
    * from the anatomy frame to the source frame.
    */
  def admit(request: RouteRequest, anatomy: SamplingAnatomy, bridge: Option[FrameBridge] = None): Either[RouteRefusal, AdmittedSurfaceRoute] =
    val required = (anatomy.frame, request.source.frame)
    for
      _ <- Either.cond(anatomy.reference.mesh == request.targetMesh, (),
        RouteRefusal.TargetMeshMismatch(request.targetMesh, anatomy.reference.mesh))
      _ <- Either.cond(anatomy.reference.hemisphere == request.hemisphere, (),
        RouteRefusal.HemisphereMismatch(request.hemisphere, anatomy.reference.hemisphere))
      toSource <- frameTransform(required, bridge)
      path <- samplingPath(request.method, anatomy.geometry)
      located <- locate(anatomy, toSource)
    yield
      val aggregation = request.semantics match
        case ValueSemantics.Continuous => SurfaceSampleAggregation.Average
        case ValueSemantics.Categorical => SurfaceSampleAggregation.Mode
      val (surfaces, placement) = located
      AdmittedSurfaceRoute(request, anatomy, bridge, VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(surfaces, path, aggregation)),
        placement)

  /** Rank candidates and return the best admitted route: same-frame anatomy
    * before bridged anatomy, then candidate order. All refusals are returned
    * when none is admissible.
    */
  def select(request: RouteRequest, candidates: Vector[RouteCandidate]): Either[Vector[(RouteCandidate, RouteRefusal)], AdmittedSurfaceRoute] =
    val outcomes = candidates.map(candidate => candidate -> admit(request, candidate.anatomy, candidate.bridge))
    val admitted = outcomes.collect { case (_, Right(route)) => route }
    admitted.sortBy(route => if route.bridge.isEmpty then 0 else 1).headOption
      .toRight(outcomes.collect { case (candidate, Left(refusal)) => candidate -> refusal })

  private def frameTransform(required: (TemplateFrame, TemplateFrame), bridge: Option[FrameBridge]): Either[RouteRefusal, Option[BridgeTransform]] =
    val (anatomyFrame, sourceFrame) = required
    bridge match
      case None if anatomyFrame == sourceFrame => Right(None)
      case None => Left(RouteRefusal.FrameMismatch(sourceFrame, anatomyFrame))
      case Some(_) if anatomyFrame == sourceFrame => Left(RouteRefusal.UnexpectedBridge(sourceFrame))
      case Some(b) if b.from == anatomyFrame && b.to == sourceFrame => Right(Some(b.transform))
      case Some(b) if b.from == sourceFrame && b.to == anatomyFrame => Left(RouteRefusal.ReversedBridge(required))
      case Some(b) => Left(RouteRefusal.BridgeMismatch(required, (b.from, b.to)))

  private def samplingPath(method: MappingMethod, geometry: AnatomicalGeometry): Either[RouteRefusal, SurfaceSamplingPath] =
    (method, geometry) match
      case (MappingMethod.MidthicknessNearest, AnatomicalGeometry.Midthickness(_)) => Right(SurfaceSamplingPath.White)
      case (MappingMethod.MidthicknessNearest, AnatomicalGeometry.WhitePial(_, _)) => Right(SurfaceSamplingPath.Midpoint)
      case (MappingMethod.DepthNearest(_), AnatomicalGeometry.Midthickness(_)) => Left(RouteRefusal.WhitePialRequired(method))
      case (MappingMethod.DepthNearest(fractions), AnatomicalGeometry.WhitePial(_, _)) =>
        if fractions.isEmpty then Left(RouteRefusal.InvalidMethod("depth fractions must be non-empty"))
        else if !fractions.forall(f => f.isFinite && f >= 0.0 && f <= 1.0) then
          Left(RouteRefusal.InvalidMethod("depth fractions must lie in [0, 1]"))
        else Right(SurfaceSamplingPath.FractionalThickness(fractions))

  /** Anatomy placed in source-frame world coordinates. A midthickness surface
    * is paired with itself so the kernel samples its own coordinates. Surface
    * coordinates go through their own surfaceToWorld first, then the bridge.
    */
  private def locate(anatomy: SamplingAnatomy, toSource: Option[BridgeTransform])
      : Either[RouteRefusal, (SurfaceGeometryPair, Option[BridgePlacement])] =
    val midthickness = anatomy.geometry match
      case AnatomicalGeometry.Midthickness(_) => true
      case AnatomicalGeometry.WhitePial(_, _) => false
    toSource match
      case None => Right((anatomy.located, None))
      case Some(BridgeTransform.AffineMap(matrix)) =>
        def place(surface: SurfaceGeometry): Either[RouteRefusal, SurfaceGeometry] =
          surface.surfaceToWorld.andThen(matrix).map(surface.withSurfaceToWorld)
            .left.map(error => RouteRefusal.BridgeComposition(error.message))
        for
          white <- place(anatomy.located.white)
          pial <- place(anatomy.located.pial)
        yield (SurfaceGeometryPair(white, pial), None)
      case Some(BridgeTransform.Displacement(_, _)) if !midthickness =>
        Left(RouteRefusal.DepthThroughPointMap(anatomy.geometry.label))
      case Some(BridgeTransform.Displacement(map, use)) =>
        val white = placeByPointMap(anatomy.located.white, map.map, use)
        val pial = if midthickness then white else placeByPointMap(anatomy.located.pial, map.map, use)
        val outcomes = if midthickness then Vector(white._2) else Vector(white._2, pial._2)
        val n = anatomy.reference.vertexCount
        val available = Array.tabulate(n)(i => outcomes.forall(_(i).placed.nonEmpty))
        Right((SurfaceGeometryPair(white._1, pial._1), Some(BridgePlacement(outcomes, available))))

  /** Place every vertex through a point map. A vertex the map cannot place
    * keeps its pre-bridge world position only so the mesh stays well formed;
    * it is excluded from sampling and reported as `BridgeUnavailable`.
    */
  private def placeByPointMap(surface: SurfaceGeometry, map: PointMap, use: PointMapUse)
      : (SurfaceGeometry, Array[PointMapOutcome]) =
    val n = surface.vertexCount
    val coordinates = new Array[Double](3 * n)
    val outcomes = new Array[PointMapOutcome](n)
    var i = 0
    while i < n do
      val p = surface.mesh.vertex(VertexId.unsafe(i))
      val world = surface.surfaceToWorld(Vector(p.x, p.y, p.z)).toOption.flatMap(w => WorldPoint.make(w(0), w(1), w(2)).toOption)
      val outcome = world.fold(PointMapOutcome.OutsideSupport): point =>
        use match
          case PointMapUse.Forward => map.forward(point)
          case PointMapUse.Inverse(policy) => map.inverse(point, policy)
      outcomes(i) = outcome
      val at = outcome.placed.orElse(world).getOrElse(WorldPoint.Origin)
      coordinates(3 * i) = at.x
      coordinates(3 * i + 1) = at.y
      coordinates(3 * i + 2) = at.z
      i += 1
    val placed = SurfaceGeometry(TriangleMesh.fromArrays(coordinates, surface.mesh.faceIndices.clone()), surface.hemisphere,
      surface.kind)
    (placed, outcomes)

/** A declared source volume checked against one route: frame, grid, support
  * and value semantics, with its support-and-finite admission mask.
  */
final class PreparedSource private[reference] (
  val route: AdmittedSurfaceRoute,
  val source: DeclaredVolume,
  private[reference] val admission: SomeMaskVolume
)

/** Per-vertex point-map outcomes for one admitted route: one array per
  * anatomical surface (midthickness, or white then pial). A vertex is
  * available only when every surface placed it.
  */
final class BridgePlacement private[reference] (
  private val outcomes: Vector[Array[PointMapOutcome]],
  private[reference] val available: Array[Boolean]
):
  def outcomesAt(vertex: Int): Vector[PointMapOutcome] = outcomes.map(_(vertex))

  def isAvailable(vertex: Int): Boolean = available(vertex)

  def unavailableCount: Int = available.count(!_)

  /** Residuals (mm) of converged inverse placements on every surface. */
  def convergedResidualsMm: Vector[Double] = outcomes.flatMap(_.toVector).collect:
    case PointMapOutcome.Converged(_, residual, _) => residual

final case class RouteCandidate(anatomy: SamplingAnatomy, bridge: Option[FrameBridge] = None)
