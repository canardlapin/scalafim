package scalafim.atlas.workflows

import image4s.{BoundaryPolicy, Categorical, NonSpatialAxes, SampleSpace, Sampled}
import image4s.geometry.{D3, Frame}
import ravel.NDArray
import reframe4s.core.AffineMap
import reframe4s.resample.ResamplingPlan as ProviderPlan
import scalafim.atlas.*
import scalafim.image.*

enum AtlasTransportError:
  case Atlas(cause: AtlasError)
  case Unsupported(detail: String)
  case Execution(detail: String)
  case Realization(cause: AtlasRealizationError)
  case EmptyCoverage

  def message: String = this match
    case Atlas(cause) => cause.message
    case Unsupported(detail) => detail
    case Execution(detail) => detail
    case Realization(cause) => cause.message
    case EmptyCoverage => "transport target contains no atlas parcels"

final case class AtlasTransportReceipt(
    source: AtlasRealizationIdentity,
    result: AtlasRealizationIdentity,
    pullFrom: SpaceId,
    pullTo: SpaceId,
    dropped: Vector[RegionId]
)

final case class AtlasTransportResult(atlas: VolumeAtlas, receipt: AtlasTransportReceipt)

/** A qualified affine nearest-label plan. Creating a plan is not execution:
  * only a successful execute returns an atlas and a completed receipt.
  */
final class AtlasTransportPlan private[workflows] (
    val route: TransformPlan,
    val source: AtlasRealizationIdentity,
    private val run: () => Either[AtlasTransportError, AtlasTransportResult]
):
  def execute(): Either[AtlasTransportError, AtlasTransportResult] = run()

object AtlasTransport:
  /** The route must pull target world points into the source atlas world.
    * Both worlds are checked against the actual grid frames by TransformPlan.
    * Only fused affine categorical transport is currently supported.
    */
  def prepare[T <: Frame[D3]](
      atlas: VolumeAtlas,
      target: GridSpec[T],
      targetSpace: SpaceId,
      route: TransformPlan
  ): Either[AtlasTransportError, AtlasTransportPlan] =
    val realization: atlas.realization.type = atlas.realization
    if route.from != targetSpace || route.to != atlas.ref.coordSpace then
      Left(AtlasTransportError.Unsupported("transport route must run target-to-source for resampling"))
    else
      val sourceGrid = GridSpec.fromGrid(realization.domain.grid)
      for
        _ <- target.grid.record.left.map(error => AtlasTransportError.Unsupported(error.message))
        pull <- route.pullback(sourceGrid, target).left.map(AtlasTransportError.Atlas.apply)
        affine <- pull match
          case value: AffineMap[T, realization.F, D3] @unchecked => Right(value)
          case _ => Left(AtlasTransportError.Unsupported("categorical atlas transport requires an affine pullback"))
        ref <- AtlasRef.checked(atlas.ref.details.copy(confidence = combinedConfidence(atlas.provenance.confidence, route.confidence)),
          AtlasRepresentation.Volume, targetSpace, targetSpace)
          .left.map(AtlasTransportError.Atlas.apply).flatMap:
            case value: AtlasRef.Volume => Right(value)
            case _ => Left(AtlasTransportError.Unsupported("transport requires a volume reference"))
      yield new AtlasTransportPlan(route, realization.identity, () =>
        val labels = SomeNeuroVolume.sampled(atlas.labelVolume)
        val space = SampleSpace.create(realization.domain.grid, NonSpatialAxes.empty)
        for
          admitted <- Sampled.categorical(space, labels.data, labels.metadata)
            .left.map(error => AtlasTransportError.Execution(error.message))
          provider <- ProviderPlan.nearest(admitted, target.grid, affine, BoundaryPolicy.Constant(0))
            .left.map(error => AtlasTransportError.Execution(error.message))
          sampled <- provider.run(provider.newWorkspace())
            .left.map(error => AtlasTransportError.Execution(error.message))
          shape = target.grid.shape
          data = NDArray.tabulate[Int](shape(0), shape(1), shape(2))((x, y, z) => sampled.image.data.at(IArray(x, y, z)))
          volume <- SomeNeuroVolume.fromRavel[Int, Categorical](data, target.toSampleSpace, labels.metadata)
            .left.map(error => AtlasTransportError.Execution(error.message))
          present = data.iterator.toSet - 0
          _ <- Either.cond(present.nonEmpty, (), AtlasTransportError.EmptyCoverage)
          kept = RegionIndex(atlas.regions.regions.filter(region => present.contains(region.id.value)))
          dropped = atlas.regions.ids.filterNot(id => present.contains(id.value)).sorted
          provenance = atlas.provenance.copy(
            support = SpatialSupport.fromRef(ref),
            labels = atlas.provenance.labels.copy(regionIds = kept.ids.sorted),
            confidence = ref.confidence,
            derivation = atlas.provenance.derivation ++ Vector(
              DerivationStep.Resampled(atlas.ref.coordSpace, targetSpace, TransformKind.Affine,
                TransformStatus.Available, route.confidence),
              DerivationStep.FilteredLabels(kept.ids.sorted, dropped)
            )
          )
          result <- VolumeAtlas.fromLabelVolumeEither(ref, kept, volume, provenance)
            .left.map(AtlasTransportError.Realization.apply)
        yield AtlasTransportResult(result, AtlasTransportReceipt(realization.identity, result.realization.identity,
          route.from, route.to, dropped))
      )

  private def combinedConfidence(source: Confidence, route: Confidence): Confidence =
    if source == Confidence.Uncertain || route == Confidence.Uncertain then Confidence.Uncertain
    else if source == Confidence.Approximate || route == Confidence.Approximate then Confidence.Approximate
    else if source == Confidence.High || route == Confidence.High then Confidence.High
    else Confidence.Exact
