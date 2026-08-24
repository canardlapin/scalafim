package scalafim.image

import image4s.Axis
import image4s.AxisKind
import image4s.Categorical
import image4s.Continuous
import image4s.ImageError
import image4s.ImageMetadata
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.ValueSemantics
import image4s.Mask as MaskSemantics
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.locus.GridDomain
import image4s.locus.SelectedSampled
import image4s.locus.SelectedSampledError
import locus4s.Region
import locus4s.RegionError
import locus4s.Selection
import locus4s.SelectionError
import locus4s.SpaceMismatch
import ravel.DType
import ravel.NDArray
import ravel.Rank

enum SelectedImageError:
  case Provider(error: SelectedSampledError)
  case Image(error: ImageError)
  case Native(error: NativeImageError)
  case Selection(error: SelectionError)
  case InvalidRegion(error: RegionError)
  case SelectionSpace(error: SpaceMismatch)
  case ExpectedSingleTimeAxis(actual: Vector[AxisKind])
  case OutsideSupport(missing: Region[?])
  case SelectionOrderMismatch(left: Vector[Int], right: Vector[Int])
  case NonSpatialAxesMismatch(
      left: Vector[image4s.AxisRecord],
      right: Vector[image4s.AxisRecord]
  )

  def message: String =
    this match
      case Provider(error) =>
        error.message
      case Image(error) =>
        error.message
      case Native(error) =>
        error.message
      case Selection(error) =>
        error.message
      case InvalidRegion(error) =>
        error.message
      case SelectionSpace(error) =>
        error.message
      case ExpectedSingleTimeAxis(actual) =>
        s"SelectedSeries requires exactly one Time axis; found $actual"
      case OutsideSupport(missing) =>
        s"${missing.cardinality} requested positions are outside selected support"
      case SelectionOrderMismatch(left, right) =>
        s"selected operands use different position order: $left versus $right"
      case NonSpatialAxesMismatch(left, right) =>
        s"selected operands use different non-spatial axes: $left versus $right"

/** One compact value per position in an exact ordered voxel selection.
  *
  * This is a zero-wrapper refinement of image4s-locus `SelectedSampled`.
  * The provider value is the sole owner of support, order, and Ravel storage.
  */
opaque type SelectedVolume[
    F <: Frame[D3],
    S,
    A,
    Sem
] <: SelectedSampled[F, D3, S, A, Sem, Rank[1]] =
  SelectedSampled[F, D3, S, A, Sem, Rank[1]]

/** Dynamic-boundary package preserving the precise grid and voxel owners.
  *
  * The package owns no support or sample storage; `value` remains the one
  * image4s-locus object.
  */
sealed trait SomeSelectedVolume[A, Sem]:
  type F <: Frame[D3]
  type S
  val value: SelectedVolume[F, S, A, Sem]

object SomeSelectedVolume:
  def apply[F0 <: Frame[D3], S0, A, Sem](
      selected: SelectedVolume[F0, S0, A, Sem]
  ): SomeSelectedVolume[A, Sem] =
    new SomeSelectedVolume[A, Sem]:
      type F = F0
      type S = S0
      val value: SelectedVolume[F, S, A, Sem] = selected

object SelectedVolume:
  def fromSelected[
      F <: Frame[D3],
      S,
      A,
      Sem
  ](
      selected: SelectedSampled[F, D3, S, A, Sem, Rank[1]]
  ): SelectedVolume[F, S, A, Sem] =
    selected

  def create[
      F <: Frame[D3],
      S,
      T,
      A,
      Sem
  ](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      data: NDArray[A, Rank[1]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Sem]
  ): Either[SelectedImageError, SelectedVolume[F, S, A, Sem]] =
    SelectedSampled
      .create(
        domain,
        selection,
        NonSpatialAxes.empty,
        data,
        metadata
      )
      .left
      .map(SelectedImageError.Provider.apply)
      .map(fromSelected)

  def continuous[F <: Frame[D3], S, T, A](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      data: NDArray[A, Rank[1]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[SelectedImageError, SelectedVolume[F, S, A, Continuous]] =
    create[F, S, T, A, Continuous](
      domain,
      selection,
      data,
      metadata
    )

  def categorical[F <: Frame[D3], S, T, A](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      data: NDArray[A, Rank[1]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Categorical]
  ): Either[SelectedImageError, SelectedVolume[F, S, A, Categorical]] =
    create[F, S, T, A, Categorical](
      domain,
      selection,
      data,
      metadata
    )

  def mask[F <: Frame[D3], S, T](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      data: NDArray[Boolean, Rank[1]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[Boolean, MaskSemantics]
  ): Either[
    SelectedImageError,
    SelectedVolume[F, S, Boolean, MaskSemantics]
  ] =
    create[F, S, T, Boolean, MaskSemantics](
      domain,
      selection,
      data,
      metadata
    )

  def gather[
      F <: Frame[D3],
      S,
      T,
      A,
      Sem
  ](
      domain: GridDomain[F, D3, S],
      volume: SomeNeuroVolume[A, Sem],
      selection: Selection[T]
  ): Either[SelectedImageError, SelectedVolume[F, S, A, Sem]] =
    SelectedSampled
      .gatherVolume(domain, volume.sampled, selection)
      .left
      .map(SelectedImageError.Provider.apply)
      .map(fromSelected)

  /** Combine two scalar selections only when their exact owner and order
    * agree. Call `reselect` first when support union or filling is intended.
    */
  def zipExact[F <: Frame[D3], S, A, B, C, LeftSem, RightSem, OutSem](
      left: SelectedVolume[F, S, A, LeftSem],
      right: SelectedVolume[F, S, B, RightSem]
  )(
      combine: (A, B) => C
  )(using
      DType[C],
      ValueSemantics[C, OutSem]
  ): Either[SelectedImageError, SelectedVolume[F, S, C, OutSem]] =
    SelectedImageSelection
      .requireSameOwner(left.domain, right.domain)
      .flatMap: _ =>
        SelectedImageSelection.requireSameOrder(
          left.selection,
          right.selection
        )
      .flatMap: _ =>
        val data =
          NDArray.tabulate[C](left.selection.size): position =>
            combine(left.data(position), right.data(position))
        create(
          left.domain,
          left.selection,
          data,
          left.metadata
        )

  /** Align both inputs to an explicitly requested support before combining.
    * Each side states its own missing-voxel policy; no numeric zero is
    * inferred as background.
    */
  def combineAt[F <: Frame[D3], S, T, A, Sem](
      left: SelectedVolume[F, S, A, Sem],
      right: SelectedVolume[F, S, A, Sem],
      requested: Selection[T],
      leftMissing: MissingVoxelPolicy[A],
      rightMissing: MissingVoxelPolicy[A]
  )(
      combine: (A, A) => A
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[SelectedImageError, SelectedVolume[F, S, A, Sem]] =
    for
      alignedLeft <- left.reselect(requested, leftMissing)
      alignedRight <- right.reselect(requested, rightMissing)
      result <- zipExact(alignedLeft, alignedRight)(combine)
    yield result

  extension [F <: Frame[D3], S, A, Sem](
      volume: SelectedVolume[F, S, A, Sem]
  )
    inline def selected: SelectedSampled[F, D3, S, A, Sem, Rank[1]] =
      volume

    inline def apply(position: Int): A =
      volume.data(position)

    def toDense(
        fill: A
    ): Either[SelectedImageError, SomeNeuroVolume[A, Sem]] =
      volume
        .selected
        .scatterVolume(fill)
        .left
        .map(SelectedImageError.Provider.apply)
        .map(SomeNeuroVolume.unsafeFromSampled)

    def reselect[T](
        requested: Selection[T],
        policy: MissingVoxelPolicy[A]
    ): Either[SelectedImageError, SelectedVolume[F, S, A, Sem]] =
      SelectedImageSelection
        .requireOwner(volume.domain, requested)
        .flatMap: _ =>
          val sourcePositions =
            SelectedImageSelection.positionMap(volume.selection)
          policy match
            case MissingVoxelPolicy.RequireCovered =>
              val missing =
                requested.ordinals.filterNot(sourcePositions.contains)
              if missing.nonEmpty then
                Region
                  .fromOrdinals(volume.domain.space, missing)
                  .left
                  .map(SelectedImageError.InvalidRegion.apply)
                  .flatMap(region =>
                    Left(SelectedImageError.OutsideSupport(region))
                  )
              else
                materializeSelection(
                  requested,
                  ordinal => volume.data(sourcePositions(ordinal))
                )
            case MissingVoxelPolicy.DropMissing =>
              Selection
                .fromOrdinals(
                  volume.domain.space,
                  requested.ordinals.filter(sourcePositions.contains)
                )
                .left
                .map(SelectedImageError.Selection.apply)
                .flatMap: retained =>
                  materializeSelection(
                    retained,
                    ordinal => volume.data(sourcePositions(ordinal))
                  )
            case MissingVoxelPolicy.Fill(value) =>
              materializeSelection(
                requested,
                ordinal =>
                  sourcePositions
                    .get(ordinal)
                    .fold(value)(volume.data.apply)
              )

    private def materializeSelection[T](
        requested: Selection[T],
        valueAtOrdinal: Int => A
    ): Either[SelectedImageError, SelectedVolume[F, S, A, Sem]] =
      given DType[A] = volume.dtype
      val ordinals = requested.ordinals
      val data =
        NDArray.tabulate[A](requested.size): position =>
          valueAtOrdinal(ordinals(position))
      volume
        .selected
        .withSelectionData(requested, data)
        .left
        .map(SelectedImageError.Provider.apply)
        .map(fromSelected)

/** Position-major selected voxel time series.
  *
  * The Ravel shape is `(position, time)`, making every voxel's complete time
  * course contiguous. Construction certifies exactly one image4s Time axis.
  */
opaque type SelectedSeries[
    F <: Frame[D3],
    S,
    A,
    Sem
] <: SelectedSampled[F, D3, S, A, Sem, Rank[2]] =
  SelectedSampled[F, D3, S, A, Sem, Rank[2]]

/** Dynamic-boundary package preserving the precise grid and voxel owners.
  * The compact provider value remains the sole support and storage owner.
  */
sealed trait SomeSelectedSeries[A, Sem]:
  type F <: Frame[D3]
  type S
  val value: SelectedSeries[F, S, A, Sem]

object SomeSelectedSeries:
  def apply[F0 <: Frame[D3], S0, A, Sem](
      selected: SelectedSeries[F0, S0, A, Sem]
  ): SomeSelectedSeries[A, Sem] =
    new SomeSelectedSeries[A, Sem]:
      type F = F0
      type S = S0
      val value: SelectedSeries[F, S, A, Sem] = selected

object SelectedSeries:
  def fromSelected[
      F <: Frame[D3],
      S,
      A,
      Sem
  ](
      selected: SelectedSampled[F, D3, S, A, Sem, Rank[2]]
  ): Either[SelectedImageError, SelectedSeries[F, S, A, Sem]] =
    val kinds = selected.nonSpatialAxes.values.map(_.kind)
    if kinds == Vector(AxisKind.Time) then Right(selected)
    else Left(SelectedImageError.ExpectedSingleTimeAxis(kinds))

  def create[
      F <: Frame[D3],
      S,
      T,
      A,
      Sem
  ](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      timeAxis: Axis,
      data: NDArray[A, Rank[2]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Sem]
  ): Either[SelectedImageError, SelectedSeries[F, S, A, Sem]] =
    if timeAxis.kind != AxisKind.Time then
      Left(
        SelectedImageError.ExpectedSingleTimeAxis(
          Vector(timeAxis.kind)
        )
      )
    else
      NonSpatialAxes
        .from(Vector(timeAxis))
        .left
        .map(SelectedImageError.Image.apply)
        .flatMap: axes =>
          SelectedSampled
            .create(domain, selection, axes, data, metadata)
            .left
            .map(SelectedImageError.Provider.apply)
            .flatMap(fromSelected)

  def continuous[F <: Frame[D3], S, T, A](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      timeAxis: Axis,
      data: NDArray[A, Rank[2]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Continuous]
  ): Either[SelectedImageError, SelectedSeries[F, S, A, Continuous]] =
    create[F, S, T, A, Continuous](
      domain,
      selection,
      timeAxis,
      data,
      metadata
    )

  def categorical[F <: Frame[D3], S, T, A](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      timeAxis: Axis,
      data: NDArray[A, Rank[2]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[A, Categorical]
  ): Either[SelectedImageError, SelectedSeries[F, S, A, Categorical]] =
    create[F, S, T, A, Categorical](
      domain,
      selection,
      timeAxis,
      data,
      metadata
    )

  def mask[F <: Frame[D3], S, T](
      domain: GridDomain[F, D3, S],
      selection: Selection[T],
      timeAxis: Axis,
      data: NDArray[Boolean, Rank[2]],
      metadata: ImageMetadata = ImageMetadata.empty
  )(using
      ValueSemantics[Boolean, MaskSemantics]
  ): Either[
    SelectedImageError,
    SelectedSeries[F, S, Boolean, MaskSemantics]
  ] =
    create[F, S, T, Boolean, MaskSemantics](
      domain,
      selection,
      timeAxis,
      data,
      metadata
    )

  def gather[
      F <: Frame[D3],
      S,
      T,
      A,
      Sem
  ](
      domain: GridDomain[F, D3, S],
      series: SomeNeuroSeries[A, Sem],
      selection: Selection[T]
  ): Either[SelectedImageError, SelectedSeries[F, S, A, Sem]] =
    SelectedSampled
      .gatherSingleAxis(domain, series.sampled, selection)
      .left
      .map(SelectedImageError.Provider.apply)
      .flatMap(fromSelected)

  /** Combine two selected series with the same exact support order and time
    * axis. Use `combineAt` when support must first be aligned explicitly.
    */
  def zipExact[F <: Frame[D3], S, A, B, C, LeftSem, RightSem, OutSem](
      left: SelectedSeries[F, S, A, LeftSem],
      right: SelectedSeries[F, S, B, RightSem]
  )(
      combine: (A, B) => C
  )(using
      DType[C],
      ValueSemantics[C, OutSem]
  ): Either[SelectedImageError, SelectedSeries[F, S, C, OutSem]] =
    SelectedImageSelection
      .requireSameOwner(left.domain, right.domain)
      .flatMap: _ =>
        SelectedImageSelection.requireSameOrder(
          left.selection,
          right.selection
        )
      .flatMap: _ =>
        val leftAxes = left.nonSpatialAxes.records
        val rightAxes = right.nonSpatialAxes.records
        if leftAxes != rightAxes then
          Left(
            SelectedImageError.NonSpatialAxesMismatch(
              leftAxes,
              rightAxes
            )
          )
        else
          val data =
            NDArray.tabulate[C](left.selection.size, left.nTime):
              (position, time) =>
                combine(
                  left.data(position, time),
                  right.data(position, time)
                )
          create(
            left.domain,
            left.selection,
            left.nonSpatialAxes.values.head,
            data,
            left.metadata
          )

  /** Align both series to one requested voxel order under explicit missing
    * policies, then combine their position-time arrays.
    */
  def combineAt[F <: Frame[D3], S, T, A, Sem](
      left: SelectedSeries[F, S, A, Sem],
      right: SelectedSeries[F, S, A, Sem],
      requested: Selection[T],
      leftMissing: MissingVoxelPolicy[A],
      rightMissing: MissingVoxelPolicy[A]
  )(
      combine: (A, A) => A
  )(using
      DType[A],
      ValueSemantics[A, Sem]
  ): Either[SelectedImageError, SelectedSeries[F, S, A, Sem]] =
    for
      alignedLeft <- left.reselect(requested, leftMissing)
      alignedRight <- right.reselect(requested, rightMissing)
      result <- zipExact(alignedLeft, alignedRight)(combine)
    yield result

  extension [F <: Frame[D3], S, A, Sem](
      series: SelectedSeries[F, S, A, Sem]
  )
    inline def selected: SelectedSampled[F, D3, S, A, Sem, Rank[2]] =
      series

    inline def apply(position: Int, time: Int): A =
      series.data(position, time)

    inline def nTime: Int =
      series.nonSpatialAxes.values.head.extent

    def toDense(
        fill: A
    ): Either[SelectedImageError, SomeNeuroSeries[A, Sem]] =
      series
        .selected
        .scatter(fill)
        .left
        .map(SelectedImageError.Provider.apply)
        .flatMap: sampled =>
          sampled
            .requireDataRank[4]
            .left
            .map(SelectedImageError.Image.apply)
            .flatMap: ranked =>
              SomeNeuroSeries
                .fromSampled(ranked)
                .left
                .map(SelectedImageError.Native.apply)

    def reselect[T](
        requested: Selection[T],
        policy: MissingVoxelPolicy[A]
    ): Either[SelectedImageError, SelectedSeries[F, S, A, Sem]] =
      SelectedImageSelection
        .requireOwner(series.domain, requested)
        .flatMap: _ =>
          val sourcePositions =
            SelectedImageSelection.positionMap(series.selection)
          policy match
            case MissingVoxelPolicy.RequireCovered =>
              val missing =
                requested.ordinals.filterNot(sourcePositions.contains)
              if missing.nonEmpty then
                Region
                  .fromOrdinals(series.domain.space, missing)
                  .left
                  .map(SelectedImageError.InvalidRegion.apply)
                  .flatMap(region =>
                    Left(SelectedImageError.OutsideSupport(region))
                  )
              else
                materializeSelection(
                  requested,
                  ordinal => sourcePositions(ordinal)
                )
            case MissingVoxelPolicy.DropMissing =>
              Selection
                .fromOrdinals(
                  series.domain.space,
                  requested.ordinals.filter(sourcePositions.contains)
                )
                .left
                .map(SelectedImageError.Selection.apply)
                .flatMap: retained =>
                  materializeSelection(
                    retained,
                    ordinal => sourcePositions(ordinal)
                  )
            case MissingVoxelPolicy.Fill(value) =>
              materializeSelectionWithFill(
                requested,
                sourcePositions,
                value
              )

    private def materializeSelection[T](
        requested: Selection[T],
        sourcePosition: Int => Int
    ): Either[SelectedImageError, SelectedSeries[F, S, A, Sem]] =
      given DType[A] = series.dtype
      val ordinals = requested.ordinals
      val data =
        NDArray.tabulate[A](requested.size, nTime): (position, time) =>
          series.data(sourcePosition(ordinals(position)), time)
      series
        .selected
        .withSelectionData(requested, data)
        .left
        .map(SelectedImageError.Provider.apply)
        .flatMap(fromSelected)

    private def materializeSelectionWithFill[T](
        requested: Selection[T],
        sourcePositions: scala.collection.mutable.HashMap[Int, Int],
        fill: A
    ): Either[SelectedImageError, SelectedSeries[F, S, A, Sem]] =
      given DType[A] = series.dtype
      val ordinals = requested.ordinals
      val data =
        NDArray.tabulate[A](requested.size, nTime): (position, time) =>
          sourcePositions
            .get(ordinals(position))
            .fold(fill)(source => series.data(source, time))
      series
        .selected
        .withSelectionData(requested, data)
        .left
        .map(SelectedImageError.Provider.apply)
        .flatMap(fromSelected)

private object SelectedImageSelection:
  def requireOwner[
      F <: Frame[D3],
      S,
      T
  ](
      domain: GridDomain[F, D3, S],
      selection: Selection[T]
  ): Either[SelectedImageError, Unit] =
    if domain.space.sameRuntimeOwnerAs(selection.space) then Right(())
    else
      Left(
        SelectedImageError.SelectionSpace(
          SpaceMismatch.between(domain.space, selection.space)
        )
      )

  def positionMap[S](
      selection: Selection[S]
  ): scala.collection.mutable.HashMap[Int, Int] =
    val result = scala.collection.mutable.HashMap.empty[Int, Int]
    val ordinals = selection.ordinals
    var position = 0
    while position < ordinals.length do
      result(ordinals(position)) = position
      position += 1
    result

  def requireSameOwner[F <: Frame[D3], S, T](
      left: GridDomain[F, D3, S],
      right: GridDomain[F, D3, T]
  ): Either[SelectedImageError, Unit] =
    if left.space.sameRuntimeOwnerAs(right.space) then Right(())
    else
      Left(
        SelectedImageError.SelectionSpace(
          SpaceMismatch.between(left.space, right.space)
        )
      )

  def requireSameOrder[S, T](
      left: Selection[S],
      right: Selection[T]
  ): Either[SelectedImageError, Unit] =
    val leftOrder = left.ordinals.toVector
    val rightOrder = right.ordinals.toVector
    if leftOrder == rightOrder then Right(())
    else
      Left(
        SelectedImageError.SelectionOrderMismatch(
          leftOrder,
          rightOrder
        )
      )
