package scalafim.image.world

import image4s.geometry.{
  CoordinateConvention,
  D3,
  Frame,
  FrameId,
  FrameMetadata,
  FrameRecord,
  LengthUnit
}

/** The only creator of world frames: persistent, RAS, millimetre, rank 3, identified by a [[WorldSpace]].
  *
  * Two frames made for equal world spaces are distinct runtime owners with one persistent key, so image4s
  * `FrameAlignment` accepts them; frames for different world spaces never align.
  */
object FrameCatalog:
  def frameId(world: WorldSpace): FrameId =
    FrameId
      .parse(WorldSpace.encode(world))
      .fold(error => throw new IllegalStateException(s"world-space encoding produced an invalid frame id: ${error.message}"), identity)

  def frame(world: WorldSpace): Frame[D3] =
    Frame.createPersistent[D3](
      frameId(world),
      metadata(world),
      LengthUnit.Millimeter,
      CoordinateConvention.RAS
    )

  /** Recover the world space a frame was made for; fails for ephemeral, non-RAS, non-mm or non-ScalaFIM frames. */
  def worldOf(frame: Frame[?]): Either[SpaceError, WorldSpace] =
    frame.persistentKey match
      case None =>
        Left(SpaceError.NotAWorldFrame("frame is ephemeral"))
      case Some(key) if key.spatialRank != 3 =>
        Left(SpaceError.NotAWorldFrame(s"rank ${key.spatialRank}"))
      case Some(key) if key.unit != LengthUnit.Millimeter =>
        Left(SpaceError.NotAWorldFrame(s"unit ${key.unit}"))
      case Some(key) if key.convention != CoordinateConvention.RAS =>
        Left(SpaceError.NotAWorldFrame(s"convention ${key.convention}"))
      case Some(key) =>
        WorldSpace.decode(key.id.value, declaredLabel = Some(frame.metadata.label))

  /** The persistence record for a world frame, for storage alongside data that lives in it. */
  def record(world: WorldSpace): FrameRecord =
    frame(world).record.fold(error => throw new IllegalStateException(error.message), identity)

  private def metadata(world: WorldSpace): FrameMetadata =
    val label = world.displayName.trim
    FrameMetadata
      .named(if label.isEmpty then "world space" else label)
      .fold(error => throw new IllegalStateException(error.message), identity)
