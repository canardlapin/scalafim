package scalafim.surface

import image4s.geometry.{Affine, D3, Frame, FrameAlignment, GeometryError, Point}
import reframe4s.core.{MapError, SpatialMap}
import reframe4s.lie.FramedAffine
import scalafim.image.PrimitiveBuffers
import scalafim.image.world.{FrameCatalog, FreeSurferVolumeGeometry, Placed, Rebind, SpaceError, WorldSpace}

/** Failures of placing surface vertices in a world frame or moving them between frames. */
enum SurfaceFrameError derives CanEqual:
  case Space(error: SpaceError)
  case Map(error: MapError)
  case Geometry(error: GeometryError)
  case NonFiniteVertex(vertex: Int)
  case VertexOutOfRange(vertex: Int, vertexCount: Int)
  case WrongWorld(role: String, expected: String, actual: WorldSpace)
  case SubjectMismatch(tkRas: WorldSpace, scanner: WorldSpace)

  def message: String =
    this match
      case Space(error)                     => error.message
      case Map(error)                       => s"surface transport failed: ${error.message}"
      case Geometry(error)                  => s"surface frame geometry failed: ${error.message}"
      case NonFiniteVertex(vertex)          => s"vertex $vertex has a non-finite placed coordinate"
      case VertexOutOfRange(vertex, count)  => s"vertex $vertex is out of range for a surface with $count vertices"
      case WrongWorld(role, expected, actual) => s"$role frame must be $expected, got ${actual.displayName}"
      case SubjectMismatch(tkRas, scanner)  => s"tkRAS frame ${tkRas.displayName} and scanner frame ${scanner.displayName} belong to different subjects"

/** Which coordinates of a [[SurfaceGeometry]] are the ones that live in the declared frame. */
enum SurfacePlacement derives CanEqual:
  /** The stored vertex coordinates (FreeSurfer surfaces: tkRAS; most GIFTI files: scanner RAS). */
  case StoredCoordinates

  /** The stored coordinates mapped through the geometry's `surfaceToWorld` (a GIFTI coordinate-system transform). */
  case SurfaceToWorld

/** A triangle mesh whose vertex coordinates are points in the world frame `F`.
  *
  * The frame is part of the type, so a FreeSurfer surface in tkRAS and a GIFTI surface in scanner RAS cannot be mixed:
  * moving between frames goes through a typed provider map ([[transport]]), and for FreeSurfer's tkRAS through the
  * volume geometry that defines it ([[toScanner]]). Coordinates are stored once, row-major `x, y, z` per vertex, and are
  * always finite.
  */
final class FramedSurface[F <: Frame[D3]] private (
    val frame: F,
    val mesh: TriangleMesh,
    val hemisphere: Hemisphere,
    val kind: SurfaceKind
):
  def vertexCount: Int = mesh.vertexCount

  def faceCount: Int = mesh.faceCount

  /** Placed coordinates, row-major `x, y, z` per vertex, in frame `F`. */
  def coordinates: IArray[Double] = IArray.unsafeFromArray(mesh.coordinates)

  /** The placed position of one vertex as a point owned by this surface's frame. */
  def vertex(id: VertexId): Either[SurfaceFrameError, Point[F, D3]] =
    val i = id.index
    if i >= vertexCount then Left(SurfaceFrameError.VertexOutOfRange(i, vertexCount))
    else
      FramedSurface.pointIn(frame, Vector(mesh.coordinates(3 * i), mesh.coordinates(3 * i + 1), mesh.coordinates(3 * i + 2)))

  /** The same surface as a plain geometry whose stored coordinates are the placed ones. */
  def geometry: SurfaceGeometry = SurfaceGeometry(mesh, hemisphere, kind)

  def hasSameTopology(other: FramedSurface[?]): Boolean = mesh.hasSameTopology(other.mesh)

  /** The world space of this surface's frame; fails for frames not made by [[FrameCatalog]]. */
  def world: Either[SurfaceFrameError, WorldSpace] =
    FrameCatalog.worldOf(frame).left.map(SurfaceFrameError.Space.apply)

  /** Move every vertex through a typed provider map out of this surface's frame. Topology is unchanged. */
  def transport[G <: Frame[D3]](map: SpatialMap[F, G, D3]): Either[SurfaceFrameError, FramedSurface[G]] =
    val out = new Array[Double](mesh.coordinates.length)
    var i = 0
    var failure = Option.empty[SurfaceFrameError]
    while i < vertexCount && failure.isEmpty do
      val moved =
        for
          point <- vertex(VertexId.unsafe(i))
          next <- map(point).left.map(SurfaceFrameError.Map.apply)
        yield next.coordinates
      moved match
        case Left(error) => failure = Some(error)
        case Right(xyz) =>
          out(3 * i) = xyz(0)
          out(3 * i + 1) = xyz(1)
          out(3 * i + 2) = xyz(2)
      i += 1
    failure.toLeft(new FramedSurface(map.target, TriangleMesh.fromArrays(out, mesh.faceIndices.clone()), hemisphere, kind))

  /** Move a FreeSurfer tkRAS surface into the subject's scanner RAS with `Norig * inverse(Torig)` of the volume that
    * defines its tkRAS (usually `orig.mgz`). This surface's frame must be a [[WorldSpace.SubjectTkRas]] frame and
    * `scanner` a [[WorldSpace.SubjectNative]] frame of the same dataset and subject.
    */
  def toScanner(scanner: Frame[D3], geometry: FreeSurferVolumeGeometry): Either[SurfaceFrameError, FramedSurface[scanner.type]] =
    for
      _ <- FramedSurface.checkTkRasPair(frame, scanner)
      moved <- transport(FramedAffine.betweenFrames[F, scanner.type, D3](frame, scanner)(geometry.tkrToScanner))
    yield moved

  /** Inverse of [[toScanner]]: move a scanner-RAS surface into the tkRAS of the volume `geometry` describes. */
  def toTkRas(tkRas: Frame[D3], geometry: FreeSurferVolumeGeometry): Either[SurfaceFrameError, FramedSurface[tkRas.type]] =
    for
      _ <- FramedSurface.checkTkRasPair(tkRas, frame)
      moved <- transport(FramedAffine.betweenFrames[F, tkRas.type, D3](frame, tkRas)(geometry.tkrToScanner.inverse))
    yield moved

  override def toString: String =
    s"FramedSurface(${hemisphere.code}, ${kind.label}, vertices=$vertexCount, frame=$frame)"

object FramedSurface:
  /** Declare that `placement` of `geometry` gives coordinates in `frame`. */
  def in(frame: Frame[D3])(geometry: SurfaceGeometry, placement: SurfacePlacement): Either[SurfaceFrameError, FramedSurface[frame.type]] =
    placed(geometry, placement).map(coords =>
      new FramedSurface[frame.type](frame, TriangleMesh.fromArrays(coords, geometry.mesh.faceIndices.clone()), geometry.hemisphere, geometry.kind)
    )

  /** Place a geometry in the catalogued frame of `world`, as a dependent pair for code without a static frame. */
  def inWorld(world: WorldSpace)(geometry: SurfaceGeometry, placement: SurfacePlacement): Either[SurfaceFrameError, Placed[FramedSurface]] =
    val frame = FrameCatalog.frame(world)
    for
      surface <- in(frame)(geometry, placement)
      packed <- Placed[FramedSurface](frame)(surface).left.map(SurfaceFrameError.Space.apply)
    yield packed

  /** Move a framed surface across a checked frame alignment (same persistent key); coordinates are unchanged. */
  given rebind: Rebind[FramedSurface] with
    def toRight[A <: Frame[D3], B <: Frame[D3]](value: FramedSurface[A], alignment: FrameAlignment[D3, A, B]): Either[SpaceError, FramedSurface[B]] =
      if value.frame.sameRuntimeOwnerAs(alignment.left) then Right(new FramedSurface[B](alignment.right, value.mesh, value.hemisphere, value.kind))
      else Left(SpaceError.FrameBinding(s"surface frame ${value.frame} is not the alignment's left frame ${alignment.left}"))

  private[surface] def pointIn[F <: Frame[D3]](frame: F, coordinates: Vector[Double]): Either[SurfaceFrameError, Point[F, D3]] =
    (for
      raw <- Point.fromVector[D3](frame, coordinates)
      self <- Frame.alignOwners[D3, frame.type, F](frame, frame)
      owned <- self.pointToRight(raw)
    yield owned).left.map(SurfaceFrameError.Geometry.apply)

  private def placed(geometry: SurfaceGeometry, placement: SurfacePlacement): Either[SurfaceFrameError, Array[Double]] =
    placement match
      case SurfacePlacement.StoredCoordinates => Right(geometry.mesh.coordinates.clone())
      case SurfacePlacement.SurfaceToWorld    => applyAffine(geometry.surfaceToWorld, geometry.mesh.coordinates)

  private def applyAffine(affine: Affine[D3], coords: Array[Double]): Either[SurfaceFrameError, Array[Double]] =
    val m = affine.rowMajor.toArray
    val out = new Array[Double](coords.length)
    var i = 0
    var bad = -1
    while i < coords.length / 3 && bad < 0 do
      val (x, y, z) = (coords(3 * i), coords(3 * i + 1), coords(3 * i + 2))
      out(3 * i) = m(0) * x + m(1) * y + m(2) * z + m(3)
      out(3 * i + 1) = m(4) * x + m(5) * y + m(6) * z + m(7)
      out(3 * i + 2) = m(8) * x + m(9) * y + m(10) * z + m(11)
      if !(out(3 * i).isFinite && out(3 * i + 1).isFinite && out(3 * i + 2).isFinite) then bad = i
      i += 1
    if bad >= 0 then Left(SurfaceFrameError.NonFiniteVertex(bad)) else Right(out)

  private def checkTkRasPair(tkRas: Frame[D3], scanner: Frame[D3]): Either[SurfaceFrameError, Unit] =
    for
      tkWorld <- FrameCatalog.worldOf(tkRas).left.map(SurfaceFrameError.Space.apply)
      scWorld <- FrameCatalog.worldOf(scanner).left.map(SurfaceFrameError.Space.apply)
      _ <- (tkWorld, scWorld) match
        case (WorldSpace.SubjectTkRas(ns, sub, _), WorldSpace.SubjectNative(ns2, sub2, _, _)) =>
          if ns == ns2 && sub == sub2 then Right(()) else Left(SurfaceFrameError.SubjectMismatch(tkWorld, scWorld))
        case (WorldSpace.SubjectTkRas(_, _, _), other) =>
          Left(SurfaceFrameError.WrongWorld("scanner", "a subject-native (scanner RAS) space", other))
        case (other, _) =>
          Left(SurfaceFrameError.WrongWorld("tkRAS", "a FreeSurfer tkRAS space", other))
    yield ()
