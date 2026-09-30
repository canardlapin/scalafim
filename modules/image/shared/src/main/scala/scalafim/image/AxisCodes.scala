package scalafim.image

import SampleSpaces.*

import gale.linalg.DMat
import image4s.ValueSemantics
import image4s.geometry.{Affine as GeometryAffine, D3}

/** The positive world direction of one voxel axis, in RAS world coordinates (nibabel's axis-code convention: `RAS`
  * means i increases to the Right, j to Anterior, k to Superior). [[AnatomicalAxis]] names the opposite (origin) side,
  * as neuroim2 does; [[AxisCodes.fromOrientation]] and [[AxisCodes.toOrientation]] convert between them.
  */
enum AxisCode(val worldAxis: Int, val sign: Int) derives CanEqual:
  case R extends AxisCode(0, 1)
  case L extends AxisCode(0, -1)
  case A extends AxisCode(1, 1)
  case P extends AxisCode(1, -1)
  case S extends AxisCode(2, 1)
  case I extends AxisCode(2, -1)

/** Axis codes of a 3D grid, one per voxel axis, with each world axis used exactly once. */
final case class AxisCodes private (first: AxisCode, second: AxisCode, third: AxisCode) derives CanEqual:
  def codes: Vector[AxisCode] = Vector(first, second, third)
  override def toString: String = codes.mkString

object AxisCodes:
  val RAS: AxisCodes = new AxisCodes(AxisCode.R, AxisCode.A, AxisCode.S)
  val LPS: AxisCodes = new AxisCodes(AxisCode.L, AxisCode.P, AxisCode.S)
  val LAS: AxisCodes = new AxisCodes(AxisCode.L, AxisCode.A, AxisCode.S)

  def make(first: AxisCode, second: AxisCode, third: AxisCode): Either[OrientationError, AxisCodes] =
    if Vector(first, second, third).map(_.worldAxis).distinct.size != 3 then
      Left(OrientationError.DuplicateAnatomicalAxes(Vector(first, second, third).map(toAnatomical)))
    else Right(new AxisCodes(first, second, third))

  /** Parse three letters such as `"RAS"` or `"PIR"`. */
  def parse(text: String): Either[OrientationError, AxisCodes] =
    val letters = text.trim.toUpperCase.toVector
    if letters.size != 3 then Left(OrientationError.Expected3Axes(letters.size))
    else
      letters
        .foldLeft[Either[OrientationError, Vector[AxisCode]]](Right(Vector.empty)): (acc, c) =>
          acc.flatMap(done => AxisCode.values.find(_.toString == c.toString).toRight(OrientationError.UnknownAxisAbbreviation(c.toString)).map(done :+ _))
        .flatMap(codes => make(codes(0), codes(1), codes(2)))

  /** The closest axis codes for a voxel-to-world affine (orthogonalised direction cosines, best signed permutation). */
  def of(affine: GeometryAffine[D3]): Either[OrientationError, AxisCodes] =
    val m = affine.rowMajor
    Orientation.findAnatomyEither(DMat.dense(3, 3, Vector(m(0), m(1), m(2), m(4), m(5), m(6), m(8), m(9), m(10)))).map(fromOrientation)

  /** Axis codes of a sample space's spatial grid. */
  def ofSpace(space: SomeSampleSpace): Either[OrientationError, AxisCodes] =
    space.affineD3.left.map(e => OrientationError.InvalidSpace(e.message)).flatMap(of)

  def fromOrientation(orientation: Orientation3D): AxisCodes =
    val codes = orientation.axes.map(fromAnatomical)
    new AxisCodes(codes(0), codes(1), codes(2))

  def toOrientation(codes: AxisCodes): Orientation3D =
    Orientation3D.unsafe(toAnatomical(codes.first), toAnatomical(codes.second), toAnatomical(codes.third))

  /** An [[AnatomicalAxis]] names the side an axis starts from; the axis code names the side it points to. */
  private def fromAnatomical(axis: AnatomicalAxis): AxisCode =
    axis match
      case AnatomicalAxis.L => AxisCode.R
      case AnatomicalAxis.R => AxisCode.L
      case AnatomicalAxis.P => AxisCode.A
      case AnatomicalAxis.A => AxisCode.P
      case AnatomicalAxis.I => AxisCode.S
      case AnatomicalAxis.S => AxisCode.I

  private def toAnatomical(code: AxisCode): AnatomicalAxis =
    code match
      case AxisCode.R => AnatomicalAxis.L
      case AxisCode.L => AnatomicalAxis.R
      case AxisCode.A => AnatomicalAxis.P
      case AxisCode.P => AnatomicalAxis.A
      case AxisCode.S => AnatomicalAxis.I
      case AxisCode.I => AnatomicalAxis.S

/** How to reach target axis codes from current ones: new voxel axis `j` reads old axis `permutation(j)`, reversed when
  * `flips(j)`. Applying it permutes and flips the data (as ravel views) and composes the affine with the matching voxel
  * map, so every voxel keeps its world position.
  */
final case class Reorientation private (from: AxisCodes, to: AxisCodes, permutation: Vector[Int], flips: Vector[Boolean]):
  /** New voxel index -> old voxel index, for a grid whose old extents are `dims`. */
  def voxelMap(dims: Vector[Int]): GeometryAffine[D3] =
    val rows = Vector.tabulate(3): oldAxis =>
      val j = permutation.indexOf(oldAxis)
      val coefficient = if flips(j) then -1.0 else 1.0
      val offset = if flips(j) then (dims(oldAxis) - 1).toDouble else 0.0
      Vector.tabulate(3)(c => if c == j then coefficient else 0.0) :+ offset
    GeometryAffine.fromRowMajor[D3](rows.flatten ++ Vector(0.0, 0.0, 0.0, 1.0)).fold(e => throw new IllegalStateException(e.message), identity)

  def isIdentity: Boolean = permutation == Vector(0, 1, 2) && flips.forall(!_)

object Reorientation:
  def between(from: AxisCodes, to: AxisCodes): Reorientation =
    val permutation = to.codes.map(target => from.codes.indexWhere(_.worldAxis == target.worldAxis))
    val flips = to.codes.zip(permutation).map((target, source) => from.codes(source).sign != target.sign)
    Reorientation(from, to, permutation, flips)

  /** Reorient a volume so its voxel axes follow `to`; data and affine move together, world identity is kept. */
  def volume[A, Sem](vol: SomeNeuroVolume[A, Sem], to: AxisCodes)(using ValueSemantics[A, Sem]): Either[OrientationError, SomeNeuroVolume[A, Sem]] =
    for
      affine <- vol.space.affineD3.left.map(e => OrientationError.InvalidSpace(e.message))
      from <- AxisCodes.of(affine)
      plan = between(from, to)
      space <- reorientedSpace(vol.space, plan)
      data = plan.flips.zipWithIndex.foldLeft(vol.values.permuteAxes(plan.permutation*)) { case (d, (flip, axis)) => if flip then d.reverse(axis) else d }
    yield SomeNeuroVolume.unsafeFromRavel[A, Sem](data, space, vol.label)

  /** Reorient a series' three spatial axes; the time axis is unchanged. */
  def series[A, Sem](vec: SomeNeuroSeries[A, Sem], to: AxisCodes)(using ValueSemantics[A, Sem]): Either[OrientationError, SomeNeuroSeries[A, Sem]] =
    for
      affine <- vec.space.affineD3.left.map(e => OrientationError.InvalidSpace(e.message))
      from <- AxisCodes.of(affine)
      plan = between(from, to)
      space <- reorientedSpace(vec.space, plan)
      data = plan.flips.zipWithIndex.foldLeft(vec.values.permuteAxes((plan.permutation :+ 3)*)) { case (d, (flip, axis)) => if flip then d.reverse(axis) else d }
    yield SomeNeuroSeries.unsafeFromRavel[A, Sem](data, space, vec.label)

  private def reorientedSpace(space: SomeSampleSpace, plan: Reorientation): Either[OrientationError, SomeSampleSpace] =
    val dims = space.grid.shape
    val newDims = plan.permutation.map(dims) ++ space.dims.drop(3)
    for
      old <- space.affineD3.left.map(e => OrientationError.InvalidSpace(e.message))
      affine <- plan.voxelMap(dims).andThen(old).left.map(e => OrientationError.InvalidSpace(e.message))
      world <- SampleSpaces.worldOf(space).left.map(e => OrientationError.InvalidSpace(e.message))
      relabelled <- SampleSpaces
        .make(newDims, axes = Some(space.nonSpatialAxes), affine = Some(affine))
        .flatMap(SampleSpaces.inWorld(_, world))
        .left
        .map(e => OrientationError.InvalidSpace(e.message))
    yield relabelled
