package scalafim.transform.x5

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.core.SpatialMap
import reframe4s.lie.FramedAffine
import scalafim.transform.*
import scalafim.transform.field.{DenseContext, DenseLattice}

/** An X5 domain: the reference grid a node is defined on. `mapping` is its row-major 4x4 voxel-to-RAS matrix. */
final case class X5Domain(grid: Boolean, size: Vector[Int], mapping: Vector[Double]) derives CanEqual

/** One `/TransformGroup/<index>` node as stored. `transform` is flattened in HDF5 (C, last-index-fastest) order. */
final case class X5Node(
    index: Int,
    kind: String,
    subtype: Option[String],
    representation: Option[String],
    shape: Vector[Int],
    transform: IArray[Double],
    domain: Option[X5Domain]
):
  override def equals(other: Any): Boolean =
    other match
      case that: X5Node =>
        index == that.index && kind == that.kind && subtype == that.subtype && representation == that.representation &&
          shape == that.shape && transform.sameElements(that.transform) && domain == that.domain
      case _ => false

  override def hashCode: Int = (index, kind, shape).##

/** An X5 file: its nodes and its `TransformChain` entries (each a list of node indices, applied in order). */
final case class X5File(nodes: Vector[X5Node], chains: Vector[Vector[Int]]) derives CanEqual

/** X5 as written by nitransforms, its reference implementation: every node maps reference (domain) RAS points to
  * moving RAS points, i.e. a pullback; linear nodes hold the 4x4 RAS matrix, densefield nodes hold displacements
  * (`y = x + d`) or deformations (`y = field`) on the domain grid. A chain applies its nodes in listed order. X5 is a
  * draft specification: this interpretation is consistent with nitransforms, not with a ratified standard.
  */
object X5Interpretation extends Interpretation[X5File, DenseContext, TransformChain]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](file: X5File, context: DenseContext[S, T]): Either[TransformError, TransformChain[S, T]] =
    interpretChain(file, 0, context)

  def interpretChain[S <: Frame[D3], T <: Frame[D3]](file: X5File, chain: Int, context: DenseContext[S, T]): Either[TransformError, TransformChain[S, T]] =
    val selected: Either[TransformError, Vector[X5Node]] =
      if file.chains.nonEmpty then
        file.chains.lift(chain).toRight(TransformError.Invalid(s"X5 file has no TransformChain $chain")).flatMap: indices =>
          indices.foldLeft[Either[TransformError, Vector[X5Node]]](Right(Vector.empty)): (acc, i) =>
            acc.flatMap(done => file.nodes.find(_.index == i).toRight(TransformError.Invalid(s"TransformChain refers to missing node $i")).map(done :+ _))
      else if file.nodes.size == 1 then Right(file.nodes)
      else Left(TransformError.Invalid(s"X5 file has ${file.nodes.size} nodes and no TransformChain; say which node or chain to use"))
    val provenance = TransformProvenance.read(TransformFormat.X5, AssetRef("X5", None))
    selected.flatMap: nodes =>
      val stages = nodes.map(n => TransformChain.Stage(s"${n.kind}/${n.subtype.getOrElse("")}", provenance))
      nodes match
        case Vector(node) if node.kind == "linear" =>
          linearMatrix(node).map: matrix =>
            TransformChain(stages, WorldTransform.Linear(FramedAffine.betweenFrames[T, S, D3](context.frames.target, context.frames.source)(matrix), provenance))
        case _ =>
          StageChain
            .compose(context.frames.target, context.frames.source, "x5", nodes.map(n => (from: Frame[D3], to: Frame[D3]) => stage(n, from, to, context)))
            .map(pull => TransformChain(stages, WorldTransform.Mapped(pull, PushAvailability.Unavailable[S, T](), provenance)))

  private def linearMatrix(node: X5Node): Either[TransformError, Affine[D3]] =
    if node.shape != Vector(4, 4) then Left(TransformError.Io(TransformIoError.Malformed("X5", s"linear node needs a 4x4 Transform, got ${node.shape.mkString("x")}")))
    else Affine.fromRowMajor[D3](IArray.genericWrapArray(node.transform).toVector).left.map(TransformError.Geometry(_))

  private def stage[S <: Frame[D3], T <: Frame[D3]](node: X5Node, from: Frame[D3], to: Frame[D3], context: DenseContext[S, T]): Either[TransformError, SpatialMap[Frame[D3], Frame[D3], D3]] =
    node.kind match
      case "linear" =>
        linearMatrix(node).map(m => SpatialMap.eraseFrameRefinements(FramedAffine.betweenFrames[from.type, to.type, D3](from, to)(m)))
      case "nonlinear" if node.subtype.contains("densefield") =>
        val absolute = node.representation match
          case Some("displacements") => Right(false)
          case Some("deformations")  => Right(true)
          case other                 => Left(TransformError.Io(TransformIoError.Malformed("X5", s"densefield representation $other")))
        for
          isAbsolute <- absolute
          domain <- node.domain.toRight(TransformError.Io(TransformIoError.Malformed("X5", "a densefield node needs a Domain")))
          dims = node.shape.take(3)
          _ <- Either.cond(node.shape.size == 4 && node.shape(3) == 3 && domain.size.take(3) == dims, (), TransformError.Io(TransformIoError.Malformed("X5", s"densefield shape ${node.shape} does not match domain ${domain.size}")))
          latticeToRas <- Affine.fromRowMajor[D3](domain.mapping).left.map(TransformError.Geometry(_))
          m = latticeToRas.rowMajor
          t = node.transform
          dense <- DenseLattice.pullback(from, to, dims, latticeToRas, context.boundary): (x, y, z) =>
            val base = ((x * dims(1) + y) * dims(2) + z) * 3 // C order: component fastest, x slowest
            Vector.tabulate(3): r =>
              if isAbsolute then t(base + r)
              else m(4 * r) * x + m(4 * r + 1) * y + m(4 * r + 2) * z + m(4 * r + 3) + t(base + r)
        yield SpatialMap.eraseFrameRefinements(dense)
      case other =>
        Left(TransformError.Io(TransformIoError.Malformed("X5", s"node type $other/${node.subtype.getOrElse("")} is not supported")))
