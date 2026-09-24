package scalafim.transform.itk

import image4s.geometry.{Affine, D3, Frame}
import reframe4s.core.SpatialMap
import reframe4s.field.CoordinateBoundaryPolicy
import reframe4s.lie.FramedAffine
import scalafim.image.world.ToolCoordinates
import scalafim.transform.*
import scalafim.transform.field.{DenseContext, DenseLattice}

/** One component of an ITK HDF5 transform file (`/TransformGroup/<index>`), exactly as stored. Parameters use primitive
  * arrays: displacement fields hold millions of values.
  */
final case class ItkHdf5Component(index: Int, typeName: String, parameters: IArray[Double], fixedParameters: IArray[Double]):
  def kind: String =
    typeName.split('_').headOption.getOrElse(typeName)

  def isComposite: Boolean = kind == "CompositeTransform"
  def isDisplacementField: Boolean = kind == "DisplacementFieldTransform"

  def asEntry: ItkEntry =
    ItkEntry(typeName, IArray.genericWrapArray(parameters).toVector, IArray.genericWrapArray(fixedParameters).toVector)

  override def equals(other: Any): Boolean =
    other match
      case that: ItkHdf5Component =>
        index == that.index && typeName == that.typeName &&
          parameters.sameElements(that.parameters) && fixedParameters.sameElements(that.fixedParameters)
      case _ => false

  override def hashCode: Int = (index, typeName, parameters.length, fixedParameters.length).##

/** The content of an ITK HDF5 transform file: an optional `CompositeTransform` marker at index 0 followed by its
  * components. ITK applies composite components last-first (the final component sees the point first).
  */
final case class ItkHdf5File(components: Vector[ItkHdf5Component]):
  def stages: Vector[ItkHdf5Component] = components.filterNot(_.isComposite)

/** What an ITK HDF5 file means: its components as pullback stages in LPS, converted once to RAS and chained. A chain of
  * linear components fuses into one affine; any displacement field makes the chain a dense map (forward direction
  * unavailable unless an inverse asset is supplied).
  */
object ItkHdf5Interpretation extends Interpretation[ItkHdf5File, DenseContext, TransformChain]:
  def interpret[S <: Frame[D3], T <: Frame[D3]](file: ItkHdf5File, context: DenseContext[S, T]): Either[TransformError, TransformChain[S, T]] =
    interpretWith(file, context, AssetRef("ITK HDF5", None))

  def interpretWith[S <: Frame[D3], T <: Frame[D3]](file: ItkHdf5File, context: DenseContext[S, T], asset: AssetRef): Either[TransformError, TransformChain[S, T]] =
    val stages = file.stages
    val markers = file.components.filter(_.isComposite)
    val provenance = TransformProvenance.read(TransformFormat.ItkHdf5, asset)
    val chainStages = stages.map(c => TransformChain.Stage(c.typeName, provenance))
    if stages.isEmpty then Left(TransformError.Invalid("ITK HDF5 file has no transform components"))
    else if markers.size > 1 || markers.exists(_.index != 0) then Left(TransformError.Invalid("a CompositeTransform marker may appear only once, at index 0"))
    else if stages.forall(c => ItkLinear.isLinear(c.asEntry)) then
      ItkLinearInterpretation
        .interpretWith(ItkTransformFile(stages.map(_.asEntry)), context.frames, asset, TransformFormat.ItkHdf5)
        .map(chain => TransformChain(chainStages, chain.composed))
    else
      // ITK applies the last component first: the travel order is the file order reversed.
      val stagesInTravelOrder = stages.reverse.map(component => (from: Frame[D3], to: Frame[D3]) => stageMap(component, from, to, context.boundary))
      StageChain
        .compose(context.frames.target, context.frames.source, "itk-hdf5", stagesInTravelOrder)
        .map(pull => TransformChain(chainStages, WorldTransform.Mapped(pull, PushAvailability.Unavailable[S, T](), provenance)))

  private def stageMap(component: ItkHdf5Component, from: Frame[D3], to: Frame[D3], boundary: CoordinateBoundaryPolicy): Either[TransformError, SpatialMap[Frame[D3], Frame[D3], D3]] =
    if component.isDisplacementField then displacement(component, from, to, boundary)
    else
      for
        mo <- ItkLinear.matrixOffset(component.asEntry).left.map(TransformError.Io(_))
        lps <- Affine.fromRowMajor[D3](mo.rowMajor4).left.map(TransformError.Geometry(_))
        ras <- ToolCoordinates.LpsToRas.andThen(lps).flatMap(_.andThen(ToolCoordinates.LpsToRas)).left.map(TransformError.Geometry(_))
      yield SpatialMap.eraseFrameRefinements(FramedAffine.betweenFrames[from.type, to.type, D3](from, to)(ras))

  /** ITK DisplacementFieldTransform: fixed = size(3), origin(3), spacing(3), direction(9, row-major), all LPS;
    * parameters = displacements interleaved per voxel, x fastest. A point `p` maps to `p + d(p)`.
    */
  private def displacement(component: ItkHdf5Component, from: Frame[D3], to: Frame[D3], boundary: CoordinateBoundaryPolicy): Either[TransformError, SpatialMap[Frame[D3], Frame[D3], D3]] =
    val f = component.fixedParameters
    if f.length != 18 then Left(TransformError.Io(TransformIoError.Malformed("ITK HDF5", s"displacement field needs 18 fixed parameters, got ${f.length}")))
    else
      val dims = Vector(f(0), f(1), f(2)).map(v => math.rint(v).toInt)
      val count = dims.product
      if dims.exists(_ <= 0) || component.parameters.length != 3 * count then
        Left(TransformError.Io(TransformIoError.Malformed("ITK HDF5", s"displacement field size $dims does not match ${component.parameters.length} parameters")))
      else if !component.parameters.forall(_.isFinite) then Left(TransformError.Io(TransformIoError.Malformed("ITK HDF5", "displacement values must be finite")))
      else
        val (origin, spacing, dir) = ((3 until 6).map(f(_)), (6 until 9).map(f(_)), (9 until 18).map(f(_)))
        val lpsGrid = Vector.tabulate(3)(r => Vector.tabulate(3)(c => dir(3 * r + c) * spacing(c)) :+ origin(r)).flatten ++ Vector(0.0, 0.0, 0.0, 1.0)
        for
          lps <- Affine.fromRowMajor[D3](lpsGrid).left.map(TransformError.Geometry(_))
          latticeToRas <- lps.andThen(ToolCoordinates.LpsToRas).left.map(TransformError.Geometry(_))
          m = latticeToRas.rowMajor
          flip = ToolCoordinates.LpsToRas.rowMajor // an LPS displacement is a vector: only the flip's diagonal applies
          p = component.parameters
          dense <- DenseLattice.pullback(from, to, dims, latticeToRas, boundary): (x, y, z) =>
            val base = 3 * (x + dims(0) * (y + dims(1) * z))
            Vector.tabulate(3): r =>
              val world = m(4 * r) * x + m(4 * r + 1) * y + m(4 * r + 2) * z + m(4 * r + 3)
              world + flip(5 * r) * p(base + r)
        yield SpatialMap.eraseFrameRefinements(dense)
