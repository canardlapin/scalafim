package scalafim.atlas

import locus4s.Region
import locus4s.data.Field
import scalafim.surface.CorticalHemisphere

/** Portable field operations over a bilateral surface realization.
  * Values remain bound to the exact combined vertex owner; no dense label
  * materialization or positional hemisphere convention is introduced.
  */
object SurfaceAtlasFields:
  def reduce(
      atlas: SurfaceAtlas,
      input: Field[atlas.realization.X, Double],
      reducer: ParcelReducer = ParcelReducer.Mean,
      mask: Option[Field[atlas.realization.X, Boolean]] = None,
      policy: ParcelReductionPolicy = ParcelReductionPolicy()
  ): Either[AtlasReductionError, Field[atlas.realization.P, Double]] =
    AtlasReduce.reduceField(atlas.realization)(input, reducer, mask, policy)

  /** Reduce independently sampled scalar fields (for example timepoints).
    * Each input must carry the realization's exact bilateral vertex owner.
    */
  def reduceSeries(
      atlas: SurfaceAtlas,
      input: Vector[Field[atlas.realization.X, Double]],
      reducer: ParcelReducer = ParcelReducer.Mean,
      mask: Option[Field[atlas.realization.X, Boolean]] = None,
      policy: ParcelReductionPolicy = ParcelReductionPolicy()
  ): Either[AtlasReductionError, Vector[Field[atlas.realization.P, Double]]] =
    input.foldLeft[Either[AtlasReductionError, Vector[Field[atlas.realization.P, Double]]]](Right(Vector.empty)): (result, field) =>
      result.flatMap(values => reduce(atlas, field, reducer, mask, policy).map(values :+ _))

  /** Exact combined-vertex region for one cortical side. */
  def vertices(atlas: SurfaceAtlas, hemisphere: CorticalHemisphere): Region[atlas.realization.X] =
    val offset = hemisphere match
      case CorticalHemisphere.Left => 0
      case CorticalHemisphere.Right => atlas.realization.leftVertexCount
    val count = atlas.vertexCount(hemisphere)
    Region.tabulate(atlas.realization.parcelAssignment.from): vertex =>
      val ordinal = vertex.ordinal
      ordinal >= offset && ordinal < offset + count
