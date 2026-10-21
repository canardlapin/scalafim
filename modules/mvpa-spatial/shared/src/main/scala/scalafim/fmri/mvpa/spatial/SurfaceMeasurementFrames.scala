package scalafim.fmri.mvpa.spatial

import locus4s.{CenteredNeighborhoodSystem, Index}
import mvpa4s.AxisRef
import mvpa4s.locus.*
import mvpa4s.measurement.{MeasurementFrame, MeasurementFrameDeclaration}

/** Compose provider-owned compact surface neighborhoods with identified MVPA evidence. */
object SurfaceMeasurementFrames:
  def surfacePatches[K, C, S](
      source: AxisRef[K] { type Locus = S },
      declaration: MeasurementFrameDeclaration,
      patches: CenteredNeighborhoodSystem[C, S],
      label: Index[S] => Option[String] = (_: Index[S]) => None
  ): Either[SpatialMeasurementFrameError, MeasurementFrame[source.Id, K, SpatialMeasurementRendition[S]]] =
    if !source.locus.sameRuntimeOwnerAs(patches.ambient) then
      Left(ownerError("surface patch centers", source, patches.ambient.toString))
    else
      val sites = Vector.newBuilder[SpatialMeasurementSite[S]]
      val centers = patches.centers.indices
      while centers.hasNext do
        val compact = centers.next()
        val center = patches.center(compact)
        sites += SpatialMeasurementSite(
          SpatialMeasurementFrames.centeredId(source, CenteredMeasurementKind.Surface, center),
          SpatialMeasurementSupport.Regional(patches.neighborhood(compact)),
          Some(center),
          label(center)
        )
      SpatialMeasurementFrames.regions(source, declaration, sites.result())

  private def ownerError[K](context: String, source: AxisRef[K], actual: String): SpatialMeasurementFrameError =
    SpatialMeasurementFrameError.WrongRuntimeOwner(context, source.locus.toString, actual)
