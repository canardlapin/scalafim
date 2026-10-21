package scalafim.fmri.mvpa.spatial

import locus4s.Index
import mvpa4s.AxisRef
import mvpa4s.locus.*
import mvpa4s.measurement.{MeasurementFrame, MeasurementFrameDeclaration}
import scalafim.locus.CenteredSearchlight

/** Surface composition remains downstream until neurosurf4s owns searchlights. */
object SurfaceMeasurementFrames:
  def surfacePatches[K, S](
      source: AxisRef[K] { type Locus = S },
      declaration: MeasurementFrameDeclaration,
      patches: CenteredSearchlight[S],
      label: Index[S] => Option[String] = (_: Index[S]) => None
  ): Either[SpatialMeasurementFrameError, MeasurementFrame[source.Id, K, SpatialMeasurementRendition[S]]] =
    val searchlight = patches.searchlight
    if !source.locus.sameRuntimeOwnerAs(searchlight.centers.space) then
      Left(ownerError("surface patch centers", source, searchlight.centers.space.toString))
    else if !source.locus.sameRuntimeOwnerAs(searchlight.neighborhoods.from) then
      Left(ownerError("surface patch source relation", source, searchlight.neighborhoods.from.toString))
    else if !source.locus.sameRuntimeOwnerAs(searchlight.neighborhoods.to) then
      Left(ownerError("surface patch target relation", source, searchlight.neighborhoods.to.toString))
    else
      val sites = Vector.newBuilder[SpatialMeasurementSite[S]]
      val centers = searchlight.centers.indicesInDomainOrder
      while centers.hasNext do
        val center = centers.next()
        searchlight.regionAt(center).foreach: support =>
          sites += SpatialMeasurementSite(
            SpatialMeasurementFrames.centeredId(source, CenteredMeasurementKind.Surface, center),
            SpatialMeasurementSupport.Regional(support),
            Some(center),
            label(center)
          )
      SpatialMeasurementFrames.regions(source, declaration, sites.result())

  private def ownerError[K](context: String, source: AxisRef[K], actual: String): SpatialMeasurementFrameError =
    SpatialMeasurementFrameError.WrongRuntimeOwner(context, source.locus.toString, actual)
