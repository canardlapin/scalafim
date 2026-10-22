package scalafim.spatial

import neurosurf4s.routing.*

import locus4s.{Region, Selection, SpaceMismatch}
import neurosurf4s.routing.DomainLocus

extension (locus: DomainLocus)
  def regionDemand(region: Region[locus.S]): Either[SpaceMismatch, FieldDemand] =
    locus.regionRows(region).map(FieldDemand.roi)

  def selectionDemand(selection: Selection[locus.S]): Either[SpaceMismatch, FieldDemand] =
    locus.selectionRows(selection).map(FieldDemand.rows)
