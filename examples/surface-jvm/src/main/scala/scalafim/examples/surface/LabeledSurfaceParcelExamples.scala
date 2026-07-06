package scalafim.examples.surface

import scalafim.surface.*

final case class SurfaceParcelRow(
  label: Int,
  name: String,
  nVertices: Int,
  representativeVertex: Int,
  boundaryContacts: Int
):
  def tabSeparated: String =
    Vector(label.toString, name, nVertices.toString, representativeVertex.toString, boundaryContacts.toString).mkString("\t")

object LabeledSurfaceParcelExamples:
  def bundledLabeledSurface(): LabeledSurface =
    labeledSurface(SurfaceIoExamples.readGifti(SurfaceExampleResources.giftiPath()))

  def labeledSurface(geometry: SurfaceGeometry): LabeledSurface =
    LabeledSurface.fromIndexed(
      geometry = geometry,
      indices = Vector.tabulate(geometry.vertexCount)(VertexId.apply),
      labels = Vector(1, 1, 1, 2),
      table = Vector(
        LabelInfo(1, "TriangleParcel", Some("#d95f02")),
        LabelInfo(2, "ApexParcel", Some("#1b9e77"))
      ),
      label = "toy-surface-labels"
    )

  def parcelRows(): Vector[SurfaceParcelRow] =
    val labeled = bundledLabeledSurface()
    val topology = MeshTopology.from(labeled.geometry.mesh)
    val parcels = SurfaceParcels.units(labeled, topology)
    val contacts = SurfaceParcels.boundaryContacts(parcels, topology)

    parcels.zipWithIndex.map { case (parcel, row) =>
      val contactsToOtherParcels =
        (0 until contacts.size).filter(_ != row).map(col => contacts.count(row, col)).sum
      SurfaceParcelRow(
        label = parcel.label,
        name = parcel.info.map(_.name).getOrElse(parcel.key.display),
        nVertices = parcel.size,
        representativeVertex = SurfaceParcels.centroidVertex(topology, parcel).index,
        boundaryContacts = contactsToOtherParcels
      )
    }

@main def summarizeSurfaceParcels(): Unit =
  println("label\tname\tnVertices\trepresentativeVertex\tboundaryContacts")
  LabeledSurfaceParcelExamples.parcelRows().foreach(row => println(row.tabSeparated))
