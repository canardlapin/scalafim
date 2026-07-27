package scalafim.dataset.zarr

import scalafim.archive.zarr.AcquisitionId
import bids4s.*

final case class BidsAcquisition private (
    relativePath: String,
    name: BidsName,
    acquisitionId: AcquisitionId
)

object BidsAcquisition:
  def fromRelativePath(value: String): Either[String, BidsAcquisition] =
    val normalized = value.replace('\\', '/').trim
    if normalized.isEmpty || normalized.startsWith("/") || normalized.split('/').exists(segment => segment.isEmpty || segment == "." || segment == "..") then
      Left("BIDS acquisition path must be a confined relative path")
    else BidsName.parse(normalized.split('/').last).left.map(_.message).flatMap: name =>
      if name.kind != "bold" || (name.extension != "nii" && name.extension != "nii.gz") then
        Left("BIDS acquisition must be a BOLD NIfTI artifact")
      else
        val identity = (name.entities.renderParts :+ name.kind).mkString("_")
        AcquisitionId.from(identity).left.map(_.message).map: id =>
          new BidsAcquisition(normalized, name, id)
