package scalafim.fmri.workflow

import bids4s.{LinkedBidsFile, LinkedBidsProject}

/** Resolves an indexed BIDS artifact through its declared root, never a recipe base URI. */
object LinkedBidsArtifactResolver:
  def resolve[A](project: LinkedBidsProject, file: LinkedBidsFile): Either[WorkflowError, WorkflowArtifactRef[A]] =
    project.resolve(file.alias, file.path) match
      case None =>
        Left(WorkflowError.InvalidValue(
          "linked BIDS artifact",
          s"${file.alias.value}:${file.path.value}",
          "is not present in the declared linked-project catalog"
        ))
      case Some(location) =>
        val value = location.value
        val encoded = if value.startsWith("/") then "file://" + encodePath(value) else value
        WorkflowArtifactRef[A](encoded)

  private def encodePath(path: String): String =
    path.split("/", -1).map(encodeSegment).mkString("/")

  private def encodeSegment(segment: String): String =
    segment.getBytes("UTF-8").iterator.map { byte =>
      val value = byte & 0xff
      if (value >= 'a' && value <= 'z') || (value >= 'A' && value <= 'Z') ||
          (value >= '0' && value <= '9') || "-._~".contains(value.toChar)
      then value.toChar.toString
      else f"%%$value%02X"
    }.mkString
