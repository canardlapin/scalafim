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
      case Some(location) => WorkflowArtifactRef[A](location.value)
