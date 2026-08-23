package scalafim.image

enum MissingVoxelPolicy[+A]:
  case RequireCovered extends MissingVoxelPolicy[Nothing]
  case DropMissing extends MissingVoxelPolicy[Nothing]
  case Fill[A](value: A) extends MissingVoxelPolicy[A]
