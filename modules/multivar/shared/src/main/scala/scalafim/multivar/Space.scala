package scalafim.multivar

enum SpaceRole:
  case Samples
  case Observed
  case Latent
  case Kernel
  case Block

  def label: String =
    this match
      case Samples  => "samples"
      case Observed => "observed"
      case Latent   => "latent"
      case Kernel   => "kernel"
      case Block    => "block"

final case class MvSpace(id: SpaceId, role: SpaceRole, dim: Dimension):
  def size: Int =
    dim.value

object MvSpace:
  def of(id: String, role: SpaceRole, dim: Int): Either[MultivarError, MvSpace] =
    for
      spaceId <- SpaceId(id)
      dimension <- Dimension.from(dim, s"${role.label} space dimension")
    yield MvSpace(spaceId, role, dimension)

