package scalafim.multivar
package contract

/** Equivalence group under which a fitted frame represents the same estimand.
  *
  * This is contract vocabulary, not an optimizer implementation detail: model
  * contracts declare it before any particular program is compiled.
  */
enum FrameSymmetry:
  case Orthogonal
  case SignedPermutation
  case Permutation
  case Identity

object FrameSymmetry:
  def meet(left: FrameSymmetry, right: FrameSymmetry): FrameSymmetry =
    (left, right) match
      case (FrameSymmetry.Identity, _) | (_, FrameSymmetry.Identity) => FrameSymmetry.Identity
      case (FrameSymmetry.Permutation, _) | (_, FrameSymmetry.Permutation) => FrameSymmetry.Permutation
      case (FrameSymmetry.SignedPermutation, _) | (_, FrameSymmetry.SignedPermutation) =>
        FrameSymmetry.SignedPermutation
      case _ => FrameSymmetry.Orthogonal
