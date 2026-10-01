package scalafim.surface.reference

import scalafim.image.SomeScalarVolume

/** A source volume whose template frame is bound to the exact bytes it was
  * decoded from. Grid identity alone cannot say which template a volume is
  * in: a `MNI152NLin6Asym` volume can sit on a grid identical to a
  * `MNI152NLin2009cAsym` one. Routes therefore execute only on declared
  * volumes. The JVM `DeclaredVolumeReader` hashes the file bytes, checks them
  * against the declared digest, and decodes those same bytes; there is no
  * public constructor.
  */
final class DeclaredVolume private (val declaration: FrameDeclaration, val volume: SomeScalarVolume[Double]):
  def frame: TemplateFrame = declaration.frame

  override def toString: String = s"DeclaredVolume(${declaration.display})"

object DeclaredVolume:
  /** Bind a volume decoded from exactly `bytes` to its declaration. */
  private[reference] def verified(
    declaration: FrameDeclaration,
    bytes: Array[Byte],
    volume: SomeScalarVolume[Double]
  ): Either[ReferenceError, DeclaredVolume] =
    declaration.checkDigest(bytes).map(_ => DeclaredVolume(declaration, volume))

  /** Test-only escape hatch: binds a declaration without seeing any bytes. */
  private[reference] def unsafeAssumeVerified(declaration: FrameDeclaration, volume: SomeScalarVolume[Double]): DeclaredVolume =
    DeclaredVolume(declaration, volume)
