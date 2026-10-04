package scalafim.surface.reference

import scalafim.surface.*
import scalafim.surface.io.GiftiSurfaceReader

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}

/** Loads a GIFTI surface under a frame declaration. The bytes are copied
  * once; the copy is hashed and checked against the declared asset digest
  * before that same copy is decoded, so later mutation of the caller's buffer
  * cannot change what was verified.
  */
object DeclaredSurfaceReader:
  def read(
    bytes: Uint8Array,
    declaration: FrameDeclaration,
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): Future[Either[ReferenceError, DeclaredSurface]] =
    val owned = new Int8Array(bytes.buffer, bytes.byteOffset, bytes.length).toArray
    declaration.checkDigest(owned) match
      case Left(error) => Future.successful(Left(error))
      case Right(()) =>
        val view = new Uint8Array(owned.length)
        var i = 0
        while i < owned.length do
          view(i) = (owned(i) & 0xff).toShort
          i += 1
        GiftiSurfaceReader.readDeclared(view, hemisphere, kind).map: decoded =>
          decoded.left.map(error => ReferenceError.AssetReadFailure(error.message))
            .flatMap(DeclaredSurface.verified(declaration, owned, _))
