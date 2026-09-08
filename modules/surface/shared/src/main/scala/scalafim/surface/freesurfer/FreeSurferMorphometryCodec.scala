package scalafim.surface.freesurfer

import scalafim.surface.*

/** Scalar FreeSurfer morphometry (.curv, .sulc, .thickness): modern big-endian
  * float32 and legacy signed int16 / 100. Binding checks counts, not anatomical
  * correspondence: callers must identify the matching mesh and vertex ordering.
  */
object FreeSurferMorphometryCodec:
  def decode(bytes: Array[Byte], geometry: SurfaceGeometry,
      label: String = ""): Either[SurfaceError, SurfaceField[Double]] =
    def invalid(reason: String) = Left(SurfaceError.InvalidField(s"FreeSurfer morphometry: $reason"))
    def u24(offset: Int): Int =
      ((bytes(offset) & 255) << 16) | ((bytes(offset + 1) & 255) << 8) | (bytes(offset + 2) & 255)
    def i32(offset: Int): Int =
      (bytes(offset).toInt << 24) | u24(offset + 1)
    if bytes.length < 3 then invalid("missing header")
    else
      val modern = u24(0) == 0xffffff
      val headerSize = if modern then 15 else 6
      if bytes.length < headerSize then invalid("truncated counts")
      else
        val vertices = if modern then i32(3) else u24(0)
        val faces = if modern then i32(7) else u24(3)
        val components = if modern then i32(11) else 1
        val width = if modern then 4 else 2
        if vertices <= 0 || vertices != geometry.vertexCount then invalid("vertex count differs from supplied geometry")
        else if faces < 0 || (faces != 0 && faces != geometry.faceCount) then invalid("face count differs from supplied geometry")
        else if components != 1 then invalid("expected one scalar per vertex")
        else if headerSize.toLong + vertices.toLong * width != bytes.length.toLong then invalid("payload length differs from declared scalar count")
        else
          val values = new Array[Double](vertices)
          var index = 0
          while index < vertices do
            val offset = headerSize + index * width
            values(index) =
              if modern then java.lang.Float.intBitsToFloat(i32(offset)).toDouble
              else (((bytes(offset).toInt << 8) | (bytes(offset + 1) & 255)).toShort.toDouble / 100.0)
            index += 1
          SurfaceField.fullEither(geometry, values.toIndexedSeq, label)
