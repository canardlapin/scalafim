package scalafim.surface.gifti

/** Independent fixture bytes: Python struct.pack("<3d"/">3d", ...),
  * base64.b64encode, gzip.compress(mtime=0) and zlib.compress.
  * Values deliberately retain information lost by Float32 conversion.
  */
object GiftiFloat64Fixtures:
  val expected = Vector(1.0000000000000002, -16777217.0, 3.141592653589793)
  val cases: Vector[(String, String, String)] = Vector(
    ("ASCII", "LittleEndian", "1.0000000000000002 -16777217 3.141592653589793"),
    ("Base64Binary", "LittleEndian", "AQAAAAAA8D8AAAAQAABwwRgtRFT7IQlA"),
    ("GZipBase64Binary", "LittleEndian", "H4sIAAAAAAAC/2NkAIEP9kBCgIGh4KCErkvIb0VOBwDOZf3AGAAAAA=="),
    ("GZipBase64Binary", "LittleEndian", "eJxjZACBD/ZAQoCBoeCghK5LyG9FTgcAKxAEtA=="),
    ("Base64Binary", "BigEndian", "P/AAAAAAAAHBcAAAEAAAAEAJIftURC0Y"),
    ("GZipBase64Binary", "BigEndian", "H4sIAAAAAAAC/7P/wAACjAcLGBgEgAwHTsXfIS66EgAX2ALSGAAAAA=="),
    ("GZipBase64Binary", "BigEndian", "eJyz/8AAAowHCxgYBIAMB07F3yEuuhIAOXsEtA==")
  )

  def xml(encoding: String, endian: String, data: String): String =
    s"""<GIFTI Version="1.0" NumberOfDataArrays="1">
       |<DataArray Intent="NIFTI_INTENT_SHAPE" DataType="NIFTI_TYPE_FLOAT64"
       |ArrayIndexingOrder="RowMajorOrder" Dimensionality="1" Dim0="3"
       |Encoding="$encoding" Endian="$endian"><Data>$data</Data></DataArray></GIFTI>""".stripMargin
