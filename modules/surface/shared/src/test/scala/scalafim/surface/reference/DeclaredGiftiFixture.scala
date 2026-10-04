package scalafim.surface.reference

/** A small GIFTI midthickness with Workbench-style coordinate metadata. */
object DeclaredGiftiFixture:
  def xml(primary: String = "CortexLeft", secondary: String = "MidThickness", geometricType: String = "Anatomical"): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<GIFTI Version="1.0" NumberOfDataArrays="2">
       |<DataArray Intent="NIFTI_INTENT_POINTSET" DataType="NIFTI_TYPE_FLOAT32" ArrayIndexingOrder="RowMajorOrder"
       |  Dimensionality="2" Dim0="3" Dim1="3" Encoding="ASCII" Endian="LittleEndian">
       |<MetaData>
       |<MD><Name>AnatomicalStructurePrimary</Name><Value>$primary</Value></MD>
       |<MD><Name>AnatomicalStructureSecondary</Name><Value>$secondary</Value></MD>
       |<MD><Name>GeometricType</Name><Value>$geometricType</Value></MD>
       |</MetaData>
       |<CoordinateSystemTransformMatrix><DataSpace></DataSpace><TransformedSpace></TransformedSpace>
       |<MatrixData>1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1</MatrixData></CoordinateSystemTransformMatrix>
       |<CoordinateSystemTransformMatrix><DataSpace>NIFTI_XFORM_TALAIRACH</DataSpace>
       |<TransformedSpace>NIFTI_XFORM_TALAIRACH</TransformedSpace>
       |<MatrixData>1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1</MatrixData></CoordinateSystemTransformMatrix>
       |<Data>0 0 0 1 0 0 0 1 0</Data></DataArray>
       |<DataArray Intent="NIFTI_INTENT_TRIANGLE" DataType="NIFTI_TYPE_INT32" ArrayIndexingOrder="RowMajorOrder"
       |  Dimensionality="2" Dim0="1" Dim1="3" Encoding="ASCII" Endian="LittleEndian">
       |<Data>0 1 2</Data></DataArray>
       |</GIFTI>
       |""".stripMargin

  def bytes(xml: String): Array[Byte] = xml.getBytes("UTF-8")

  def declaration(frame: TemplateFrame, bytes: Array[Byte], name: String = "tpl-fsLR/fixture_midthickness.surf.gii"): FrameDeclaration =
    FrameDeclaration.make(
      frame,
      FrameBasis.literature("10.1093/cercor/bhr291", "fixture declared in this frame").toOption.get,
      AssetProvenance.make(TemplateId.unsafe("fsLR"), name, "fixture", AssetSha256.of(bytes).value).toOption.get
    ).toOption.get
