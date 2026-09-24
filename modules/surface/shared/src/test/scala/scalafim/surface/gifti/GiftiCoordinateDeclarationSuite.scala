package scalafim.surface.gifti

class GiftiCoordinateDeclarationSuite extends munit.FunSuite:

  private val eye = "1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1"
  private val shifted = "1 0 0 5 0 1 0 -3 0 0 1 2 0 0 0 1"

  private def system(dataSpace: Option[String], transformedSpace: Option[String], matrix: String): String =
    val data = dataSpace.fold("")(value => s"<DataSpace><![CDATA[$value]]></DataSpace>")
    val transformed = transformedSpace.fold("")(value => s"<TransformedSpace><![CDATA[$value]]></TransformedSpace>")
    s"<CoordinateSystemTransformMatrix>$data$transformed<MatrixData>$matrix</MatrixData></CoordinateSystemTransformMatrix>"

  private def md(entries: (String, String)*): String =
    if entries.isEmpty then ""
    else entries.map((name, value) => s"<MD><Name>$name</Name><Value>$value</Value></MD>").mkString("<MetaData>", "", "</MetaData>")

  private def document(pointSetMetadata: String, documentMetadata: String, systems: String*): GiftiDocument =
    val xml =
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<GIFTI Version="1.0" NumberOfDataArrays="2">$documentMetadata
         |<DataArray Intent="NIFTI_INTENT_POINTSET" DataType="NIFTI_TYPE_FLOAT32" ArrayIndexingOrder="RowMajorOrder"
         |  Dimensionality="2" Dim0="3" Dim1="3" Encoding="ASCII">$pointSetMetadata${systems.mkString}
         |<Data>0 0 0 1 0 0 0 1 0</Data></DataArray>
         |<DataArray Intent="NIFTI_INTENT_TRIANGLE" DataType="NIFTI_TYPE_INT32" ArrayIndexingOrder="RowMajorOrder"
         |  Dimensionality="2" Dim0="1" Dim1="3" Encoding="ASCII">
         |<Data>0 1 2</Data></DataArray>
         |</GIFTI>""".stripMargin
    GiftiXmlParser.parseString(xml).fold(error => fail(error.message), doc => doc)

  test("every coordinate system is retained in file order with its declared spaces and matrix"):
    val declaration = GiftiCoordinateDeclaration.fromDocument(document(
      md("GeometricType" -> "Anatomical", "AnatomicalStructurePrimary" -> "CortexLeft",
        "AnatomicalStructureSecondary" -> "MidThickness"),
      "",
      system(Some(""), Some(""), eye),
      system(Some("NIFTI_XFORM_TALAIRACH"), Some("NIFTI_XFORM_MNI_152"), shifted),
      system(Some("NIFTI_XFORM_SCANNER_ANAT"), Some("NIFTI_XFORM_ALIGNED_ANAT"), eye),
      system(Some("nifti_xform_unknown"), None, eye)
    )).toOption.get
    assertEquals(declaration.coordinateSystems.map(s => (s.dataSpace, s.transformedSpace)), Vector(
      (None, None),
      (Some(GiftiDeclaredSpace.Talairach), Some(GiftiDeclaredSpace.Mni152)),
      (Some(GiftiDeclaredSpace.ScannerAnatomical), Some(GiftiDeclaredSpace.AlignedAnatomical)),
      (Some(GiftiDeclaredSpace.Unknown), None)))
    assertEquals(declaration.declaredSpaces.length, 3)
    assertEquals(declaration.coordinateSystems(1).matrixRowMajor(3), 5.0)
    assertEquals(declaration.coordinateSystems(1).matrixRowMajor(7), -3.0)
    assertEqualsDouble(declaration.coordinateSystems(1).affine.toOption.get.matrix(2, 3), 2.0, 0.0)
    assertEquals(declaration.geometricType, Some(GiftiGeometricType.Anatomical))
    assertEquals(declaration.primaryStructure, Some(GiftiPrimaryStructure.CortexLeft))
    assertEquals(declaration.secondaryStructure, Some(GiftiSecondaryStructure.MidThickness))

  test("missing fields are None, not guessed"):
    val declaration = GiftiCoordinateDeclaration.fromDocument(document("", "")).toOption.get
    assertEquals(declaration, GiftiCoordinateDeclaration(Vector.empty, None, None, None))
    val bare = GiftiCoordinateDeclaration.fromDocument(document("", "", system(None, None, eye))).toOption.get
    assertEquals(bare.coordinateSystems.map(_.dataSpace), Vector(None))
    assertEquals(bare.declaredSpaces, Vector.empty)

  test("unknown strings are preserved verbatim"):
    val declaration = GiftiCoordinateDeclaration.fromDocument(document(
      md("GeometricType" -> "Wobbly", "AnatomicalStructurePrimary" -> "Thalamus",
        "AnatomicalStructureSecondary" -> "Layer4"),
      "",
      system(Some(" MNI152NLin6Asym "), Some("fsLR"), eye)
    )).toOption.get
    assertEquals(declaration.declaredSpaces,
      Vector((Some(GiftiDeclaredSpace.Other("MNI152NLin6Asym")), Some(GiftiDeclaredSpace.Other("fsLR")))))
    assertEquals(declaration.geometricType, Some(GiftiGeometricType.Other("Wobbly")))
    assertEquals(declaration.primaryStructure, Some(GiftiPrimaryStructure.Other("Thalamus")))
    assertEquals(declaration.secondaryStructure, Some(GiftiSecondaryStructure.Other("Layer4")))

  test("structure and type fall back to document metadata; pointset metadata wins"):
    val fallback = GiftiCoordinateDeclaration.fromDocument(document(
      md("GeometricType" -> "VeryInflated"),
      md("GeometricType" -> "Spherical", "AnatomicalStructurePrimary" -> "CortexRight")
    )).toOption.get
    assertEquals(fallback.geometricType, Some(GiftiGeometricType.VeryInflated))
    assertEquals(fallback.primaryStructure, Some(GiftiPrimaryStructure.CortexRight))
    assertEquals(fallback.secondaryStructure, None)

  test("a singular declared matrix is recorded; only its affine view fails"):
    val singular = "1 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1"
    val declaration = GiftiCoordinateDeclaration.fromDocument(document("", "",
      system(Some("NIFTI_XFORM_MNI_152"), Some("NIFTI_XFORM_MNI_152"), singular))).toOption.get
    assertEquals(declaration.coordinateSystems.head.transformedSpace, Some(GiftiDeclaredSpace.Mni152))
    assert(declaration.coordinateSystems.head.affine.isLeft)
