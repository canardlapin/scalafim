package scalafim.surface.gifti

object GiftiPlacementFixtures:
  def transform(target: Option[String], offset: Double = 10.0): GiftiTransform =
    GiftiTransform(Some("NIFTI_XFORM_SCANNER_ANAT"), target,
      Vector(1.0, 0, 0, offset, 0, 2, 0, 20, 0, 0, 3, 30, 0, 0, 0, 1))
  val mni = transform(Some(GiftiTargetSpace.Mni152.code))
  val scanner = transform(Some(GiftiTargetSpace.ScannerAnatomical.code), 100.0)
  val unknown = transform(Some("NIFTI_XFORM_UNKNOWN"))
  val identityUnknown = unknown.copy(matrixData = Vector(1.0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))
  final case class Case(name: String, transforms: Vector[GiftiTransform], selection: GiftiTransformSelection,
    expected: Option[GiftiPlacement])
  private val auto = GiftiTransformSelection.Unambiguous
  private val mniTarget = GiftiTransformSelection.Target(GiftiTargetSpace.Mni152)
  val cases = Vector(
    Case("absent transform is explicitly native", Vector.empty, auto, Some(GiftiPlacement.NativeCoordinates)),
    Case("single recognized target retains provenance", Vector(mni), auto, Some(GiftiPlacement.Transformed(mni, GiftiTargetSpace.Mni152))),
    Case("target is selected independently of first position", Vector(scanner, mni), mniTarget, Some(GiftiPlacement.Transformed(mni, GiftiTargetSpace.Mni152))),
    Case("permuted transforms retain target selection", Vector(mni, scanner), mniTarget, Some(GiftiPlacement.Transformed(mni, GiftiTargetSpace.Mni152))),
    Case("unknown preceding explicit target is not applied", Vector(unknown, mni), mniTarget, Some(GiftiPlacement.Transformed(mni, GiftiTargetSpace.Mni152))),
    Case("ambiguous automatic selection is refused", Vector(scanner, mni), auto, None),
    Case("same target different transforms is ambiguous", Vector(mni, mni.copy(matrixData = scanner.matrixData)), mniTarget, None),
    Case("explicit transform selection resolves ambiguity", Vector(mni, mni.copy(matrixData = scanner.matrixData)), GiftiTransformSelection.Transform(0), Some(GiftiPlacement.Transformed(mni, GiftiTargetSpace.Mni152))),
    Case("unknown target refused", Vector(unknown), auto, None),
    Case("missing target refused", Vector(transform(None)), auto, None),
    Case("unknown identity does not prove a frame", Vector(identityUnknown), auto, None),
    Case("explicit native retains unknown metadata", Vector(unknown), GiftiTransformSelection.NativeCoordinates, Some(GiftiPlacement.NativeCoordinates)),
    Case("missing requested target refused", Vector(scanner), mniTarget, None),
    Case("negative index refused", Vector(mni), GiftiTransformSelection.Transform(-1), None),
    Case("out of range index refused", Vector(mni), GiftiTransformSelection.Transform(1), None),
    Case("explicit unknown target still refused", Vector(unknown), GiftiTransformSelection.Transform(0), None),
    Case("non-affine homogeneous matrix refused", Vector(mni.copy(matrixData = mni.matrixData.updated(12, 0.5))), auto, None)
  )

  def xml(transforms: Vector[GiftiTransform]): String =
    val metadata = transforms.map { transform =>
      "<CoordinateSystemTransformMatrix>" +
        transform.dataSpace.map(s => s"<DataSpace>$s</DataSpace>").getOrElse("") +
        transform.transformedSpace.map(s => s"<TransformedSpace>$s</TransformedSpace>").getOrElse("") +
        s"<MatrixData>${transform.matrixData.mkString(" ")}</MatrixData></CoordinateSystemTransformMatrix>"
    }.mkString
    s"""<GIFTI Version="1.0" NumberOfDataArrays="2">
       |<DataArray Intent="NIFTI_INTENT_POINTSET" DataType="NIFTI_TYPE_FLOAT64"
       |ArrayIndexingOrder="RowMajorOrder" Dimensionality="2" Dim0="3" Dim1="3" Encoding="ASCII">
       |$metadata<Data>1.0000000000000002 2 3 4 5 6 7 8 9</Data></DataArray>
       |<DataArray Intent="NIFTI_INTENT_TRIANGLE" DataType="NIFTI_TYPE_INT32"
       |ArrayIndexingOrder="RowMajorOrder" Dimensionality="2" Dim0="1" Dim1="3" Encoding="ASCII">
       |<Data>0 1 2</Data></DataArray></GIFTI>""".stripMargin
