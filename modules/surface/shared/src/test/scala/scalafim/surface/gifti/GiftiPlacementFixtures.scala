package scalafim.surface.gifti

object GiftiPlacementFixtures:
  private val identityMatrix = Vector(1.0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)

  def transform(target: Option[String], offset: Double = 10.0): GiftiTransform =
    GiftiTransform(Some("NIFTI_XFORM_SCANNER_ANAT"), target,
      Vector(1.0, 0, 0, offset, 0, 2, 0, 20, 0, 0, 3, 30, 0, 0, 0, 1))
  val mni = transform(Some(GiftiTargetSpace.Mni152.code))
  val scanner = transform(Some(GiftiTargetSpace.ScannerAnatomical.code), 100.0)
  val unknown = transform(Some("NIFTI_XFORM_UNKNOWN"))
  val identityUnknown = unknown.copy(matrixData = identityMatrix)
  /** Workbench-style pair: an undeclared identity followed by a declared Talairach identity. */
  val blankIdentity = GiftiTransform(None, None, identityMatrix)
  val talairachIdentity = GiftiTransform(Some("NIFTI_XFORM_TALAIRACH"), Some("NIFTI_XFORM_TALAIRACH"), identityMatrix)

  def isIdentity(transform: GiftiTransform): Boolean = transform.matrixData == identityMatrix

  enum Expect:
    case Native
    case Placed(index: Int, target: GiftiTargetSpace)
    case Refused

  final case class Case(name: String, transforms: Vector[GiftiTransform], selection: GiftiTransformSelection,
    expected: Expect)
  private val auto = GiftiTransformSelection.Unambiguous
  private val mniTarget = GiftiTransformSelection.Target(GiftiTargetSpace.Mni152)
  import Expect.*
  val cases = Vector(
    Case("absent transform is explicitly native", Vector.empty, auto, Native),
    Case("single recognized target retains provenance", Vector(mni), auto, Placed(0, GiftiTargetSpace.Mni152)),
    Case("target is selected independently of first position", Vector(scanner, mni), mniTarget, Placed(1, GiftiTargetSpace.Mni152)),
    Case("permuted transforms retain target selection", Vector(mni, scanner), mniTarget, Placed(0, GiftiTargetSpace.Mni152)),
    Case("unknown preceding explicit target is not applied", Vector(unknown, mni), mniTarget, Placed(1, GiftiTargetSpace.Mni152)),
    Case("ambiguous automatic selection is refused", Vector(scanner, mni), auto, Refused),
    Case("same target different transforms is ambiguous", Vector(mni, mni.copy(matrixData = scanner.matrixData)), mniTarget, Refused),
    Case("explicit transform selection resolves ambiguity", Vector(mni, mni.copy(matrixData = scanner.matrixData)), GiftiTransformSelection.Transform(0), Placed(0, GiftiTargetSpace.Mni152)),
    Case("unknown non-identity target refused", Vector(unknown), auto, Refused),
    Case("missing non-identity target refused", Vector(transform(None)), auto, Refused),
    Case("unknown identity stays native and claims no frame", Vector(identityUnknown), auto, Native),
    Case("identity-only workbench pair stays native", Vector(blankIdentity, talairachIdentity), auto, Native),
    Case("identity-only workbench pair can select its declared target", Vector(blankIdentity, talairachIdentity),
      GiftiTransformSelection.Target(GiftiTargetSpace.Talairach), Placed(1, GiftiTargetSpace.Talairach)),
    Case("single declared identity retains provenance", Vector(talairachIdentity), auto, Placed(0, GiftiTargetSpace.Talairach)),
    Case("identity before a non-identity transform is not silently applied", Vector(identityUnknown, mni), auto, Refused),
    Case("explicit native retains unknown metadata", Vector(unknown), GiftiTransformSelection.NativeCoordinates, Native),
    Case("missing requested target refused", Vector(scanner), mniTarget, Refused),
    Case("negative index refused", Vector(mni), GiftiTransformSelection.Transform(-1), Refused),
    Case("out of range index refused", Vector(mni), GiftiTransformSelection.Transform(1), Refused),
    Case("explicit unknown target still refused", Vector(unknown), GiftiTransformSelection.Transform(0), Refused),
    Case("non-affine homogeneous matrix refused", Vector(mni.copy(matrixData = mni.matrixData.updated(12, 0.5))), auto, Refused)
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
