package scalafim.image.world

/** The NIfTI xform code of the affine that was actually selected (sform or qform), shared with GIFTI coordinate-system
  * metadata. Codes follow the NIfTI-1 standard.
  */
enum XformCode derives CanEqual:
  case Unknown, ScannerAnatomical, AlignedAnatomical, Talairach, Mni152, TemplateOther

object XformCode:
  def fromNifti(code: Int): Either[SpaceError, XformCode] =
    code match
      case 0 => Right(Unknown)
      case 1 => Right(ScannerAnatomical)
      case 2 => Right(AlignedAnatomical)
      case 3 => Right(Talairach)
      case 4 => Right(Mni152)
      case 5 => Right(TemplateOther)
      case _ => Left(SpaceError.UnknownXformCode(code.toString))

  /** GIFTI `DataSpace`/`TransformedSpace` names, e.g. `NIFTI_XFORM_MNI_152`. */
  def fromGifti(name: String): Either[SpaceError, XformCode] =
    name.trim match
      case "NIFTI_XFORM_UNKNOWN"      => Right(Unknown)
      case "NIFTI_XFORM_SCANNER_ANAT" => Right(ScannerAnatomical)
      case "NIFTI_XFORM_ALIGNED_ANAT" => Right(AlignedAnatomical)
      case "NIFTI_XFORM_TALAIRACH"    => Right(Talairach)
      case "NIFTI_XFORM_MNI_152"      => Right(Mni152)
      case "NIFTI_XFORM_TEMPLATE_OTHER" => Right(TemplateOther)
      case other                      => Left(SpaceError.UnknownXformCode(other))

/** What anchors a subject-native space, when the caller knows it. */
final case class NativeContext(
    namespace: DatasetNamespace,
    subject: SubjectId,
    session: Option[SessionId],
    reference: ReferenceAcquisition
)

/** Everything a file says, or a caller asserts, about the world space its coordinates live in. */
final case class SpaceEvidence(
    xform: Option[XformCode] = None,
    bidsSpace: Option[String] = None,
    native: Option[NativeContext] = None,
    assertion: Option[WorldSpace] = None
)

/** Turns file evidence into a [[WorldSpace]]. Precedence: an explicit assertion, then the BIDS `space-` entity, then
  * the xform code of the selected affine. Contradictions and under-determined templates are typed failures, never
  * guesses; absence of any evidence is [[WorldSpace.Unresolved]].
  */
object SpaceResolver:
  /** BIDS `space-` labels that denote the subject's own (native) coordinates rather than a template. */
  val NativeBidsLabels: Set[String] = Set("T1w", "T2w", "orig", "individual", "scanner", "boldref", "func", "fsnative")

  def resolve(evidence: SpaceEvidence): Either[SpaceError, WorldSpace] =
    val fromBids = evidence.bidsSpace.map(label => bidsSpace(label.trim, evidence.native))
    val fromXform = evidence.xform.map(code => xformSpace(code, evidence.native))
    evidence.assertion match
      case Some(asserted) =>
        fromBids match
          case Some(Right(bids)) if bids != asserted && !bids.isInstanceOf[WorldSpace.SubjectNative] =>
            Left(SpaceError.ConflictingEvidence(asserted.displayName, s"BIDS space-${evidence.bidsSpace.getOrElse("")}"))
          case _ => Right(asserted)
      case None =>
        fromBids match
          case Some(result) =>
            result.flatMap(space => consistent(space, evidence.xform).map(_ => space))
          case None =>
            fromXform.getOrElse(Right(WorldSpace.Unresolved))

  private def bidsSpace(label: String, native: Option[NativeContext]): Either[SpaceError, WorldSpace] =
    if label.isEmpty then Left(SpaceError.EmptyIdentifier("BIDS space"))
    else if NativeBidsLabels.contains(label) then
      native
        .map(c => WorldSpace.SubjectNative(c.namespace, c.subject, c.session, c.reference))
        .toRight(SpaceError.MissingNativeContext(s"BIDS space-$label names subject-native coordinates"))
    else TemplateName(label).map(WorldSpace.Template(_))

  private def xformSpace(code: XformCode, native: Option[NativeContext]): Either[SpaceError, WorldSpace] =
    code match
      case XformCode.Mni152 =>
        Left(SpaceError.AmbiguousTemplate("NIFTI_XFORM_MNI_152 does not say which MNI152 template; supply the BIDS space- entity or an assertion"))
      case XformCode.Talairach =>
        Left(SpaceError.AmbiguousTemplate("NIFTI_XFORM_TALAIRACH does not name a template; supply an assertion"))
      case XformCode.TemplateOther =>
        Left(SpaceError.AmbiguousTemplate("NIFTI_XFORM_TEMPLATE_OTHER does not name a template; supply an assertion"))
      case XformCode.ScannerAnatomical | XformCode.AlignedAnatomical =>
        Right(native.fold(WorldSpace.Unresolved)(c => WorldSpace.SubjectNative(c.namespace, c.subject, c.session, c.reference)))
      case XformCode.Unknown =>
        Right(WorldSpace.Unresolved)

  /** A BIDS-named space must not contradict the file's own xform code. */
  private def consistent(space: WorldSpace, xform: Option[XformCode]): Either[SpaceError, Unit] =
    (space, xform) match
      case (WorldSpace.Template(name), Some(XformCode.ScannerAnatomical)) =>
        Left(SpaceError.ConflictingEvidence(s"BIDS space-${name.value}", "NIFTI_XFORM_SCANNER_ANAT"))
      case (WorldSpace.Template(name), Some(XformCode.Mni152)) if !name.value.startsWith("MNI152") =>
        Left(SpaceError.ConflictingEvidence(s"BIDS space-${name.value}", "NIFTI_XFORM_MNI_152"))
      case (WorldSpace.Template(name), Some(XformCode.Talairach)) if !(name.value == "MNI305" || name.value.contains("Talairach")) =>
        Left(SpaceError.ConflictingEvidence(s"BIDS space-${name.value}", "NIFTI_XFORM_TALAIRACH"))
      case (WorldSpace.SubjectNative(_, _, _, _), Some(code @ (XformCode.Mni152 | XformCode.Talairach))) =>
        Left(SpaceError.ConflictingEvidence("BIDS subject-native space", s"xform code $code"))
      case _ =>
        Right(())
