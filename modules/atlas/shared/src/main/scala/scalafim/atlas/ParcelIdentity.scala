package scalafim.atlas

/** How source integer labels acquire persistent parcel identity.
  *
  * SharedRegionIds is an explicit assertion that an atlas variant uses one
  * common numeric encoding across representations. SourceLabels makes no
  * such assertion. Glasser identity comes from checked anatomical names.
  */
enum ParcelIdentity:
  case SourceLabels, SharedRegionIds, GlasserHcpMmp1

enum ParcelIdentityError:
  case IncompatibleGlasserAtlas(family: String, model: String)
  case MissingGlasserKey(id: RegionId)
  case InvalidGlasserKey(value: String)
  case ConflictingHemisphere(id: RegionId, key: GlasserParcelKey, declared: Hemisphere)
  case ConflictingSurfaceHemisphere(id: RegionId, key: GlasserParcelKey, actual: Hemisphere)
  case DuplicateGlasserKeys(keys: Vector[GlasserParcelKey])

  def message: String =
    this match
      case IncompatibleGlasserAtlas(family, model) =>
        s"Glasser parcel keys require glasser:HCP-MMP1.0, got $family:$model"
      case MissingGlasserKey(id) =>
        s"Glasser source label $id requires a full anatomical label"
      case InvalidGlasserKey(value) =>
        s"invalid Glasser parcel key '$value'; expected L_<area>_ROI or R_<area>_ROI"
      case ConflictingHemisphere(id, key, declared) =>
        s"Glasser source label $id declares $declared but its key is ${key.value}"
      case ConflictingSurfaceHemisphere(id, key, actual) =>
        s"Glasser source label $id with key ${key.value} occurs on the $actual surface"
      case DuplicateGlasserKeys(keys) =>
        s"duplicate Glasser parcel keys: ${keys.map(_.value).mkString(", ")}"

/** Complete hemisphere-qualified HCP-MMP name, independent of label number. */
opaque type GlasserParcelKey = String

object GlasserParcelKey:
  def from(value: String): Either[ParcelIdentityError, GlasserParcelKey] =
    if value.matches("[LR]_[A-Za-z0-9][A-Za-z0-9-]*_ROI") then Right(value)
    else Left(ParcelIdentityError.InvalidGlasserKey(value))

  /** xcpEngine node names use Left_<area>/Right_<area>; surface tables use
    * the complete L_<area>_ROI/R_<area>_ROI name. Preserve the area exactly.
    */
  def fromSourceLabel(value: String): Either[ParcelIdentityError, GlasserParcelKey] =
    val separator = value.indexOf('_')
    val prefix = value.take(separator).toLowerCase
    val area = value.drop(separator + 1).stripSuffix("_ROI")
    val canonical =
      prefix match
        case "l" | "lh" | "left" => s"L_${area}_ROI"
        case "r" | "rh" | "right" => s"R_${area}_ROI"
        case _ => value
    from(canonical)

  extension (key: GlasserParcelKey)
    inline def value: String = key

    def area: String = key.substring(2, key.length - 4)

    def hemisphere: Hemisphere =
      if key.startsWith("L_") then Hemisphere.Left else Hemisphere.Right

  private[atlas] def fromRegion(
      region: AtlasRegionMetadata
  ): Either[ParcelIdentityError, GlasserParcelKey] =
    for
      fullLabel <- region.labelFull.toRight(ParcelIdentityError.MissingGlasserKey(region.id))
      key <- from(fullLabel.value)
      _ <- region.hemisphere match
        case Some(declared) if declared != key.hemisphere =>
          Left(ParcelIdentityError.ConflictingHemisphere(region.id, key, declared))
        case _ => Right(())
    yield key
