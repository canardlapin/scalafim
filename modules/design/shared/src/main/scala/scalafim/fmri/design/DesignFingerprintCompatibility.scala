package scalafim.fmri.design

/** String-only receipts establish a digest match, not collision-free identity.
  * A legacy-v2 match cannot verify ordinary source-event identity.
  */
enum SavedDesignFingerprintMatch:
  case CurrentV3DigestMatch
  case LegacyV2DigestMatch

enum SavedDesignFingerprintError:
  case MalformedValue
  case UnsupportedVersion(version: String)
  case Mismatch

  def message: String = this match
    case MalformedValue => "expected a versioned design fingerprint with a lowercase 64-bit hexadecimal digest"
    case UnsupportedVersion(version) => s"unsupported saved design fingerprint version: $version; rebuild from its saved model specification"
    case Mismatch => "saved design fingerprint does not match the current design or the admitted legacy projection"

/** Explicit receipt compatibility, separate from current schema validation,
  * fingerprint equality and coefficient-axis compatibility. No legacy digest
  * is silently promoted into a current fingerprint.
  */
object DesignFingerprintCompatibility:
  private val Value = "(design-schema/v[1-9][0-9]*):([0-9a-f]{1,16})".r

  def compareSavedValue(
      schema: DesignSchema,
      savedValue: String
  ): Either[SavedDesignFingerprintError, SavedDesignFingerprintMatch] =
    savedValue match
      case Value("design-schema/v3", _) =>
        Either.cond(savedValue == schema.fingerprint.value,
          SavedDesignFingerprintMatch.CurrentV3DigestMatch, SavedDesignFingerprintError.Mismatch)
      case Value("design-schema/v2", _) =>
        val legacy = DesignFingerprint.legacyV2Value(schema.matrix, schema.rows, schema.columns, schema.audit)
        Either.cond(savedValue == legacy,
          SavedDesignFingerprintMatch.LegacyV2DigestMatch, SavedDesignFingerprintError.Mismatch)
      case Value(version, _) => Left(SavedDesignFingerprintError.UnsupportedVersion(version))
      case _ => Left(SavedDesignFingerprintError.MalformedValue)
