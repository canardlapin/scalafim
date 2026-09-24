package scalafim.image.world

import scala.util.Random

/** Scope for subject-level identity: two datasets that both contain `sub-01` must not collide.
  *
  * Use the BIDS `DatasetDOI` when present, else a content hash of `dataset_description.json`, else an
  * explicit label chosen by the caller.
  */
opaque type DatasetNamespace = String

object DatasetNamespace:
  def apply(value: String): Either[SpaceError, DatasetNamespace] =
    SpaceIdentifier.normalize("dataset namespace", value)

  extension (namespace: DatasetNamespace)
    inline def value: String = namespace

/** Exact geometry of the acquisition that anchors a native space: dimensions, the selected voxel-to-world affine,
  * and the NIfTI xform codes it came from. Stored losslessly (raw IEEE-754 bits), so equal digests mean identical
  * geometry rather than a probable hash match.
  */
final case class GeometryDigest private (canonical: String) derives CanEqual

object GeometryDigest:
  def apply(
      dims: Vector[Int],
      voxelToWorldRowMajor: Vector[Double],
      qformCode: Int,
      sformCode: Int
  ): Either[SpaceError, GeometryDigest] =
    if dims.size != 3 || dims.exists(_ <= 0) then Left(SpaceError.InvalidGeometry(s"dims must be three positive extents, got $dims"))
    else if voxelToWorldRowMajor.size != 16 && voxelToWorldRowMajor.size != 12 then
      Left(SpaceError.InvalidGeometry(s"voxel-to-world affine needs 12 or 16 row-major values, got ${voxelToWorldRowMajor.size}"))
    else if voxelToWorldRowMajor.exists(v => v.isNaN || v.isInfinite) then
      Left(SpaceError.InvalidGeometry("voxel-to-world affine must be finite"))
    else
      val bits = voxelToWorldRowMajor.take(12).map(v => java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(v + 0.0)))
      Right(GeometryDigest(s"${dims.mkString("x")};q$qformCode;s$sformCode;${bits.mkString(",")}"))

  private[world] def fromCanonical(canonical: String): GeometryDigest =
    GeometryDigest(canonical)

/** The acquisition that anchors a native world space. Distinct boldrefs within one session are distinct spaces until
  * an identity-certified coregistration says otherwise.
  *
  * @param entities
  *   BIDS entities other than subject/session that distinguish the acquisition (task, run, acq, echo, ...)
  */
final case class ReferenceAcquisition(entities: Map[String, String], geometry: GeometryDigest) derives CanEqual:
  require(entities.forall((k, v) => k.trim.nonEmpty && v.trim.nonEmpty), "BIDS entities must be non-empty")

  private[world] def canonical: String =
    entities.toVector
      .sortBy(_._1)
      .map((k, v) => s"${ReferenceAcquisition.strict(k)}-${ReferenceAcquisition.strict(v)}")
      .mkString("_") + "|" + geometry.canonical

object ReferenceAcquisition:
  /** Escape everything but ASCII alphanumerics and '.', so '-', '_' and '|' stay structural. */
  private[world] def strict(text: String): String =
    text.flatMap: c =>
      if c.isLetterOrDigit && c < 128 || c == '.' then c.toString else f"%%${c.toInt}%04X"

  private[world] def unstrict(text: String): Either[SpaceError, String] =
    WorldSpace.unescapeComponent(text)

/** A freshly minted identity for a space declared by the caller; the label never participates in equality. */
final case class DeclaredToken private (value: String) derives CanEqual

object DeclaredToken:
  private val sessionNonce: String = java.lang.Long.toHexString(Random.nextLong())
  private var counter: Long = 0L

  private[world] def fresh(): DeclaredToken =
    synchronized:
      counter += 1
      DeclaredToken(s"$sessionNonce-$counter")

  private[world] def restored(value: String): Either[SpaceError, DeclaredToken] =
    SpaceIdentifier.normalize("declared token", value).map(DeclaredToken(_))

/** A continuous RAS-millimetre coordinate system: the identity that world frames carry.
  *
  * A `WorldSpace` is not a sampled domain. The T1w grid, a coregistered BOLD grid, and white/pial meshes of one subject
  * can all live in one world space; `scalafim.spatial.SpaceRef` names the sampled domains themselves.
  */
enum WorldSpace derives CanEqual:
  /** A standard template; globally unique by name. */
  case Template(name: TemplateName)

  /** Subject scanner-RAS anchored by one reference acquisition, scoped by dataset. */
  case SubjectNative(
      namespace: DatasetNamespace,
      subject: SubjectId,
      session: Option[SessionId],
      reference: ReferenceAcquisition
  )

  /** FreeSurfer surface RAS (tkRAS) of one subject's conformed volume. Not per hemisphere. */
  case SubjectTkRas(namespace: DatasetNamespace, subject: SubjectId, conformed: ReferenceAcquisition)

  /** A caller-declared space. Fresh by construction; the label is display-only. */
  case Declared(token: DeclaredToken, label: String)

  /** No identity evidence. Every unresolved RAS-mm space shares this identity, which preserves the historical
    * behaviour of independently loaded volumes aligning with one another. It is an explicit admission that the
    * space is unknown, not evidence that two such spaces coincide.
    */
  case Unresolved

  def displayName: String =
    this match
      case Template(name)                       => name.value
      case SubjectNative(ns, sub, ses, _)       => s"${ns.value}/${sub.value}${ses.fold("")(s => s"/${s.value}")}/native"
      case SubjectTkRas(ns, sub, _)             => s"${ns.value}/${sub.value}/tkRAS"
      case Declared(_, label)                   => label
      case Unresolved                           => "unresolved RAS"

object WorldSpace:
  /** Declare a new space. Two declarations never share an identity, whatever their labels. */
  def declare(label: String): Either[SpaceError, WorldSpace] =
    SpaceIdentifier.normalize("declared space label", label).map(l => Declared(DeclaredToken.fresh(), l))

  def template(name: String): Either[SpaceError, WorldSpace] =
    TemplateName(name).map(Template(_))

  /** Persistent identifier text. Components are percent-encoded, so no two distinct spaces encode alike. */
  def encode(space: WorldSpace): String =
    space match
      case Template(name) =>
        s"scalafim-world:template:${escape(name.value)}"
      case SubjectNative(ns, sub, ses, ref) =>
        s"scalafim-world:native:${escape(ns.value)}:${escape(sub.value)}:${ses.fold("")(s => escape(s.value))}:${escape(ref.canonical)}"
      case SubjectTkRas(ns, sub, ref) =>
        s"scalafim-world:tkras:${escape(ns.value)}:${escape(sub.value)}:${escape(ref.canonical)}"
      case Declared(token, label) =>
        s"scalafim-world:declared:${escape(token.value)}:${escape(label)}"
      case Unresolved =>
        UnresolvedId

  /** Inverse of [[encode]]; used when restoring persisted frame records. */
  def decode(text: String): Either[SpaceError, WorldSpace] =
    if text == UnresolvedId then Right(Unresolved)
    else
      text.split(":", -1).toList match
        case "scalafim-world" :: "template" :: name :: Nil =>
          unescape(name).flatMap(TemplateName(_)).map(Template(_))
        case "scalafim-world" :: "native" :: ns :: sub :: ses :: ref :: Nil =>
          for
            namespace <- unescape(ns).flatMap(DatasetNamespace(_))
            subject   <- unescape(sub).flatMap(SubjectId(_))
            session   <- if ses.isEmpty then Right(None) else unescape(ses).flatMap(SessionId(_)).map(Some(_))
            reference <- unescape(ref).flatMap(decodeReference)
          yield SubjectNative(namespace, subject, session, reference)
        case "scalafim-world" :: "tkras" :: ns :: sub :: ref :: Nil =>
          for
            namespace <- unescape(ns).flatMap(DatasetNamespace(_))
            subject   <- unescape(sub).flatMap(SubjectId(_))
            reference <- unescape(ref).flatMap(decodeReference)
          yield SubjectTkRas(namespace, subject, reference)
        case "scalafim-world" :: "declared" :: token :: label :: Nil =>
          for
            t <- unescape(token).flatMap(DeclaredToken.restored)
            l <- unescape(label)
          yield Declared(t, l)
        case _ =>
          Left(SpaceError.UnrecognisedWorldSpaceId(text))

  /** The historical shared RAS-mm D3 frame id, kept byte-identical so persisted records still restore. */
  private[world] val UnresolvedId = "scalafim-ras-d3"

  private def decodeReference(canonical: String): Either[SpaceError, ReferenceAcquisition] =
    canonical.split("\\|", 2).toList match
      case entityText :: geometry :: Nil =>
        val entities =
          if entityText.isEmpty then Right(Map.empty[String, String])
          else
            entityText
              .split("_")
              .toVector
              .map(_.split("-", 2).toList)
              .foldLeft[Either[SpaceError, Map[String, String]]](Right(Map.empty)):
                case (Right(acc), k :: v :: Nil) =>
                  for
                    key   <- ReferenceAcquisition.unstrict(k)
                    value <- ReferenceAcquisition.unstrict(v)
                  yield acc.updated(key, value)
                case (Right(_), other)           => Left(SpaceError.UnrecognisedWorldSpaceId(other.mkString("-")))
                case (left, _)                   => left
        entities.map(ReferenceAcquisition(_, GeometryDigest.fromCanonical(geometry)))
      case _ =>
        Left(SpaceError.UnrecognisedWorldSpaceId(canonical))

  private def escape(component: String): String =
    val out = new StringBuilder
    component.foreach: c =>
      if c.isLetterOrDigit && c < 128 || c == '.' || c == '-' || c == '_' then out.append(c)
      else out.append(f"%%${c.toInt}%04X")
    out.toString

  private[world] def unescapeComponent(component: String): Either[SpaceError, String] =
    unescape(component)

  private def unescape(component: String): Either[SpaceError, String] =
    val out = new StringBuilder
    var i = 0
    var failed = false
    while i < component.length && !failed do
      val c = component.charAt(i)
      if c == '%' then
        if i + 5 <= component.length then
          val hex = component.substring(i + 1, i + 5)
          if hex.forall(h => Character.digit(h, 16) >= 0) then
            out.append(Integer.parseInt(hex, 16).toChar)
            i += 5
          else failed = true
        else failed = true
      else
        out.append(c)
        i += 1
    if failed then Left(SpaceError.UnrecognisedWorldSpaceId(component)) else Right(out.toString)
