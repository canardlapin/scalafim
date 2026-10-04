package scalafim.atlas

import scalafim.image.world.XformCode

enum AtlasCoordinateAdmission:
  /** Header evidence must be consistent with the source-declared template. */
  case RequireTemplateHeader
  /** The header identifies no specific subject/template frame. Bind coordinates
    * to this immutable artifact only; source template is descriptive provenance,
    * and the result does not acquire a standard-template routing frame.
    */
  case DeclaredArtifactCoordinates(expectedXform: XformCode, reason: String)

/** Executable asset specification: immutable release/revision and mandatory
  * expected bytes. Acquisition is interpreted only on the JVM.
  */
final case class PinnedAtlasAsset(
    key: String, fileName: String, url: String, revision: String,
    sha256: String, minBytes: Long = 1L
):
  require(key.trim.nonEmpty && revision.trim.nonEmpty, "asset key and immutable revision must be non-empty")
  require(fileName.nonEmpty && !fileName.contains('/') && !fileName.contains('\\') &&
    fileName != "." && fileName != "..", "asset cache file name must be a single safe component")
  require(url.startsWith("https://") && !Vector("/master/", "/main/", "/HEAD/", "/latest/").exists(url.contains),
    "pinned asset requires an HTTPS immutable source URL")
  require(sha256.matches("[0-9a-f]{64}"), "asset requires a lowercase SHA-256 digest")
  require(minBytes >= 1L, "asset must contain bytes")

enum AtlasAcquisitionError:
  case Integrity(asset: String, expected: String, actual: String)
  case Parse(detail: String)
  case Io(detail: String)
  case Coverage(unknown: Vector[Int])
  case EmptyCoverage
  case Realization(error: AtlasRealizationError)

  def message: String = this match
    case Integrity(asset, expected, actual) => s"asset $asset SHA-256 mismatch: expected $expected, found $actual"
    case Parse(detail) => s"atlas metadata parse failed: $detail"
    case Io(detail) => s"atlas acquisition failed: $detail"
    case Coverage(unknown) => s"atlas payload contains undeclared labels: ${unknown.mkString(",")}"
    case EmptyCoverage => "atlas payload contains no parcels"
    case Realization(error) => error.message
