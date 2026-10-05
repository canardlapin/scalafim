package scalafim.phrfcmp.ingest

/** Dense row-major matrix of doubles. Construction validates `data.length == rows * cols`. */
final case class Matrix private (rows: Int, cols: Int, data: Array[Double]):
  inline def apply(r: Int, c: Int): Double = data(r * cols + c)

object Matrix:
  def of(rows: Int, cols: Int, data: Array[Double]): Either[IngestRefusal, Matrix] =
    if rows >= 0 && cols >= 0 && rows.toLong * cols.toLong == data.length.toLong then Right(new Matrix(rows, cols, data))
    else Left(IngestRefusal.Inconsistent(s"matrix ${rows}x$cols does not match ${data.length} values"))

/**
  * Everything a fitter may see. Truth-bearing arrays are deliberately not representable here: they live only in
  * [[ScoreTruth]], so tuning code that receives a `FitInputs` cannot reach them.
  *
  * Contract for callers: tuning and fitting code must be handed a `FitInputs` only, never the enclosing
  * [[BoundDataset]] (which also carries the [[ScoreTruth]]). Only the scorer may hold a `ScoreTruth`.
  *
  * `yPool` is `None` for condition cells and a (possibly zero-row) matrix for trial cells.
  */
final case class FitInputs private[ingest] (
    y: Matrix,
    yPool: Option[Matrix],
    nuisance: Matrix,
    sampleTime: Array[Double],
    runId: Array[Int],
    evOnset: Array[Double],
    evCond: Array[Int],
    evStim: Array[Int],
    evRun: Array[Int],
    evDuration: Array[Double]
)

/** Generator truth, consumed by the scorer only. Trial-only arrays are `None` for condition cells. */
final case class ScoreTruth private[ingest] (
    signal: Matrix,
    evOnsetTrue: Array[Double],
    truthT: Array[Double],
    truthKernel: Matrix,
    truthParams: Matrix,
    condCoef: Matrix,
    condPeakAmp: Matrix,
    signalScale: Array[Double],
    nuisanceCoef: Matrix,
    nuisanceCoefPool: Option[Matrix],
    trialBeta: Option[Matrix],
    signalConditionMean: Option[Matrix]
)

enum RootKind:
  case Harness, Pilot, Confirmatory

object RootKind:
  def parse(s: String): Option[RootKind] = s match
    case "harness"      => Some(Harness)
    case "pilot"        => Some(Pilot)
    case "confirmatory" => Some(Confirmatory)
    case _              => None

enum CellKind:
  case Condition, Trial

/** The frozen (v0 `cells.json`) specification a dataset must realise. */
final case class CellSpec(
    cellId: String,
    kind: CellKind,
    nVoxels: Int,
    nPool: Int,
    tr: Double,
    trAlignedOnsets: Boolean
)

/**
  * Values supplied by the caller (frozen in v0), never read from the manifest being validated. The stream seeds are
  * re-derived from `rootHex`, `cell.cellId` and `dataset`, and the denylist is pinned here rather than trusted.
  */
final case class IngestExpectation(
    rootKind: RootKind,
    rootHex: String,
    generatorCodeSha256: String,
    cell: CellSpec,
    dataset: Int,
    denylist: Set[Long]
)

final case class ArrayEntry(dtype: String, shape: Vector[Int], npySha256: String)

/** The generator per-dataset manifest (schema `phrf-gen-npz-1`), reduced to the fields S1 binds. */
final case class Manifest(
    schema: String,
    generatorCodeSha256: String,
    rootKind: RootKind,
    rootHex: String,
    rootDenylisted: Boolean,
    denylist: Set[Long],
    dataset: Int,
    streams: Map[String, String],
    streamCheckSeeds: Map[String, String],
    streamHitFlags: Map[String, Boolean],
    file: String,
    npzSha256: String,
    arrays: Map[String, ArrayEntry],
    cell: CellSpec,
    nSamples: Int,
    nEvents: Int
)

/** A dataset that passed every binding check. `inputSha256` is the verified whole-file hash. */
final case class BoundDataset(
    manifest: Manifest,
    inputSha256: String,
    arrayHashes: Map[String, String],
    fit: FitInputs,
    truth: ScoreTruth
)

/** Typed reason ingestion refused; no exception crosses this boundary. */
enum IngestRefusal:
  case Npz(error: NpzError)
  case ManifestParse(detail: String)
  case SchemaMismatch(found: String)
  case RootKindMismatch(expected: RootKind, found: String)
  case RootMismatch(expected: String, found: String)
  case RootDenylisted
  case StreamDenylisted(purpose: String)
  case DenylistMismatch(expected: Set[Long], found: Set[Long])
  case SeedMismatch(purpose: String, expected: String, found: String)
  case HitFlagMismatch(purpose: String, flag: Boolean, recomputed: Boolean)
  case GeneratorCodeMismatch(expected: String, found: String)
  case NpzHashMismatch(expected: String, actual: String)
  case ArrayHashMismatch(name: String, expected: String, actual: String)
  case ArrayMissing(name: String)
  case ArrayNotInManifest(name: String)
  case DtypeMismatch(name: String, manifest: String, actual: String)
  case ShapeMismatch(name: String, manifest: Vector[Int], actual: Vector[Int])
  case CellMismatch(field: String, expected: String, found: String)
  case Inconsistent(detail: String)
  case Unreadable(path: String, detail: String)

  def message: String = this match
    case Npz(e)                    => e.message
    case ManifestParse(d)          => s"manifest unreadable: $d"
    case SchemaMismatch(f)         => s"manifest schema '$f' is not phrf-gen-npz-1"
    case RootKindMismatch(e, f)    => s"root_kind '$f' but expected $e"
    case RootMismatch(e, f)        => s"root_hex $f but expected $e"
    case RootDenylisted            => "root is denylisted"
    case StreamDenylisted(p)       => s"stream '$p' hits the denylist"
    case DenylistMismatch(e, f)    => s"manifest denylist ${f.toVector.sorted} differs from the frozen ${e.toVector.sorted}"
    case SeedMismatch(p, e, f)     => s"stream '$p' seed $f but the derived seed is $e"
    case HitFlagMismatch(p, f, r)  => s"stream '$p' hit flag is $f but recomputation gives $r"
    case GeneratorCodeMismatch(e, f) => s"generator_code_sha256 $f but frozen value is $e"
    case NpzHashMismatch(e, a)     => s"npz sha256 $a does not match manifest $e"
    case ArrayHashMismatch(n, e, a) => s"array $n sha256 $a does not match manifest $e"
    case ArrayMissing(n)           => s"array $n missing from npz"
    case ArrayNotInManifest(n)     => s"array $n present in npz but not declared by the manifest or schema"
    case DtypeMismatch(n, m, a)    => s"array $n dtype $a but manifest says $m"
    case ShapeMismatch(n, m, a)    => s"array $n shape $a but manifest says $m"
    case CellMismatch(f, e, g)     => s"cell field $f is $g but expected $e"
    case Inconsistent(d)           => s"inconsistent dimensions: $d"
    case Unreadable(p, d)          => s"cannot read $p: $d"
