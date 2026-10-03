package scalafim.fmri.mvpa.artifacts

import gale.linalg.DMat
import java.io.{BufferedInputStream, BufferedOutputStream, DataInputStream, DataOutputStream}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.util.control.NonFatal
import scalafim.archive.ContentDigest
import scalafim.archive.io.{LocalObjectStore, LocalStoreError, VerifiedFileObject}
import scalafim.estimates.FileReference
import scalafim.fmri.mvpa.{AxisRecord, AxisRef}
import scalafim.fmri.mvpa.pattern.*
import ujson.*

/** Immutable local v1 archive. Metadata is written after all payload leaves and
  * only a verified metadata reference is returned to callers. */
final class LocalPatternArchive private (private val objects: LocalObjectStore, val limits: PatternArchiveLimits):
  val root: Path = objects.root
  private def error(value: LocalStoreError): PatternArchiveError = value match
    case LocalStoreError.Invalid(x) => PatternArchiveError.Invalid(x)
    case LocalStoreError.Integrity(x) => PatternArchiveError.Integrity(x)
    case LocalStoreError.Conflict(x) => PatternArchiveError.Integrity(x)
    case LocalStoreError.Io(x) => PatternArchiveError.Io(x)
  private def ref(value: VerifiedFileObject): FileReference = FileReference(value.path, value.digest, value.bytes)
  private def verified(value: FileReference): VerifiedFileObject = VerifiedFileObject(value.path, value.digest, value.bytes)
  private def protect[A](f: => Either[PatternArchiveError, A]): Either[PatternArchiveError, A] =
    try f catch case NonFatal(e) => Left(PatternArchiveError.Io(Option(e.getMessage).getOrElse(e.getClass.getName)))
  private def cells(rows: Int, columns: Int): Either[PatternArchiveError, Long] =
    ProfilePayload.cells(rows, columns).filter(_ <= limits.maximumCells).toRight(PatternArchiveError.Unsupported(s"matrix cell count exceeds ${limits.maximumCells}"))
  private def payload(name: String, matrix: DMat, path: String): Either[PatternArchiveError, ProfilePayload] =
    for
      count <- cells(matrix.rows, matrix.cols)
      objectValue <- objects.write(path)({ output =>
        val out = new DataOutputStream(new BufferedOutputStream(output))
        try
          var row = 0
          while row < matrix.rows do
            var col = 0
            while col < matrix.cols do
              out.writeLong(java.lang.Long.reverseBytes(java.lang.Double.doubleToRawLongBits(matrix(row, col))))
              col += 1
            row += 1
        finally out.close()
      }).left.map(error)
      _ <- if objectValue.bytes == count * 8L then Right(()) else Left(PatternArchiveError.Integrity(s"written $name has an unexpected byte length"))
      _ <- objects.verify(objectValue).left.map(error)
    yield ProfilePayload(name, matrix.rows, matrix.cols, ref(objectValue))
  private def vector(name: String, values: Vector[Double], path: String): Either[PatternArchiveError, ProfilePayload] =
    payload(name, DMat.dense(values.length, 1, values), path)
  private def verify(reference: FileReference): Either[PatternArchiveError, Unit] = objects.verify(verified(reference)).left.map(error)
  private def load(payload: ProfilePayload): Either[PatternArchiveError, DMat] = protect:
    for
      count <- cells(payload.rows, payload.columns)
      _ <- if payload.reference.bytes == count * 8L then Right(()) else Left(PatternArchiveError.Integrity(s"payload ${payload.name} byte length differs from shape"))
      _ <- verify(payload.reference)
      matrix <-
        val values = new Array[Double](count.toInt)
        val in = new DataInputStream(new BufferedInputStream(Files.newInputStream(root.resolve(payload.reference.path))))
        try
          var i = 0
          while i < values.length do
            values(i) = java.lang.Double.longBitsToDouble(java.lang.Long.reverseBytes(in.readLong()))
            i += 1
          if in.read() != -1 then Left(PatternArchiveError.Integrity(s"payload ${payload.name} contains trailing bytes"))
          else Right(DMat.dense(payload.rows, payload.columns, values.toVector))
        finally in.close()
    yield matrix

  def write(
      relative: String,
      declaration: PatternScientificDeclaration,
      artifact: PatternArtifact,
      trainingAxis: AxisRef[?]
  ): Either[PatternArchiveError, CompletedPatternProfile] =
    if trainingAxis.descriptor != artifact.trainingBinding.declaredSampleAxis then Left(PatternArchiveError.Invalid("supplied training axis does not match the artifact binding descriptor"))
    else
      PatternScientificDeclaration.coverage(declaration.expectedUnitIds, declaration.committedUnitIds).flatMap: _ =>
        preflight(artifact).flatMap: _ =>
          writeVerified(relative, declaration, artifact, trainingAxis)

  private def writeVerified(
      relative: String,
      declaration: PatternScientificDeclaration,
      artifact: PatternArtifact,
      trainingAxis: AxisRef[?]
  ): Either[PatternArchiveError, CompletedPatternProfile] =
      val prefix = s"patterns/$relative"
      val factorA = payload("A", artifact.factors.neuralByComponent, s"$prefix/A.f64")
      val factorC = payload("C", artifact.factors.targetByComponent, s"$prefix/C.f64")
      val targetPayloads: Either[PatternArchiveError, Vector[ProfilePayload]] = artifact.target match
        case TargetGeometry.Categorical(x) =>
          for p <- vector("target-prior", x.priors.values, s"$prefix/target-prior.f64"); c <- payload("target-contrast", x.contrast, s"$prefix/target-contrast.f64") yield Vector(p, c)
        case TargetGeometry.Continuous(x) =>
          for p <- vector("target-prior", x.priorScale.values, s"$prefix/target-prior.f64"); m <- vector("target-metric", x.metricDiagonal.values, s"$prefix/target-metric.f64"); bs <- sequence(x.blockWeights.zipWithIndex.map { case ((name, w), ordinal) => vector(s"block:$name", w.values, s"$prefix/block-$ordinal.f64") }) yield p +: m +: bs
      val intercept: Either[PatternArchiveError, Vector[ProfilePayload]] = artifact.centering match
        case CenteringPolicy.CenteredBeforeFit(_, _) => Right(Vector.empty)
        case CenteringPolicy.ExplicitIntercept(values, _) => vector("intercept", values.values, s"$prefix/intercept.f64").map(Vector(_))
      for
        a <- factorA
        c <- factorC
        t <- targetPayloads
        i <- intercept
        provisional = CompletedPatternProfile(CompletedPatternProfile.CurrentVersion, declaration, ProfileAxis(artifact.factors.neuralAxis.toRecord), ProfileAxis(artifact.factors.targetAxis.toRecord), ProfileAxis(artifact.factors.componentAxis.toRecord), ProfileAxis(trainingAxis.toRecord), artifact.target, artifact.centering, artifact.degenerateTarget, artifact.residualCovariance, artifact.trainingBinding, artifact.trainingLineage, artifact.diagnostics, artifact.interpretation, artifact.factors.gauge, artifact.factors.coordinateGauge, a +: c +: (t ++ i), FileReference("pending", ContentDigest.unsafeSha256("0" * 64), 0))
        encoded <- PatternMetadata.encode(provisional)
        bytes = encoded.getBytes(UTF_8)
        _ <- if bytes.length <= limits.maximumMetadataBytes then Right(()) else Left(PatternArchiveError.Unsupported("metadata exceeds configured byte limit"))
        published <- objects.write(s"$prefix/profile.json")(_.write(bytes)).left.map(error)
        _ <- objects.verify(published).left.map(error)
      yield provisional.copy(metadata = ref(published))

  private def preflight(artifact: PatternArtifact): Either[PatternArchiveError, Unit] =
    val target = artifact.target match
      case TargetGeometry.Categorical(x) => Vector(x.priors.values.length -> 1, x.contrast.rows -> x.contrast.cols)
      case TargetGeometry.Continuous(x) => Vector(x.priorScale.values.length -> 1, x.metricDiagonal.values.length -> 1) ++ x.blockWeights.map { case (_, value) => value.values.length -> 1 }
    val intercept = artifact.centering match
      case CenteringPolicy.CenteredBeforeFit(_, _) => Vector.empty
      case CenteringPolicy.ExplicitIntercept(value, _) => Vector(value.values.length -> 1)
    val shapes = Vector(artifact.factors.neuralByComponent.rows -> artifact.factors.neuralByComponent.cols, artifact.factors.targetByComponent.rows -> artifact.factors.targetByComponent.cols) ++ target ++ intercept
    shapes.foldLeft[Either[PatternArchiveError, Long]](Right(0L)): (state, shape) =>
      state.flatMap: total =>
        for
          count <- cells(shape._1, shape._2)
          next <- try Right(Math.addExact(total, count)) catch case _: ArithmeticException => Left(PatternArchiveError.Invalid("aggregate payload cells overflow"))
          _ <- if next <= limits.maximumCells then Right(()) else Left(PatternArchiveError.Unsupported("aggregate payload cells exceed configured limit"))
        yield next
    .map(_ => ())

  def read(reference: FileReference, trainingAxis: AxisRef[?]): Either[PatternArchiveError, RestoredPatternArtifact] = protect:
    for
      _ <- if reference.bytes <= limits.maximumMetadataBytes then Right(()) else Left(PatternArchiveError.Unsupported("metadata exceeds configured byte limit"))
      _ <- verify(reference)
      text = Files.readString(root.resolve(reference.path), UTF_8)
      decoded <- PatternMetadata.decode(text, reference)
      _ <- validatePayloadNames(decoded)
      _ <- validateDeclaredShapes(decoded)
      _ <- validatePayloads(decoded.payloads)
      _ <- if trainingAxis.toRecord == decoded.training.record then Right(()) else Left(PatternArchiveError.Integrity("caller training axis does not match persisted binding record"))
      restored <- rebuild(decoded, trainingAxis)
    yield restored

  private def validatePayloadNames(decoded: PatternMetadata.Decoded): Either[PatternArchiveError, Unit] =
    val target = decoded.targetPolicy match
      case PatternMetadata.TargetPolicy.Categorical(_) => Set("target-prior", "target-contrast")
      case PatternMetadata.TargetPolicy.Continuous(blocks) => Set("target-prior", "target-metric") ++ blocks.map(name => s"block:$name")
    val intercept = decoded.centering match
      case PatternMetadata.StoredCenteringPolicy.Centered(_, _) => Set.empty[String]
      case PatternMetadata.StoredCenteringPolicy.Intercept(_) => Set("intercept")
    val expected = Set("A", "C") ++ target ++ intercept
    if decoded.payloads.map(_.name).toSet == expected && decoded.payloads.length == expected.size then Right(())
    else Left(PatternArchiveError.Unsupported("metadata declares an unsupported or incomplete payload profile"))

  /** Shape declarations are checked before a single Float64 array is allocated.
    * The records retain ordered keys, so dimensions are available without
    * rebuilding an axis or materializing a numerical payload. */
  private def validateDeclaredShapes(decoded: PatternMetadata.Decoded): Either[PatternArchiveError, Unit] =
    val neural = decoded.neural.record.stableKeys.length
    val target = decoded.target.record.stableKeys.length
    val components = decoded.components.record.stableKeys.length
    val policyShapes: Either[PatternArchiveError, Map[String, (Int, Int)]] = decoded.targetPolicy match
      case PatternMetadata.TargetPolicy.Categorical(conditions) =>
        val rows = conditions.stableKeys.length
        Right(Map("target-prior" -> (rows -> 1), "target-contrast" -> (rows -> target)))
      case PatternMetadata.TargetPolicy.Continuous(blocks) =>
        Right((Map("target-prior" -> (target -> 1), "target-metric" -> (target -> 1)) ++ blocks.map(name => s"block:$name" -> (target -> 1))).toMap)
    val intercept = decoded.centering match
      case PatternMetadata.StoredCenteringPolicy.Centered(_, _) => Map.empty[String, (Int, Int)]
      case PatternMetadata.StoredCenteringPolicy.Intercept(_) => Map("intercept" -> (neural -> 1))
    val expected = Map("A" -> (neural -> components), "C" -> (target -> components)) ++ intercept
    for
      shapes <- policyShapes
      _ <- decoded.payloads.find(payload => shapes.get(payload.name).exists(shape => payload.rows != shape._1 || payload.columns != shape._2)).map(payload => Left(PatternArchiveError.Integrity(s"payload ${payload.name} shape does not match declared axes"))).getOrElse(Right(()))
      _ <- decoded.payloads.find(payload => expected.get(payload.name).exists(shape => payload.rows != shape._1 || payload.columns != shape._2)).map(payload => Left(PatternArchiveError.Integrity(s"payload ${payload.name} shape does not match declared axes"))).getOrElse(Right(()))
      _ <- decoded.covariance match
        case PatternMetadata.CovariancePolicy.None => Right(())
        case PatternMetadata.CovariancePolicy.LowRank(key, _) if key == s"axis-sha256-${decoded.neural.record.coordinateSignature}" => Right(())
        case PatternMetadata.CovariancePolicy.Provider(key, _) if key == s"axis-sha256-${decoded.neural.record.coordinateSignature}" => Right(())
        case _ => Left(PatternArchiveError.Integrity("residual covariance axis does not match the declared neural axis"))
    yield ()

  private def validatePayloads(payloads: Vector[ProfilePayload]): Either[PatternArchiveError, Unit] =
    if payloads.map(_.name).distinct != payloads.map(_.name) then Left(PatternArchiveError.Integrity("payload names are not unique"))
    else
      payloads.foldLeft[Either[PatternArchiveError, (Long, Long)]](Right(0L -> 0L)): (state, value) =>
        state.flatMap: (totalCells, totalBytes) =>
          for
            count <- cells(value.rows, value.columns)
            bytes <- ProfilePayload.bytes(value.rows, value.columns).toRight(PatternArchiveError.Integrity(s"payload ${value.name} shape overflows Float64 bytes"))
            _ <- if value.reference.bytes == bytes then Right(()) else Left(PatternArchiveError.Integrity(s"payload ${value.name} byte length differs from shape"))
            nextCells <- try Right(Math.addExact(totalCells, count)) catch case _: ArithmeticException => Left(PatternArchiveError.Integrity("aggregate payload cells overflow"))
            _ <- if nextCells <= limits.maximumCells then Right(()) else Left(PatternArchiveError.Unsupported("aggregate payload cells exceed configured limit"))
            nextBytes <- try Right(Math.addExact(totalBytes, bytes)) catch case _: ArithmeticException => Left(PatternArchiveError.Integrity("aggregate payload bytes overflow"))
            _ <- verify(value.reference)
          yield nextCells -> nextBytes
      .map(_ => ())

  private def rebuild(decoded: PatternMetadata.Decoded, training: AxisRef[?]): Either[PatternArchiveError, RestoredPatternArtifact] =
    def axis(value: ProfileAxis): Either[PatternArchiveError, AxisRef[String]] = AxisRef.restore(value.record).left.map(e => PatternArchiveError.Integrity(e.toString))
    def payload(name: String): Either[PatternArchiveError, ProfilePayload] = decoded.payloads.find(_.name == name).toRight(PatternArchiveError.Integrity(s"missing $name payload"))
    for
      neural <- axis(decoded.neural); targetAxis <- axis(decoded.target); components <- axis(decoded.components)
      aPayload <- payload("A"); cPayload <- payload("C"); a <- load(aPayload); c <- load(cPayload)
      factors <- PatternFactors(neural, targetAxis, components, a, c, decoded.gauge, decoded.coordinateGauge).left.map(e => PatternArchiveError.Integrity(e.toString))
      geometry <- restoreTarget(decoded, targetAxis, payload, axis)
      centering <- restoreCentering(decoded, neural, payload)
      covariance <- decoded.covariance(neural.descriptor)
      binding <- TrainingBinding(training.descriptor, decoded.bindingSource, decoded.bindingFingerprint).left.map(e => PatternArchiveError.Integrity(e.toString))
      artifact <- PatternArtifact(factors, geometry, centering, decoded.degenerateTarget, covariance, binding, decoded.lineage, decoded.diagnostics).left.map(e => PatternArchiveError.Integrity(e.toString))
      profile = CompletedPatternProfile(CompletedPatternProfile.CurrentVersion, decoded.declaration, decoded.neural, decoded.target, decoded.components, decoded.training, geometry, centering, decoded.degenerateTarget, covariance, binding, decoded.lineage, decoded.diagnostics, decoded.interpretation, decoded.gauge, decoded.coordinateGauge, decoded.payloads, decoded.metadata)
    yield RestoredPatternArtifact(profile, artifact, training)

  private def restoreTarget(decoded: PatternMetadata.Decoded, targetAxis: AxisRef[String], find: String => Either[PatternArchiveError, ProfilePayload], axis: ProfileAxis => Either[PatternArchiveError, AxisRef[String]]): Either[PatternArchiveError, TargetGeometry] = decoded.targetPolicy match
    case PatternMetadata.TargetPolicy.Categorical(conditionsRecord) =>
      for conditions <- axis(ProfileAxis(conditionsRecord)); priorPayload <- find("target-prior"); contrastPayload <- find("target-contrast"); prior <- load(priorPayload); contrast <- load(contrastPayload); values <- AxisValues(conditions, Vector.tabulate(prior.rows)(row => prior(row, 0))).left.map(e => PatternArchiveError.Integrity(e.toString)); target <- TargetGeometry.categorical(conditions, targetAxis, contrast, values).left.map(e => PatternArchiveError.Integrity(e.toString)) yield target
    case PatternMetadata.TargetPolicy.Continuous(blockNames) =>
      for priorPayload <- find("target-prior"); metricPayload <- find("target-metric"); priorM <- load(priorPayload); metricM <- load(metricPayload); prior <- AxisValues(targetAxis, Vector.tabulate(priorM.rows)(row => priorM(row, 0))).left.map(e => PatternArchiveError.Integrity(e.toString)); metric <- AxisValues(targetAxis, Vector.tabulate(metricM.rows)(row => metricM(row, 0))).left.map(e => PatternArchiveError.Integrity(e.toString)); blocks <- sequence(blockNames.map(name => find(s"block:$name").flatMap(load).flatMap(m => AxisValues(targetAxis, Vector.tabulate(m.rows)(row => m(row, 0))).left.map(e => PatternArchiveError.Integrity(e.toString)).map(name -> _)))); target <- TargetGeometry.continuous(targetAxis, prior, metric, blocks).left.map(e => PatternArchiveError.Integrity(e.toString)) yield target
  private def restoreCentering(decoded: PatternMetadata.Decoded, neural: AxisRef[String], find: String => Either[PatternArchiveError, ProfilePayload]): Either[PatternArchiveError, CenteringPolicy] = decoded.centering match
    case PatternMetadata.StoredCenteringPolicy.Centered(neuralReceipt, targetReceipt) => Right(CenteringPolicy.CenteredBeforeFit(neuralReceipt, targetReceipt))
    case PatternMetadata.StoredCenteringPolicy.Intercept(receipt) => find("intercept").flatMap(load).flatMap(m => AxisValues(neural, Vector.tabulate(m.rows)(row => m(row, 0))).left.map(e => PatternArchiveError.Integrity(e.toString)).map(CenteringPolicy.ExplicitIntercept(_, receipt)))
  private def sequence[A](values: Vector[Either[PatternArchiveError, A]]): Either[PatternArchiveError, Vector[A]] = values.foldLeft(Right(Vector.empty): Either[PatternArchiveError, Vector[A]])((acc, next) => acc.flatMap(xs => next.map(xs :+ _)))

object LocalPatternArchive:
  def open(root: Path, limits: PatternArchiveLimits = PatternArchiveLimits()): Either[PatternArchiveError, LocalPatternArchive] = LocalObjectStore.open(root).left.map(e => PatternArchiveError.Io(e.message)).map(new LocalPatternArchive(_, limits))

private object PatternMetadata:
  private val Schema = PatternProfileMetadata.Schema
  enum TargetPolicy:
    case Categorical(conditions: AxisRecord)
    case Continuous(blockNames: Vector[String])
  enum StoredCenteringPolicy:
    case Centered(neuralReceipt: String, targetReceipt: String)
    case Intercept(receipt: String)
  enum CovariancePolicy:
    case None
    case LowRank(axisStableKey: String, rank: Int)
    case Provider(axisStableKey: String, name: String)
    def apply(neural: scalafim.fmri.mvpa.AxisDescriptor): Either[PatternArchiveError, ResidualCovarianceCapability] = this match
      case None => Right(ResidualCovarianceCapability.NotFitted)
      case LowRank(key, rank) if key == neural.stableKey => Right(ResidualCovarianceCapability.DiagonalPlusLowRank(neural, rank))
      case Provider(key, name) if key == neural.stableKey => Right(ResidualCovarianceCapability.ProviderBacked(neural, name))
      case _ => Left(PatternArchiveError.Integrity("residual covariance axis does not match the neural axis"))
  final case class Decoded(
      declaration: PatternScientificDeclaration,
      neural: ProfileAxis,
      target: ProfileAxis,
      components: ProfileAxis,
      training: ProfileAxis,
      payloads: Vector[ProfilePayload],
      targetPolicy: TargetPolicy,
      centering: StoredCenteringPolicy,
      degenerateTarget: DegenerateTargetPolicy,
      covariance: CovariancePolicy,
      bindingSource: String,
      bindingFingerprint: String,
      lineage: Vector[String],
      diagnostics: PatternFitDiagnostics,
      interpretation: InterpretationStatus,
      gauge: GaugeEvidence,
      coordinateGauge: CoordinateGauge,
      metadata: FileReference
  )
  /** ujson's generic Long writer emits a string on this cross build. Archive
    * bytes use an exact JSON number, bounded to IEEE-754's integer range. */
  private def ref(value: FileReference): Value =
    require(value.bytes <= 9007199254740991L, "metadata supports exact byte counts through 2^53 - 1")
    Obj("Path" -> value.path, "SHA256" -> value.digest.value, "Bytes" -> Num(value.bytes.toDouble))
  private def axis(value: AxisRecord): Value = Obj("Namespace" -> value.namespace, "Role" -> value.role.toString, "StableKeys" -> Arr.from(value.stableKeys), "Basis" -> value.basis, "Units" -> value.units, "Scale" -> value.scale, "Lineage" -> Arr.from(value.lineage), "CoordinateSignature" -> value.coordinateSignature)
  private def payload(value: ProfilePayload): Value = Obj("Name" -> value.name, "Rows" -> value.rows, "Columns" -> value.columns, "Reference" -> ref(value.reference))
  def encode(p: CompletedPatternProfile): Either[PatternArchiveError, String] = try Right(ujson.write(Obj("Schema" -> Schema, "Version" -> p.version, "Declaration" -> Obj("Plan" -> p.declaration.planIdentity, "Measurement" -> p.declaration.measurementIdentity, "Value" -> p.declaration.valueIdentity, "Sources" -> Arr.from(p.declaration.sourceRevisions.map((n, r) => Obj("Name" -> n, "Revision" -> r))), "Seed" -> p.declaration.seed.toString, "Reducer" -> p.declaration.reducerIdentity, "Expected" -> Arr.from(p.declaration.expectedUnitIds), "Committed" -> Arr.from(p.declaration.committedUnitIds)), "Axes" -> Obj("Neural" -> axis(p.neural.record), "Target" -> axis(p.target.record), "Components" -> axis(p.components.record), "Training" -> axis(p.training.record)), "Payloads" -> Arr.from(p.payloads.map(payload)), "Policy" -> policy(p)), indent = 2) + "\n") catch case NonFatal(e) => Left(PatternArchiveError.Invalid(Option(e.getMessage).getOrElse("cannot encode metadata")))
  private def policy(p: CompletedPatternProfile): Value =
    val target = p.targetGeometry match
      case TargetGeometry.Categorical(x) => Obj("Kind" -> "categorical", "Conditions" -> axis(x.conditions.toRecord))
      case TargetGeometry.Continuous(x) => Obj("Kind" -> "continuous", "Blocks" -> Arr.from(x.blockWeights.map(_._1)))
    val center = p.centering match
      case scalafim.fmri.mvpa.pattern.CenteringPolicy.CenteredBeforeFit(n, t) => Obj("Kind" -> "centered", "Neural" -> n, "Target" -> t)
      case scalafim.fmri.mvpa.pattern.CenteringPolicy.ExplicitIntercept(_, r) => Obj("Kind" -> "intercept", "Receipt" -> r)
    val deg = p.degenerateTarget match
      case DegenerateTargetPolicy.Refuse => Obj("Kind" -> "refuse")
      case DegenerateTargetPolicy.RecordExperimental(r) => Obj("Kind" -> "experimental", "Reason" -> r)
    val cov = p.residualCovariance match
      case ResidualCovarianceCapability.NotFitted => Obj("Kind" -> "none")
      case ResidualCovarianceCapability.DiagonalPlusLowRank(a, r) => Obj("Kind" -> "lowrank", "Axis" -> axisRecord(a), "Rank" -> r)
      case ResidualCovarianceCapability.ProviderBacked(a, n) => Obj("Kind" -> "provider", "Axis" -> axisRecord(a), "Name" -> n)
    val gauge = p.gauge match
      case GaugeEvidence.PendingNumericalCheck => Obj("Kind" -> "pending")
      case GaugeEvidence.DeclaredSolverDiagnostic(s, r) => Obj("Kind" -> "declared", "Solver" -> s, "Rank" -> r)
      case GaugeEvidence.VerifiedByGaleGramCholesky(t) => Obj("Kind" -> "gale", "Tolerance" -> t)
    Obj("Target" -> target, "Centering" -> center, "Degenerate" -> deg, "Covariance" -> cov, "Binding" -> Obj("Source" -> p.trainingBinding.source, "Fingerprint" -> p.trainingBinding.fingerprintDigest), "Lineage" -> Arr.from(p.trainingLineage), "Diagnostics" -> Obj("Objective" -> Arr.from(p.diagnostics.objective), "Solver" -> p.diagnostics.solver, "Notes" -> Arr.from(p.diagnostics.notes)), "Gauge" -> gauge, "Interpretation" -> "experimental-fit-only", "CoordinateGauge" -> "unfixed-basis")
  private def axisRecord(a: scalafim.fmri.mvpa.AxisDescriptor): Value = Obj("StableKey" -> a.stableKey)
  private def readRef(v: Value): FileReference = FileReference(v("Path").str, ContentDigest.unsafeSha256(v("SHA256").str), integer(v("Bytes"), "reference bytes", 0, 9007199254740991L))
  private def readAxis(v: Value): AxisRecord = AxisRecord(v("Namespace").str, multivar.core.SpaceRole.valueOf(v("Role").str), v("StableKeys").arr.toVector.map(_.str), v("Basis").str, v("Units").str, v("Scale").str, v("Lineage").arr.toVector.map(_.str), v("CoordinateSignature").str)
  private def readPayload(v: Value): ProfilePayload = ProfilePayload(v("Name").str, integer(v("Rows"), "rows", 1, Int.MaxValue).toInt, integer(v("Columns"), "columns", 1, Int.MaxValue).toInt, readRef(v("Reference")))
  private def invalid[A](f: => A): Either[PatternArchiveError, A] = try Right(f) catch case NonFatal(e) => Left(PatternArchiveError.Invalid(Option(e.getMessage).getOrElse("invalid metadata")))
  private def integer(value: Value, field: String, minimum: Long, maximum: Long): Long =
    val n = value.num
    require(n.isFinite && n == math.floor(n) && n >= minimum && n <= maximum, s"$field must be an exact integer in range")
    n.toLong
  def decode(text: String, metadata: FileReference): Either[PatternArchiveError, Decoded] = invalid:
    val root = ujson.read(text)
    require(root("Schema").str == Schema, "unsupported pattern profile schema")
    require(integer(root("Version"), "version", 1, 1) == 1L, "unsupported pattern profile version")
    val d = root("Declaration")
    val declaration = PatternScientificDeclaration(d("Plan").str, d("Measurement").str, d("Value").str, d("Sources").arr.toVector.map(v => v("Name").str -> v("Revision").str), java.lang.Long.parseLong(d("Seed").str), d("Reducer").str, d("Expected").arr.toVector.map(_.str), d("Committed").arr.toVector.map(_.str))
    val axes = root("Axes")
    val neural = ProfileAxis(readAxis(axes("Neural")))
    val target = ProfileAxis(readAxis(axes("Target")))
    val components = ProfileAxis(readAxis(axes("Components")))
    val training = ProfileAxis(readAxis(axes("Training")))
    val policy = root("Policy")
    val targetPolicy = policy("Target") match
      case v if v("Kind").str == "categorical" => TargetPolicy.Categorical(readAxis(v("Conditions")))
      case v if v("Kind").str == "continuous" => TargetPolicy.Continuous(v("Blocks").arr.toVector.map(_.str))
      case _ => throw new IllegalArgumentException("unsupported target geometry")
    val centering = policy("Centering") match
      case v if v("Kind").str == "centered" => StoredCenteringPolicy.Centered(v("Neural").str, v("Target").str)
      case v if v("Kind").str == "intercept" => StoredCenteringPolicy.Intercept(v("Receipt").str)
      case _ => throw new IllegalArgumentException("unsupported centering policy")
    val degeneracy = policy("Degenerate") match
      case v if v("Kind").str == "refuse" => DegenerateTargetPolicy.Refuse
      case v if v("Kind").str == "experimental" => DegenerateTargetPolicy.RecordExperimental(v("Reason").str)
      case _ => throw new IllegalArgumentException("unsupported degeneracy policy")
    val covariance = policy("Covariance") match
      case v if v("Kind").str == "none" => CovariancePolicy.None
      case v if v("Kind").str == "lowrank" => CovariancePolicy.LowRank(v("Axis")("StableKey").str, integer(v("Rank"), "covariance rank", 0, Int.MaxValue).toInt)
      case v if v("Kind").str == "provider" => CovariancePolicy.Provider(v("Axis")("StableKey").str, v("Name").str)
      case _ => throw new IllegalArgumentException("unsupported covariance policy")
    val binding = policy("Binding")
    val diagnosticsValue = policy("Diagnostics")
    val diagnostics = PatternFitDiagnostics(diagnosticsValue("Objective").arr.toVector.map(_.num), diagnosticsValue("Solver").str, diagnosticsValue("Notes").arr.toVector.map(_.str)).fold(e => throw new IllegalArgumentException(e.toString), identity)
    val gauge = policy("Gauge") match
      case v if v("Kind").str == "pending" => GaugeEvidence.PendingNumericalCheck
      case v if v("Kind").str == "declared" => GaugeEvidence.DeclaredSolverDiagnostic(v("Solver").str, integer(v("Rank"), "gauge rank", 0, Int.MaxValue).toInt)
      case v if v("Kind").str == "gale" => GaugeEvidence.VerifiedByGaleGramCholesky(v("Tolerance").num)
      case _ => throw new IllegalArgumentException("unsupported gauge")
    require(policy.obj.get("Interpretation").exists(_.str == "experimental-fit-only"), "unsupported interpretation")
    require(policy.obj.get("CoordinateGauge").exists(_.str == "unfixed-basis"), "unsupported coordinate gauge")
    Decoded(declaration, neural, target, components, training, root("Payloads").arr.toVector.map(readPayload), targetPolicy, centering, degeneracy, covariance, binding("Source").str, binding("Fingerprint").str, policy("Lineage").arr.toVector.map(_.str), diagnostics, InterpretationStatus.ExperimentalFitOnly, gauge, CoordinateGauge.UnfixedBasis, metadata)
