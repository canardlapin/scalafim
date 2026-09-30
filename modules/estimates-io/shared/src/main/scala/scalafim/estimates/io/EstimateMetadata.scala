package scalafim.estimates.io

import scalafim.estimates.*
import scalafim.archive.ContentDigest
import scalafim.image.SampleSpaces
import scalafim.image.SampleSpaces.*
import image4s.geometry.{Affine, D3}
import upickle.default.*
import scala.util.control.NonFatal

/** Digest-pinned projections of the JSON catalog and unit observation order. */
final case class EstimateIndexTables(estimands: FileReference, observations: FileReference):
  require(estimands.path != observations.path)

/** Shared metadata codec. Profile members are ordinary JSON; physical payloads
  * remain external. Decoding invokes the same validating public constructors.
  */
object EstimateMetadata:
  val version = "0.2.0"
  val coreSchema = "scalafim-estimates-core-nifti-1"
  val developmentSchema = "scalafim-estimates-development-1"
  val wireVersion = "1.0.0"
  val compactSchema = "scalafim-estimates-core-nifti-2"
  val compactWireVersion = "2.0.0"

  /** Canonical UTF-8 projections. JSON remains the scientific authority; these
    * tables are digest-pinned and must agree byte-for-byte with its ordered IDs.
    */
  def estimandsTsv(value: EstimandCatalog): String =
    "index\testimand_id\n" + value.entries.zipWithIndex.map { (entry, index) =>
      s"$index\t${entry.id.value}\n"
    }.mkString

  def observationsTsv(value: EstimateUnit): String =
    "index\tobservation_id\n" + value.observations.zipWithIndex.map { (observation, index) =>
      s"$index\t${observation.id.value}\n"
    }.mkString

  private def checked[A](body: => A): Either[EstimateError, A] =
    try Right(body)
    catch case NonFatal(error) => Left(EstimateError.Invalid(Option(error.getMessage).getOrElse("invalid metadata")))

  private given codecContentDigest: ReadWriter[ContentDigest] = readwriter[String].bimap(_.value, ContentDigest.unsafeSha256)
  private given codecDatasetId: ReadWriter[DatasetId] = readwriter[String].bimap(_.value, DatasetId.apply)
  private given codecModelRevisionId: ReadWriter[ModelRevisionId] = readwriter[String].bimap(_.value, ModelRevisionId.apply)
  private given codecUnitId: ReadWriter[UnitId] = readwriter[String].bimap(_.value, UnitId.apply)
  private given codecUnitRevisionId: ReadWriter[UnitRevisionId] = readwriter[String].bimap(_.value, UnitRevisionId.apply)
  private given codecCollectionRevisionId: ReadWriter[CollectionRevisionId] = readwriter[String].bimap(_.value, CollectionRevisionId.apply)
  private given codecEstimandId: ReadWriter[EstimandId] = readwriter[String].bimap(_.value, EstimandId.apply)
  private given codecProductId: ReadWriter[ProductId] = readwriter[String].bimap(_.value, ProductId.apply)
  private given codecObservationId: ReadWriter[ObservationId] = readwriter[String].bimap(_.value, ObservationId.apply)
  private given codecColumnId: ReadWriter[ColumnId] = readwriter[String].bimap(_.value, ColumnId.apply)
  private given codecAcquisitionId: ReadWriter[AcquisitionId] = readwriter[String].bimap(_.value, AcquisitionId.apply)
  private given codecRepresentationId: ReadWriter[RepresentationId] = readwriter[String].bimap(_.value, RepresentationId.apply)
  private given codecParticipantId: ReadWriter[ParticipantId] = macroRW
  private given codecResponseCoordinateNotApplicabletype: ReadWriter[ResponseCoordinate.NotApplicable.type] = macroRW
  private given codecResponseCoordinateFirInterval: ReadWriter[ResponseCoordinate.FirInterval] = macroRW
  private given codecResponseCoordinateSample: ReadWriter[ResponseCoordinate.Sample] = macroRW
  private given codecResponseCoordinate: ReadWriter[ResponseCoordinate] = macroRW
  private given codecEstimandKind: ReadWriter[EstimandKind] = readwriter[String].bimap(_.toString, EstimandKind.valueOf)
  private given codecEstimandDefinition: ReadWriter[EstimandDefinition] = macroRW
  private given codecEstimandCatalog: ReadWriter[EstimandCatalog] = macroRW
  private given codecEstimandBinding: ReadWriter[EstimandBinding] = macroRW
  private given codecPoolingScope: ReadWriter[PoolingScope] = readwriter[String].bimap(_.toString, PoolingScope.valueOf)
  private given codecObservation: ReadWriter[Observation] = macroRW
  private given codecNumericPrecision: ReadWriter[NumericPrecision] = readwriter[String].bimap(_.toString, NumericPrecision.valueOf)
  private given codecStatisticKind: ReadWriter[StatisticKind] = readwriter[String].bimap(_.toString, StatisticKind.valueOf)
  private given codecProductKindEffecttype: ReadWriter[ProductKind.Effect.type] = macroRW
  private given codecProductKindStandardErrortype: ReadWriter[ProductKind.StandardError.type] = macroRW
  private given codecProductKindVariancetype: ReadWriter[ProductKind.Variance.type] = macroRW
  private given codecProductKindResidualVariancetype: ReadWriter[ProductKind.ResidualVariance.type] = macroRW
  private given codecProductKindCovariancetype: ReadWriter[ProductKind.Covariance.type] = macroRW
  private given codecProductKindDegreesOfFreedomValuestype: ReadWriter[ProductKind.DegreesOfFreedomValues.type] = macroRW
  private given codecProductKindStatistic: ReadWriter[ProductKind.Statistic] = macroRW
  private given codecProductKind: ReadWriter[ProductKind] = macroRW
  private given codecProductOutcomeAvailable: ReadWriter[ProductOutcome.Available] = macroRW
  private given codecProductOutcomeNotRequestedtype: ReadWriter[ProductOutcome.NotRequested.type] = macroRW
  private given codecProductOutcomeUnsupported: ReadWriter[ProductOutcome.Unsupported] = macroRW
  private given codecProductOutcomeFailed: ReadWriter[ProductOutcome.Failed] = macroRW
  private given codecProductOutcome: ReadWriter[ProductOutcome] = macroRW
  private given codecProductTargetsScalar: ReadWriter[ProductTargets.Scalar] = macroRW
  private given codecProductTargetsUpperTriangle: ReadWriter[ProductTargets.UpperTriangle] = macroRW
  private given codecProductTargets: ReadWriter[ProductTargets] = macroRW
  private given codecProductDescriptor: ReadWriter[ProductDescriptor] = macroRW
  private given codecDfRole: ReadWriter[DfRole] = readwriter[String].bimap(_.toString, DfRole.valueOf)
  private given codecDfValueUnknown: ReadWriter[DfValue.Unknown] = macroRW
  private given codecDfValueNotApplicabletype: ReadWriter[DfValue.NotApplicable.type] = macroRW
  private given codecDfValueScalar: ReadWriter[DfValue.Scalar] = macroRW
  private given codecDfValueProduct: ReadWriter[DfValue.Product] = macroRW
  private given codecDfValue: ReadWriter[DfValue] = macroRW
  private given codecDegreesOfFreedom: ReadWriter[DegreesOfFreedom] = macroRW
  private given codecMarginalVarianceOriginKnown: ReadWriter[MarginalVarianceOrigin.Known] = macroRW
  private given codecMarginalVarianceOriginEstimated: ReadWriter[MarginalVarianceOrigin.Estimated] = macroRW
  private given codecMarginalVarianceOriginUnknown: ReadWriter[MarginalVarianceOrigin.Unknown] = macroRW
  private given codecMarginalVarianceOrigin: ReadWriter[MarginalVarianceOrigin] = macroRW
  private given codecMarginalUncertaintyDescriptor: ReadWriter[MarginalUncertaintyDescriptor] = macroRW
  private given codecReferenceDistributionUnknown: ReadWriter[ReferenceDistribution.Unknown] = macroRW
  private given codecReferenceDistributionNormaltype: ReadWriter[ReferenceDistribution.Normal.type] = macroRW
  private given codecReferenceDistributionStudentT: ReadWriter[ReferenceDistribution.StudentT] = macroRW
  private given codecReferenceDistributionFisherF: ReadWriter[ReferenceDistribution.FisherF] = macroRW
  private given codecReferenceDistribution: ReadWriter[ReferenceDistribution] = macroRW
  private given codecTestTail: ReadWriter[TestTail] = readwriter[String].bimap(_.toString, TestTail.valueOf)
  private given codecHypothesisTarget: ReadWriter[HypothesisTarget] = macroRW
  private given codecStatisticCorrespondenceKnown: ReadWriter[StatisticCorrespondence.Known] = macroRW
  private given codecStatisticCorrespondenceUnknown: ReadWriter[StatisticCorrespondence.Unknown] = macroRW
  private given codecStatisticCorrespondence: ReadWriter[StatisticCorrespondence] = macroRW
  private given codecStatisticProductLink: ReadWriter[StatisticProductLink] = macroRW
  private given codecStatisticSemantics: ReadWriter[StatisticSemantics] = macroRW
  private given codecCovarianceEquationAbsolutetype: ReadWriter[CovarianceEquation.Absolute.type] = macroRW
  private given codecCovarianceEquationNormalized: ReadWriter[CovarianceEquation.Normalized] = macroRW
  private given codecCovarianceEquation: ReadWriter[CovarianceEquation] = macroRW
  private given codecCovarianceDescriptor: ReadWriter[CovarianceDescriptor] = macroRW
  private given codecEstimabilityEvidenceUnknown: ReadWriter[EstimabilityEvidence.Unknown] = macroRW
  private given codecEstimabilityEvidenceFullRank: ReadWriter[EstimabilityEvidence.FullRank] = macroRW
  private given codecEstimabilityEvidenceDesign: ReadWriter[EstimabilityEvidence.Design] = macroRW
  private given codecEstimabilityEvidenceSubspace: ReadWriter[EstimabilityEvidence.Subspace] = macroRW
  private given codecEstimabilityEvidence: ReadWriter[EstimabilityEvidence] = macroRW
  private given codecScientificFactKnown: ReadWriter[ScientificFact.Known] = macroRW
  private given codecScientificFactUnknown: ReadWriter[ScientificFact.Unknown] = macroRW
  private given codecScientificFact: ReadWriter[ScientificFact] = macroRW
  private given codecAcquisitionScans: ReadWriter[AcquisitionScans] = macroRW
  private given codecExternalInput: ReadWriter[ExternalInput] = macroRW
  private given codecEstimateProvenance: ReadWriter[EstimateProvenance] = macroRW
  private given codecPinnedUnit: ReadWriter[PinnedUnit] = macroRW
  private given codecUnitOutcomePublished: ReadWriter[UnitOutcome.Published] = macroRW
  private given codecUnitOutcomeFailed: ReadWriter[UnitOutcome.Failed] = macroRW
  private given codecUnitOutcomeMissing: ReadWriter[UnitOutcome.Missing] = macroRW
  private given codecUnitOutcome: ReadWriter[UnitOutcome] = macroRW
  private given codecEstimateCollection: ReadWriter[EstimateCollection] = macroRW
  private given codecPinnedEstimateSet: ReadWriter[PinnedEstimateSet] = macroRW

  private given codecFileReference: ReadWriter[FileReference] = readwriter[ujson.Value].bimap(
    ref =>
      require(ref.bytes <= 9007199254740991L, "metadata codec supports exact byte counts through 2^53 - 1")
      ujson.Obj("Path" -> ref.path, "SHA256" -> ref.digest.value, "Bytes" -> ujson.Num(ref.bytes.toDouble)),
    value =>
      val bytes = value("Bytes").num
      require(bytes >= 0 && bytes <= 9007199254740991.0 && bytes == math.floor(bytes), "Bytes must be an exact nonnegative JSON integer")
      val hash = value("SHA256").str
      require(hash.matches("[0-9a-f]{64}"), "SHA256 must use lowercase hexadecimal")
      FileReference(value("Path").str, ContentDigest.unsafeSha256(hash), bytes.toLong)
  )

  private given codecEstimateDomain: ReadWriter[EstimateDomain] = readwriter[ujson.Value].bimap(
    domain => ujson.Obj(
      "Dimensions" -> writeJs(domain.dimensions),
      "VoxelToWorldRASMillimetres" -> writeJs(domain.space.affineD3.toOption.get.rowMajor),
      "WorldFrame" -> domain.worldFrame,
      "Support" -> writeJs(domain.support)
    ),
    value =>
      val affine = Affine.fromRowMajor[D3](read[Vector[Double]](value("VoxelToWorldRASMillimetres")))
        .fold(e => throw new IllegalArgumentException(e.message), identity)
      val space = SampleSpaces(read[Vector[Int]](value("Dimensions")), affine = Some(affine))
      EstimateDomain.make(space, read[Vector[Int]](value("Support")), value("WorldFrame").str)
        .fold(e => throw new IllegalArgumentException(e.message), identity)
  )

  private given codecEstimateUnit: ReadWriter[EstimateUnit] = macroRW
  private given codecEstimandPair: ReadWriter[EstimandPair] = macroRW
  private given codecNiftiStoredDatatype: ReadWriter[NiftiStoredDatatype] = readwriter[String].bimap(_.toString, NiftiStoredDatatype.valueOf)
  private given codecNiftiRepresentation: ReadWriter[NiftiRepresentation] = macroRW
  private given codecEstimateIndexTables: ReadWriter[EstimateIndexTables] = macroRW
  private given codecSharedValidityBroadcast: ReadWriter[SharedValidityBroadcast] = readwriter[String].bimap(_.toString, SharedValidityBroadcast.valueOf)
  private given codecSharedCovarianceRepresentation: ReadWriter[SharedCovarianceRepresentation] = macroRW

  private def encodeRepresentation(value: EstimateRepresentation): ujson.Value = value match
    case EstimateRepresentation.Nifti(record) =>
      val encoded = writeJs(record)
      encoded("pairOrder") = writeJs(record.pairOrder)
      encoded("qformAlternativeFrame") = writeJs(record.qformAlternativeFrame)
      encoded("storedDatatype") = writeJs(record.storedDatatype)
      ujson.Obj("Tag" -> "Nifti", "Content" -> encoded)
    case EstimateRepresentation.SharedNormalizedUpperTriangle(record) =>
      val encoded = writeJs(record)
      encoded("precision") = writeJs(record.precision)
      encoded("validityBroadcast") = writeJs(record.validityBroadcast)
      ujson.Obj("Tag" -> "SharedNormalizedUpperTriangle", "Content" -> encoded)

  private def decodeRepresentation(value: ujson.Value): EstimateRepresentation =
    require(value.obj.keySet == Set("Tag", "Content"), "representation has unknown fields")
    val record = value("Content")
    value("Tag").str match
      case "Nifti" =>
        require(record.obj.keySet == Set("product", "observation", "values", "validity", "precision", "slope", "intercept",
          "volumeOrder", "selectedTransform", "pairOrder", "qformAlternativeFrame", "storedDatatype"), "Core-2 NIfTI fields differ")
        val decoded = read[NiftiRepresentation](record)
        require(decoded.storedDatatype.nonEmpty, "Core-2 NIfTI requires explicit stored datatype")
        EstimateRepresentation.Nifti(decoded)
      case "SharedNormalizedUpperTriangle" =>
        require(record.obj.keySet == Set("product", "observation", "table", "estimands", "precision", "validityBroadcast"),
          "shared representation fields differ")
        require(record("table").obj.keySet == Set("Path", "SHA256", "Bytes"), "shared table reference has unknown fields")
        EstimateRepresentation.SharedNormalizedUpperTriangle(read[SharedCovarianceRepresentation](record))
      case _ => throw new IllegalArgumentException("unknown Core-2 representation tag")

  private def document(kind: String, value: ujson.Value, schema: String = coreSchema): String =
    val envelope = ujson.Obj("ProfileVersion" -> version, "Schema" -> schema, "DocumentKind" -> kind, "Content" -> value)
    if schema == coreSchema then envelope("WireVersion") = wireVersion
    else if schema == compactSchema then envelope("WireVersion") = compactWireVersion
    ujson.write(envelope, indent = 2) + "\n"

  private def envelope(text: String, kind: String): (String, ujson.Value) =
    val value = ujson.read(text)
    val schema = value("Schema").str
    require(schema == coreSchema || schema == developmentSchema || (schema == compactSchema && kind == "unit"), "unsupported estimate metadata schema")
    require(value("ProfileVersion").str == version, "unsupported estimate profile version")
    require(value("DocumentKind").str == kind, "wrong estimate document kind")
    if schema == coreSchema || schema == compactSchema then
      require(value("WireVersion").str == (if schema == coreSchema then wireVersion else compactWireVersion), "unsupported Core-NIfTI wire version")
      require(value.obj.keySet == Set("Schema", "WireVersion", "ProfileVersion", "DocumentKind", "Content"),
        "Core-NIfTI envelope has unknown fields")
    (schema, value("Content"))

  private def content(text: String, kind: String): ujson.Value = envelope(text, kind)._2

  def schema(text: String, kind: String): Either[EstimateError, String] = checked(envelope(text, kind)._1)

  def catalog(value: EstimandCatalog): String = document("catalog", writeJs(value))
  def readCatalog(text: String): Either[EstimateError, EstimandCatalog] = checked(read[EstimandCatalog](content(text, "catalog")))

  /** The catalog is an immutable referenced leaf, not a second editable inline authority. */
  def unit(value: EstimateUnit, catalogReference: FileReference,
      representations: Vector[NiftiRepresentation] = Vector.empty,
      tables: Option[EstimateIndexTables] = None): String =
    val encoded = writeJs(value)
    encoded.obj.remove("catalog")
    encoded("Catalog") = writeJs(catalogReference)
    encoded("Representations") = writeJs(representations)
    tables.foreach(table => encoded("Tables") = writeJs(table))
    encoded("ModelRevisionId") = writeJs(value.catalog.model)
    if tables.nonEmpty then
      encoded("covariance") = writeJs(value.covariance)
      encoded("statistics") = writeJs(value.statistics)
      encoded("degreesOfFreedom") = writeJs(value.degreesOfFreedom)
      encoded("marginalUncertainty") = writeJs(value.marginalUncertainty)
    document("unit", encoded, if tables.nonEmpty then coreSchema else developmentSchema)

  def compactUnit(value: EstimateUnit, catalogReference: FileReference,
      representations: Vector[EstimateRepresentation], tables: EstimateIndexTables): String =
    val encoded = ujson.read(unit(value, catalogReference, tables = Some(tables)))("Content")
    encoded("Representations") = ujson.Arr.from(representations.map(encodeRepresentation))
    document("unit", encoded, compactSchema)

  def catalogReference(text: String): Either[EstimateError, FileReference] =
    checked(read[FileReference](content(text, "unit")("Catalog")))

  def readUnit(text: String, catalog: EstimandCatalog): Either[EstimateError, EstimateUnit] = checked:
    val (wireSchema, encoded) = envelope(text, "unit")
    require(read[ModelRevisionId](encoded("ModelRevisionId")) == catalog.model, "unit and catalog model revisions differ")
    if wireSchema == coreSchema || wireSchema == compactSchema then
      val required = Set("dataset", "unit", "revision", "domain", "observations", "bindings", "products",
        "outcomes", "estimability", "provenance", "covariance", "statistics", "degreesOfFreedom",
        "marginalUncertainty", "Catalog", "Representations", "Tables", "ModelRevisionId")
      require(encoded.obj.keySet == required, "Core-NIfTI unit fields differ from the versioned schema")
      read[EstimateIndexTables](encoded("Tables"))
      if wireSchema == coreSchema then read[Vector[NiftiRepresentation]](encoded("Representations"))
      else encoded("Representations").arr.foreach(decodeRepresentation)
    else
      encoded.obj.get("statistics").foreach: entries =>
        entries.arr.foreach: statistic =>
          Vector("effect", "standardError").foreach: field =>
            statistic.obj.get(field) match
              case Some(ujson.Str(id)) =>
                statistic(field) = writeJs(StatisticProductLink(ProductId(id),
                  StatisticCorrespondence.Unknown("development-1 link lacks hypothesis correspondence")))
              case _ => ()
    encoded.obj.remove("Catalog")
    encoded.obj.remove("Representations")
    encoded.obj.remove("Tables")
    encoded.obj.remove("ModelRevisionId")
    encoded("catalog") = writeJs(catalog)
    read[EstimateUnit](encoded)

  def representations(text: String): Either[EstimateError, Vector[NiftiRepresentation]] =
    checked:
      val (schema, encoded) = envelope(text, "unit")
      require(schema != compactSchema, "Core-1 representation helper refuses Core-2; use allRepresentations")
      read[Vector[NiftiRepresentation]](encoded("Representations"))

  def allRepresentations(text: String): Either[EstimateError, Vector[EstimateRepresentation]] = checked:
    val (schema, encoded) = envelope(text, "unit")
    if schema == compactSchema then encoded("Representations").arr.toVector.map(decodeRepresentation)
    else read[Vector[NiftiRepresentation]](encoded("Representations")).map(EstimateRepresentation.Nifti.apply)

  def indexTables(text: String): Either[EstimateError, Option[EstimateIndexTables]] = checked:
    val encoded = content(text, "unit")
    encoded.obj.get("Tables").map(value => read[EstimateIndexTables](value))

  def collection(value: EstimateCollection): String = document("collection", writeJs(value))
  def readCollection(text: String): Either[EstimateError, EstimateCollection] = checked(read[EstimateCollection](content(text, "collection")))

  def pointer(value: PinnedEstimateSet): String = document("pointer", writeJs(value))
  def readPointer(text: String): Either[EstimateError, PinnedEstimateSet] = checked(read[PinnedEstimateSet](content(text, "pointer")))
