package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.SemanticSpace
import multivar.core.ValueId

opaque type PairCoordinateId = String

object PairCoordinateId:
  def apply(value: String): Either[AxisIdentityError, PairCoordinateId] =
    AxisKey(value).map(_.value)

  private[mvpa] def unsafe(value: String): PairCoordinateId =
    value

  extension (id: PairCoordinateId) inline def value: String = id

  given AxisKeyCodec[PairCoordinateId] with
    override def encode(key: PairCoordinateId): AxisKey = AxisKey.unsafe(key.value)

enum RelationalQueryError:
  case Identity(error: ScientificIdentityError)
  case Axis(error: AxisRefError)
  case Evidence(error: EvidenceTableError)
  case Column(error: ColumnError)
  case InvalidEffectPurpose(actual: AxisPurpose)
  case InvalidNeuralPurpose(actual: AxisPurpose)
  case TooFewItems(actual: Int)
  case EmptyRectangularSide(side: String)
  case UnknownItem(key: String)
  case PairQuerySourceMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case PairQueryOutputMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case PairQueryWitnessMismatch(boundary: String)
  case NeuralMetricMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case NeuralMetricWitnessMismatch
  case NonFiniteRdmValue(position: Int, value: Double)

  def message: String =
    this match
      case Identity(error)              => error.message
      case Axis(error)                  => error.message
      case Evidence(error)              => error.message
      case Column(error)                => error.message
      case InvalidEffectPurpose(actual) =>
        s"effect query requires an '${AxisPurpose.Effects.value}' source axis, obtained '${actual.value}'"
      case InvalidNeuralPurpose(actual) =>
        s"neural query requires a '${AxisPurpose.NeuralFeatures.value}' axis, obtained '${actual.value}'"
      case TooFewItems(actual) =>
        s"within-domain pair coordinates require at least two items, obtained $actual"
      case EmptyRectangularSide(side) =>
        s"rectangular pair coordinates require a non-empty $side axis"
      case UnknownItem(key) =>
        s"pair domain does not contain item '$key'"
      case PairQuerySourceMismatch(expected, actual) =>
        s"RDM contrast source ${actual.value} does not match effect domain ${expected.value}"
      case PairQueryOutputMismatch(expected, actual) =>
        s"RDM contrast output ${actual.value} does not match pair domain ${expected.value}"
      case PairQueryWitnessMismatch(boundary) =>
        s"RDM contrast query uses a different nominal $boundary witness"
      case NeuralMetricMismatch(expected, actual) =>
        s"neural metric axis ${actual.value} does not match relation axis ${expected.value}"
      case NeuralMetricWitnessMismatch =>
        "neural metric and relation use different nominal witnesses"
      case NonFiniteRdmValue(position, value) =>
        s"RDM contains non-finite value $value at pair position $position"

final case class WithinDomainPair[K] private[mvpa] (
    id: PairCoordinateId,
    first: K,
    second: K,
    firstPosition: Int,
    secondPosition: Int
)

/** Canonical unordered pair coordinates for one exact ordered effect domain. */
final class WithinPairDomain[
    Items <: SemanticSpace,
    ItemKey
] private (
    val items: AxisRef.Aux[ItemKey, Items],
    val pairs: Vector[WithinDomainPair[ItemKey]],
    val pairAxis: AxisRef[PairCoordinateId],
    val identity: ScientificComponentFingerprint,
    private val positions: Map[(ItemKey, ItemKey), Int]
):
  def size: Int = pairs.size

  def position(first: ItemKey, second: ItemKey): Either[RelationalQueryError, Int] =
    positions
      .get(first -> second)
      .orElse(positions.get(second -> first))
      .toRight:
        val codec = WithinPairDomain.codecOf(items)
        RelationalQueryError.UnknownItem(
          s"${codec.encode(first).value},${codec.encode(second).value}"
        )

  def contrastQuery: EffectQuery[Items, pairAxis.Id, ItemKey, PairCoordinateId] =
    WithinPairDomain.contrastQuery(this)

object WithinPairDomain:
  val PairOrderProtocol = "canonical-upper-triangle-v1"
  private val Kind = EstimandKind.unsafe("within-pair-domain")

  def apply[K](
      items: AxisRef[K]
  ): Either[
    RelationalQueryError,
    WithinPairDomain[items.Id, K]
  ] =
    if items.identity.purpose != AxisPurpose.Effects then
      Left(RelationalQueryError.InvalidEffectPurpose(items.identity.purpose))
    else if items.size < 2 then Left(RelationalQueryError.TooFewItems(items.size))
    else
      EstimandIdentity(
        Kind,
        Vector(
          "items" -> items.identity.fingerprint.value,
          "pair-order" -> PairOrderProtocol
        )
      ).left
        .map(RelationalQueryError.Identity.apply)
        .flatMap: component =>
          val pairs = Vector.newBuilder[WithinDomainPair[K]]
          val pairKeys = Vector.newBuilder[PairCoordinateId]
          val positions = Map.newBuilder[(K, K), Int]
          var first = 0
          var pairPosition = 0
          while first < items.size - 1 do
            var second = first + 1
            while second < items.size do
              val id = pairId(component.fingerprint, pairPosition)
              val left = items.keys(first)
              val right = items.keys(second)
              pairs += WithinDomainPair(id, left, right, first, second)
              pairKeys += id
              positions += (left -> right) -> pairPosition
              pairPosition += 1
              second += 1
            first += 1
          AxisRef
            .create(
              AxisId.unsafe(s"pairs-${component.fingerprint.value.takeRight(24)}"),
              AxisPurpose.unsafe("effect-pairs"),
              pairKeys.result(),
              CoordinateBasis.unsafe(
                "canonical-pairs",
                "items" -> items.identity.fingerprint.value,
                "protocol" -> PairOrderProtocol
              ),
              None,
              AxisScale.nominal,
              CoordinateProvenance.unsafe(
                "scalafim-relational-query",
                PairOrderProtocol,
                component.fingerprint.value
              )
            )
            .left
            .map(RelationalQueryError.Axis.apply)
            .map: pairAxis =>
              new WithinPairDomain(
                items,
                pairs.result(),
                pairAxis,
                component.fingerprint,
                positions.result()
              )

  private def pairId(
      identity: ScientificComponentFingerprint,
      position: Int
  ): PairCoordinateId =
    PairCoordinateId.unsafe(
      s"pair-$position-${identity.value.takeRight(16)}"
    )

  private def contrastQuery[I <: SemanticSpace, K](
      domain: WithinPairDomain[I, K]
  ): EffectQuery[I, domain.pairAxis.Id, K, PairCoordinateId] =
    val builder = Matrix.newBuilder(domain.size, domain.items.size)
    var pair = 0
    while pair < domain.size do
      val coordinate = domain.pairs(pair)
      builder(pair, coordinate.firstPosition) = 1.0
      builder(pair, coordinate.secondPosition) = -1.0
      pair += 1
    EffectQuery
      .dense(
        domain.items,
        domain.pairAxis,
        builder.result(),
        ValueId.unsafe(s"pair-contrasts-${domain.identity.value}")
      )
      .fold(error => throw new IllegalStateException(error.message), identity)

  private def codecOf[K](axis: AxisRef[K]): AxisKeyCodec[K] =
    new AxisKeyCodec[K]:
      private val encoded = axis.keys.zip(axis.identity.orderedKeys).toMap
      override def encode(key: K): AxisKey =
        encoded.getOrElse(key, AxisKey.unsafe("unknown-item"))

final case class RectangularDomainPair[L, R] private[mvpa] (
    id: PairCoordinateId,
    left: L,
    right: R,
    leftPosition: Int,
    rightPosition: Int
)

/** Canonical row-major product coordinates for distinct effect domains. */
final class RectangularPairDomain[
    Left <: SemanticSpace,
    Right <: SemanticSpace,
    LeftKey,
    RightKey
] private (
    val left: AxisRef.Aux[LeftKey, Left],
    val right: AxisRef.Aux[RightKey, Right],
    val pairs: Vector[RectangularDomainPair[LeftKey, RightKey]],
    val pairAxis: AxisRef[PairCoordinateId],
    val identity: ScientificComponentFingerprint
):
  def size: Int = pairs.size

object RectangularPairDomain:
  val PairOrderProtocol = "canonical-rectangular-row-major-v1"
  private val Kind = EstimandKind.unsafe("rectangular-pair-domain")

  def apply[LK, RK](
      left: AxisRef[LK],
      right: AxisRef[RK]
  ): Either[
    RelationalQueryError,
    RectangularPairDomain[left.Id, right.Id, LK, RK]
  ] =
    if left.identity.purpose != AxisPurpose.Effects then
      Left(RelationalQueryError.InvalidEffectPurpose(left.identity.purpose))
    else if right.identity.purpose != AxisPurpose.Effects then
      Left(RelationalQueryError.InvalidEffectPurpose(right.identity.purpose))
    else if left.size == 0 then Left(RelationalQueryError.EmptyRectangularSide("left"))
    else if right.size == 0 then Left(RelationalQueryError.EmptyRectangularSide("right"))
    else
      EstimandIdentity(
        Kind,
        Vector(
          "left" -> left.identity.fingerprint.value,
          "pair-order" -> PairOrderProtocol,
          "right" -> right.identity.fingerprint.value
        )
      ).left
        .map(RelationalQueryError.Identity.apply)
        .flatMap: component =>
          val pairs = Vector.newBuilder[RectangularDomainPair[LK, RK]]
          val keys = Vector.newBuilder[PairCoordinateId]
          var leftPosition = 0
          var pairPosition = 0
          while leftPosition < left.size do
            var rightPosition = 0
            while rightPosition < right.size do
              val id = PairCoordinateId.unsafe(
                s"pair-$pairPosition-${component.fingerprint.value.takeRight(16)}"
              )
              pairs += RectangularDomainPair(
                id,
                left.keys(leftPosition),
                right.keys(rightPosition),
                leftPosition,
                rightPosition
              )
              keys += id
              pairPosition += 1
              rightPosition += 1
            leftPosition += 1
          AxisRef
            .create(
              AxisId.unsafe(s"pairs-${component.fingerprint.value.takeRight(24)}"),
              AxisPurpose.unsafe("effect-pairs"),
              keys.result(),
              CoordinateBasis.unsafe(
                "rectangular-pairs",
                "left" -> left.identity.fingerprint.value,
                "protocol" -> PairOrderProtocol,
                "right" -> right.identity.fingerprint.value
              ),
              None,
              AxisScale.nominal,
              CoordinateProvenance.unsafe(
                "scalafim-relational-query",
                PairOrderProtocol,
                component.fingerprint.value
              )
            )
            .left
            .map(RelationalQueryError.Axis.apply)
            .map: pairAxis =>
              new RectangularPairDomain(
                left,
                right,
                pairs.result(),
                pairAxis,
                component.fingerprint
              )

/** Ordered linear query from one exact effect axis into an exact query axis. */
final class EffectQuery[
    Effects <: SemanticSpace,
    Query <: SemanticSpace,
    EffectKey,
    QueryKey
] private (
    val source: AxisRef.Aux[EffectKey, Effects],
    val output: AxisRef.Aux[QueryKey, Query],
    val coefficients: DMat,
    val map: EvidenceTable[Query, Effects, QueryKey, EffectKey],
    val identity: ScientificComponentFingerprint
)

object EffectQuery:
  private val Kind = EstimandKind.unsafe("effect-query")

  def dense[EK, QK](
      source: AxisRef[EK],
      output: AxisRef[QK],
      values: DMat,
      valueId: ValueId
  ): Either[
    RelationalQueryError,
    EffectQuery[source.Id, output.Id, EK, QK]
  ] =
    if source.identity.purpose != AxisPurpose.Effects then
      Left(RelationalQueryError.InvalidEffectPurpose(source.identity.purpose))
    else
      EvidenceTable
        .dense(output, source, values, valueId)
        .left
        .map(RelationalQueryError.Evidence.apply)
        .flatMap: table =>
          EstimandIdentity(
            Kind,
            Vector(
              "map" -> table.table.valueIdentity.stableKey,
              "coefficients" -> coefficientDigest(values),
              "output" -> output.identity.fingerprint.value,
              "source" -> source.identity.fingerprint.value
            )
          ).left
            .map(RelationalQueryError.Identity.apply)
            .map: component =>
              new EffectQuery(source, output, values, table, component.fingerprint)

  private def coefficientDigest(values: DMat): String =
    val writer = CanonicalWriter()
    writer.string("scalafim-mvpa-effect-query-coefficients/v1")
    writer.int(values.rows)
    writer.int(values.cols)
    var row = 0
    while row < values.rows do
      var column = 0
      while column < values.cols do
        writer.double(values(row, column))
        column += 1
      row += 1
    AxisDigest.sha256Hex(writer.result())

/** Ordered bilinear closure of one exact neural boundary. */
final class NeuralQuery[
    Neural <: SemanticSpace,
    NeuralKey
] private (
    val neural: AxisRef.Aux[NeuralKey, Neural],
    val form: EvidenceTable[Neural, Neural, NeuralKey, NeuralKey],
    val kind: NeuralQueryKind,
    val identity: ScientificComponentFingerprint
)

enum NeuralQueryKind:
  case IdentityInnerProduct
  case FixedPrecision

  def label: String =
    this match
      case IdentityInnerProduct => "identity-inner-product"
      case FixedPrecision       => "fixed-precision"

object NeuralQuery:
  private val Kind = EstimandKind.unsafe("neural-query")

  def identity[K](
      neural: AxisRef[K]
  ): Either[RelationalQueryError, NeuralQuery[neural.Id, K]] =
    if neural.identity.purpose != AxisPurpose.NeuralFeatures then
      Left(RelationalQueryError.InvalidNeuralPurpose(neural.identity.purpose))
    else
      fixed(
        neural,
        DMat.eye(neural.size),
        ValueId.unsafe(s"identity-metric-${neural.identity.fingerprint.value}"),
        NeuralQueryKind.IdentityInnerProduct
      )

  def fixedPrecision[K](
      neural: AxisRef[K],
      precision: DMat,
      valueId: ValueId
  ): Either[RelationalQueryError, NeuralQuery[neural.Id, K]] =
    if neural.identity.purpose != AxisPurpose.NeuralFeatures then
      Left(RelationalQueryError.InvalidNeuralPurpose(neural.identity.purpose))
    else fixed(neural, precision, valueId, NeuralQueryKind.FixedPrecision)

  private def fixed[K](
      neural: AxisRef[K],
      values: DMat,
      valueId: ValueId,
      queryKind: NeuralQueryKind
  ): Either[RelationalQueryError, NeuralQuery[neural.Id, K]] =
    EvidenceTable
      .dense(neural, neural, values, valueId)
      .left
      .map(RelationalQueryError.Evidence.apply)
      .flatMap: form =>
        EstimandIdentity(
          Kind,
          Vector(
            "form" -> form.table.valueIdentity.stableKey,
            "kind" -> queryKind.label,
            "neural" -> neural.identity.fingerprint.value
          )
        ).left
          .map(RelationalQueryError.Identity.apply)
          .map: component =>
            new NeuralQuery(neural, form, queryKind, component.fingerprint)

enum EffectCentering:
  case None
  case GrandMean

  def label: String =
    this match
      case None      => "none"
      case GrandMean => "grand-mean"

enum RdmNormalization:
  case Raw
  case DivideByNeuralDimension

  def label: String =
    this match
      case Raw                     => "raw"
      case DivideByNeuralDimension => "divide-by-neural-dimension"

/** All scientific choices that define a crossvalidated RDM, already bound to its exact effect domain, neural metric,
  * and partition pairing design.
  */
final class RdmDefinition[
    LeftPartitions <: SemanticSpace,
    RightPartitions <: SemanticSpace,
    Effects <: SemanticSpace,
    Neural <: SemanticSpace,
    EffectKey,
    NeuralKey
] private (
    val domain: WithinPairDomain[Effects, EffectKey],
    val contrasts: EffectQuery[
      Effects,
      domain.pairAxis.Id,
      EffectKey,
      PairCoordinateId
    ],
    val metric: NeuralQuery[Neural, NeuralKey],
    val design: PairingDesign[LeftPartitions, RightPartitions],
    val centering: EffectCentering,
    val normalization: RdmNormalization,
    val identity: EstimandIdentity
)

object RdmDefinition:
  private val Kind = EstimandKind.unsafe("relational-rdm")

  def apply[LP <: SemanticSpace, RP <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK](
      domain: WithinPairDomain[E, EK],
      metric: NeuralQuery[N, NK],
      design: PairingDesign[LP, RP],
      centering: EffectCentering = EffectCentering.None,
      normalization: RdmNormalization = RdmNormalization.Raw
  ): Either[RelationalQueryError, RdmDefinition[LP, RP, E, N, EK, NK]] =
    val contrasts = domain.contrastQuery
    EstimandIdentity(
      Kind,
      Vector(
        "centering" -> centering.label,
        "effect-query" -> contrasts.identity.value,
        "geometry" -> "crossvalidated-bilinear",
        "normalization" -> normalization.label,
        "pair-domain" -> domain.identity.value,
        "pair-order" -> WithinPairDomain.PairOrderProtocol,
        "partition-design" -> design.identity.fingerprint.value,
        "precision-policy" -> metric.kind.label,
        "precision" -> metric.identity.value,
        "squaredness" -> "squared"
      )
    ).left
      .map(RelationalQueryError.Identity.apply)
      .map: identity =>
        new RdmDefinition(
          domain,
          contrasts,
          metric,
          design,
          centering,
          normalization,
          identity
        )

/** RDM values remain columns of the definition's exact canonical pair axis. */
final class IdentifiedRdm[
    LP <: SemanticSpace,
    RP <: SemanticSpace,
    E <: SemanticSpace,
    N <: SemanticSpace,
    EK,
    NK
] private (
    val definition: RdmDefinition[LP, RP, E, N, EK, NK],
    val distances: Column[definition.domain.pairAxis.Id, Double]
):
  def distance(first: EK, second: EK): Either[RelationalQueryError, Double] =
    definition.domain
      .position(first, second)
      .flatMap: position =>
        distances.at(position).left.map(RelationalQueryError.Column.apply)

object IdentifiedRdm:
  def apply[LP <: SemanticSpace, RP <: SemanticSpace, E <: SemanticSpace, N <: SemanticSpace, EK, NK](
      definition: RdmDefinition[LP, RP, E, N, EK, NK],
      values: Seq[Double]
  ): Either[RelationalQueryError, IdentifiedRdm[LP, RP, E, N, EK, NK]] =
    val vector = values.toVector
    var position = 0
    while position < vector.length do
      if !vector(position).isFinite then return Left(RelationalQueryError.NonFiniteRdmValue(position, vector(position)))
      position += 1
    Column(definition.domain.pairAxis, vector).left
      .map(RelationalQueryError.Column.apply)
      .map(column => new IdentifiedRdm(definition, column))
