package scalafim.umvpaspike

import gale.linalg.{DMat, DoubleLinearOperator}
import multivar.core.*
import resample4s.core.{Draw, Injection, Permutation, Reindexing, Selection}
import scalafim.locus.{DomainFactory, SomeFiniteDomain}
import scalafim.response.{DomainId, DuplicatePolicy, OrderedIndices}

/** M0.03 TEST-ONLY prototype. It composes the real provider types; it is not the production axis API. Deliberately
  * bounded to 64 coordinates so the collision-free full descriptor encoding cannot become a whole-brain key. M1 must
  * choose a scalable verified signature rather than copying this codec.
  */
object IdentityPrototype:
  enum Error:
    case InvalidAxis(reason: String)
    case AxisMismatch(boundary: String)
    case Shape(reason: String)
    case Unsupported(reason: String)

  final case class Key(value: String, occurrence: Vector[Int] = Vector.empty)

  /** Untrusted decoded metadata, not a compatibility witness. */
  final case class DecodedAxis(
      namespace: String,
      keys: Vector[Key],
      basis: String,
      units: String,
      scale: String,
      derivation: Vector[String] = Vector.empty
  ):
    private[IdentityPrototype] def canonical: String =
      def field(s: String): String = s.length.toString + ":" + s
      val parts = Vector(namespace, basis, units, scale) ++
        Vector(keys.length.toString) ++ keys.flatMap(k => Vector(k.value, k.occurrence.mkString(","))) ++
        Vector(derivation.length.toString) ++ derivation
      // Length framing plus four-hex-digit UTF-16 code units is injective;
      // this prototype does not pretend a short hash proves full compatibility.
      "axis-" + parts.map(field).mkString.map(c => f"${c.toInt}%04x").mkString

  final class Axis private[IdentityPrototype] (
      val decoded: DecodedAxis,
      val ref: SpaceRef,
      val locus: SomeFiniteDomain
  ):
    type Id = ref.Id
    val evidence: SpaceEvidence[Id] = ref.evidence
    val responseDomain: DomainId[Id] = DomainId.unsafe[Id](decoded.canonical)
    def size: Int = decoded.keys.length
    def keyAt(point: locus4s.Index[locus.S]): Key = decoded.keys(point.ordinal)

  object Axis:
    def load(decoded: DecodedAxis): Either[Error, Axis] =
      val labels = Vector(decoded.namespace, decoded.basis, decoded.units, decoded.scale) ++ decoded.keys.map(_.value)
      if decoded.keys.isEmpty || decoded.keys.length > 64 then
        Left(Error.InvalidAxis("prototype requires 1..64 coordinates"))
      else if labels.exists(s => s.isEmpty || s.length > 128 || s.exists(_.isControl)) then
        Left(Error.InvalidAxis("empty, control-containing or overlong coordinate metadata"))
      else if decoded.keys.distinct.length != decoded.keys.length then
        Left(Error.InvalidAxis("duplicate occurrence identity"))
      else if decoded.keys.exists(_.occurrence.exists(_ < 0)) then
        Left(Error.InvalidAxis("negative occurrence address"))
      else if decoded.keys.exists(_.occurrence.length > 32) then
        Left(Error.InvalidAxis("prototype occurrence depth exceeded"))
      else if decoded.derivation.length > 8 || decoded.derivation.foldLeft(0L)(_ + _.length) > 65536 then
        Left(Error.InvalidAxis("prototype derivation budget exceeded"))
      else
        for
          ref <- SpaceRef
            .of(decoded.canonical, SpaceRole.Observed, decoded.keys.length)
            .left
            .map(error => Error.InvalidAxis(error.message))
          locus <- DomainFactory
            .ephemeral(decoded.namespace, decoded.keys.length)
            .left
            .map(error => Error.InvalidAxis(error.message))
        yield new Axis(decoded, ref, locus)

  enum ReindexKind:
    case Selection, Injection, Draw, Permutation

  final class Restriction[P <: SemanticSpace] private[IdentityPrototype] (
      val parent: SpaceEvidence[P],
      val child: Axis,
      val ordinals: Vector[Int],
      val kind: ReindexKind,
      val responseSelection: OrderedIndices[P]
  )(val leg: Lin[Primal[P], Primal[child.Id]])

  def restrict(parent: Axis, by: Reindexing): Either[Error, Restriction[parent.Id]] =
    if by.codomain != parent.size then Left(Error.Shape("foreign reindexing population"))
    else if by.domain == 0 || by.domain > 64 then
      Left(Error.Unsupported("prototype requires 1..64 selected occurrences"))
    else
      val kind = by match
        case _: Selection   => ReindexKind.Selection
        case _: Injection   => ReindexKind.Injection
        case _: Draw        => ReindexKind.Draw
        case _: Permutation => ReindexKind.Permutation
      val ordinals = by.toVector
      val keys = ordinals.zipWithIndex.map { (ordinal, position) =>
        val key = parent.decoded.keys(ordinal)
        if kind == ReindexKind.Draw then key.copy(occurrence = key.occurrence :+ position) else key
      }
      val declared = parent.decoded.copy(
        keys = keys,
        derivation = Vector(parent.decoded.canonical, kind.toString, ordinals.mkString(","))
      )
      for
        child <- Axis.load(declared)
        selected <- OrderedIndices
          .fromInts[parent.Id](
            parent.responseDomain,
            parent.size,
            ordinals,
            if kind == ReindexKind.Draw then DuplicatePolicy.Allow else DuplicatePolicy.Reject
          )
          .left
          .map(error => Error.Shape(error.toString))
        leg <- Lin
          .fromDenseMatrix(
            DMat.dense(
              child.size,
              parent.size,
              Vector.tabulate(child.size * parent.size)(i =>
                if ordinals(i / parent.size) == i % parent.size then 1.0 else 0.0
              )
            ),
            CoordinateEvidence.primal(parent.evidence),
            CoordinateEvidence.primal(child.evidence),
            ValueIdentity.source(ValueId.unsafe("restriction-" + child.decoded.canonical))
          )
          .left
          .map(error => Error.Shape(error.message))
      yield new Restriction(parent.evidence, child, ordinals, kind, selected)(leg)

  final class Column[S <: SemanticSpace] private[IdentityPrototype] (
      val rows: SpaceEvidence[S],
      val values: Vector[Double]
  ):
    def reindex(by: Restriction[S]): Column[by.child.Id] =
      new Column(by.child.evidence, by.ordinals.map(values))

  def column(rows: Axis, declared: DecodedAxis, values: Vector[Double]): Either[Error, Column[rows.Id]] =
    if rows.decoded != declared then Left(Error.AxisMismatch("target rows"))
    else if values.length != rows.size then Left(Error.Shape("target length"))
    else Right(new Column(rows.evidence, values))

  final class Responses[S <: SemanticSpace, F <: SemanticSpace] private[IdentityPrototype] (
      val table: Table[S, F]
  ):
    def reindex(by: Restriction[S]): Responses[by.child.Id, F] =
      new Responses(table.andThen(by.leg))

  def responses(
      rows: Axis,
      features: Axis,
      declaredRows: DecodedAxis,
      declaredFeatures: DecodedAxis,
      values: DMat,
      source: SourceRevision
  ): Either[Error, Responses[rows.Id, features.Id]] =
    bind(rows, features, declaredRows, declaredFeatures, values, source)
      .map(evidence => new Responses(evidence.table))

  final case class SourceRevision(locator: String, revision: String)
  enum ValueKnowledge:
    case Unknown
  enum PayloadVerification:
    case CallerDeclared

  final case class Inspection(
      rows: Int,
      columns: Int,
      preview: Vector[Key],
      omitted: Int,
      source: SourceRevision,
      neuralMoments: ValueKnowledge,
      payloadVerification: PayloadVerification
  )

  final class Evidence[S <: SemanticSpace, N <: SemanticSpace] private[IdentityPrototype] (
      val table: Table[S, N],
      val rowMetadata: DecodedAxis,
      val source: SourceRevision
  ):
    def inspect(limit: Int): Either[Error, Inspection] =
      if limit < 0 || limit > 64 then Left(Error.Unsupported("inspection limit must be 0..64"))
      else
        Right(
          Inspection(
            table.rows,
            table.cols,
            rowMetadata.keys.take(limit),
            math.max(0, rowMetadata.keys.length - limit),
            source,
            ValueKnowledge.Unknown,
            PayloadVerification.CallerDeclared
          )
        )

  def bind(
      rows: Axis,
      neural: Axis,
      declaredRows: DecodedAxis,
      declaredNeural: DecodedAxis,
      operator: DoubleLinearOperator,
      source: SourceRevision
  ): Either[Error, Evidence[rows.Id, neural.Id]] =
    if rows.decoded != declaredRows then Left(Error.AxisMismatch("observation rows"))
    else if neural.decoded != declaredNeural then Left(Error.AxisMismatch("neural coordinates"))
    else if Vector(source.locator, source.revision).exists(s => s.isEmpty || s.length > 1024 || s.exists(_.isControl))
    then Left(Error.Unsupported("explicit source revision with bounded metadata required"))
    else
      Lin
        .fromLinearMap(
          operator,
          CoordinateEvidence.dual(neural.evidence),
          CoordinateEvidence.primal(rows.evidence),
          ValueIdentity.source(ValueId.unsafe("evidence-" + encode(source.locator) + "-" + encode(source.revision)))
        )
        .left
        .map(error => Error.Shape(error.message))
        .map(table => new Evidence(table, declaredRows, source))

  private def encode(value: String): String = value.map(c => f"${c.toInt}%04x").mkString

  trait Measurement[N <: SemanticSpace]:
    val local: Axis
    val leg: Lin[Primal[N], Primal[local.Id]]

  def measurement(
      source: Axis,
      target: Axis,
      weights: DMat,
      identity: ValueIdentity
  ): Either[Error, Measurement[source.Id]] =
    Lin
      .fromDenseMatrix(
        weights,
        CoordinateEvidence.primal(source.evidence),
        CoordinateEvidence.primal(target.evidence),
        identity
      )
      .left
      .map(error => Error.Shape(error.message))
      .map { fitted =>
        new Measurement[source.Id]:
          val local: target.type = target
          val leg = fitted
      }

  trait Measured[S <: SemanticSpace]:
    val local: Axis
    val table: Table[S, local.Id]

  def measure[S <: SemanticSpace, N <: SemanticSpace](source: Evidence[S, N], by: Measurement[N]): Measured[S] =
    new Measured[S]:
      val local: by.local.type = by.local
      val table: Table[S, local.Id] = by.leg.star.andThen(source.table)
