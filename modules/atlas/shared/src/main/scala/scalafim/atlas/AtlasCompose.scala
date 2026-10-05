package scalafim.atlas

import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import locus4s.PartialMap
import locus4s.PartialMapError
import locus4s.PartialSurjection

enum AtlasCompositionOverlap derives CanEqual:
  case Reject, PreferFirst, PreferSecond

enum AtlasCompositionOccluded derives CanEqual:
  /** Keep the complete declared parcel inventory or fail. */
  case Reject
  /** Remove parcels with no surviving voxels; correspondence records the loss. */
  case Drop

enum AtlasCompositionError:
  case ConflictingSpaces(firstTemplate: SpaceId, secondTemplate: SpaceId,
    firstCoordinates: SpaceId, secondCoordinates: SpaceId)
  case Geometry(error: GeometryError)
  case ParcelCountOverflow(first: Int, second: Int)
  case Overlap(voxelOrdinal: Int, firstKey: String, secondKey: String)
  case Occluded(firstKeys: Vector[String], secondKeys: Vector[String])
  case Realization(error: AtlasRealizationError)
  case Remap(error: PartialMapError)
  case Publication(error: AtlasPublicationError)

  def message: String = this match
    case ConflictingSpaces(ft, st, fc, sc) =>
      s"composition requires identical declared spaces: template $ft/$st, coordinates $fc/$sc"
    case Geometry(error) => error.message
    case ParcelCountOverflow(first, second) => s"combined parcel count exceeds Int capacity: $first + $second"
    case Overlap(voxel, first, second) => s"overlap at voxel ordinal $voxel: $first / $second"
    case Occluded(first, second) => s"fully occluded parcels: first=${first.mkString(",")}, second=${second.mkString(",")}"
    case Realization(error) => error.message
    case Remap(error) => error.message
    case Publication(error) => error.message

/** A fresh parcel owner on the first parent's exact frame and spatial owner.
  * Remaps express parcel correspondence, not equality of pre/post-overlap fibers.
  */
sealed trait VolumeAtlasComposition[F0 <: Frame[D3], S, A, B]:
  type P
  val realization: VolumeAtlasRealization { type F = F0; type X = S; type P = VolumeAtlasComposition.this.P }
  val firstRemap: PartialMap[A, P]
  val secondRemap: PartialMap[B, P]

  final def atlas: VolumeAtlas = VolumeAtlas.fromRealization(realization)

object AtlasCompose:
  /** Compose assignments on exactly congruent grids, without dense labels or
    * resampling. Parent order is significant. New ids follow canonical parent
    * order; display order follows each parent's presentation. Network names are
    * scoped by parent, even when their text agrees.
    */
  def volume(
      first: VolumeAtlasRealization,
      second: VolumeAtlasRealization,
      overlap: AtlasCompositionOverlap,
      occluded: AtlasCompositionOccluded
  ): Either[AtlasCompositionError, VolumeAtlasComposition[first.F, first.X, first.P, second.P]] =
    for
      _ <- Either.cond(
        first.ref.templateSpace == second.ref.templateSpace && first.ref.coordSpace == second.ref.coordSpace,
        (), AtlasCompositionError.ConflictingSpaces(first.ref.templateSpace, second.ref.templateSpace,
          first.ref.coordSpace, second.ref.coordSpace)
      )
      _ <- Grid.exactCongruence(first.domain.grid, second.domain.grid)
        .left.map(AtlasCompositionError.Geometry.apply)
      _ <- Either.cond(first.parcelDomain.size.toLong + second.parcelDomain.size.toLong <= Int.MaxValue,
        (), AtlasCompositionError.ParcelCountOverflow(first.parcelDomain.size, second.parcelDomain.size))
      winners <- chooseWinners(first, second, overlap)
      result <- build(first, second, overlap, occluded, winners)
    yield result

  private def chooseWinners(
      first: VolumeAtlasRealization,
      second: VolumeAtlasRealization,
      policy: AtlasCompositionOverlap
  ): Either[AtlasCompositionError, Array[Int]] =
    val winners = Array.fill(first.domain.space.size)(-1)
    var failure = Option.empty[AtlasCompositionError]
    var ordinal = 0
    while ordinal < winners.length && failure.isEmpty do
      val a = first.parcelAssignment(first.domain.space.indexAtValidatedOrdinal(ordinal))
      val b = second.parcelAssignment(second.domain.space.indexAtValidatedOrdinal(ordinal))
      if a.nonEmpty && b.nonEmpty && policy == AtlasCompositionOverlap.Reject then
        failure = Some(AtlasCompositionError.Overlap(ordinal, first.parcelKeys(a.get), second.parcelKeys(b.get)))
      else
        val useSecond = b.nonEmpty && (a.isEmpty || policy == AtlasCompositionOverlap.PreferSecond)
        winners(ordinal) =
          if useSecond then first.parcelDomain.size + b.get.ordinal
          else a.fold(-1)(_.ordinal)
      ordinal += 1
    failure.toLeft(winners)

  private def build(
      first: VolumeAtlasRealization,
      second: VolumeAtlasRealization,
      overlap: AtlasCompositionOverlap,
      occluded: AtlasCompositionOccluded,
      winners: Array[Int]
  ): Either[AtlasCompositionError, VolumeAtlasComposition[first.F, first.X, first.P, second.P]] =
    val offset = first.parcelDomain.size
    val count = offset + second.parcelDomain.size
    val present = Array.fill(count)(false)
    winners.foreach(winner => if winner >= 0 then present(winner) = true)
    val kept = present.indices.filter(present.apply).toVector
    val firstKeys = first.parcelDomainRecord.elementKeys
    val secondKeys = second.parcelDomainRecord.elementKeys
    val lostFirst = firstKeys.indices.filterNot(present.apply).map(firstKeys).toVector
    val lostSecond = secondKeys.indices.filterNot(i => present(offset + i)).map(secondKeys).toVector
    if occluded == AtlasCompositionOccluded.Reject && (lostFirst.nonEmpty || lostSecond.nonEmpty) then
      Left(AtlasCompositionError.Occluded(lostFirst, lostSecond))
    else
      val ids = kept.zipWithIndex.map((old, fresh) => old -> RegionId(fresh + 1)).toMap
      val regions = RegionIndex(kept.map: old =>
        val fromFirst = old < offset
        val metadata =
          if fromFirst then first.metadata(first.parcelDomain.indexAtValidatedOrdinal(old))
          else second.metadata(second.parcelDomain.indexAtValidatedOrdinal(old - offset))
        metadata.copy(id = ids(old), network = metadata.network.map(id =>
          NetworkId(scopedNetwork(fromFirst, id.value))
        ))
      )
      val firstKept = firstKeys.indices.map(present.apply).toVector
      val secondKept = secondKeys.indices.map(i => present(offset + i)).toVector
      val model = modelFor(firstKeys, secondKeys, firstKept, secondKept, overlap, occluded)
      val confidence =
        if first.provenance.confidence.ordinal >= second.provenance.confidence.ordinal then first.provenance.confidence
        else second.provenance.confidence
      val ref = AtlasRef.volume("composite", model, first.ref.templateSpace, first.ref.coordSpace,
        source = Some("atlas-compose/v1"), confidence = confidence,
        lineage = Some(s"Exact-grid composition of ${first.ref.name} and ${second.ref.name}"))
      val sources = first.provenance.sourceArtifacts.map(s => s.copy(id = s"first:${s.id}")) ++
        second.provenance.sourceArtifacts.map(s => s.copy(id = s"second:${s.id}"))
      val base = AtlasProvenance.fromRef(ref, regions).withSourceArtifacts(sources)
        .copy(citations = (first.provenance.citations ++ second.provenance.citations).distinct)
      for
        keys <- AtlasParcelDomain.keysForOrigin(base.identity, ref.parcelVariant, ref.parcelIdentity,
          ref.representation, ref.source, regions)
          .left.map(error => AtlasCompositionError.Publication(AtlasPublicationError.InvalidParcelIdentity(error)))
        correspondence = present.indices.map(old => ids.get(old).map(id => keys(id.value - 1))).toVector
        firstEvidence = parentEvidence(first, correspondence.take(offset))
        secondEvidence = parentEvidence(second, correspondence.drop(offset))
        provenance = base.withDerivationStep(DerivationStep.ComposedParcels(firstEvidence, secondEvidence, overlap, occluded))
        display = first.displayOrder.indices.flatMap(p => ids.get(p.ordinal)).toVector ++
          second.displayOrder.indices.flatMap(p => ids.get(offset + p.ordinal)).toVector
        child <- AtlasRealization.buildVolumeIn[first.F, first.X](
          first.registry, ref, regions, first.domain, first.parcellation.imageMetadata, provenance,
          [P] => (parcels: locus4s.FiniteDomain[P], ordinals: Map[RegionId, Int]) =>
            PartialSurjection.fromOptionalTargetOrdinals(first.domain.space, parcels,
              winners.iterator.map(old => ids.get(old).map(ordinals)).toVector)
              .left.map(AtlasRealization.partialAssignmentError),
          Some(display)
        ).left.map(AtlasCompositionError.Realization.apply)
        firstMap <- PartialMap.fromOptionalTargetOrdinals(first.parcelDomain, child.parcelDomain,
          firstKeys.indices.map(i => ids.get(i).map(id => id.value - 1)))
          .left.map(AtlasCompositionError.Remap.apply)
        secondMap <- PartialMap.fromOptionalTargetOrdinals(second.parcelDomain, child.parcelDomain,
          secondKeys.indices.map(i => ids.get(offset + i).map(id => id.value - 1)))
          .left.map(AtlasCompositionError.Remap.apply)
        _ <- child.validateNeuropublishProjection(child.neuropublishProjection)
          .left.map(AtlasCompositionError.Publication.apply)
      yield
        new VolumeAtlasComposition[first.F, first.X, first.P, second.P]:
          type P = child.P
          val realization: VolumeAtlasRealization { type F = first.F; type X = first.X; type P = child.P } = child
          val firstRemap: PartialMap[first.P, child.P] = firstMap
          val secondRemap: PartialMap[second.P, child.P] = secondMap

  private def parentEvidence(parent: VolumeAtlasRealization, correspondence: Vector[Option[String]]): NeuropublishCompositionParentV1 =
    NeuropublishCompositionParentV1(
      AtlasPublicationProjection.provenance(parent.ref, parent.provenance), parent.ref.parcelIdentity, parent.ref.source,
      parent.parcelDomainRecord, AtlasPublicationProjection.metadata(parent.parcelKeys, parent.metadata),
      parent.displayOrder.indices.map(parent.parcelKeys.apply).toVector,
      Vector(parent.volumeSupportDomain),
      parent.identity.assignmentDigests.map(d => NeuropublishDigestV1(d.algorithm, d.value)), correspondence
    )

  private def scopedNetwork(first: Boolean, value: String): String =
    s"${if first then "first" else "second"}:${value.length}:$value"

  private def modelFor(
      firstKeys: Vector[String], secondKeys: Vector[String],
      firstKept: Vector[Boolean], secondKept: Vector[Boolean],
      overlap: AtlasCompositionOverlap, occluded: AtlasCompositionOccluded
  ): String =
    val parts = Vector(overlap.toString, occluded.toString, "parent-scoped-networks/v1", firstKeys.size.toString) ++
      firstKeys.zip(firstKept).flatMap((key, keep) => Vector(key, keep.toString)) ++
      Vector(secondKeys.size.toString) ++ secondKeys.zip(secondKept).flatMap((key, keep) => Vector(key, keep.toString))
    val bytes = AtlasPublicationBinaryV1.finiteIndexed("scalafim.atlas/exact-grid-composition", "1", parts)
    s"exact-grid-v1-${AtlasPublicationSha256.hex(bytes)}"

  /** Validate nested origin evidence at publication and metric admission. This
    * checks declared identities and correspondence, not omitted asset bytes.
    */
  private[atlas] def validateProvenance(
      provenance: NeuropublishAtlasProvenanceV1,
      metadata: Vector[NeuropublishParcelMetadataV1]
  ): Either[AtlasPublicationError, Unit] =
    val compositions = provenance.derivation.collect:
      case value: NeuropublishAtlasDerivationV1.ComposedParcels => value
    val error = AtlasPublicationError.AtlasProvenanceMismatch.apply
    if provenance.identity.family != "composite" then
      if compositions.isEmpty then Right(()) else Left(error("composition record requires composite identity"))
    else if compositions.length != 1 then Left(error("composite requires one scoped composition record"))
    else
      val step = compositions.head
      for
        _ <- validateParent(step.first)
        _ <- validateParent(step.second)
        model = modelFor(step.first.parcelDomain.elementKeys, step.second.parcelDomain.elementKeys,
          step.first.correspondence.map(_.nonEmpty), step.second.correspondence.map(_.nonEmpty), step.overlap, step.occluded)
        _ <- Either.cond(provenance.identity.model == model, (), error("composite namespace disagrees with parent-key ancestry"))
        _ <- Either.cond(step.occluded != AtlasCompositionOccluded.Reject ||
          (step.first.correspondence ++ step.second.correspondence).forall(_.nonEmpty), (), error("reject-occlusion record contains dropped parcels"))
        _ <- Either.cond(step.first.supportDomains.map(_.identity) == step.second.supportDomains.map(_.identity),
          (), error("composition parent support identities differ"))
        rows = step.first.parcelMetadata.zip(step.first.correspondence).collect:
          case (row, Some(key)) => row.copy(key = key, network = row.network.map(scopedNetwork(true, _)))
        secondRows = step.second.parcelMetadata.zip(step.second.correspondence).collect:
          case (row, Some(key)) => row.copy(key = key, network = row.network.map(scopedNetwork(false, _)))
        expected = (rows ++ secondRows).zipWithIndex.map((row, i) => row.copy(id = i + 1))
        _ <- Either.cond(expected.nonEmpty, (), error("composition has no surviving parcels"))
        regions <- regionIndex(expected)
        expectedKeys <- AtlasParcelDomain.keysForOrigin(AtlasIdentity("composite", model), None,
          ParcelIdentity.SourceLabels, AtlasRepresentation.Volume, Some("atlas-compose/v1"), regions)
          .left.map(AtlasPublicationError.InvalidParcelIdentity.apply)
        _ <- Either.cond(expected.map(_.key) == expectedKeys, (), error("composition correspondence has foreign or repeated targets"))
        selected <- validateSelections(provenance, step, expected)
        _ <- Either.cond(metadata == selected, (), error("composite metadata disagrees with recorded composition and selection"))
        _ <- Either.cond(sameSpaces(provenance.declaredSupport, step.first.provenance.declaredSupport) &&
          sameSpaces(provenance.declaredSupport, step.second.provenance.declaredSupport), (), error("composition declared spaces differ"))
      yield ()

  private def validateSelections(
      provenance: NeuropublishAtlasProvenanceV1,
      composition: NeuropublishAtlasDerivationV1.ComposedParcels,
      initial: Vector[NeuropublishParcelMetadataV1]
  ): Either[AtlasPublicationError, Vector[NeuropublishParcelMetadataV1]] =
    val error = AtlasPublicationError.AtlasProvenanceMismatch.apply
    provenance.derivation.dropWhile(_ != composition).drop(1)
      .foldLeft[Either[AtlasPublicationError, Vector[NeuropublishParcelMetadataV1]]](Right(initial)):
        (result, step) => result.flatMap: rows =>
          step match
            case NeuropublishAtlasDerivationV1.SelectedParcels(domain, support, digests, kept, dropped) =>
              val byKey = rows.map(row => row.key -> row).toMap
              val all = kept ++ dropped
              for
                _ <- AtlasPublicationProjection.validateParcelDomain(
                  NeuropublishFiniteIndexedDomainV1("selection-parent", domain, rows.map(_.key)))
                _ <- Either.cond(kept.nonEmpty && all.distinct.length == all.length && all.toSet == byKey.keySet,
                  (), error("selection does not partition its actual parent parcels"))
                _ <- Either.cond(support == composition.first.supportDomains.map(_.identity) && digests.length == 1 &&
                  digests.forall(d => d.algorithm == "sha256" && d.value.matches("[0-9a-f]{64}")),
                  (), error("selection parent support or assignment reference is invalid"))
              yield kept.map(byKey)
            case _ => Right(rows)

  private def validateParent(parent: NeuropublishCompositionParentV1): Either[AtlasPublicationError, Unit] =
    val error = AtlasPublicationError.AtlasProvenanceMismatch.apply
    for
      _ <- AtlasPublicationProjection.validateParcelDomain(parent.parcelDomain)
      _ <- AtlasPublicationProjection.validateParcelMetadata(parent.parcelDomain, parent.parcelMetadata)
      _ <- AtlasPublicationProjection.validateDisplayOrder(parent.parcelDomain, parent.displayOrder)
      _ <- AtlasPublicationProjection.validateProvenanceRecord(parent.provenance, parent.parcelMetadata)
      _ <- Either.cond(parent.provenance.labels.backgroundSourceLabel == Some(0) &&
        parent.provenance.labels.encoding == "volume-integer-labels", (), error("composition parent requires hard-volume labels with background 0"))
      _ <- AtlasPublicationProjection.validateDeclaredSupport(parent.provenance.declaredSupport,
        parent.provenance.labels.encoding, parent.supportDomains, error)
      spaces <- parent.provenance.declaredSupport match
        case NeuropublishDeclaredSupportV1.Volume(t, c, _, _) => Right((t, c))
        case _ => Left(error("composition parent requires volume support"))
      template <- SpaceId.from(spaces._1).left.map(e => error(e.message))
      coordinate <- SpaceId.from(spaces._2).left.map(e => error(e.message))
      _ <- AtlasRef.checked(AtlasDetails(parent.provenance.identity.family, parent.provenance.identity.model),
        AtlasRepresentation.Volume, template, coordinate).left.map(e => error(e.message))
      _ <- Either.cond(parent.source.forall(_.trim.nonEmpty), (), error("parent source must be non-empty"))
      _ <- Either.cond(parent.correspondence.length == parent.parcelDomain.elementKeys.length,
        (), error("parent correspondence count differs from canonical parcel count"))
      _ <- Either.cond(parent.supportDomains.length == 1 && parent.assignmentDigests.length == 1 &&
        parent.supportDomains.forall(_.localId.trim.nonEmpty) &&
        parent.assignmentDigests.forall(d => d.algorithm == "sha256" && d.value.matches("[0-9a-f]{64}")),
        (), error("parent requires a valid volume support identity and assignment digest"))
      _ <- AtlasPublicationProjection.validateVolumeDomain(parent.supportDomains.head)
      regions <- regionIndex(parent.parcelMetadata)
      identity = parent.provenance.identity
      release <- scala.util.Try(identity.release.map(r => AtlasRelease(r.label, r.year, r.version, r.commit, r.date)))
        .toEither.left.map(_ => error("invalid parent release"))
      keys <- AtlasParcelDomain.keysForOrigin(AtlasIdentity(identity.family, identity.model, identity.variant, release),
        identity.parcelVariant, parent.parcelIdentity, AtlasRepresentation.Volume, parent.source, regions)
        .left.map(AtlasPublicationError.InvalidParcelIdentity.apply)
      _ <- Either.cond(keys == parent.parcelDomain.elementKeys, (), error("parent parcel namespace disagrees with provenance"))
    yield ()

  /** Bind each exact composition node to its own recorded support, including
    * nested nodes. Reuse the metric's compact identity reference at this boundary.
    */
  private[atlas] def validateSupport(
      provenance: NeuropublishAtlasProvenanceV1,
      expected: Vector[ParcelMetricDomainReference]
  ): Either[AtlasPublicationError, Unit] =
    provenance.derivation.foldLeft[Either[AtlasPublicationError, Unit]](Right(())):
      (result, step) => result.flatMap: _ =>
        step match
          case NeuropublishAtlasDerivationV1.ComposedParcels(first, second, _, _) =>
            val a = first.supportDomains.map(d => reference(d.identity))
            val b = second.supportDomains.map(d => reference(d.identity))
            for
              _ <- Either.cond(a == expected && b == expected, (),
                AtlasPublicationError.AtlasProvenanceMismatch("composition parents differ from the node's exact support"))
              _ <- validateSupport(first.provenance, a)
              _ <- validateSupport(second.provenance, b)
            yield ()
          case _ => Right(())

  private[atlas] def reference(identity: NeuropublishDomainIdentityV1): ParcelMetricDomainReference =
    ParcelMetricDomainReference(identity.descriptorId, identity.descriptorVersion, identity.size, identity.structuralFingerprint)

  private def sameSpaces(a: NeuropublishDeclaredSupportV1, b: NeuropublishDeclaredSupportV1): Boolean =
    (a, b) match
      case (NeuropublishDeclaredSupportV1.Volume(at, ac, _, _), NeuropublishDeclaredSupportV1.Volume(bt, bc, _, _)) =>
        at == bt && ac == bc
      case _ => false

  private def regionIndex(rows: Vector[NeuropublishParcelMetadataV1]): Either[AtlasPublicationError, RegionIndex] =
    val error = AtlasPublicationError.AtlasProvenanceMismatch.apply
    val initial: Either[AtlasPublicationError, Vector[AtlasRegionMetadata]] =
      if rows.isEmpty then Left(error("parent parcel inventory is empty")) else Right(Vector.empty)
    rows.foldLeft(initial):
      (result, row) =>
        for
          current <- result
          hemisphere <- row.hemisphere match
            case None => Right(None)
            case Some(value) => Hemisphere.values.find(_.toString.toLowerCase == value)
              .map(h => Some(h)).toRight(error("unsupported parent hemisphere"))
          _ <- Either.cond(row.network.forall(_.trim.nonEmpty), (), error("parent network name is empty"))
          region <- AtlasRegionMetadata.checked(RegionId(row.id), row.label, Some(row.fullLabel), hemisphere,
            row.network.map(NetworkId.apply), row.color.map((r, g, b) => Rgb(r, g, b)), row.attributes.toMap)
            .left.map(e => error(e.message))
        yield current :+ region
    .map(RegionIndex.apply)
