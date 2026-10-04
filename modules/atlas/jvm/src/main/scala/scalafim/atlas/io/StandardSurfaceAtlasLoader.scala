package scalafim.atlas.io

import java.nio.file.Path
import scalafim.atlas.*
import scalafim.surface.*
import scalafim.surface.io.FreeSurferAnnotationReader
import scalafim.surface.io.FreeSurferSurfaceReader

/** JVM interpreter for pin-qualified FreeSurfer annotation pairs. */
object StandardSurfaceAtlasLoader:
  def load(
      request: StandardSurfaceAnnotationRequest,
      geometry: SurfaceGiftiAtlasLoader.Geometry,
      store: AtlasStore = FileAtlasStore.default,
      policy: AssetPolicy = AssetPolicy.CacheOrDownload
  ): Either[AtlasAcquisitionError, SurfaceAtlas] =
    for
      left <- PinnedVolumeLoader.resolve(request.left, store, policy)
      right <- PinnedVolumeLoader.resolve(request.right, store, policy)
      atlas <- loadFromPaths(request, geometry, left, right)
    yield atlas

  def loadFromPaths(
      request: StandardSurfaceAnnotationRequest,
      geometry: SurfaceGiftiAtlasLoader.Geometry,
      leftPath: Path,
      rightPath: Path
  ): Either[AtlasAcquisitionError, SurfaceAtlas] =
    for
      _ <- PinnedVolumeLoader.verify(request.left, leftPath)
      _ <- PinnedVolumeLoader.verify(request.right, rightPath)
      left <- FreeSurferAnnotationReader.read(leftPath).left.map(AtlasAcquisitionError.Parse.apply)
      right <- FreeSurferAnnotationReader.read(rightPath).left.map(AtlasAcquisitionError.Parse.apply)
      _ <- Either.cond(left.vertexAnnotations.length == geometry.left.vertexCount && right.vertexAnnotations.length == geometry.right.vertexCount,
        (), AtlasAcquisitionError.Parse("annotation vertex count does not match supplied left/right geometry"))
      atlas <- build(request, geometry, left, right, leftPath, rightPath).left.map(AtlasAcquisitionError.Parse.apply)
    yield atlas

  /** Reads only geometry named by a standard request after verifying its pinned
    * bytes, then loads the matching annotation pair. This is the executable
    * admission path for requests that publish geometry receipts.
    */
  def loadFromPathsWithVerifiedGeometry(
      request: StandardSurfaceAnnotationRequest,
      leftAnnotation: Path,
      rightAnnotation: Path,
      leftGeometry: Path,
      rightGeometry: Path
  ): Either[AtlasAcquisitionError, SurfaceAtlas] =
    for
      pins <- request.geometry.toRight(AtlasAcquisitionError.Parse("standard request has no pinned geometry receipt"))
      _ <- PinnedVolumeLoader.verify(pins.left, leftGeometry)
      _ <- PinnedVolumeLoader.verify(pins.right, rightGeometry)
      left <- FreeSurferSurfaceReader.readBinaryEither(leftGeometry).left.map(error => AtlasAcquisitionError.Parse(error.message))
      right <- FreeSurferSurfaceReader.readBinaryEither(rightGeometry).left.map(error => AtlasAcquisitionError.Parse(error.message))
      geometryArtifacts = Vector(
        sourceArtifact(request, pins.left, leftGeometry, ArtifactRole.Geometry),
        sourceArtifact(request, pins.right, rightGeometry, ArtifactRole.Geometry)
      )
      enriched = request.copy(ref = request.ref.withDetails(details => details.copy(artifacts = details.artifacts ++ Vector(
        AtlasArtifact(ArtifactRole.Geometry, request.sourceName, pins.left.fileName, sourceUrl = Some(pins.left.url), license = request.license.legacyString),
        AtlasArtifact(ArtifactRole.Geometry, request.sourceName, pins.right.fileName, sourceUrl = Some(pins.right.url), license = request.license.legacyString)
      ))))
      atlas <- loadFromPaths(enriched, SurfaceGiftiAtlasLoader.Geometry(left, right), leftAnnotation, rightAnnotation)
      provenance = atlas.provenance.copy(
        sources = NonEmptyVector.unsafe(atlas.provenance.sourceArtifacts ++ geometryArtifacts),
        derivation = atlas.provenance.derivation ++ geometryArtifacts.map(source => DerivationStep.Loaded(source.id))
      )
      result <- PinnedVolumeLoader.attempt(SurfaceAtlas.fromLabeledSurfaces(atlas.ref, atlas.regions, atlas.left, atlas.right, provenance))
    yield result

  private def build(request: StandardSurfaceAnnotationRequest, geometry: SurfaceGiftiAtlasLoader.Geometry,
      left: FreeSurferAnnotationReader.Annotation, right: FreeSurferAnnotationReader.Annotation,
      leftPath: Path, rightPath: Path): Either[String, SurfaceAtlas] =
    val leftNames = left.table.map(row => row.id -> row).toMap
    val rightNames = right.table.map(row => row.id -> row).toMap
    orderedCodes(request.ref, "left", left, leftNames).flatMap: leftCodes =>
      orderedCodes(request.ref, "right", right, rightNames).flatMap: rightCodes =>
        val codes = leftCodes.map(code => scalafim.atlas.Hemisphere.Left -> code) ++ rightCodes.map(code => scalafim.atlas.Hemisphere.Right -> code)
        val ids = codes.zipWithIndex.toMap
        val metadata = codes.zipWithIndex.map: entry =>
          val ((hemisphere, code), ordinal) = entry
          val row = if hemisphere == scalafim.atlas.Hemisphere.Left then leftNames(code) else rightNames(code)
          val parsed = metadataFor(request.ref, row.name)
          AtlasRegionMetadata.fromStrings(RegionId(ordinal + 1), parsed.label, Some(row.name), Some(hemisphere),
            network = parsed.network, color = parsed.color,
            attributes = parsed.attributes ++ Map("annotation_code" -> code.toString))
        def labeled(
            hemisphere: scalafim.atlas.Hemisphere,
            values: Array[Int],
            source: Map[Int, LabelInfo],
            label: String
        ): LabeledSurface =
          val map = codes.collect { case (`hemisphere`, code) => code -> (ids(hemisphere -> code) + 1) }.toMap
          val remapped = values.map(code => map.getOrElse(code, 0))
          val table = map.toVector.sortBy(_._2).map: (code, id) =>
            val row = source(code)
            LabelInfo(id, row.name, row.color)
          LabeledSurface(if hemisphere == scalafim.atlas.Hemisphere.Left then geometry.left else geometry.right,
            Array.tabulate(remapped.length)(identity), remapped, table, label)
        try
          val regions = RegionIndex(metadata)
          val ref = executedRef(request)
          val artifacts = Vector(sourceArtifact(request, request.left, leftPath), sourceArtifact(request, request.right, rightPath))
          val original = AtlasProvenance.loaded(ref, regions, regions.ids)
          val derivation = artifacts.flatMap(source => Vector(
            DerivationStep.Loaded(source.id),
            DerivationStep.ParsedLabels(source.id, LabelTableSchema("FreeSurfer annotation colour table", Vector("index", "name", "R", "G", "B", "A")))
          ))
          val provenance = original.copy(
            sources = NonEmptyVector.unsafe(artifacts),
            labels = LabelSchema.fromRegions(ref, regions, artifacts),
            derivation = derivation
          )
          Right(SurfaceAtlas.fromLabeledSurfaces(ref, regions,
            labeled(scalafim.atlas.Hemisphere.Left, left.vertexAnnotations, leftNames, "left FreeSurfer annotation"),
            labeled(scalafim.atlas.Hemisphere.Right, right.vertexAnnotations, rightNames, "right FreeSurfer annotation"), provenance))
        catch case error: IllegalArgumentException => Left(error.getMessage)

  /** The FreeSurfer colour table is the source's declared semantic ordering.
    * Packed RGBA values are lookup encodings and must never decide canonical
    * parcel order.
    */
  private def orderedCodes(
      ref: SurfaceAtlasRef,
      side: String,
      annotation: FreeSurferAnnotationReader.Annotation,
      names: Map[Int, LabelInfo]
  ): Either[String, Vector[Int]] =
    val background = annotation.table.iterator
      .filter: row =>
        row.name.startsWith("Background+") || row.name.equalsIgnoreCase("background") ||
          (ref.parcelIdentity == ParcelIdentity.GlasserHcpMmp1 && row.name == "???")
      .map(_.id)
      .toSet
    val present = annotation.vertexAnnotations.iterator.filter(code => code != 0 && !background.contains(code)).toSet
    val duplicates = annotation.table.groupBy(_.id).collect { case (id, rows) if rows.size > 1 => id }.toVector.sorted
    val missing = present.diff(names.keySet).toVector.sorted
    if duplicates.nonEmpty then Left(s"$side annotation colour table repeats codes: ${duplicates.mkString(",")}")
    else if missing.nonEmpty then Left(s"$side annotation codes absent from colour table: ${missing.mkString(",")}")
    else Right(annotation.table.iterator.map(_.id).filter(present).toVector)

  private final case class ParsedName(label: String, network: Option[NetworkId], color: Option[Rgb], attributes: Map[String, String])

  private def metadataFor(ref: SurfaceAtlasRef, name: String): ParsedName =
    if ref.parcelIdentity == ParcelIdentity.GlasserHcpMmp1 then
      ParsedName(name, None, None, Map("source_label" -> name, "atlas" -> "HCP-MMP1.0"))
    else schaeferMetadata(name)

  private def schaeferMetadata(name: String): ParsedName =
    val tokens = name.stripPrefix("7Networks_").stripPrefix("17Networks_").split("_").toVector
    val network = tokens.lift(1).map(NetworkId.apply)
    val label = if tokens.length >= 2 then tokens.takeRight(2).mkString("_") else name
    ParsedName(label, network, None, Map("source_label" -> name))

  private def executedRef(request: StandardSurfaceAnnotationRequest): SurfaceAtlasRef =
    val artifacts = Vector(request.left, request.right).map: asset =>
      AtlasArtifact(ArtifactRole.SurfaceAnnotation, request.sourceName, asset.fileName,
        sourceUrl = Some(asset.url), license = request.license.legacyString,
        notes = Some(s"Immutable revision ${asset.revision}; expected SHA-256 ${asset.sha256}"))
    request.ref.withDetails(details => details.copy(artifacts = details.artifacts ++ artifacts))

  private def sourceArtifact(
      request: StandardSurfaceAnnotationRequest,
      asset: PinnedAtlasAsset,
      path: Path,
      role: ArtifactRole = ArtifactRole.SurfaceAnnotation
  ): SourceArtifact =
    SourceArtifact(s"${role.legacy}:${asset.key}", role,
      request.sourceName, asset.fileName, sourceUri = Some(asset.url), localPath = Some(path.toString),
      digest = Some(Digest.sha256(asset.sha256)), licenseInfo = request.license,
      notes = Some(s"Immutable revision ${asset.revision}"))
