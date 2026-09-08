package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.Matrix
import multivar.core.ValueId
import scalafim.atlas.{Region as AtlasRegion, *}
import scalafim.fmri.mvpa.*
import scalafim.image.*
import scalafim.locus.{DomainFactory, FiniteSpace, Parcellation, Region as LocusRegion, Selection, SpaceKey}
import scalafim.surface.{
  DistanceMetric,
  Hemisphere as SurfaceHemisphere,
  LabelInfo,
  LabeledSurface,
  MeshTopology,
  SurfaceGeometry,
  SurfaceKind,
  SurfaceLocusDomain,
  TriangleMesh,
  VertexId
}

class SpatialFramesSuite extends munit.FunSuite:

  private val neuroSpace =
    NeuroSpace(
      dims = Vector(3, 2, 1),
      spacing = Some(Vector(1.0, 1.0, 1.0)),
      origin = Some(Vector(0.0, 0.0, 0.0))
    )
  private val volumeSpace = VolumeSpace(neuroSpace)
  private val packedVolume =
    VolumeDomain.semantic(SpaceKey.unsafe("mvpa-spatial-volume"), volumeSpace)
  private type Voxel = packedVolume.S
  private val volumeDomain: VolumeDomain[Voxel] = packedVolume.value
  private val volumeAxis: IdentifiedLocusAxis[Voxel] =
    SpatialAxes.volume(volumeDomain).toOption.get

  private val parcelResolution =
    DomainFactory.unsafeRestore(SpaceKey.unsafe("mvpa-spatial-parcels"), 2)
  private type Parcel = parcelResolution.S
  private val parcels: FiniteSpace[Parcel] = parcelResolution.space

  private val surfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(1.0, 1.0, 0.0)
        ),
        Vector((0, 1, 2), (1, 3, 2))
      ),
      SurfaceHemisphere.Left,
      SurfaceKind.Pial
    )
  private val surfaceTopology = MeshTopology.from(surfaceGeometry.mesh)
  private val packedSurface =
    SurfaceLocusDomain
      .semantic(
        SpaceKey.unsafe("mvpa-spatial-surface"),
        surfaceGeometry
      )
      .toOption
      .get
  private type Vertex = packedSurface.S
  private val surfaceDomain: SurfaceLocusDomain[Vertex] = packedSurface.value
  private val surfaceAxis: IdentifiedLocusAxis[Vertex] =
    SpatialAxes.surface(surfaceDomain).toOption.get

  private def labelVolume: NeuroVol[Int] =
    NeuroVol.fromLinear(
      PrimitiveBuffers.fromArray(Array(0, 2, 1, 2, 1, 0)),
      neuroSpace
    )

  private def toyVolumeAtlas: VolumeAtlas =
    val regions =
      RegionIndex(
        Vector(
          AtlasRegion(RegionId(1), "semantic"),
          AtlasRegion(RegionId(2), "visual")
        )
      )
    val ref =
      AtlasRef(
        family = "toy",
        model = "toy",
        representation = AtlasRepresentation.Volume,
        templateSpace = SpaceId.Custom,
        coordSpace = SpaceId.Custom,
        confidence = Confidence.Exact
      )
    VolumeAtlas.fromLabelVolume(ref, regions, labelVolume, label = "toy")

  private def sampleAxis: AxisRef[SampleId] =
    AxisRef
      .create(
        AxisId.unsafe("spatial-frame-samples"),
        AxisPurpose.Samples,
        Vector(SampleId.unsafe("sample-a"), SampleId.unsafe("sample-b")),
        CoordinateBasis.unsafe("trial-table"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("fixture", "v1")
      )
      .toOption
      .get

  private def matrix(rows: Seq[Seq[Double]]): DMat =
    require(rows.nonEmpty)
    require(rows.forall(_.length == rows.head.length))
    val builder = Matrix.newBuilder(rows.length, rows.head.length)
    var row = 0
    while row < rows.length do
      var column = 0
      while column < rows.head.length do
        builder(row, column) = rows(row)(column)
        column += 1
      row += 1
    builder.result()

  test("locus owner, coordinates, and geometry all participate in neural-axis identity"):
    val fields = volumeAxis.features.identity.basis.fields.map(field => field.name -> field.value).toMap
    assertEquals(fields("locus-domain-id"), volumeDomain.finiteSpace.id.value)
    assertEquals(fields("locus-domain-size"), "6")
    assert(fields.contains("locus-domain-fingerprint"))
    assertEquals(fields("dimensions"), "3x2x1")
    assert(fields.contains("affine-row-major"))
    val domainKey = volumeDomain.finiteSpace.key
    val coordinateProvenance = volumeAxis.features.identity.coordinateProvenance
    assertEquals(coordinateProvenance.source, s"locus-domain:${domainKey.id.value}")
    assertEquals(
      coordinateProvenance.revision,
      domainKey.fingerprint.map(_.value).getOrElse(s"size-${domainKey.size}")
    )

    val foreignPacked =
      VolumeDomain.semantic(SpaceKey.unsafe("equal-size-foreign-volume"), volumeSpace)
    val foreignAxis = SpatialAxes.volume(foreignPacked.value).toOption.get
    assertNotEquals(
      volumeAxis.features.identity.fingerprint,
      foreignAxis.features.identity.fingerprint
    )

    val rescaledSpace =
      VolumeSpace(
        NeuroSpace(
          dims = Vector(3, 2, 1),
          spacing = Some(Vector(2.0, 1.0, 1.0)),
          origin = Some(Vector(0.0, 0.0, 0.0))
        )
      )
    val rescaledPacked =
      VolumeDomain.semantic(SpaceKey.unsafe("mvpa-spatial-volume"), rescaledSpace)
    val rescaledAxis = SpatialAxes.volume(rescaledPacked.value).toOption.get
    assertNotEquals(
      volumeAxis.features.identity.fingerprint,
      rescaledAxis.features.identity.fingerprint
    )

  test("regions use ambient order and selections preserve declared order"):
    val region =
      LocusRegion
        .fromOrdinals(volumeDomain.finiteSpace, Vector(4, 1, 3))
        .toOption
        .get
    val selection =
      Selection
        .fromOrdinals(volumeDomain.finiteSpace, Vector(4, 1, 3))
        .toOption
        .get
    val regionFrame =
      LocusFrames
        .region(
          volumeAxis,
          MeasurementId.unsafe("language-region"),
          region,
          Some("language")
        )
        .toOption
        .get
    val selectionFrame =
      LocusFrames
        .selection(
          volumeAxis,
          MeasurementId.unsafe("ordered-selection"),
          selection,
          Some("ordered")
        )
        .toOption
        .get

    assertEquals(
      regionFrame.entries.head.measurement.local.identity.orderedKeys.map(_.value),
      Vector(
        volumeAxis.features.keys(1).value,
        volumeAxis.features.keys(3).value,
        volumeAxis.features.keys(4).value
      )
    )
    assertEquals(
      selectionFrame.entries.head.measurement.local.identity.orderedKeys.map(_.value),
      Vector(
        volumeAxis.features.keys(4).value,
        volumeAxis.features.keys(1).value,
        volumeAxis.features.keys(3).value
      )
    )
    assertEquals(regionFrame.entries.head.rendition.label, Some("language"))
    assertEquals(selectionFrame.entries.head.rendition.label, Some("ordered"))

  test("parcellations lower directly to hard measurement legs"):
    val parcellation =
      Parcellation
        .fromAssignments(
          volumeDomain.finiteSpace,
          parcels,
          Vector(Some(0), None, Some(1), Some(0), Some(1), None)
        )
        .toOption
        .get
    val frame =
      LocusFrames
        .parcellation(
          volumeAxis,
          parcellation,
          parcel => Some(s"parcel-${parcel.ordinal}")
        )
        .toOption
        .get

    assertEquals(
      frame.entries.map(_.measurement.identity.id.value),
      Vector("parcel-0", "parcel-1")
    )
    assertEquals(
      frame.entries.map(_.rendition.support.ordinalsInDomainOrder.toVector),
      Vector(Vector(0, 3), Vector(2, 4))
    )
    assert(frame.entries.forall(_.measurement.identity.kind == MeasurementKind.HardSelection))

  test("searchlight frames are canonical while rendition retains center and scatter order"):
    val radius = SearchlightRadius.make(0.0).toOption.get
    val firstCenters =
      Selection
        .fromOrdinals(volumeDomain.finiteSpace, Vector(4, 1))
        .toOption
        .get
    val reversedCenters =
      Selection
        .fromOrdinals(volumeDomain.finiteSpace, Vector(1, 4))
        .toOption
        .get
    val first =
      VolumeFrames
        .metricSearchlights(volumeAxis, volumeDomain, radius, firstCenters)
        .toOption
        .get
    val reversed =
      VolumeFrames
        .metricSearchlights(volumeAxis, volumeDomain, radius, reversedCenters)
        .toOption
        .get

    assertEquals(first.identity, reversed.identity)
    assertEquals(
      first.entries.map(_.measurement.identity.id.value),
      Vector("searchlight-1", "searchlight-4")
    )
    assertEquals(first.entries.map(_.rendition.ambientCenter.ordinal), Vector(1, 4))
    assertEquals(first.entries.map(_.rendition.center.ordinal), Vector(1, 0))
    assertEquals(first.entries.map(_.rendition.ambientCenter.ordinal), Vector(1, 4))
    assertEquals(
      first.entries.map(_.rendition.support.ordinalsInDomainOrder.toVector),
      Vector(Vector(1), Vector(4))
    )

  test("empty searchlights fail explicitly while singleton searchlights remain valid"):
    val radius = SearchlightRadius.make(0.0).toOption.get
    val empty = Selection.empty(volumeDomain.finiteSpace).toOption.get
    val singleton =
      Selection
        .fromOrdinals(volumeDomain.finiteSpace, Vector(3))
        .toOption
        .get

    assert(
      VolumeFrames
        .metricSearchlights(volumeAxis, volumeDomain, radius, empty)
        .left
        .exists:
          case SpatialFrameError.Measurement(MeasurementError.EmptyFrame) => true
          case _                                                          => false
    )

    val frame =
      VolumeFrames
        .metricSearchlights(volumeAxis, volumeDomain, radius, singleton)
        .toOption
        .get
    assertEquals(frame.size, 1)
    assertEquals(frame.entries.head.rendition.ambientCenter.ordinal, 3)
    assert(frame.entries.head.rendition.support.contains(volumeDomain.finiteSpace.indexOption(3).get))

  test("equal-size foreign locus owners fail closed before ordinal use"):
    val foreignPacked =
      VolumeDomain.semantic(SpaceKey.unsafe("foreign-runtime-owner"), volumeSpace)
    val foreign = foreignPacked.value
    val foreignRegion =
      LocusRegion
        .fromOrdinals(foreign.finiteSpace, Vector(0, 2))
        .toOption
        .get
        .asInstanceOf[LocusRegion[Voxel]]

    assert(
      LocusFrames
        .region(
          volumeAxis,
          MeasurementId.unsafe("foreign-region"),
          foreignRegion
        )
        .left
        .exists:
          case SpatialFrameError.WrongDomain(_, _) => true
          case _                                   => false
    )

  test("volume ROI, labels, and atlas builders emit typed renditions directly"):
    val roi = VoxelRegion.make(volumeSpace, Array(5, 0)).toOption.get
    val roiFrame =
      VolumeFrames
        .roi(
          volumeAxis,
          volumeDomain,
          MeasurementId.unsafe("volume-roi"),
          roi,
          Some("edge")
        )
        .toOption
        .get
    val labelsFrame =
      VolumeFrames.labels(volumeAxis, volumeDomain, labelVolume).toOption.get
    val atlas = toyVolumeAtlas
    val atlasFrame =
      VolumeFrames.atlas(volumeAxis, volumeDomain, atlas).toOption.get

    assertEquals(
      roiFrame.entries.head.rendition.support.ordinalsInDomainOrder.toVector,
      Vector(0, 5)
    )
    assertEquals(
      labelsFrame.entries.map(_.measurement.identity.id.value),
      Vector("volume-label-1", "volume-label-2")
    )
    assertEquals(
      labelsFrame.entries.map(_.rendition.support.ordinalsInDomainOrder.toVector),
      Vector(Vector(2, 4), Vector(1, 3))
    )
    assertEquals(
      atlasFrame.entries.map(_.measurement.identity.id.value),
      Vector("atlas-region-1", "atlas-region-2")
    )
    assertEquals(
      atlasFrame.entries.map(_.rendition.region.label),
      Vector("semantic", "visual")
    )
    assert(atlasFrame.entries.forall(_.rendition.atlas == atlas.ref))

  test("spatial frame measurements compose directly with typed evidence"):
    val frame =
      VolumeFrames.labels(volumeAxis, volumeDomain, labelVolume).toOption.get
    val patterns =
      matrix(
        Vector(
          Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0),
          Vector(11.0, 12.0, 13.0, 14.0, 15.0, 16.0)
        )
      )
    val evidence =
      EvidenceTable
        .dense(
          sampleAxis,
          volumeAxis.features,
          patterns,
          ValueId.unsafe("spatial-frame-evidence")
        )
        .toOption
        .get
    val measured =
      evidence.measureColumns(frame.entries.head.measurement).toOption.get
    val summed =
      measured
        .rightMultiply(matrix(Vector(Vector(1.0), Vector(1.0))))
        .toOption
        .get

    assertEquals(measured.columns.identity, frame.entries.head.measurement.local.identity)
    assertEqualsDouble(summed(0, 0), 8.0, 1e-12, clue = "")
    assertEqualsDouble(summed(1, 0), 28.0, 1e-12, clue = "")

  test("surface labels and metric neighborhoods use the same frame model"):
    val labeled =
      LabeledSurface.fromIndexed(
        surfaceGeometry,
        Vector(VertexId(0), VertexId(1), VertexId(2), VertexId(3)),
        Vector(2, 2, 1, 1),
        Vector(LabelInfo(1, "posterior"), LabelInfo(2, "anterior"))
      )
    val parcels =
      SurfaceFrames
        .labeled(surfaceAxis, surfaceDomain, labeled)
        .toOption
        .get
    val centers =
      Selection
        .fromOrdinals(surfaceDomain.finiteSpace, Vector(3, 1))
        .toOption
        .get
    val searchlights =
      SurfaceFrames
        .metricSearchlights(
          surfaceAxis,
          surfaceDomain,
          surfaceTopology,
          0.0,
          centers,
          DistanceMetric.Geodesic
        )
        .toOption
        .get

    assertEquals(
      parcels.entries.map(_.measurement.identity.id.value),
      Vector("surface-label-1", "surface-label-2")
    )
    assertEquals(
      parcels.entries.map(_.rendition.support.ordinalsInDomainOrder.toVector),
      Vector(Vector(2, 3), Vector(0, 1))
    )
    assertEquals(searchlights.entries.map(_.rendition.ambientCenter.ordinal), Vector(1, 3))
    assertEquals(searchlights.entries.map(_.rendition.center.ordinal), Vector(1, 0))
    assertEquals(
      searchlights.entries.map(_.rendition.support.ordinalsInDomainOrder.toVector),
      Vector(Vector(1), Vector(3))
    )
