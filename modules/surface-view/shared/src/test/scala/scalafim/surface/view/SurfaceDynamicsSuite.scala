package scalafim.surface.view

import intaglio.*
import scalafim.image.*
import scalafim.surface.*

class SurfaceDynamicsSuite extends munit.FunSuite:
  private val surfaceId = SurfaceId.unsafe("left")
  private val fieldId = SurfaceTemporalFieldId.unsafe("bold")

  private def geometry(
    offset: Double = 0.0,
    faces: Seq[(Int, Int, Int)] = Seq((0, 1, 2)),
    transform: DMat = DMat.eye(4)
  ): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(
          Seq(offset + 0.0, 0.0, 0.0),
          Seq(offset + 1.0, 0.0, 0.0),
          Seq(offset + 0.0, 1.0, 0.0)
        ),
        faces
      ),
      Hemisphere.Left,
      SurfaceKind.Inflated,
      transform
    )

  private val axis = SurfaceTimeAxis.uniform(
    3,
    step = SurfaceSeconds.unsafe(1.0)
  ).toOption.get

  private def temporalMatrix(target: SurfaceGeometry): SurfaceMatrix[Double] =
    SurfaceMatrix.fromRows(
      target,
      Vector(VertexId(0), VertexId(1), VertexId(2)),
      Vector(
        Vector(0.0, 10.0, 20.0),
        Vector(1.0, 11.0, 21.0),
        Vector(2.0, 12.0, 22.0)
      )
    )

  test("time axes are strictly increasing and resolve nearest and linear samples"):
    assert(SurfaceTimeAxis.make(Vector.empty).isLeft)
    assert(SurfaceTimeAxis.make(Vector(
      SurfaceSeconds.unsafe(0.0),
      SurfaceSeconds.unsafe(0.0)
    )).isLeft)
    assertEquals(
      axis.sample(SurfaceSeconds.unsafe(0.6), SurfaceInterpolation.Nearest).toOption.get,
      SurfaceFrameSample(1, 1, 0.0)
    )
    val linear = axis.sample(SurfaceSeconds.unsafe(0.25), SurfaceInterpolation.Linear).toOption.get
    assertEquals(linear.lowerFrame, 0)
    assertEquals(linear.upperFrame, 1)
    assertEqualsDouble(linear.alpha, 0.25, 0.0)
    assert(axis.sample(SurfaceSeconds.unsafe(3.0), SurfaceInterpolation.Linear).isLeft)

  test("SurfaceMatrix temporal fields read columns lazily"):
    val target = geometry()
    val field = SurfaceTemporalField.fromMatrix(fieldId, temporalMatrix(target), axis).toOption.get
    val sampled = SurfaceFrameCache.Disabled.sample(
      field,
      SurfaceSeconds.unsafe(0.5),
      SurfaceInterpolation.Linear
    ).toOption.get
    assertEqualsDouble(sampled.frame(0), 5.0, 0.0)
    assertEqualsDouble(sampled.frame(1), 6.0, 0.0)
    assertEqualsDouble(sampled.frame(2), 7.0, 0.0)
    assertEquals(sampled.receipt.sourceReads, 2)
    assertEquals(sampled.cache.size, 0)

  test("lazy sources validate frame shape and bounded LRU caches evict deterministically"):
    val target = geometry()
    var reads = 0
    val source = new SurfaceDoubleFrameSource:
      val frameCount = 3
      def readFrame(index: Int): Either[SurfaceViewError, Array[Double]] =
        reads += 1
        Right(Array.fill(target.vertexCount)(index.toDouble))
    val field = SurfaceTemporalField.lazyFrames(fieldId, target, axis, source).toOption.get
    val cache0 = SurfaceFrameCache.empty(2)
    val first = cache0.prefetch(field, Vector(0, 1)).toOption.get
    assertEquals(first.cache.cachedFrames(fieldId), Vector(0, 1))
    assertEquals(first.receipt.sourceReads, 2)
    val hit = first.cache.prefetch(field, Vector(0)).toOption.get
    assertEquals(hit.receipt.hits, 1)
    assertEquals(reads, 2)
    val evicted = hit.cache.prefetch(field, Vector(2)).toOption.get
    assertEquals(evicted.cache.size, 2)
    assertEquals(evicted.cache.cachedFrames(fieldId), Vector(0, 2))
    assertEquals(evicted.receipt.evictedFrames, Vector(1))

    val badSource = new SurfaceDoubleFrameSource:
      val frameCount = 3
      def readFrame(index: Int): Either[SurfaceViewError, Array[Double]] = Right(Array(1.0))
    val bad = SurfaceTemporalField.lazyFrames(fieldId, target, axis, badSource).toOption.get
    assert(SurfaceFrameCache.Disabled.sample(bad, axis.first, SurfaceInterpolation.Nearest).isLeft)

  test("playback reducer clamps, loops, reverses, and produces bounded prefetch requests"):
    val initial = SurfacePlaybackState.initial(axis)
    val playing = SurfacePlayback.reduce(axis, initial, SurfacePlaybackAction.Play).toOption.get
    val advanced = SurfacePlayback.reduce(
      axis,
      playing,
      SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(0.75))
    ).toOption.get
    assertEqualsDouble(advanced.playhead.value, 0.75, 0.0)
    val stopped = SurfacePlayback.reduce(
      axis,
      advanced,
      SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(5.0))
    ).toOption.get
    assertEquals(stopped.status, SurfacePlaybackStatus.Paused)
    assertEqualsDouble(stopped.playhead.value, 2.0, 0.0)

    val reverse = playing.copy(
      playhead = SurfaceSeconds.unsafe(0.25),
      rate = SurfacePlaybackRate.unsafe(-1.0),
      looping = true
    )
    val wrapped = SurfacePlayback.reduce(
      axis,
      reverse,
      SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(0.5))
    ).toOption.get
    assertEqualsDouble(wrapped.playhead.value, 1.75, 1e-12)
    assertEquals(SurfacePlayback.prefetchFrames(axis, reverse, 4), Vector.empty)
    assert(SurfacePlayback.reduce(
      axis,
      playing,
      SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(-1.0))
    ).isLeft)

  test("prefetched temporal materialization distinguishes source reads from layer uploads"):
    val target = geometry()
    val field = SurfaceTemporalField.fromMatrix(fieldId, temporalMatrix(target), axis).toOption.get
    val colorizer = ScalarColorizer(DisplayWindow.unsafe(0.0, 22.0))
    val first = SurfacePlayback.prepareScalar(
      field,
      SurfaceLayerId.unsafe("dynamic"),
      surfaceId,
      SurfaceSeconds.unsafe(0.5),
      SurfaceInterpolation.Linear,
      colorizer,
      SurfaceFrameCache.empty(3),
      prefetchFrames = Vector(2)
    ).toOption.get
    assertEquals(first.receipt.cache.sourceReads, 3)
    assert(first.receipt.layerUploadRequired)
    assertEquals(first.cache.size, 3)

    val repeated = SurfacePlayback.prepareScalar(
      field,
      SurfaceLayerId.unsafe("dynamic"),
      surfaceId,
      SurfaceSeconds.unsafe(0.5),
      SurfaceInterpolation.Linear,
      colorizer,
      first.cache,
      previousSample = Some(first.receipt.sample)
    ).toOption.get
    assertEquals(repeated.receipt.cache.sourceReads, 0)
    assertEquals(repeated.receipt.cache.hits, 2)
    assert(!repeated.receipt.layerUploadRequired)

    val next = SurfacePlayback.prepareScalar(
      field,
      SurfaceLayerId.unsafe("dynamic"),
      surfaceId,
      SurfaceSeconds.unsafe(1.5),
      SurfaceInterpolation.Linear,
      colorizer,
      repeated.cache,
      previousSample = Some(repeated.receipt.sample)
    ).toOption.get
    assertEquals(next.receipt.cache.sourceReads, 0)
    assert(next.receipt.layerUploadRequired)

  test("morphing preserves exact endpoints and rejects topology or transform mismatch"):
    val from = geometry(0.0)
    val to = geometry(2.0)
    assertEquals(SurfaceMorph.interpolate(from, to, SurfaceMorphFraction.unsafe(0.0)), Right(from))
    assertEquals(SurfaceMorph.interpolate(from, to, SurfaceMorphFraction.unsafe(1.0)), Right(to))
    val midpoint = SurfaceMorph.interpolate(from, to, SurfaceMorphFraction.unsafe(0.5)).toOption.get
    assertEqualsDouble(midpoint.mesh.vertex(VertexId(0)).x, 1.0, 0.0)
    assert(midpoint.mesh.hasSameTopology(from.mesh))
    val forward = SurfaceMorph.interpolate(from, to, SurfaceMorphFraction.unsafe(0.25)).toOption.get
    val reverse = SurfaceMorph.interpolate(to, from, SurfaceMorphFraction.unsafe(0.75)).toOption.get
    assertEquals(forward.mesh.coordinates.toVector, reverse.mesh.coordinates.toVector)

    val rewound = geometry(2.0, faces = Seq((0, 2, 1)))
    assert(SurfaceMorph.interpolate(from, rewound, SurfaceMorphFraction.unsafe(0.5)).isLeft)
    val translated = geometry(2.0, transform = DMat.fromRows(Vector(
      Vector(1.0, 0.0, 0.0, 1.0),
      Vector(0.0, 1.0, 0.0, 0.0),
      Vector(0.0, 0.0, 1.0, 0.0),
      Vector(0.0, 0.0, 0.0, 1.0)
    )))
    assert(SurfaceMorph.interpolate(from, translated, SurfaceMorphFraction.unsafe(0.5)).isLeft)

  test("linked surface and image selections roundtrip through world coordinates without flips"):
    val transform = DMat.fromRows(Vector(
      Vector(1.0, 0.0, 0.0, 10.0),
      Vector(0.0, 1.0, 0.0, 20.0),
      Vector(0.0, 0.0, 1.0, 30.0),
      Vector(0.0, 0.0, 0.0, 1.0)
    ))
    val target = geometry(transform = transform)
    val selection = SurfaceSelection(surfaceId, VertexId(2))
    val volume = VolumeSpace(NeuroSpace(Vector(64, 64, 64)))
    val linked = SurfaceWorldLink.toVolume(selection, target, volume).toOption.get
    assertEquals(linked.world, WorldPoint(10.0, 21.0, 30.0))
    assertEquals(linked.voxel, VoxelPoint(10.0, 21.0, 30.0))
    assertEquals(
      SurfaceWorldLink.nearestVertex(surfaceId, target, linked.world, SurfaceLinkRadius.unsafe(0.0)),
      Right(selection)
    )
    assert(SurfaceWorldLink.nearestVertex(
      surfaceId,
      target,
      WorldPoint(100.0, 100.0, 100.0),
      SurfaceLinkRadius.unsafe(1.0)
    ).isLeft)

  test("typed annotations and bounded selection histories retain reproducible sequence ids"):
    val selection = SurfaceSelection(surfaceId, VertexId(1))
    val annotationId = SurfaceAnnotationId.unsafe("peak")
    assert(SurfaceAnnotation.make(annotationId, selection, "", Rgba32.unsafe(255, 0, 0)).isLeft)
    val annotation = SurfaceAnnotation.make(
      annotationId,
      selection,
      "peak activation",
      Rgba32.unsafe(255, 0, 0)
    ).toOption.get
    assertEquals(annotation.label, "peak activation")
    val layer = SurfaceAnnotations.layer(
      SurfaceLayerId.unsafe("annotations"),
      surfaceId,
      geometry(),
      Vector(annotation)
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, geometry()).toOption.get),
      Vector(layer)
    ).toOption.get
    val plan = SurfaceCompiler.compile(model, SurfaceViewerState.initial(model)).toOption.get
    assertEquals(plan.layers.head.colors(1), Rgba32.unsafe(255, 0, 0).toPackedInt)
    assertEquals(plan.layers.head.colors(0), Rgba32.unsafe(0, 0, 0, 0).toPackedInt)
    val history0 = SurfaceSelectionHistory.make(2).toOption.get
    val history1 = history0.append(selection, WorldPoint(0.0, 0.0, 0.0), Some(annotationId))
    val history2 = history1.append(selection, WorldPoint(1.0, 0.0, 0.0))
    val history3 = history2.append(selection, WorldPoint(2.0, 0.0, 0.0))
    assertEquals(history3.records.map(_.sequence), Vector(1L, 2L))
    assertEquals(history3.records.head.world, WorldPoint(1.0, 0.0, 0.0))
