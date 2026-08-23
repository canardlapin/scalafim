package scalafim.image.view

import intaglio.*
import ravel.NDArray as RavelArray
import scalafim.image.*

class AdvancedViewSuite extends munit.FunSuite:

  private def volume(
    space: VolumeSpace,
    label: String
  )(value: (Int, Int, Int) => Double): NeuroVol[Double] =
    val shape = space.shape
    val data = PrimitiveBuffers.tabulate[Double](shape.product) { index =>
      val z = index % shape.z
      val xy = index / shape.z
      val y = xy % shape.y
      val x = xy / shape.y
      value(x, y, z)
    }
    NeuroVol.copyFromCanonicalArray(data, space.toNeuroSpace, label)

  private def layer(
    id: String,
    source: NeuroVol[Double],
    mapping: LayerMapping = LayerMapping.WorldAligned
  ): SliceLayer =
    SliceLayer(
      LayerId.unsafe(id),
      source,
      SliceSampling.Nearest(-1.0),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 4.0)),
      mapping = mapping
    )

  private def images(frame: ViewerFrame, plane: AnatomicalPlane): Vector[Grob.Image] =
    val index = frame.panels.all.indexWhere(_.anatomicalPlane == plane)
    frame.scene.grobs(index).asInstanceOf[Grob.Group].children.collect { case image: Grob.Image => image }

  test("lazy frame sources load only requested timepoints and report failures") {
    val space = VolumeSpace(NeuroSpace(Vector(3, 3, 2)))
    val frames = Vector(
      volume(space, "zero")((_, _, _) => 0.0),
      volume(space, "one")((_, _, _) => 1.0)
    )
    var reads = Vector.empty[Int]
    val source = VolumeSource.lazyFrames(space, 2) { index =>
      reads = reads :+ index
      Right(frames(index))
    }.toOption.get
    val lazyLayer = SliceLayer.fromSource(
      LayerId.unsafe("lazy"),
      source,
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 1.0))
    )
    val model = ViewerModel.unsafe(space, Vector(lazyLayer))
    val state = ViewerState.centered(space)
    val device = DeviceContext.unsafe(600.0, 400.0)
    val cache = ViewerCache.empty(6)

    val first = ViewerCompiler.compileCached(model, state, device, cache).toOption.get
    assertEquals(reads, Vector(0))
    assertEquals(first.profile.sourceReads, 1)
    val second = ViewerCompiler.compileCached(model, state, device, first.cache).toOption.get
    assertEquals(reads, Vector(0))
    assertEquals(second.profile.cacheHits, 3)
    assertEquals(second.profile.sourceReads, 0)

    val third = ViewerCompiler.compileCached(model, state.copy(timepoint = 1), device, second.cache).toOption.get
    assertEquals(reads, Vector(0, 1))
    assertEquals(third.profile.cacheMisses, 3)
    assertEquals(third.profile.sourceReads, 1)
    assert(VolumeSource.lazyFrames[Double](space, 0)(_ => Left("unused")).isLeft)
  }

  test("temporal models reject implicit one-frame sources but accept explicit invariants") {
    val space = VolumeSpace(NeuroSpace(Vector(2, 2, 2)))
    val frame = volume(space, "frame")((_, _, _) => 1.0)
    val temporal = SliceLayer.series(
      LayerId.unsafe("temporal"),
      frame.concat(frame),
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 2.0))
    )
    val oneFrameSource = VolumeSource.lazyFrames(space, 1)(_ => Right(frame)).toOption.get
    val implicitTemporal = SliceLayer.fromSource(
      LayerId.unsafe("implicit-temporal"),
      oneFrameSource,
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 2.0))
    )
    val explicitInvariant = SliceLayer.series(
      LayerId.unsafe("invariant"),
      frame.toVec,
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 2.0))
    )

    assert(ViewerModel.make(space, Vector(temporal, implicitTemporal)).isLeft)
    val model = ViewerModel.make(space, Vector(temporal, explicitInvariant)).toOption.get
    assertEquals(model.timepointCount, 2)
  }

  test("cache profiles separate sampling colorization and plane-specific redraw work") {
    val space = VolumeSpace(NeuroSpace(Vector(4, 3, 2)))
    val source = volume(space, "source")((x, y, z) => x + y + z)
    val model = ViewerModel.unsafe(space, Vector(layer("one", source), layer("two", source)))
    val state = ViewerState.centered(space)
    val device = DeviceContext.unsafe(800.0, 500.0)

    val cold = ViewerCompiler.compileCached(model, state, device, ViewerCache.empty(12)).toOption.get
    assertEquals(cold.profile.panelCount, 3)
    assertEquals(cold.profile.layerRequests, 6)
    assertEquals(cold.profile.cacheHits, 0)
    assertEquals(cold.profile.cacheMisses, 6)
    assert(cold.profile.sampledPixels > 0L)
    assertEquals(cold.profile.sampleCacheHits, 0)
    assertEquals(cold.profile.sampleCacheMisses, 6)
    assertEquals(cold.profile.colorizedPixels, cold.profile.sampledPixels)
    assertEquals(cold.profile.sourceReads, 2)
    assertEquals(cold.cache.size, 6)
    assertEquals(cold.cache.sampledSliceCount, 6)

    val warm = ViewerCompiler.compileCached(model, state, device, cold.cache).toOption.get
    assertEquals(warm.profile.cacheHits, 6)
    assertEquals(warm.profile.cacheMisses, 0)
    assertEquals(warm.profile.sampledPixels, 0L)
    assertEquals(warm.profile.colorizedPixels, 0L)
    assertEquals(warm.profile.sourceReads, 0)
    assertEqualsDouble(warm.profile.hitRate, 1.0, 0.0)

    val resized = ViewerCompiler.compileCached(
      model,
      state,
      DeviceContext.unsafe(500.0, 900.0),
      warm.cache
    ).toOption.get
    assertEquals(resized.profile.cacheHits, 6)

    val windowedState = state.copy(
      layerPresentation = Map(
        LayerId.unsafe("one") -> LayerPresentation(window = Some(DisplayWindow.unsafe(0.0, 8.0)))
      )
    )
    val windowed = ViewerCompiler.compileCached(model, windowedState, device, resized.cache).toOption.get
    assertEquals(windowed.profile.cacheHits, 3)
    assertEquals(windowed.profile.cacheMisses, 3)
    assertEquals(windowed.profile.sampleCacheHits, 3)
    assertEquals(windowed.profile.sampleCacheMisses, 0)
    assertEquals(windowed.profile.sampledPixels, 0L)
    assert(windowed.profile.colorizedPixels > 0L)
    assertEquals(windowed.profile.sourceReads, 0)

    val thresholdedState = windowedState.copy(
      layerPresentation = windowedState.layerPresentation.updated(
        LayerId.unsafe("one"),
        LayerPresentation(
          window = Some(DisplayWindow.unsafe(0.0, 8.0)),
          threshold = Some(DisplayThreshold.transparentBand(1.0, 2.0).toOption.get)
        )
      )
    )
    val thresholded = ViewerCompiler.compileCached(
      model,
      thresholdedState,
      device,
      windowed.cache
    ).toOption.get
    assertEquals(thresholded.profile.cacheHits, 3)
    assertEquals(thresholded.profile.cacheMisses, 3)
    assertEquals(thresholded.profile.sampleCacheHits, 3)
    assertEquals(thresholded.profile.sampledPixels, 0L)
    assert(thresholded.profile.colorizedPixels > 0L)
    assertEquals(thresholded.profile.sourceReads, 0)

    val moved = ViewerCompiler.compileCached(
      model,
      state.copy(cursor = state.cursor + AnatomicalDirection.Superior.unit.scaled(1.0)),
      device,
      thresholded.cache
    ).toOption.get
    assertEquals(moved.profile.cacheHits, 4)
    assertEquals(moved.profile.cacheMisses, 2)
    assertEquals(moved.profile.sampleCacheHits, 0)
    assertEquals(moved.profile.sampleCacheMisses, 2)
    assertEquals(moved.profile.sourceReads, 2)

    val coldMoved = ViewerCompiler.compile(
      model,
      state.copy(cursor = state.cursor + AnatomicalDirection.Superior.unit.scaled(1.0)),
      device
    ).toOption.get
    AnatomicalPlane.values.foreach { plane =>
      assertEquals(images(moved.frame, plane).map(_.image), images(coldMoved, plane).map(_.image))
    }
    assert(images(moved.frame, AnatomicalPlane.Sagittal).head.image.eq(images(warm.frame, AnatomicalPlane.Sagittal).head.image))
    assert(images(moved.frame, AnatomicalPlane.Coronal).head.image.eq(images(warm.frame, AnatomicalPlane.Coronal).head.image))
    assert(!images(moved.frame, AnatomicalPlane.Axial).head.image.eq(images(warm.frame, AnatomicalPlane.Axial).head.image))
    assert(ViewerCache.make(-1).isLeft)
  }

  test("nonlinear pullback layers sample reference grids in their own source coordinates") {
    val space = VolumeSpace(NeuroSpace(Vector(5, 3, 1)))
    val source = volume(space, "x")((x, _, _) => x.toDouble)
    val fieldGrid = GridSpec.fromVolumeSpace(space)
    val field =
      RavelArray.tabulate[Double](
        fieldGrid.shape.x,
        fieldGrid.shape.y,
        fieldGrid.shape.z,
        3
      ) { (x, _, _, component) =>
        if component == 0 && x >= 2 then 1.0 else 0.0
      }
    val morphism = DenseFieldMorphism.displacement(
      SpatialDomainId("source"),
      SpatialDomainId("reference"),
      fieldGrid,
      field,
      Resample.Method.Nearest
    ).toOption.get
    val model = ViewerModel.unsafe(
      space,
      Vector(
        layer("aligned", source),
        layer("warped", source, LayerMapping.Pullback(morphism))
      )
    )
    val frame = ViewerCompiler.compile(
      model,
      ViewerState.centered(space),
      DeviceContext.unsafe(600.0, 400.0)
    ).toOption.get
    val axial = images(frame, AnatomicalPlane.Axial)

    assertEquals(axial(0).image.pixelUnsafe(2, 1).red, 128)
    assertEquals(axial(1).image.pixelUnsafe(2, 1).red, 191)
    assertEquals(axial(1).image.pixelUnsafe(4, 1).red, 0)
  }

  test("cache reuse preserves left-right convention and asymmetric raster orientation") {
    val space = VolumeSpace(NeuroSpace(Vector(5, 3, 2)))
    val source = volume(space, "asymmetric-x")((x, _, _) => x.toDouble)
    val model = ViewerModel.unsafe(space, Vector(layer("anatomy", source)))
    val leftState = ViewerState.centered(space, LeftRightConvention.PatientLeftOnLeft)
    val rightState = leftState.copy(convention = LeftRightConvention.PatientRightOnLeft)
    val device = DeviceContext.unsafe(640.0, 480.0)

    val left = ViewerCompiler.compileCached(model, leftState, device, ViewerCache.empty(8)).toOption.get
    val cachedRight = ViewerCompiler.compileCached(model, rightState, device, left.cache).toOption.get
    val coldRight = ViewerCompiler.compile(model, rightState, device).toOption.get

    assertEquals(cachedRight.profile.cacheHits, 1)
    assertEquals(cachedRight.profile.cacheMisses, 2)
    AnatomicalPlane.values.foreach { plane =>
      assertEquals(images(cachedRight.frame, plane).map(_.image), images(coldRight, plane).map(_.image))
    }
    val leftAxial = images(left.frame, AnatomicalPlane.Axial).head.image
    val rightAxial = images(cachedRight.frame, AnatomicalPlane.Axial).head.image
    assertEquals(leftAxial.pixelUnsafe(0, 0), rightAxial.pixelUnsafe(rightAxial.width - 1, 0))
    assertEquals(leftAxial.pixelUnsafe(leftAxial.width - 1, 0), rightAxial.pixelUnsafe(0, 0))
  }

  test("linked view policies synchronize only declared state") {
    val space = VolumeSpace(NeuroSpace(Vector(3, 3, 3)))
    val source = volume(space, "source")((_, _, _) => 1.0)
    val temporal = SliceLayer.series(
      LayerId.unsafe("temporal"),
      source.concat(source),
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 2.0))
    )
    val temporalModel = ViewerModel.unsafe(space, Vector(temporal))
    val staticModel = ViewerModel.unsafe(space, Vector(layer("static", source)))
    val sourceState = ViewerState.centered(space).copy(
      cursor = WorldPoint(10.0, 20.0, 30.0),
      timepoint = 1,
      convention = LeftRightConvention.PatientRightOnLeft
    )
    val target = ViewerState.centered(space).copy(pixelSpacing = PixelSpacing(2.0, 2.0))

    val spatial = ViewLink.Spatial.synchronize(sourceState, target, staticModel).toOption.get
    assertEquals(spatial.cursor, sourceState.cursor)
    assertEquals(spatial.convention, sourceState.convention)
    assertEquals(spatial.timepoint, 0)
    assertEquals(spatial.pixelSpacing, target.pixelSpacing)

    val full = ViewLink.SpatialAndTime.synchronize(sourceState, target, temporalModel).toOption.get
    assertEquals(full.timepoint, 1)
    assert(ViewLink.SpatialAndTime.synchronize(sourceState, target, staticModel).isLeft)
  }
