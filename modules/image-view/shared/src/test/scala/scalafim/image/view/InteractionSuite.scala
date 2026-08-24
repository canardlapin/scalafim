package scalafim.image.view

import intaglio.*
import scalafim.image.*
import scalafim.image.SampleSpaces.*

class InteractionSuite extends munit.FunSuite:

  private val sampleSpace =
    SampleSpaces.requireVolumeD3(SampleSpaces(Vector(4, 3, 2))).toOption.get
  private val space = sampleSpace.grid

  private def constantVolume(value: Double, label: String): SomeScalarVolume[Double] =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.fillConst[Double](space.nVoxels, value),
      sampleSpace,
      label
    )

  private val scalarId = LayerId.unsafe("scalar")
  private val maskId = LayerId.unsafe("mask")
  private val scalarLayer =
    SliceLayer(
      scalarId,
      constantVolume(2.0, "scalar"),
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 10.0))
    )
  private val maskLayer =
    SliceLayer(
      maskId,
      SomeMaskVolume.unsafeCopyFromCanonicalArray(
        PrimitiveBuffers.fillConst[Boolean](space.nVoxels, true),
        sampleSpace,
        "mask"
      ),
      SliceSampling.Nearest(false),
      MaskColorizer(Rgba32.unsafe(255, 0, 0, 128))
    )
  private val model = ViewerModel.unsafe(space, Vector(scalarLayer, maskLayer))
  private val initial = ViewerSession(
    ViewerState.centered(space).copy(sliceStep = SliceStep.unsafe(2.0)),
    DeviceContext.unsafe(900.0, 700.0)
  )

  private def groupFor(frame: ViewerFrame, plane: AnatomicalPlane): Grob.Group =
    val index = frame.panels.all.indexWhere(_.anatomicalPlane == plane)
    frame.scene.grobs(index).asInstanceOf[Grob.Group]

  private def images(frame: ViewerFrame, plane: AnatomicalPlane): Vector[Grob.Image] =
    groupFor(frame, plane).children.collect { case image: Grob.Image => image }

  test("centered state derives its cursor and steps from reference geometry") {
    val centered = ViewerState.centered(space)
    assertEquals(centered.cursor, WorldPoint(1.5, 1.0, 0.5))
    assertEqualsDouble(centered.pixelSpacing.horizontal, 1.0, 0.0)
    assertEqualsDouble(centered.sliceStep.millimeters, 1.0, 0.0)
  }

  test("panel picking is an exact root-npc to world-space roundtrip") {
    val panels = ViewerCompiler.panels(space, initial.state, initial.device)
    val axial = panels.axial
    val target = axial.grid.worldAt(PixelCoord(2, 1)).toOption.get
    val (rootX, rootY) = axial.cursorRootNpc(target)
    val picked = ViewerReducer.reduce(
      model,
      initial,
      ViewerAction.Pick(AnatomicalPlane.Axial, ViewerPointer.unsafe(rootX, rootY))
    ).toOption.get

    assertEqualsDouble(picked.state.cursor.x, target.x, 1e-12)
    assertEqualsDouble(picked.state.cursor.y, target.y, 1e-12)
    assertEqualsDouble(picked.state.cursor.z, target.z, 1e-12)

    val outside = ViewerReducer.reduce(
      model,
      initial,
      ViewerAction.Pick(AnatomicalPlane.Axial, ViewerPointer.unsafe(0.99, 0.01))
    )
    assert(outside.isLeft)
  }

  test("scrolling follows positive anatomical normals independent of display mirroring") {
    val origin = initial.copy(state = initial.state.copy(cursor = WorldPoint.Origin))
    val sagittal = ViewerReducer.reduce(model, origin, ViewerAction.Scroll(AnatomicalPlane.Sagittal, 1)).toOption.get
    val coronal = ViewerReducer.reduce(model, origin, ViewerAction.Scroll(AnatomicalPlane.Coronal, 1)).toOption.get
    val axial = ViewerReducer.reduce(model, origin, ViewerAction.Scroll(AnatomicalPlane.Axial, 1)).toOption.get

    assertEquals(sagittal.state.cursor, WorldPoint(2.0, 0.0, 0.0))
    assertEquals(coronal.state.cursor, WorldPoint(0.0, 2.0, 0.0))
    assertEquals(axial.state.cursor, WorldPoint(0.0, 0.0, 2.0))

    val mirrored = origin.copy(
      state = origin.state.copy(convention = LeftRightConvention.PatientRightOnLeft)
    )
    val mirroredAxial = ViewerReducer.reduce(
      model,
      mirrored,
      ViewerAction.Scroll(AnatomicalPlane.Axial, 1)
    ).toOption.get
    assertEquals(mirroredAxial.state.cursor, axial.state.cursor)
  }

  test("zoomed panel views preserve inverse picking and stay plane-local") {
    val view = PanelView.unsafe(ZoomLevel.unsafe(2.0), centerX = 0.6, centerY = 0.4)
    val zoomed = ViewerReducer.reduce(
      model,
      initial,
      ViewerAction.SetPanelView(AnatomicalPlane.Axial, view)
    ).toOption.get
    val panel = ViewerCompiler.panels(space, zoomed.state, zoomed.device).axial
    val rootX = panel.rect.left + panel.rect.width * 0.5
    val rootY = panel.rect.bottom + panel.rect.height * 0.5
    val pickedWorld = panel.worldAtRootNpc(rootX, rootY).get
    val (roundtripX, roundtripY) = panel.cursorRootNpc(pickedWorld)

    assertEqualsDouble(roundtripX, rootX, 1e-12)
    assertEqualsDouble(roundtripY, rootY, 1e-12)
    assertEquals(zoomed.state.panelViews.axial, view)
    assertEquals(zoomed.state.panelViews.coronal, PanelView.Default)

    val image = images(zoomed.frame(model).toOption.get, AnatomicalPlane.Axial).head
    image.at.x match
      case LengthExpr.Const(length) => assertEqualsDouble(length.value, -0.7, 1e-12)
      case other => fail(s"expected a constant x location, got $other")
    image.at.y match
      case LengthExpr.Const(length) => assertEqualsDouble(length.value, -0.3, 1e-12)
      case other => fail(s"expected a constant y location, got $other")
    assertEquals(image.size, Size.npcUnsafe(2.0, 2.0))

    val reset = ViewerReducer.reduce(
      model,
      zoomed,
      ViewerAction.ResetPanelView(AnatomicalPlane.Axial)
    ).toOption.get
    assertEquals(reset.state.panelViews.axial, PanelView.Default)
    assert(ZoomLevel.make(0.5).isLeft)
    assert(PanelView.make(ZoomLevel.unsafe(2.0), 0.1, 0.5).isLeft)
  }

  test("window threshold opacity and visibility actions alter presentation without mutating layers") {
    val windowed = ViewerReducer.reduce(
      model,
      initial,
      ViewerAction.SetWindow(scalarId, DisplayWindow.unsafe(2.0, 4.0))
    ).toOption.get
    val faded = ViewerReducer.reduce(
      model,
      windowed,
      ViewerAction.SetOpacity(scalarId, LayerOpacity.unsafe(0.25))
    ).toOption.get
    val thresholded = ViewerReducer.reduce(
      model,
      faded,
      ViewerAction.SetThreshold(
        scalarId,
        DisplayThreshold.transparentBand(1.5, 2.5).toOption.get
      )
    ).toOption.get
    val frame = thresholded.frame(model).toOption.get
    val scalar = images(frame, AnatomicalPlane.Axial).head

    assertEqualsDouble(scalar.alpha, 0.25, 0.0)
    assertEquals(scalar.image.pixelUnsafe(1, 1).alpha, 0)
    assertEquals(model.layers.head.opacity, LayerOpacity.Opaque)

    val disabled = ViewerReducer.reduce(
      model,
      thresholded,
      ViewerAction.SetThreshold(scalarId, DisplayThreshold.Disabled)
    ).toOption.get
    assertEquals(images(disabled.frame(model).toOption.get, AnatomicalPlane.Axial).head.image.pixelUnsafe(1, 1).alpha, 255)

    val hidden = ViewerReducer.reduce(
      model,
      thresholded,
      ViewerAction.SetVisibility(scalarId, false)
    ).toOption.get
    assertEquals(images(hidden.frame(model).toOption.get, AnatomicalPlane.Axial).length, 1)

    val unsupported = ViewerReducer.reduce(
      model,
      initial,
      ViewerAction.SetWindow(maskId, DisplayWindow.unsafe(0.0, 1.0))
    )
    assert(unsupported.isLeft)
    assert(
      ViewerReducer.reduce(
        model,
        initial,
        ViewerAction.SetThreshold(maskId, DisplayThreshold.Disabled)
      ).isLeft
    )
  }

  test("timepoint actions drive temporal layers while static overlays persist") {
    val first = constantVolume(0.0, "first")
    val second = constantVolume(10.0, "second")
    val seriesLayer = SliceLayer.series(
      scalarId,
      first.concatenate(second),
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 10.0))
    )
    val temporalModel = ViewerModel.unsafe(space, Vector(seriesLayer, maskLayer))
    assertEquals(temporalModel.timepointCount, 2)

    val atSecond = ViewerReducer.reduce(
      temporalModel,
      initial,
      ViewerAction.SetTimepoint(1)
    ).toOption.get
    val frame = atSecond.frame(temporalModel).toOption.get
    val axial = images(frame, AnatomicalPlane.Axial)
    assertEquals(axial.length, 2)
    assertEquals(axial.head.image.pixelUnsafe(1, 1).red, 255)
    assertEquals(axial(1).image.pixelUnsafe(1, 1).alpha, 128)

    assert(ViewerReducer.reduce(temporalModel, initial, ViewerAction.SetTimepoint(2)).isLeft)
    assert(ViewerCompiler.compile(temporalModel, initial.state.copy(timepoint = -1), initial.device).isLeft)
  }

  test("models reject temporal layers with incompatible frame counts") {
    val one = constantVolume(1.0, "one")
    val twoFrames = SliceLayer.series(
      LayerId.unsafe("two"),
      one.concatenate(one),
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 2.0))
    )
    val threeFrames = SliceLayer.series(
      LayerId.unsafe("three"),
      one.concatenate(one, one),
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 2.0))
    )

    assert(ViewerModel.make(space, Vector(twoFrames, threeFrames)).isLeft)
  }

  test("resize changes only device layout and reduction is deterministic") {
    val resizedAction = ViewerAction.Resize(DeviceContext.unsafe(500.0, 1000.0))
    val first = ViewerReducer.reduce(model, initial, resizedAction).toOption.get
    val second = ViewerReducer.reduce(model, initial, resizedAction).toOption.get

    assertEquals(first, second)
    assertEquals(first.state, initial.state)
    assertNotEquals(
      first.frame(model).toOption.get.panels.axial.rect,
      initial.frame(model).toOption.get.panels.axial.rect
    )
    assert(ViewerPointer.make(Double.NaN, 0.0).isLeft)
    assert(SliceStep.make(0.0).isLeft)
    assert(
      ViewerReducer.reduce(
        model,
        initial,
        ViewerAction.SetVisibility(LayerId.unsafe("missing"), false)
      ).isLeft
    )
  }
