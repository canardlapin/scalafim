package scalafim.image.view

import intaglio.*
import scalafim.image.*

class AdaptiveLayoutSuite extends munit.FunSuite:
  private val planes = Vector(AnatomicalPlane.Sagittal, AnatomicalPlane.Coronal, AnatomicalPlane.Axial)
  private val dimensions = Vector(64, 80, 48)
  private val affine = DMat.fromRows(Vector(
    Vector(2.0, 0.0, 0.0, -64.0), Vector(0.0, 2.5, 0.0, -100.0),
    Vector(0.0, 0.0, 3.0, -72.0), Vector(0.0, 0.0, 0.0, 1.0)))
  private val space = VolumeSpace(NeuroSpace(dimensions, trans = Some(affine)))
  private val state = ViewerState.centered(space)
  private val sizes = Vector((320.0, 960.0), (600.0, 900.0), (800.0, 800.0),
    (900.0, 600.0), (1800.0, 450.0), (2400.0, 1600.0))

  private def physical(panel: PanelReceipt): (Double, Double) =
    (panel.grid.dimensions.width * panel.grid.spacing.horizontal,
      panel.grid.dimensions.height * panel.grid.spacing.vertical)

  private def assertAspect(panel: PanelReceipt, device: DeviceContext): Unit =
    val (width, height) = physical(panel)
    assertEqualsDouble(panel.rect.width * device.width / (panel.rect.height * device.height), width / height, 1e-12)

  test("adaptive resize maximizes physical image area among admitted arrangements with fixed pixel margins") {
    val layout = OrthogonalLayout.adaptive(12.0, 8.0).toOption.get
    sizes.foreach { (width, height) =>
      val device = DeviceContext.unsafe(width, height)
      val panels = ViewerCompiler.panels(space, state, device, layout).all
      val occupied = panels.map(p => p.rect.width * width * p.rect.height * height).sum
      // Independent rectangular packing calculation; no native fitting/layout calls in the oracle.
      val cellSizes = Vector(((width - 24.0 - 8.0) / 2.0, (height - 24.0 - 8.0) / 2.0),
        ((width - 24.0 - 16.0) / 3.0, height - 24.0),
        (width - 24.0, (height - 24.0 - 16.0) / 3.0))
      val expected = cellSizes.map { (cw, ch) =>
        panels.map { panel =>
          val (pw, ph) = physical(panel)
          val scale = math.min(cw / pw, ch / ph)
          pw * ph * scale * scale
        }.sum
      }.max
      assertEqualsDouble(occupied, expected, 1e-7)
      panels.foreach { p =>
        assertAspect(p, device)
        assert(p.rect.left * width >= 12.0 - 1e-8)
        assert(p.rect.bottom * height >= 12.0 - 1e-8)
        assert(p.rect.right * width <= width - 12.0 + 1e-8)
        assert(p.rect.top * height <= height - 12.0 + 1e-8)
      }
      panels.combinations(2).foreach { pair =>
        val a = pair(0).rect
        val b = pair(1).rect
        val xGap = math.max(a.left - b.right, b.left - a.right) * width
        val yGap = math.max(a.bottom - b.top, b.bottom - a.top) * height
        assert(math.max(xGap, yGap) >= 8.0 - 1e-8)
      }
    }
  }

  test("adaptive portrait and wide docks improve on the former L fallback") {
    Vector((320.0, 960.0), (1800.0, 450.0)).foreach { (w, h) =>
      val device = DeviceContext.unsafe(w, h)
      def area(layout: OrthogonalLayout): Double =
        ViewerCompiler.panels(space, state, device, layout).all.map(p => p.rect.width * p.rect.height).sum
      val legacyL = OrthogonalLayout.make(12.0, 8.0, OrthogonalArrangement.LShape,
        OrthogonalLayoutUnits.LogicalPixels).toOption.get
      assert(area(OrthogonalLayout.adaptive(12.0, 8.0).toOption.get) > area(legacyL))
    }
  }

  test("every focused plane fits the whole inset viewport without stretching on resize") {
    planes.foreach { plane =>
      val layout = OrthogonalLayout.make(12.0, 8.0, OrthogonalArrangement.SinglePlane(plane),
        OrthogonalLayoutUnits.LogicalPixels).toOption.get
      sizes.foreach { (width, height) =>
        val device = DeviceContext.unsafe(width, height)
        val panels = ViewerCompiler.panels(space, state, device, layout)
        val selected = panels(plane)
        assertEquals(panels.all.filter(_.visible).map(_.anatomicalPlane), Vector(plane))
        assertAspect(selected, device)
        val (pw, ph) = physical(selected)
        val scale = math.min((width - 24.0) / pw, (height - 24.0) / ph)
        assertEqualsDouble(selected.rect.width * width, pw * scale, 1e-9)
        assertEqualsDouble(selected.rect.height * height, ph * scale, 1e-9)
        assertEqualsDouble((selected.rect.left + selected.rect.right) / 2.0, 0.5, 1e-12)
        assertEqualsDouble((selected.rect.bottom + selected.rect.top) / 2.0, 0.5, 1e-12)
        panels.all.filterNot(_.visible).foreach(p => assertEquals(p.worldAtRootNpc(0.5, 0.5), None))
      }
    }
  }

  test("focus renders and samples only the selected plane and pointer actions cannot target hidden planes") {
    val smallSpace = VolumeSpace(NeuroSpace(Vector(8, 7, 6)))
    val volume = NeuroVol.fromLinear(PrimitiveBuffers.tabulate[Double](smallSpace.nVoxels)(_.toDouble),
      smallSpace.toNeuroSpace, "fixture")
    val layer = SliceLayer(LayerId.unsafe("anatomy"), volume, SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 335.0)))
    val model = ViewerModel.unsafe(smallSpace, Vector(layer))
    val view = PanelView.unsafe(ZoomLevel.unsafe(2.0), 0.4, 0.6)
    val selectedState = ViewerState.centered(smallSpace).copy(
      panelViews = PanelReceipts.fill(view), convention = LeftRightConvention.PatientRightOnLeft)
    planes.foreach { plane =>
      val layout = OrthogonalLayout.make(12.0, 8.0, OrthogonalArrangement.SinglePlane(plane),
        OrthogonalLayoutUnits.LogicalPixels).toOption.get
      val session = ViewerSession(selectedState, DeviceContext.unsafe(900.0, 600.0), layout)
      val compiled = session.compileCached(model, ViewerCache.Disabled).toOption.get
      val frame = compiled.frame
      assertEquals(frame.scene.size, 1)
      assertEquals(compiled.profile.panelCount, 1)
      assertEquals(compiled.profile.layerRequests, 1)
      assertEquals(compiled.profile.sampledPixels, frame.panels(plane).grid.dimensions.pixelCount.toLong)
      assertEquals(frame.visibleReadouts.map(_.anatomicalPlane), Vector(plane))
      assertEquals(frame.visibleReadouts.head.layers.map(_.layer), Vector(layer.id))
      frame.panels.all.filterNot(_.visible).foreach(p => assertEquals(frame.readouts(p.anatomicalPlane).layers, Vector.empty))
      val (x, y) = frame.panels(plane).cursorRootNpc(selectedState.cursor)
      val action = ViewerEvents.pick(frame, x * 900.0, (1.0 - y) * 600.0).toOption.get
      action match
        case ViewerAction.Pick(pickedPlane, _) => assertEquals(pickedPlane, plane)
        case _ => fail("expected a pick")
      val picked = ViewerReducer.reduce(model, session, action).toOption.get
      assertEqualsDouble(picked.state.cursor.x, selectedState.cursor.x, 1e-10)
      assertEqualsDouble(picked.state.cursor.y, selectedState.cursor.y, 1e-10)
      assertEqualsDouble(picked.state.cursor.z, selectedState.cursor.z, 1e-10)
      planes.filterNot(_ == plane).foreach { hidden =>
        assert(ViewerReducer.reduce(model, session, ViewerAction.Pick(hidden, ViewerPointer.unsafe(x, y))).isLeft)
      }
      assert(ViewerEvents.pick(frame, 0.0, 0.0).isLeft)
      val resized = ViewerReducer.reduce(model, session, ViewerAction.Resize(DeviceContext.unsafe(400.0, 1000.0))).toOption.get
      val restored = resized.copy(layout = OrthogonalLayout.adaptive().toOption.get)
      assertEquals(restored.state, selectedState)
      assertEquals(restored.frame(model).toOption.get.visiblePanels.length, 3)
    }
  }

  test("tiny viewports remain finite while invalid spacing is rejected") {
    assert(OrthogonalLayout.adaptive(-1.0).isLeft)
    assert(OrthogonalLayout.adaptive(Double.NaN).isLeft)
    assert(OrthogonalLayout.adaptive(12.0, Double.PositiveInfinity).isLeft)
    assert(OrthogonalLayout.make(0.4, 0.2, OrthogonalArrangement.Automatic).isLeft)
    val device = DeviceContext.unsafe(1.0, 2.0)
    val layouts = Vector(OrthogonalLayout.adaptive().toOption.get,
      OrthogonalLayout.make(12.0, 8.0, OrthogonalArrangement.SinglePlane(AnatomicalPlane.Axial),
        OrthogonalLayoutUnits.LogicalPixels).toOption.get)
    layouts.foreach { layout =>
      ViewerCompiler.panels(space, state, device, layout).all.foreach { p =>
        assert(p.rect.width.isFinite && p.rect.width > 0.0)
        assert(p.rect.height.isFinite && p.rect.height > 0.0)
        assert(p.rect.left >= 0.0 && p.rect.right <= 1.0)
        assert(p.rect.bottom >= 0.0 && p.rect.top <= 1.0)
        assertAspect(p, device)
      }
    }
  }
