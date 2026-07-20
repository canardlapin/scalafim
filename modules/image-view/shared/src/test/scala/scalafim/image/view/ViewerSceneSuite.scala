package scalafim.image.view

import scalafim.graphics.*
import scalafim.image.*

class ViewerSceneSuite extends munit.FunSuite:

  private val referenceSpace =
    VolumeSpace(NeuroSpace(Vector(4, 3, 2)))

  private val anatomy =
    NeuroVol.fromLinear(
      NArrayUtil.tabulate[Double](referenceSpace.nVoxels)(_.toDouble),
      referenceSpace.toNeuroSpace,
      "anatomy"
    )

  private val shiftedMaskSpace =
    val affine = DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 1.0),
        Vector(0.0, 1.0, 0.0, 1.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )
    VolumeSpace(NeuroSpace(Vector(1, 1, 1), trans = Some(affine)))

  private val shiftedMask =
    NeuroVol.fromLinear(NArrayUtil.fillConst[Boolean](1, true), shiftedMaskSpace.toNeuroSpace, "mask")

  private val anatomyLayer =
    SliceLayer(
      LayerId.unsafe("anatomy"),
      anatomy,
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, referenceSpace.nVoxels.toDouble - 1.0))
    )

  private val maskLayer =
    SliceLayer(
      LayerId.unsafe("mask"),
      shiftedMask,
      SliceSampling.Nearest(false),
      MaskColorizer(Rgba32.unsafe(255, 0, 0, 180)),
      opacity = LayerOpacity.unsafe(0.75)
    )

  private val model =
    ViewerModel.unsafe(referenceSpace, Vector(anatomyLayer, maskLayer))

  private val state =
    ViewerState(
      cursor = WorldPoint(1.0, 1.0, 0.0),
      pixelSpacing = PixelSpacing(1.0, 1.0)
    )

  private def groups(frame: ViewerFrame): Vector[Grob.Group] =
    frame.scene.grobs.collect { case group: Grob.Group => group }

  test("orthogonal compilation produces three named receipts and pure scene groups") {
    val device = DeviceContext.unsafe(1200.0, 800.0)
    val first = ViewerCompiler.compile(model, state, device).toOption.get
    val second = ViewerCompiler.compile(model, state, device).toOption.get

    assertEquals(first, second)
    assertEquals(first.scene.size, 3)
    assertEquals(groups(first).length, 3)
    assertEquals(first.panels.sagittal.anatomicalPlane, AnatomicalPlane.Sagittal)
    assertEquals(first.panels.coronal.anatomicalPlane, AnatomicalPlane.Coronal)
    assertEquals(first.panels.axial.anatomicalPlane, AnatomicalPlane.Axial)
    first.panels.all.foreach { panel =>
      val (x, y) = panel.cursorRootNpc(state.cursor)
      assert(panel.rect.contains(x, y))
    }
  }

  test("panel fitting preserves physical slice aspect on a non-square device") {
    val device = DeviceContext.unsafe(1200.0, 800.0)
    val frame = ViewerCompiler.compile(model, state, device).toOption.get

    frame.panels.all.foreach { panel =>
      val renderedAspect = panel.rect.width * device.width / (panel.rect.height * device.height)
      val gridAspect =
        panel.grid.dimensions.width * panel.grid.spacing.horizontal /
          (panel.grid.dimensions.height * panel.grid.spacing.vertical)
      assertEqualsDouble(renderedAspect, gridAspect, 1e-12)
    }
  }

  test("heterogeneous layers sample in world space and retain draw order") {
    val frame = ViewerCompiler.compile(model, state, DeviceContext.unsafe(800.0, 800.0)).toOption.get
    val axialIndex = frame.panels.all.indexWhere(_.anatomicalPlane == AnatomicalPlane.Axial)
    val axial = groups(frame)(axialIndex)
    val images = axial.children.collect { case image: Grob.Image => image }

    assertEquals(images.length, 2)
    assertEqualsDouble(images(1).alpha, 0.75, 0.0)
    val maskRaster = images(1).image
    assertEquals(maskRaster.pixelUnsafe(1, 1), Rgba32.unsafe(255, 0, 0, 180))
    assertEquals(maskRaster.pixelUnsafe(0, 0).alpha, 0)
  }

  test("crosshairs and orientation labels are explicit removable decorations") {
    val decorated = ViewerCompiler.compile(model, state, DeviceContext.unsafe(800.0, 800.0)).toOption.get
    groups(decorated).foreach { group =>
      assertEquals(group.children.count(_.isInstanceOf[Grob.Segments]), 1)
      assertEquals(group.children.count(_.isInstanceOf[Grob.Text]), 4)
    }

    val plainState = state.copy(showCrosshair = false, showOrientationLabels = false)
    val plain = ViewerCompiler.compile(model, plainState, DeviceContext.unsafe(800.0, 800.0)).toOption.get
    groups(plain).foreach { group =>
      assertEquals(group.children.count(_.isInstanceOf[Grob.Segments]), 0)
      assertEquals(group.children.count(_.isInstanceOf[Grob.Text]), 0)
      assertEquals(group.children.count(_.isInstanceOf[Grob.Image]), 2)
    }
  }

  test("left-right convention changes labels and raster orientation together") {
    val device = DeviceContext.unsafe(800.0, 800.0)
    val patientLeft = ViewerCompiler.compile(model, state, device).toOption.get
    val patientRight = ViewerCompiler.compile(
      model,
      state.copy(convention = LeftRightConvention.PatientRightOnLeft),
      device
    ).toOption.get
    val leftAxial = groups(patientLeft).last
    val rightAxial = groups(patientRight).last
    val leftLabels = leftAxial.children.collect { case text: Grob.Text => text.label }
    val rightLabels = rightAxial.children.collect { case text: Grob.Text => text.label }
    val leftRaster = leftAxial.children.collectFirst { case image: Grob.Image => image.image }.get
    val rightRaster = rightAxial.children.collectFirst { case image: Grob.Image => image.image }.get

    assertEquals(leftLabels.take(2), Vector("L", "R"))
    assertEquals(rightLabels.take(2), Vector("R", "L"))
    assertEquals(leftRaster.pixelUnsafe(0, 0), rightRaster.pixelUnsafe(rightRaster.width - 1, 0))
  }

  test("viewer models reject empty and duplicate layer sets") {
    assert(ViewerModel.make(referenceSpace, Vector.empty).isLeft)
    assert(ViewerModel.make(referenceSpace, Vector(anatomyLayer, anatomyLayer)).isLeft)
    assert(OrthogonalLayout.make(0.5, 0.0).isLeft)
  }
