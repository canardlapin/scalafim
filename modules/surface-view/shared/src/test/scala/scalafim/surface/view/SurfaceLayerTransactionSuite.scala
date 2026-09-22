package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceLayerTransactionSuite extends munit.FunSuite:
  private val leftId = SurfaceId.unsafe("left")
  private val rightId = SurfaceId.unsafe("right")
  private val leftScalarId = SurfaceLayerId.unsafe("left-scalar")
  private val leftMissingId = SurfaceLayerId.unsafe("left-missing")
  private val rightScalarId = SurfaceLayerId.unsafe("right-scalar")
  private val rightMissingId = SurfaceLayerId.unsafe("right-missing")
  private val mapping = ScalarColorizer(DisplayWindow.unsafe(-1.0, 1.0))
  private val transparent = Rgba32.unsafe(0, 0, 0, 0)

  private def geometry(hemisphere: Hemisphere, offset: Double): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(
          Seq(offset, 0.0, 0.0),
          Seq(offset + 1.0, 0.0, 0.0),
          Seq(offset, 1.0, 0.0)
        ),
        Seq((0, 1, 2))
      ),
      hemisphere,
      SurfaceKind.Inflated
    )

  private val left = geometry(Hemisphere.Left, -2.0)
  private val right = geometry(Hemisphere.Right, 2.0)

  private def scalar(id: SurfaceLayerId, surface: SurfaceId, geometry: SurfaceGeometry,
      values: Array[Double]): SurfaceLayer =
    SurfaceLayer.scalar(id, surface, geometry, values, mapping).toOption.get

  private def missing(id: SurfaceLayerId, surface: SurfaceId, geometry: SurfaceGeometry,
      values: IndexedSeq[Rgba32]): SurfaceLayer =
    SurfaceLayer.packedRgba(id, surface, geometry, values).toOption.get

  private def model: SurfaceViewerModel =
    SurfaceViewerModel.make(
      Vector(
        SurfaceAsset.make(leftId, left).toOption.get,
        SurfaceAsset.make(rightId, right).toOption.get
      ),
      Vector(
        scalar(leftScalarId, leftId, left, Array(-1.0, 0.0, 1.0)),
        missing(leftMissingId, leftId, left, Vector.fill(3)(transparent)),
        scalar(rightScalarId, rightId, right, Array(1.0, 0.0, -1.0)),
        missing(rightMissingId, rightId, right, Vector.fill(3)(transparent))
      )
    ).toOption.get

  private def bilateral(viewer: SurfaceViewerModel): SurfaceViewerState =
    SurfaceViewer.reduce(
      viewer,
      SurfaceViewerState.initial(viewer),
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(leftId, rightId))
    ).toOption.get

  test("compatible scalar and packed-RGBA replacements share assets and own admitted arrays"):
    val viewer = model
    val state = bilateral(viewer)
    val scalarValues = Array(1.0, 1.0, 1.0)
    val packedValues = Array.fill(3)(Rgba32.unsafe(255, 0, 0, 128))
    val replacements = Vector(
      scalar(leftScalarId, leftId, left, scalarValues),
      missing(leftMissingId, leftId, left, packedValues.toIndexedSeq),
      scalar(rightScalarId, rightId, right, Array(-1.0, -1.0, -1.0)),
      missing(rightMissingId, rightId, right, Vector.fill(3)(transparent))
    )
    val transaction = SurfaceLayerTransaction.prepare(viewer, state, replacements).toOption.get
    scalarValues(0) = -1.0
    packedValues(0) = transparent

    assert(transaction.nextModel.surfaces.head eq viewer.surfaces.head)
    assertEquals(transaction.nextModel.layers.map(_.id), viewer.layers.map(_.id))
    assertEquals(transaction.layerIds, replacements.map(_.id))
    val leftScalar = transaction.preparedPlan.layers.find(_.layer == leftScalarId).get
    val leftMissing = transaction.preparedPlan.layers.find(_.layer == leftMissingId).get
    assertEquals(leftScalar.colors(0), leftScalar.colors(1))
    assertEquals(leftMissing.colors(0), Rgba32.unsafe(255, 0, 0, 128).toPackedInt)

  test("provider permits a unilateral compatible transaction and rejects malformed groups"):
    val viewer = model
    val state = bilateral(viewer)
    val one = scalar(leftScalarId, leftId, left, Array(0.0, 0.0, 0.0))
    assert(SurfaceLayerTransaction.prepare(viewer, state, Vector(one)).isRight)
    val remapped = SurfaceLayer.scalar(
      leftScalarId,
      leftId,
      left,
      Array(0.0, 0.0, 0.0),
      ScalarColorizer(DisplayWindow.unsafe(-2.0, 2.0))
    ).toOption.get
    assert(SurfaceLayerTransaction.prepare(viewer, state, Vector(remapped)).isRight)
    assertEquals(
      SurfaceLayerTransaction.prepare(viewer, state, Vector(one, one)).left.toOption,
      Some(SurfaceViewError.DuplicateLayerId(leftScalarId))
    )

    val wrongAssociation = SurfaceLayer.faceScalar(
      leftScalarId,
      leftId,
      SurfaceFaceField.make(left, Array(0.0), frameCount = 1).toOption.get,
      mapping
    )
    assert(SurfaceLayerTransaction.prepare(viewer, state, Vector(wrongAssociation)).left.toOption.exists:
      case SurfaceViewError.IncompatibleLayerReplacement(`leftScalarId`, reason) => reason.contains("association")
      case _ => false
    )
    val unknown = scalar(SurfaceLayerId.unsafe("unknown"), leftId, left, Array(0.0, 0.0, 0.0))
    assertEquals(
      SurfaceLayerTransaction.prepare(viewer, state, Vector(unknown)).left.toOption,
      Some(SurfaceViewError.UnknownLayer(SurfaceLayerId.unsafe("unknown")))
    )

  test("apply rebases live camera and selection but rejects stale layer presentation"):
    val viewer = model
    val state = bilateral(viewer)
    val transaction = SurfaceLayerTransaction.prepare(
      viewer,
      state,
      Vector(scalar(leftScalarId, leftId, left, Array(0.25, 0.5, 0.75)))
    ).toOption.get
    val selected = SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.Select(leftId, VertexId(2))).toOption.get
    val moved = SurfaceViewer.reduce(viewer, selected, SurfaceViewerAction.SetPan(0.2, -0.1)).toOption.get
    val currentPlan = SurfaceCompiler.compile(viewer, moved).toOption.get
    val rebased = transaction.rebase(viewer, moved, currentPlan).toOption.get

    assertEquals(rebased.receipt.cameraKey, currentPlan.receipt.cameraKey)
    assertEquals(rebased.readouts.head.vertex, 2)
    assertEquals(rebased.readouts.head.layerValues.find(_._1 == leftScalarId).map(_._2), Some("0.75"))

    val hidden = SurfaceViewer.reduce(
      viewer,
      moved,
      SurfaceViewerAction.SetLayerVisible(leftScalarId, visible = false)
    ).toOption.get
    assert(transaction.rebase(viewer, hidden, SurfaceCompiler.compile(viewer, hidden).toOption.get).isLeft)

  test("render-equivalent and hidden replacements remain native-layer clean"):
    val viewer = model
    val state = bilateral(viewer)
    val same = SurfaceLayerTransaction.prepare(
      viewer,
      state,
      Vector(scalar(leftScalarId, leftId, left, Array(-1.0, 0.0, 1.0)))
    ).toOption.get
    val before = SurfaceCompiler.compile(viewer, state).toOption.get
    val identical = same.rebase(viewer, state, before).toOption.get
    assertEquals(identical.receipt.layerKeys, before.receipt.layerKeys)
    assert(identical.readouts eq same.preparedPlan.readouts)
    assert(identical.chrome eq same.preparedPlan.chrome)

    val hiddenState = SurfaceViewer.reduce(
      viewer,
      state,
      SurfaceViewerAction.SetLayerVisible(leftScalarId, visible = false)
    ).toOption.get
    val hidden = SurfaceLayerTransaction.prepare(
      viewer,
      hiddenState,
      Vector(scalar(leftScalarId, leftId, left, Array(1.0, 1.0, 1.0)))
    ).toOption.get
    val hiddenBefore = SurfaceCompiler.compile(viewer, hiddenState).toOption.get
    val hiddenAfter = hidden.rebase(viewer, hiddenState, hiddenBefore).toOption.get
    assertEquals(hiddenAfter.receipt.layerKeys, hiddenBefore.receipt.layerKeys)
