package consumer

import image4s.ImageMetadata
import image4s.geometry.{Affine, ContinuousIndex, D3, Frame, Point}
import intaglio.*
import neuroim4s.image.*
import neuroim4s.image.io.Nifti
import neuroim4s.spaces.{SpaceEvidence, WorldSpace}
import neuroim4s.transform.{TransformProvenance, WorldTransform}
import neuroim4s.view.*
import neuroim4s.view.java2d.Java2DViewerHost
import ravel.NDArray
import reframe4s.lie.FramedAffine

import java.nio.file.Files

/** One connected public-provider acceptance scenario. The value oracle is independent of storage,
  * world mapping, and viewer sampling: value(x,y,z) = 100*x + 10*y + z.
  */
object NeuroimCompositionConsumer:
  private val providerRevision = "21df6416e4e28e97cfbe39e20d7303349be7e4cf"
  private val dimensions = Vector(5, 4, 3)
  private val targetIndex = Vector(3, 1, 1)
  private val expectedWorldCoordinates = Vector(14.5, -2.0, 9.5)
  private val expectedValue = 311.0
  private val expectedPixel = 0xff9f9f9f
  private val translation = Vector(2.0, -3.0, 1.0)

  def main(args: Array[String]): Unit =
    try
      val receipt = scenario()
      println(s"SCENARIO PASS provider=$providerRevision io-transform-java2d-pick value=${receipt.value} pixel=${receipt.pixel} classpathEntries=${receipt.classpathEntries}")
    catch
      case error: Throwable =>
        Console.err.println(s"SCENARIO FAIL ${error.getClass.getName}: ${error.getMessage}")
        throw error

  private final case class Receipt(value: Double, pixel: String, classpathEntries: Int)

  private def scenario(): Receipt =
    val classpathEntries = refuseForbiddenClasspath()
    val directory = Files.createTempDirectory("neuroim-composition-")
    val path = directory.resolve("analytic-volume.nii")
    try
      val sourceSpace = requireRight(
        SampleSpaces.requireVolumeD3(
          SampleSpaces(dimensions, spacing = Some(Vector(1.5, 2.0, 2.5)), origin = Some(Vector(10.0, -4.0, 7.0)))
        ),
        "create source space"
      )
      val source = requireRight(
        NeuroVolume.continuous(
          sourceSpace,
          NDArray.tabulate[Double](5, 4, 3)((x, y, z) => analytic(x, y, z)),
          ImageMetadata.named("independent analytic noncubic fixture")
        ),
        "create source volume"
      )
      requireRight(Nifti.writeVolume(path, SomeNeuroVolume.eraseSpace(source)), "write NIfTI")

      val assertedWorld = requireRight(WorldSpace.declare("composition consumer target world"), "declare target world")
      val loaded = requireRight(
        Nifti.readVolume(path, SpaceEvidence(assertion = Some(assertedWorld))),
        "read NIfTI with typed evidence"
      ).image
      require(loaded.grid.shape == dimensions, s"NIfTI shape ${loaded.grid.shape} differs from $dimensions")
      requireClose(loaded(3, 1, 1), expectedValue, 1e-12, "NIfTI analytic round-trip")

      val target = requireRight(
        loaded.grid.pointAt(
          requireRight(ContinuousIndex.fromVector[D3](targetIndex.map(_.toDouble)), "target voxel index")
        ),
        "target world point"
      )
      target.coordinates.zip(expectedWorldCoordinates).zipWithIndex.foreach:
        case ((actual, expected), axis) =>
          requireClose(actual, expected, 1e-12, s"NIfTI world affine target axis $axis")
      val queryFrame = requireRight(Frame.named[D3]("composition consumer query world"), "query frame")
      val targetFrame = loaded.grid.frame
      val transform = WorldTransform.Linear(
        FramedAffine.betweenFrames[targetFrame.type, queryFrame.type, D3](targetFrame, queryFrame)(
          translationAffine(-translation(0), -translation(1), -translation(2))
        ),
        TransformProvenance.constructed("analytic query-to-loaded-world translation")
      )
      val query = requireRight(
        Point.in(queryFrame)(
          target.coordinates(0) - translation(0),
          target.coordinates(1) - translation(1),
          target.coordinates(2) - translation(2)
        ),
        "query source point"
      )
      val transformedCursor = requireRight(transform.mapPoint(query), "map query into loaded world")
      requireClose(transformedCursor.coordinates(0), target.coordinates(0), 1e-12, "transformed cursor x")
      requireClose(transformedCursor.coordinates(1), target.coordinates(1), 1e-12, "transformed cursor y")
      requireClose(transformedCursor.coordinates(2), target.coordinates(2), 1e-12, "transformed cursor z")

      val layer = SliceLayer(
        LayerId.unsafe("analytic"),
        loaded,
        SliceSampling.Nearest(0.0),
        ScalarColorizer(DisplayWindow.unsafe(0.0, 500.0))
      )
      val model = ViewerModel.unsafe(loaded.grid, Vector(layer))
      val state = ViewerState.centered(loaded.grid).copy(
        cursor = transformedCursor,
        showCrosshair = false,
        showOrientationLabels = false
      )
      val session = ViewerSession(state, DeviceContext.unsafe(700.0, 500.0))
      val (compiled, image) = requireRight(Java2DViewerHost.renderImage(model, session), "render Java2D slice")
      val axial = compiled.frame.panels.axial
      val (rootX, rootY) = axial.cursorRootNpc(transformedCursor.toWorldPoint)
      val deviceX = rootX * session.device.width
      val deviceY = (1.0 - rootY) * session.device.height
      val pixel = image.getRGB(math.round(deviceX).toInt, math.round(deviceY).toInt)
      require(pixel == expectedPixel, f"Java2D picked pixel: expected 0x$expectedPixel%08x, got 0x$pixel%08x")

      val action = requireRight(Java2DViewerHost.pickAction(compiled, deviceX, deviceY), "pick rendered axial panel")
      val picked = requireRight(ViewerReducer.reduce(model, session, action), "apply rendered pick")
      val pickedFrame = requireRight(picked.frame(model), "compile picked cursor frame")
      val sampled = pickedFrame.readouts.axial.layers.collectFirst {
        case LayerReadout(id, LayerSampleValue.Scalar(value)) if id == LayerId.unsafe("analytic") => value
      }.getOrElse(throw new IllegalStateException("picked axial readout lacks analytic scalar layer"))
      requireClose(sampled, expectedValue, 1e-9, "transform-derived rendered pick analytic value")

      val foreignFrame = requireRight(Frame.named[D3]("composition consumer foreign world"), "foreign frame")
      val foreignCursor = requireRight(Point.in(foreignFrame)(target.coordinates*), "foreign cursor")
      ViewerReducer.reduce(model, session, ViewerAction.SetCursor(foreignCursor)) match
        case Left(ImageViewError.CursorFrameMismatch(_)) => ()
        case other => throw new IllegalStateException(s"foreign world owner was not refused: $other")

      Receipt(sampled, f"0x$pixel%08x", classpathEntries)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory)

  private def analytic(x: Int, y: Int, z: Int): Double =
    100.0 * x + 10.0 * y + z

  private def translationAffine(x: Double, y: Double, z: Double): Affine[D3] =
    requireRight(
      Affine.fromRowMajor[D3](Vector(
        1.0, 0.0, 0.0, x,
        0.0, 1.0, 0.0, y,
        0.0, 0.0, 1.0, z,
        0.0, 0.0, 0.0, 1.0
      )),
      "translation affine"
    )

  private def refuseForbiddenClasspath(): Int =
    val entries = System.getProperty("java.class.path", "").split(java.io.File.pathSeparator).toVector.filter(_.nonEmpty)
    val forbidden = entries.filter: entry =>
      val lower = entry.replace('\\', '/').toLowerCase
      lower.matches(".*(?:^|/)(?:scalafim|surface|neuroatlas4s|neurosurf4s|mvpa4s)[^/]*(?:\\.jar|/target/.*|/classes).*")
    require(forbidden.isEmpty, s"refusing unrelated runtime artifact(s): ${forbidden.mkString(", ")}")
    entries.size

  private def requireClose(actual: Double, expected: Double, tolerance: Double, label: String): Unit =
    require(math.abs(actual - expected) <= tolerance, s"$label: expected $expected, got $actual, tolerance $tolerance")

  private def requireRight[A](value: Either[?, A], label: String): A =
    value.fold(error => throw new IllegalStateException(s"$label: $error"), identity)
