package consumer

import image4s.{Axis, AxisKind, ImageMetadata, NonSpatialAxes, SampleSpace}
import image4s.geometry.{ContinuousIndex, D3, Frame, Point}
import intaglio.*
import neuroim4s.image.*
import neuroim4s.image.io.Nifti
import neuroim4s.spaces.{SpaceEvidence, WorldSpace}
import neuroim4s.transform.{Frames, NativeTransform, TransformFiles, TransformFormat}
import neuroim4s.transform.itk.ItkLinearInterpretation
import neuroim4s.view.*
import neuroim4s.view.java2d.Java2DViewerHost
import ravel.NDArray

import java.nio.file.Files

/** One connected public-provider acceptance scenario. The value oracle is independent of storage,
  * world mapping, and viewer sampling: value(x,y,z) = 100*x + 10*y + z.
  */
object NeuroimCompositionConsumer:
  private val providerRevision = "4f75c2bf141449d032f9fe3c558ef13a22a4d59f"
  private val dimensions = Vector(5, 4, 3)
  private val targetIndex = Vector(3, 1, 1)
  private val expectedWorldCoordinates = Vector(14.5, -2.0, 9.5)
  private val expectedValue = 311.0
  private val expectedPixel = 0xff9f9f9f
  private val translation = Vector(2.0, -3.0, 1.0)

  def main(args: Array[String]): Unit =
    try
      val receipt = scenario()
      println(s"SCENARIO PASS provider=$providerRevision steps=classpath,nifti-volume,nifti-series,series-volume,itk-load,itk-interpret,transform,java2d-render,pick,foreign-frame-refusal value=${receipt.value} pixel=${receipt.pixel} classpathEntries=${receipt.classpathEntries}")
    catch
      case error: Throwable =>
        Console.err.println(s"SCENARIO FAIL ${error.getClass.getName}: ${error.getMessage}")
        throw error

  private final case class Receipt(value: Double, pixel: String, classpathEntries: Int)

  private def scenario(): Receipt =
    val classpathEntries = refuseForbiddenClasspath()
    val directory = Files.createTempDirectory("neuroim-composition-")
    val path = directory.resolve("analytic-volume.nii")
    val seriesPath = directory.resolve("analytic-series.nii")
    val transformPath = directory.resolve("query-to-target.tfm")
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
      val evidence = SpaceEvidence(assertion = Some(assertedWorld))
      val roundTrip = requireRight(Nifti.readVolume(path, evidence), "read NIfTI volume with typed evidence").image
      requireClose(roundTrip(3, 1, 1), expectedValue, 1e-12, "NIfTI volume analytic round-trip")
      val timeAxis = requireRight(Axis.ordinal("time", AxisKind.Time, 2), "create time axis")
      val seriesAxes = requireRight(NonSpatialAxes.from(Vector(timeAxis)), "create series axes")
      val seriesSpace = SampleSpace.create(sourceSpace.grid, seriesAxes)
      val sourceSeries = requireRight(
        NeuroSeries.continuous(
          seriesSpace,
          NDArray.tabulate[Double](5, 4, 3, 2)((x, y, z, time) => analytic(x, y, z) + 1000.0 * time),
          ImageMetadata.named("independent analytic 4D fixture")
        ),
        "create source series"
      )
      requireRight(Nifti.writeSeries(seriesPath, SomeNeuroSeries.eraseSpace(sourceSeries)), "write NIfTI series")
      val series = requireRight(Nifti.readSeries(seriesPath, evidence), "read NIfTI series with typed evidence").image
      require(series.nVolumes == 2, s"expected two timepoints, got ${series.nVolumes}")
      requireClose(series(3, 1, 1, 1), 1311.0, 1e-12, "NIfTI series second timepoint")
      require(series.volumeAt(2).isLeft, "out-of-range series volume was not refused")
      val loaded = requireRight(series.volumeAt(0), "select first series volume")
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
      // ITK text stores the target-to-source pullback in LPS. The RAS pullback
      // (-2, 3, -1) is therefore represented by LPS offsets (2, -3, -1).
      val itkText = "#Insight Transform File V1.0\n# Transform 0\nTransform: AffineTransform_double_3_3\nParameters: 1 0 0 0 1 0 0 0 1 2 -3 -1\nFixedParameters: 0 0 0\n"
      Files.writeString(transformPath, itkText)
      val decoded = requireRight(TransformFiles.load(transformPath, Some(TransformFormat.ItkText)), "load ITK transform file")
      val transform = decoded.native match
        case NativeTransform.Itk(file, TransformFormat.ItkText) =>
          requireRight(ItkLinearInterpretation.linear(file, Frames[queryFrame.type, targetFrame.type](queryFrame, targetFrame), decoded.asset, decoded.format), "admit ITK transform")
        case other => throw new IllegalStateException(s"expected ITK text transform, got $other")
      require(transform.source eq queryFrame, "transform lost query frame owner")
      require(transform.target eq targetFrame, "transform lost loaded frame owner")
      val query = requireRight(
        Point.in(queryFrame)(
          target.coordinates(0) - translation(0),
          target.coordinates(1) - translation(1),
          target.coordinates(2) - translation(2)
        ),
        "query source point"
      )
      // The decoded grid existentially packages its frame type. Recover the
      // transform's precise target type through checked owner alignment.
      def intoTransformTarget[F <: Frame[D3]](point: Point[F, D3]): Point[targetFrame.type, D3] =
        requireRight(
          Frame.alignOwners[D3, F, targetFrame.type](point.frame, targetFrame).flatMap(_.pointToRight(point)),
          "admit loaded target point owner"
        )
      val pulled = requireRight(transform.pullPoint(intoTransformTarget(target)), "pull target into query world")
      pulled.coordinates.zip(query.coordinates).foreach { (actual, expected) =>
        requireClose(actual, expected, 1e-12, "ITK pullback coordinate")
      }
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

      // Start elsewhere within the same axial plane: a no-op reducer must not satisfy the oracle.
      val initialCursor = requireRight(Point.in(targetFrame)(11.5, -2.0, 9.5), "initial in-plane cursor")
      val initialSession = session.copy(state = state.copy(cursor = initialCursor))
      val initialFrame = requireRight(initialSession.frame(model), "compile initial cursor frame")
      require(initialFrame.panels.axial.rect == axial.rect, "pick and initial session panel geometry differ")
      def axialValue(frame: ViewerFrame): Double =
        frame.readouts.axial.layers.collectFirst {
          case LayerReadout(id, LayerSampleValue.Scalar(value)) if id == LayerId.unsafe("analytic") => value
        }.getOrElse(throw new IllegalStateException("axial readout lacks analytic scalar layer"))
      requireClose(axialValue(initialFrame), 111.0, 1e-9, "initial cursor independent value")
      val action = requireRight(Java2DViewerHost.pickAction(compiled, deviceX, deviceY), "pick rendered axial panel")
      val picked = requireRight(ViewerReducer.reduce(model, initialSession, action), "apply rendered pick")
      require(picked.state.cursor.frame eq targetFrame, "picked cursor lost the loaded frame owner")
      picked.state.cursor.coordinates.zip(Vector(14.5, -2.0, 9.5)).foreach { (actual, expected) =>
        requireClose(actual, expected, 1e-9, "picked cursor independent world coordinate")
      }
      val pickedFrame = requireRight(picked.frame(model), "compile picked cursor frame")
      val sampled = axialValue(pickedFrame)
      requireClose(sampled, expectedValue, 1e-9, "transform-derived rendered pick analytic value")

      val foreignFrame = requireRight(Frame.named[D3]("composition consumer foreign world"), "foreign frame")
      val foreignCursor = requireRight(Point.in(foreignFrame)(target.coordinates*), "foreign cursor")
      ViewerReducer.reduce(model, session, ViewerAction.SetCursor(foreignCursor)) match
        case Left(ImageViewError.CursorFrameMismatch(_)) => ()
        case other => throw new IllegalStateException(s"foreign world owner was not refused: $other")

      Receipt(sampled, f"0x$pixel%08x", classpathEntries)
    finally
      Files.deleteIfExists(transformPath)
      Files.deleteIfExists(seriesPath)
      Files.deleteIfExists(path)
      Files.deleteIfExists(directory)

  private def analytic(x: Int, y: Int, z: Int): Double =
    100.0 * x + 10.0 * y + z

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
