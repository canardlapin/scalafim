package scalafim.spatial

import scalafim.image.{DMat, NeuroSpace}

class FieldSourceSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def volumeDomain(name: String, voxels: Int = 4): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(
      SamplingGeometry.volume(NeuroSpace(Vector(voxels, 1, 1), trans = Some(DMat.eye(4))))
    )
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def descriptor(domain: Domain, label: String = "archive-run"): FieldSourceDescriptor =
    spatialValue(
      FieldSourceDescriptor.make(
        spatialValue(FieldSourceId(s"source:${domain.id.value}")),
        spatialValue(FieldSourceRevision("revision-1")),
        label,
        domain.id,
        domain.geometry,
        domain.nElements,
        observations = 3
      )
    )

  private def rootData: DoubleMatrix =
    DoubleMatrix.fromRows(
      Vector(
        Vector(0.0, 10.0, 20.0),
        Vector(1.0, 11.0, 21.0),
        Vector(2.0, 12.0, 22.0),
        Vector(3.0, 13.0, 23.0)
      )
    )

  test("source-backed views remain IO-free and terminal demand performs one compact read"):
    val root = volumeDomain("root")
    given SpatialGraph = spatialValue(SpatialGraph.build(Vector(root), Vector.empty))
    val source = RecordingFieldSource(descriptor(root), rootData)
    val field = spatialValue(Field.fromSource(root, source))
    val view = apiValue(field.rows(3, 1).flatMap(_.timeBlock(1, 2)))

    assertEquals(source.validationCalls, 0)
    assertEquals(source.requests, Vector.empty)
    assertEquals(view.describe.label, "archive-run")

    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime
    val first = apiValue(view.value)
    val second = apiValue(view.value)

    assertEquals(first.toRows, Vector(Vector(13.0, 23.0), Vector(11.0, 21.0)))
    assertEquals(second.toRows, first.toRows)
    assertEquals(source.validationCalls, 2)
    assertEquals(source.requests.length, 1)
    assertEquals(source.requests.head.sourceRows, Vector(1, 3))
    assertEquals(source.requests.head.observations, Vector(1, 2))
    assertEquals(runtime.stats.executions, 1L)
    assertEquals(runtime.stats.resultCacheHits, 1L)

  test("cached values never hide a source that has become stale"):
    val root = volumeDomain("root")
    given SpatialGraph = spatialValue(SpatialGraph.build(Vector(root), Vector.empty))
    val source = RecordingFieldSource(descriptor(root), rootData)
    val field = spatialValue(Field.fromSource(root, source))
    val runtime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = runtime

    assertEquals(apiValue(field.value).rows, 4)
    source.validationFailure = Some(
      SpatialError.FieldSourceStale(source.descriptor.id, "revision-1", "revision-2")
    )

    assertEquals(
      field.value.left.toOption,
      Some(FieldApiError.Spatial(SpatialError.FieldSourceStale(source.descriptor.id, "revision-1", "revision-2")))
    )
    assertEquals(source.requests.length, 1)
    assertEquals(runtime.stats.resultCacheHits, 0L)

  test("source construction validates root domain, geometry, and shape without reading"):
    val expected = volumeDomain("expected")
    val other = volumeDomain("other")
    val wrongDomain = RecordingFieldSource(descriptor(other), rootData)

    assertEquals(
      Field.fromSource(expected, wrongDomain).left.toOption,
      Some(SpatialError.FieldSourceDomainMismatch(wrongDomain.descriptor.id, expected.id, other.id))
    )

    val wrongGeometryDescriptor = spatialValue(
      FieldSourceDescriptor.make(
        spatialValue(FieldSourceId("wrong-geometry")),
        spatialValue(FieldSourceRevision("revision-1")),
        "wrong",
        expected.id,
        volumeDomain("geometry", voxels = 5).geometry,
        rows = 5,
        observations = 3
      )
    )
    val wrongGeometry = RecordingFieldSource(wrongGeometryDescriptor, rootData)
    assertEquals(
      Field.fromSource(expected, wrongGeometry).left.toOption,
      Some(SpatialError.FieldSourceGeometryMismatch(wrongGeometry.descriptor.id))
    )
    assertEquals(wrongDomain.validationCalls, 0)
    assertEquals(wrongGeometry.validationCalls, 0)
    assertEquals(wrongDomain.requests, Vector.empty)
    assertEquals(wrongGeometry.requests, Vector.empty)

  test("unavailable sources and request-contract violations are typed and do not poison caches"):
    val root = volumeDomain("root")
    given SpatialGraph = spatialValue(SpatialGraph.build(Vector(root), Vector.empty))
    val unavailable = RecordingFieldSource(descriptor(root), rootData)
    unavailable.validationFailure = Some(
      SpatialError.FieldSourceUnavailable(unavailable.descriptor.id, "offline")
    )
    val unavailableField = spatialValue(Field.fromSource(root, unavailable))
    val unavailableRuntime = LazyFieldRuntime(summon[SpatialGraph])
    given FieldRuntime = unavailableRuntime

    assertEquals(
      unavailableField.value.left.toOption,
      Some(FieldApiError.Spatial(SpatialError.FieldSourceUnavailable(unavailable.descriptor.id, "offline")))
    )
    assertEquals(unavailable.requests, Vector.empty)

    val mismatched = RequestMismatchingSource(descriptor(root), rootData)
    val mismatchedField = spatialValue(Field.fromSource(root, mismatched))
    assertEquals(
      mismatchedField.rows(0, 1).flatMap(_.value).left.toOption,
      Some(FieldApiError.Spatial(SpatialError.FieldSourceRequestMismatch(mismatched.descriptor.id)))
    )
    assertEquals(unavailableRuntime.stats.executions, 0L)

private final class RecordingFieldSource(
  override val descriptor: FieldSourceDescriptor,
  root: DoubleMatrix
) extends FieldSource:
  var validationCalls: Int = 0
  var validationFailure: Option[SpatialError] = None
  var requests: Vector[FieldSourceRequest] = Vector.empty

  override def validate(): Either[SpatialError, Unit] =
    validationCalls += 1
    validationFailure match
      case Some(error) => Left(error)
      case None => Right(())

  override def read(request: FieldSourceRequest): Either[SpatialError, FieldSourceBlock] =
    requests = requests :+ request
    val rootValues = root.copyData
    val selected = new Array[Double](request.rows * request.columns)
    var outputRow = 0
    while outputRow < request.rows do
      val sourceRow = request.sourceRows(outputRow)
      var outputColumn = 0
      while outputColumn < request.columns do
        val sourceColumn = request.observations(outputColumn)
        selected(outputRow * request.columns + outputColumn) =
          rootValues(sourceRow * root.cols + sourceColumn)
        outputColumn += 1
      outputRow += 1
    FieldSourceBlock.make(
      descriptor.id,
      request,
      DoubleMatrix.unsafe(request.rows, request.columns, selected)
    )

private final class RequestMismatchingSource(
  override val descriptor: FieldSourceDescriptor,
  root: DoubleMatrix
) extends FieldSource:
  override def validate(): Either[SpatialError, Unit] =
    Right(())

  override def read(request: FieldSourceRequest): Either[SpatialError, FieldSourceBlock] =
    val otherRows = request.sourceRows.reverse
    FieldSourceRequest.make(descriptor, otherRows, request.observations).flatMap { other =>
      FieldSourceBlock.make(
        descriptor.id,
        other,
        DoubleMatrix.unsafe(other.rows, other.columns, new Array[Double](other.rows * other.columns))
      )
    }
