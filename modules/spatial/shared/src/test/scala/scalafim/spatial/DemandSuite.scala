package scalafim.spatial

import scalafim.image.{DMat, NeuroSpace, SpatialAxis, VoxelCoord}
import scalafim.linalg.{DoubleMatrix, LinearMapError}
import scalafim.surface.{Hemisphere, SurfaceGeometry, SurfaceKind, TriangleMesh}

class DemandSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def demandValue[A](result: Either[DemandError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def linearValue[A](result: Either[LinearMapError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def planValue[A](result: Either[ViewPlanError, A]): A =
    result match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def volumeDomain(name: String, dims: Vector[Int] = Vector(3, 2, 2)): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry = spatialValue(SamplingGeometry.volume(NeuroSpace(dims, trans = Some(DMat.eye(4)))))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def surfaceDomain(name: String): Domain =
    val mesh =
      TriangleMesh.fromRows(
        vertices = Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(1.0, 1.0, 0.0)
        ),
        faces = Vector((0, 1, 2), (1, 3, 2))
      )
    val geometry = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Midthickness)
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val sampling = spatialValue(SamplingGeometry.surface(geometry))
    spatialValue(Domain.build(id, SpaceRef.Surface(subject, Hemisphere.Left, SurfaceKind.Midthickness), sampling))

  private def identityAffine(name: String, source: Domain, target: Domain): Morphism =
    spatialValue(
      Morphism.build(
        id = spatialValue(MorphismId(name)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = spatialValue(CoordinateMap.affine3D(DMat.eye(4)))
      )
    )

  test("volume, surface, ROI, mask, and time demands normalize to target indices"):
    val volume = volumeDomain("volume")
    val surface = surfaceDomain("surface")
    val block = demandValue(TimeBlock.build(start = 1, length = 2))
    val region = demandValue(VoxelBox.build(VoxelCoord(1, 0, 0), VoxelCoord(3, 2, 1)))

    val voxels = demandValue(
      ResolvedDemand.resolve(
        FieldDemand.voxels(Vector(VoxelCoord(2, 1, 0), VoxelCoord(0, 0, 1))),
        volume,
        observations = 4
      )
    )
    val slice = demandValue(ResolvedDemand.resolve(FieldDemand.slice(SpatialAxis.Z, 1), volume, 4))
    val box = demandValue(ResolvedDemand.resolve(FieldDemand.box(region), volume, 4))
    val mask = demandValue(
      ResolvedDemand.resolve(
        FieldDemand.mask(Vector(true, false, true, false, false, false, false, false, false, false, false, true)),
        volume,
        4
      )
    )
    val vertices = demandValue(ResolvedDemand.resolve(FieldDemand.vertices(Vector(3, 1)), surface, 4))
    val timed = demandValue(ResolvedDemand.resolve(FieldDemand.time(block), volume, 4))

    assertEquals(voxels.targetRows, Vector(5, 6))
    assertEquals(slice.targetRows, Vector(6, 7, 8, 9, 10, 11))
    assertEquals(box.targetRows, Vector(1, 2, 4, 5))
    assertEquals(mask.targetRows, Vector(0, 2, 11))
    assertEquals(vertices.targetRows, Vector(3, 1))
    assertEquals(timed.observationIndices, Vector(1, 2))

  test("row and time-block refinements compose relative to the current demand"):
    val volume = volumeDomain("volume")
    val firstBlock = demandValue(TimeBlock.build(1, 4))
    val secondBlock = demandValue(TimeBlock.build(1, 2))
    val first = demandValue(
      ResolvedDemand.resolve(
        FieldDemand(SpatialDemand.Rows(Vector(8, 2, 5)), ObservationDemand.Block(firstBlock)),
        volume,
        observations = 6
      )
    )
    val second = demandValue(
      first.refine(
        FieldDemand(SpatialDemand.Rows(Vector(2, 0)), ObservationDemand.Block(secondBlock)),
        volume
      )
    )

    assertEquals(second.targetRows, Vector(5, 8))
    assertEquals(second.observationIndices, Vector(2, 3))
    assertNotEquals(first.fingerprint, second.fingerprint)

  test("structured demands enforce domain kinds, bounds, masks, and terminal composition"):
    val volume = volumeDomain("volume")
    val surface = surfaceDomain("surface")
    val selected = demandValue(ResolvedDemand.resolve(FieldDemand.rows(Vector(2, 1)), volume, 3))

    assertEquals(
      ResolvedDemand.resolve(FieldDemand.vertices(Vector(0)), volume, 3).left.toOption,
      Some(DemandError.DomainKindMismatch("vertex", DomainKind.Surface, DomainKind.Volume))
    )
    assertEquals(
      ResolvedDemand.resolve(FieldDemand.slice(SpatialAxis.Z, 2), volume, 3).left.toOption,
      Some(DemandError.IndexOutOfBounds("slice", 2, 2))
    )
    assertEquals(
      ResolvedDemand.resolve(FieldDemand.mask(Vector(true)), surface, 3).left.toOption,
      Some(DemandError.MaskLengthMismatch(4, 1))
    )
    assertEquals(
      selected.refine(FieldDemand.slice(SpatialAxis.X, 0), volume).left.toOption,
      Some(DemandError.StructuredSelectionRequiresFullDomain("slice"))
    )

  test("normalized demand fingerprints include spatial and observation semantics"):
    val volume = volumeDomain("volume")
    val rows = demandValue(ResolvedDemand.resolve(FieldDemand.rows(Vector(4, 1)), volume, 5))
    val roi = demandValue(ResolvedDemand.resolve(FieldDemand.roi(Vector(4, 1)), volume, 5))
    val timed = demandValue(
      rows.refine(FieldDemand.time(demandValue(TimeBlock.build(1, 2))), volume)
    )

    assertEquals(rows.fingerprint, roi.fingerprint)
    assertNotEquals(rows.fingerprint, timed.fingerprint)
    assert(rows.fingerprint.value.contains("rows=4,1"))
    assert(timed.fingerprint.value.contains("observations=1,2"))

  test("ViewPlan retains normalized spatial and time demand without executing"):
    val volume = volumeDomain("volume")
    val rootId = planValue(FieldRootId("run-01"))
    val root = planValue(ViewPlan.root(rootId, volume.id, volume.nElements, observations = 5))
    val block = demandValue(TimeBlock.build(2, 2))
    val selected = planValue(
      root.select(
        FieldDemand(SpatialDemand.Slice(SpatialAxis.Z, 1), ObservationDemand.Block(block)),
        volume
      )
    )

    assertEquals(selected.sampleCount, 6)
    assertEquals(selected.observations, 2)
    assertEquals(selected.intent.rowSelection, RowSelection.Rows(Vector(6, 7, 8, 9, 10, 11)))
    assertEquals(selected.intent.observationSelection, ObservationSelection.Indices(Vector(2, 3)))
    assertEquals(selected.steps.length, 1)
    selected.steps.head match
      case ViewPlanStep.SelectDemand(domain, _, rows, observations) =>
        assertEquals(domain, volume.id)
        assertEquals(rows, Vector(6, 7, 8, 9, 10, 11))
        assertEquals(observations, Vector(2, 3))
      case other =>
        fail(s"expected a demand plan step, got $other")

  test("selected evaluation matches full evaluation and narrows exact source support"):
    val root = volumeDomain("root", Vector(4, 1, 1))
    val target = volumeDomain("target", Vector(4, 1, 1))
    val graph = spatialValue(SpatialGraph.build(Vector(root, target), Vector(identityAffine("root-target", root, target))))
    val selectedRequest =
      CompileRequest.forRows(
        root.id,
        target.id,
        RowSelection.Rows(Vector(3, 1)),
        sampling = SamplingPolicy.Nearest
      )
    val full = spatialValue(OperatorCompiler.compile(graph, CompileRequest(root.id, target.id, sampling = SamplingPolicy.Nearest)))
    val program = spatialValue(PullbackProgram.compile(graph, selectedRequest))
    val selected = spatialValue(VolumeAffineOperatorCompiler.compile(program))
    val support = spatialValue(PullbackSupportPlanner.standard.plan(program, selected))
    val data = DoubleMatrix.fromRows(
      Vector(
        Vector(0.0, 10.0, 20.0, 30.0),
        Vector(1.0, 11.0, 21.0, 31.0),
        Vector(2.0, 12.0, 22.0, 32.0),
        Vector(3.0, 13.0, 23.0, 33.0)
      )
    )
    val fullValues = linearValue(full.forward(data)).toRows
    val selectedValues = linearValue(selected.forward(data)).toRows
    val time = demandValue(TimeBlock.build(1, 2))
    val demand = demandValue(ResolvedDemand.resolve(FieldDemand.time(time), target, data.cols))
    val source = RecordingSource(data)

    source.read(support.sourceRows, demand.observationIndices)

    assertEquals(selectedValues, Vector(fullValues(3), fullValues(1)))
    assertEquals(support.sourceRows, Vector(1, 3))
    assertEquals(support.precision, SupportPrecision.Exact)
    assertEquals(source.requestedRows, Vector(1, 3))
    assertEquals(source.requestedObservations, Vector(1, 2))
    assert(support.sourceRows.length < root.nElements)
    assert(demand.observationIndices.length < data.cols)

private final class RecordingSource(data: DoubleMatrix):
  var requestedRows: Vector[Int] = Vector.empty
  var requestedObservations: Vector[Int] = Vector.empty

  def read(rows: Vector[Int], observations: Vector[Int]): Unit =
    requestedRows = rows
    requestedObservations = observations
    assert(rows.forall(row => row >= 0 && row < data.rows))
    assert(observations.forall(observation => observation >= 0 && observation < data.cols))
