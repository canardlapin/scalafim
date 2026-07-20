package scalafim.spatial

import scalafim.image.{DMat, DenseFieldMorphism, GridSpec, Indexing, NArrayUtil, NDArray, NeuroSpace, Resample, SpatialDomainId, SpatialPoint}
import scalafim.linalg.{DoubleMatrix, LinearMapError}

class NonlinearPullbackSuite extends munit.FunSuite:

  private def spatialValue[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def imageValue[A](result: Either[scalafim.image.MorphismError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def linearValue[A](result: Either[LinearMapError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def domain(name: String): Domain =
    val id = spatialValue(DomainId(name))
    val subject = spatialValue(SubjectId("sub-01"))
    val modality = spatialValue(Modality(name))
    val geometry =
      spatialValue(SamplingGeometry.volume(NeuroSpace(Vector(4, 1, 1), trans = Some(DMat.eye(4)))))
    spatialValue(Domain.build(id, SpaceRef.Volume(subject, None, modality), geometry))

  private def denseCoordinates(
    source: Domain,
    target: Domain,
    sourceX: Vector[Double]
  ): DenseFieldMorphism =
    require(sourceX.length == 4)
    val grid = GridSpec.identity(Vector(4, 1, 1))
    val data =
      NArrayUtil.tabulate[Double](grid.nVoxels * 3) { i =>
        val component = i / grid.nVoxels
        val coordinate = Indexing.indexToGrid3D(grid.shape, i % grid.nVoxels)
        component match
          case 0 => sourceX(coordinate.x)
          case 1 => coordinate.y.toDouble
          case _ => coordinate.z.toDouble
      }
    imageValue(
      DenseFieldMorphism.coordinates(
        SpatialDomainId(source.id.value),
        SpatialDomainId(target.id.value),
        grid,
        NDArray(data, grid.dims :+ 3),
        interpolation = Resample.Method.Linear
      )
    )

  private def warp(
    name: String,
    source: Domain,
    target: Domain,
    forward: DenseFieldMorphism,
    inverse: Option[DenseFieldMorphism] = None,
    inverseClaim: Inverse = Inverse.None
  ): Morphism =
    spatialValue(
      Morphism.build(
        id = spatialValue(MorphismId(name)),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Warp3D,
        routeTag = RouteTag.Anatomical,
        inverse = inverseClaim,
        coordinateMap = spatialValue(CoordinateMap.dense3D(forward, inverse))
      )
    )

  private def affine(source: Domain, target: Domain, translation: Double): Morphism =
    val matrix =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, translation),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    spatialValue(
      Morphism.build(
        id = spatialValue(MorphismId("affine")),
        source = source.id,
        target = target.id,
        kind = MorphismKind.Affine3D,
        routeTag = RouteTag.Anatomical,
        inverse = Inverse.Exact("analytic"),
        coordinateMap = spatialValue(CoordinateMap.affine3D(matrix))
      )
    )

  private def graph(domains: Vector[Domain], morphisms: Vector[Morphism]): SpatialGraph =
    spatialValue(SpatialGraph.build(domains, morphisms))

  test("an affine-equivalent dense coordinate field matches the affine one-pass operator"):
    val root = domain("root")
    val target = domain("target")
    val dense = warp("dense-half", root, target, denseCoordinates(root, target, Vector(0.5, 1.5, 2.5, 3.5)))
    val affineMap = affine(root, target, 0.5)
    val denseOperator =
      spatialValue(OperatorCompiler.compile(graph(Vector(root, target), Vector(dense)), CompileRequest(root.id, target.id)))
    val affineOperator =
      spatialValue(OperatorCompiler.compile(graph(Vector(root, target), Vector(affineMap)), CompileRequest(root.id, target.id)))
    val values = DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(8.0), Vector(0.0), Vector(4.0)))

    val denseResult = linearValue(denseOperator.forward(values))
    val affineResult = linearValue(affineOperator.forward(values))

    assertEquals(denseResult.copyData.toVector, affineResult.copyData.toVector)
    assertEquals(denseOperator.qc.coverage, affineOperator.qc.coverage)
    assertEquals(denseOperator.provenance.compiler, "volume-pullback-fused-v1")

  test("an independent nonlinear coordinate fixture fixes direction and convention"):
    val root = domain("root")
    val target = domain("target")
    val transform = warp("nonlinear", root, target, denseCoordinates(root, target, Vector(0.0, 0.5, 2.5, 3.0)))
    val g = graph(Vector(root, target), Vector(transform))
    val program = spatialValue(PullbackProgram.compile(g, CompileRequest(root.id, target.id)))
    val operator = spatialValue(OperatorCompiler.compile(g, CompileRequest(root.id, target.id)))
    val values = DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(10.0), Vector(20.0), Vector(30.0)))
    val result = linearValue(operator.forward(values))

    assertEquals(spatialValue(program.pullback(SpatialPoint(1.0, 0.0, 0.0))), SpatialPoint(0.5, 0.0, 0.0))
    assertEquals(spatialValue(program.pullback(SpatialPoint(2.0, 0.0, 0.0))), SpatialPoint(2.5, 0.0, 0.0))
    assertEquals(result.copyData.toVector, Vector(0.0, 5.0, 25.0, 30.0))
    assertEquals(program.compilerTrace.map(_.value), Vector("dense-warp-pullback-v1"))
    assertEquals(program.steps.map(_.coordinateMap.fingerprint), Vector(transform.coordinateMap.fingerprint))

  test("dense map content participates in pullback fingerprints"):
    val root = domain("root")
    val target = domain("target")
    val first = warp("same-id", root, target, denseCoordinates(root, target, Vector(0.0, 1.0, 2.0, 3.0)))
    val second = warp("same-id", root, target, denseCoordinates(root, target, Vector(0.0, 0.75, 2.0, 3.0)))
    val firstProgram =
      spatialValue(PullbackProgram.compile(graph(Vector(root, target), Vector(first)), CompileRequest(root.id, target.id)))
    val secondProgram =
      spatialValue(PullbackProgram.compile(graph(Vector(root, target), Vector(second)), CompileRequest(root.id, target.id)))

    assertNotEquals(first.coordinateMap.fingerprint, second.coordinateMap.fingerprint)
    assertNotEquals(firstProgram.fingerprint, secondProgram.fingerprint)

  test("dense inverse availability is supplied explicitly and preserves direction"):
    val root = domain("root")
    val target = domain("target")
    val forward = denseCoordinates(root, target, Vector(0.0, 1.0, 2.0, 3.0))
    val reverse = denseCoordinates(target, root, Vector(0.0, 1.0, 2.0, 3.0))
    val unavailable = warp("without-inverse", root, target, forward, inverseClaim = Inverse.Approximate("fit", 0.9))
    val available =
      warp(
        "with-inverse",
        root,
        target,
        forward,
        inverse = Some(reverse),
        inverseClaim = Inverse.Approximate("fit", 0.9)
      )

    assertEquals(unavailable.reversed.left.toOption, Some(SpatialError.NonInvertibleMorphism(unavailable.id)))
    val reversed = spatialValue(available.reversed)
    assertEquals(reversed.source, target.id)
    assertEquals(reversed.target, root.id)
    assert(reversed.isInverted)
    assertEquals(
      spatialValue(reversed.coordinateMap.transform(SpatialPoint(2.0, 0.0, 0.0))),
      SpatialPoint(2.0, 0.0, 0.0)
    )
    assertEquals(ImageMorphismBridge.lower(available).map(_.kind).toOption, Some(scalafim.image.MorphismKind.DenseCoordinateField))

  test("the default lazy runtime materializes nonlinear views with one value resampling"):
    val root = domain("root")
    val target = domain("target")
    val transform = warp("lazy-nonlinear", root, target, denseCoordinates(root, target, Vector(0.0, 0.5, 2.5, 3.0)))
    val g = graph(Vector(root, target), Vector(transform))
    val field =
      Field.fromMatrix(
        root.id,
        DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(10.0), Vector(20.0), Vector(30.0))),
        "root-values"
      )
    val view = apiValue(field.to(target)(using g))
    val runtime = LazyFieldRuntime(g)
    val result = apiValue(view.value(using runtime))

    assertEquals(result.copyData.toVector, Vector(0.0, 5.0, 25.0, 30.0))
    assertEquals(runtime.stats.executions, 1L)
    assertEquals(runtime.lastTrace.map(_.valueResamplingPasses), Some(1))
