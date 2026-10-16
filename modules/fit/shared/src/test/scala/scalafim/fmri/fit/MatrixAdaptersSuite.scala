package scalafim.fmri.fit

import gale.linalg.Matrix

class MatrixAdaptersSuite extends munit.FunSuite:
  test("Gale column binding preserves logical row-major values"):
    val left = Matrix.dense(2, 1, Seq(1.0, 3.0))
    val right = Matrix.dense(2, 1, Seq(2.0, 4.0))
    val bound = MatrixAdapters.bindColumns(left, right)

    assertEquals((bound.rows, bound.cols), (2, 2))
    assertEqualsDouble(bound(0, 0), 1.0, 0.0)
    assertEqualsDouble(bound(0, 1), 2.0, 0.0)
    assertEqualsDouble(bound(1, 0), 3.0, 0.0)
    assertEqualsDouble(bound(1, 1), 4.0, 0.0)

  test("fitting requires the scan selection recorded by the coverage policy"):
    import scalafim.dataset.*
    import scalafim.image.SampleSpaces
    import scalafim.fmri.hrf.*
    import scalafim.fmri.hrf.design.SamplingFrame
    import scalafim.fmri.design.{EventSupportPolicy, ResponseSupportRequest, ScanIndex}
    import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec}
    val frame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0), startTime = Seq(0.0))
    val dataset = FmriDataset.unsafe(
      InMemoryDatasetBackend(DatasetId("support-fit"), Matrix.dense(4, 1, Seq(1.0, 2.0, 3.0, 4.0)), SampleSpaces(Vector(1, 1, 1))),
      frame, events = DatasetEvents(Vector(Map("onset" -> "0.0", "condition" -> "A")))).dataset
    val request = ResponseSupportRequest((1 to 4).map(ScanIndex.unsafeOneBased).toVector,
      EventSupportPolicy.RejectUnobservedAllowPartial)
    val model = FmriModelBuilder.buildModelEither(dataset,
      ModelBuildSpec("onset ~ hrf(condition)", defaultHrf = Hrfs.fir(1, 2.s), responseSupport = Some(request)))
      .fold(error => fail(error.message), identity)
    assert(MatrixAdapters.designMatrix(model, Vector(0, 1, 2, 3)).isRight)
    assert(MatrixAdapters.designMatrix(model, Vector(0, 2, 3)).left.exists {
      case FitError.InvalidFitAxis("event response support", detail) => detail.contains("rebuild")
      case _ => false
    })
    assert(MatrixAdapters.designMatrix(model, Vector(-1, 1)).isLeft)
