package scalafim.dataset

import scalafim.image.NeuroSpace

class DeferredResponseBlockSourceSuite extends munit.FunSuite:
  private val shape = DatasetShape(NeuroSpace(Vector(2,1,1)),3)
  private val voxels = VoxelDomain.fullUnsafe(shape)
  test("declared shape and selection resolution do not open response storage") {
    var opens = 0
    val source = DeferredResponseBlockSource.make(shape,voxels)(() => {
      opens += 1
      Left(DatasetError.StorageFailure("unavailable payload"))
    }).toOption.get
    assertEquals(source.shape,shape)
    assert(DataSelection.All.resolveEither(source.acquisitionDomain).isRight)
    assertEquals(opens,0)
    assert(source.readBlock().isLeft)
    assertEquals(opens,1)
  }
  test("changed acquisition fails before delegating a read") {
    var reads = 0
    val other = new ResponseBlockSource:
      val shape = DatasetShape(NeuroSpace(Vector(2,1,1)),4)
      val voxelDomain = voxels
      val metadata = DatasetMetadata.Empty
      protected[dataset] def readResolved(selection: ResolvedDataSelection): Either[DatasetError,FmriSeries] =
        reads += selection.nTimepoints
        Left(DatasetError.StorageFailure("must not read"))
    val source = DeferredResponseBlockSource.make(shape,voxels)(() => Right(other)).toOption.get
    assert(source.readBlock().left.toOption.exists(_.isInstanceOf[DatasetError.ShapeMismatch]))
    assertEquals(reads,0)
  }
