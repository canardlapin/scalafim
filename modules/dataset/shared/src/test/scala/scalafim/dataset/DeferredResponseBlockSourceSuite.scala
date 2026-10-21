package scalafim.dataset

import neuroim4s.image.SampleSpaces
import gale.linalg.DMat

class DeferredResponseBlockSourceSuite extends munit.FunSuite:
  private val shape = DatasetShape.unsafe(SampleSpaces(Vector(2,1,1)),3)
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
      val shape = DatasetShape.unsafe(SampleSpaces(Vector(2,1,1)),4)
      val voxelDomain = voxels
      val metadata = DatasetMetadata.Empty
      protected[dataset] def readResolved(selection: ResolvedDataSelection): Either[DatasetError,FmriSeries] =
        reads += selection.nTimepoints
        Left(DatasetError.StorageFailure("must not read"))
    val source = DeferredResponseBlockSource.make(shape,voxels)(() => Right(other)).toOption.get
    assert(source.readBlock().left.toOption.exists(_.isInstanceOf[DatasetError.ShapeMismatch]))
    assertEquals(reads,0)
  }

  test("failed opens can retry and an admitted source is opened only once"):
    var opens = 0
    var reads = 0
    val actual = new ResponseBlockSource:
      val shape = DeferredResponseBlockSourceSuite.this.shape
      val voxelDomain = voxels
      val metadata = DatasetMetadata.Empty
      protected[dataset] def readResolved(selection: ResolvedDataSelection): Either[DatasetError,FmriSeries] =
        reads += 1
        FmriSeries.make(
          DMat.tabulate(selection.nTimepoints, selection.nVoxels)((r,c) =>
            selection.timepointIndices(r).value * 10.0 + selection.voxelIndexValues(c).value),
          selection.voxelIndexValues, selection.timepointIndices, shape, metadata)
    val deferred = DeferredResponseBlockSource.make(shape,voxels)(() =>
      opens += 1
      if opens == 1 then Left(DatasetError.StorageFailure("temporarily unavailable")) else Right(actual)
    ).toOption.get
    assert(deferred.readBlock(DataSelection(voxels = VoxelSelection.indices(99))).isLeft)
    assertEquals(opens,0)
    assert(deferred.readBlock().isLeft)
    val selection = DataSelection(time = TimepointSelection.indices(2,0), voxels = VoxelSelection.indices(1))
    val block = deferred.readBlock(selection).toOption.get
    assertEquals(block.timepoints,Vector(2,0))
    assertEquals(block.voxelIndices,Vector(1))
    assertEqualsDouble(block.data(0,0),21.0,0.0)
    assertEqualsDouble(block.data(1,0),1.0,0.0)
    assert(deferred.readBlock(selection).isRight)
    assertEquals(opens,2)
    assertEquals(reads,2)

  test("identical geometry in a foreign frame cannot replace the reviewed acquisition"):
    var reads = 0
    val foreign = new ResponseBlockSource:
      val shape = DatasetShape.unsafe(SampleSpaces(Vector(2,1,1)),3)
      val voxelDomain = VoxelDomain.fullUnsafe(shape)
      val metadata = DatasetMetadata.Empty
      protected[dataset] def readResolved(selection: ResolvedDataSelection): Either[DatasetError,FmriSeries] =
        reads += 1
        Left(DatasetError.StorageFailure("must not read"))
    val deferred = DeferredResponseBlockSource.make(shape,voxels)(() => Right(foreign)).toOption.get
    assert(deferred.readBlock().left.toOption.exists(_.isInstanceOf[DatasetError.ShapeMismatch]))
    assertEquals(reads,0)
