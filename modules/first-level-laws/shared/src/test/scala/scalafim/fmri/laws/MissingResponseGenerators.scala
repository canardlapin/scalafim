package scalafim.fmri.laws

import org.scalacheck.{Gen, Shrink}

final case class MissingResponseCase(
    runLengths: Vector[Int],
    voxels: Int,
    missingVoxelCount: Int,
    selectedTimepoints: Vector[Int],
    voxelOrder: Vector[Int],
    chunkWidth: Int,
    seed: Int
):
  val rows: Int = runLengths.sum
  val selectedMissingVoxels: Set[Int] = (0 until missingVoxelCount).toSet
  val retainedVoxels: Set[Int] = (0 until voxels).toSet -- selectedMissingVoxels
  val omittedTimepoints: Vector[Int] = (0 until rows).filterNot(selectedTimepoints.contains).toVector

object MissingResponseGenerators:

  val cases: Gen[MissingResponseCase] =
    for
      firstRun <- Gen.choose(10, 15)
      secondRun <- Gen.choose(10, 15)
      voxels <- Gen.choose(3, 6)
      missing <- Gen.choose(1, voxels - 1)
      seed <- Gen.choose(0, 1000000)
      width <- Gen.choose(1, voxels)
    yield
      val rows = firstRun + secondRun
      val omitted = Set(1, firstRun + 1)
      val selected = (0 until rows).filterNot(omitted.contains).toVector
      val rotation = seed % voxels
      val rotated = (0 until voxels).toVector.drop(rotation) ++ (0 until voxels).toVector.take(rotation)
      val order = if seed % 2 == 0 then rotated else rotated.reverse
      MissingResponseCase(
        runLengths = Vector(firstRun, secondRun),
        voxels = voxels,
        missingVoxelCount = missing,
        selectedTimepoints = selected,
        voxelOrder = order,
        chunkWidth = width,
        seed = seed
      )

  given Shrink[MissingResponseCase] = Shrink.withLazyList { generated =>
    val simplerVoxels = math.max(3, generated.voxels - 1)
    val simplerMissing = math.min(generated.missingVoxelCount, simplerVoxels - 1)
    val simplerOrder = generated.voxelOrder.filter(_ < simplerVoxels)
    val candidates = Vector(
      generated.copy(chunkWidth = 1),
      generated.copy(seed = 0),
      generated.copy(
        voxels = simplerVoxels,
        missingVoxelCount = simplerMissing,
        voxelOrder = simplerOrder,
        chunkWidth = math.min(generated.chunkWidth, simplerVoxels)
      )
    )
    LazyList.from(candidates.distinct.filterNot(_ == generated))
  }
