package scalafim.transform.scenarios

import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import scalafim.transform.AssetRef
import scalafim.transform.itk.{ItkHdf5Container, ItkHdf5File}

/** Explicit external-asset gate. Missing paths or wrong hashes fail; this is not
  * a silently skipped default-CI test. Invoke Test/runMain with both original H5 paths.
  */
object Demo1OriginalContainerQualification:
  import Demo1NativeContract.*

  def main(args: Array[String]): Unit =
    require(args.length == 2, "provide the original forward and inverse HDF5 paths")
    val specifications = Vector(
      (args(0), ForwardSha, "t1_to_template", "t1"),
      (args(1), InverseSha, "template_to_t1", "inverse")
    )
    specifications.foreach: (path, expectedHash, cropName, direction) =>
      val bytes = Files.readAllBytes(Paths.get(path))
      val digest = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
      require(digest == expectedHash, s"original source identity mismatch: $path")
      val original = checked(ItkHdf5Container.read(bytes))
      val crop = file(cropName)
      val retained = assertRetainedSamples(original, crop)
      val asset = AssetRef(Paths.get(path).getFileName.toString, Some(digest))
      val observations = if direction == "t1" then
        val forward = t1Transform(original, asset)
        nativePointObservations("t1", template)(forward) ++
          nativePointObservations("bold", template)(scannerTransform(false).andThen(forward))
      else
        val backward = inverseTransform(original, asset)
        nativePointObservations("inverse", t1)(backward) ++ nativePointObservations("roundtrip", t1)(backward)
      require(observations.forall(_.passed), observations.filterNot(_.passed).map(_.render).mkString("\n"))
      println(s"DEMO1_ORIGINAL_CONTAINER direction=$direction sha256=$digest retained_values=$retained native_queries=${observations.size} status=pass")

  private def assertRetainedSamples(original: ItkHdf5File, crop: ItkHdf5File): Int =
    require(original.components.map(_.typeName) == crop.components.map(_.typeName), "stage order/types changed")
    var retained = 0
    original.components.zip(crop.components).foreach: (full, part) =>
      if !full.isDisplacementField then require(full == part, "affine parameters, centre or composite marker changed")
      else
        val a = full.fixedParameters
        val b = part.fixedParameters
        require((6 until 18).forall(i => a(i) == b(i)), "field spacing/direction changed")
        val start = Vector.tabulate(3): axis =>
          val value = (0 until 3).map(row => a(9+3*row+axis) * (b(3+row)-a(3+row))).sum / a(6+axis)
          require(value == math.rint(value), "crop origin is not an exact original voxel")
          value.toInt
        val size = Vector.tabulate(3)(axis => b(axis).toInt)
        require((0 until 3).forall(i => start(i) >= 0 && start(i)+size(i) <= a(i).toInt), "crop exceeds original field")
        var z = 0
        while z < size(2) do
          var y = 0
          while y < size(1) do
            var x = 0
            while x < size(0) do
              val fullIndex = 3 * (start(0)+x+a(0).toInt * (start(1)+y+a(1).toInt * (start(2)+z)))
              val partIndex = 3 * (x+size(0) * (y+size(1)*z))
              var component = 0
              while component < 3 do
                require(java.lang.Double.doubleToRawLongBits(full.parameters(fullIndex+component)) ==
                  java.lang.Double.doubleToRawLongBits(part.parameters(partIndex+component)), "retained field sample bits changed")
                retained += 1
                component += 1
              x += 1
            y += 1
          z += 1
    retained
