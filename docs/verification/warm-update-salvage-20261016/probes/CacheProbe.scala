package salvage
import java.nio.file.{Files, Path}
import java.util.zip.GZIPOutputStream
import scalafim.dataset.io.NiftiStagingCache
object CacheProbe:
  def main(args: Array[String]): Unit =
    val root = Files.createTempDirectory("scalafim-cache-salvage-")
    val source = root.resolve("synthetic.nii.gz")
    def write(value: Byte): Unit =
      val stream = new GZIPOutputStream(Files.newOutputStream(source))
      try stream.write(Array.fill[Byte](512)(value)) finally stream.close()
    write(1)
    val size = Files.size(source)
    val timestamp = Files.getLastModifiedTime(source)
    val cache = NiftiStagingCache.unsafe(root.resolve("cache"))
    val first = cache.stage(source).toOption.get
    require(Files.readAllBytes(first).forall(_ == 1.toByte))
    write(2)
    require(Files.size(source) == size, "fixture must retain compressed size")
    Files.setLastModifiedTime(source, timestamp)
    val second = cache.stage(source).toOption.get
    val fresh = Files.readAllBytes(second).forall(_ == 2.toByte)
    Files.write(second, Array.fill[Byte](512)(3.toByte))
    val afterCorruption = cache.stage(source).toOption.get
    val repaired = Files.readAllBytes(afterCorruption).forall(_ == 2.toByte)
    println(s"""{"sameSourceSize":true,"sameSourceMtime":true,"returnedFreshContent":$fresh,"sameCachePath":${first == second},"repairedCorruptedCache":$repaired}""")
