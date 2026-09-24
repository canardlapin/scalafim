package scalafim.transform

import java.nio.file.{Files, Path}

import io.jhdf.HdfFile
import io.jhdf.api.WritableGroup
import scalafim.transform.itk.ItkHdf5File
import scalafim.transform.x5.X5File

import scala.util.control.NonFatal

/** Writes ITK HDF5 transform files and X5 files (jHDF, JVM only). Values are written as float64, so reading back
  * reproduces every parameter exactly.
  */
object Hdf5Writers:
  def itk(file: ItkHdf5File): Either[TransformIoError, Array[Byte]] =
    viaTemp("itk"): root =>
      val group = root.putGroup("TransformGroup")
      file.components.foreach: component =>
        val node = group.putGroup(component.index.toString)
        node.putDataset("TransformType", Array(component.typeName))
        if !component.isComposite then
          node.putDataset("TransformParameters", IArray.genericWrapArray(component.parameters).toArray)
          node.putDataset("TransformFixedParameters", IArray.genericWrapArray(component.fixedParameters).toArray): Unit

  def x5(file: X5File): Either[TransformIoError, Array[Byte]] =
    viaTemp("x5"): root =>
      root.putAttribute("Format", "X5")
      root.putAttribute("Version", Array(1))
      val group = root.putGroup("TransformGroup")
      file.nodes.foreach: node =>
        val g = group.putGroup(node.index.toString)
        g.putAttribute("Type", node.kind)
        node.subtype.foreach(g.putAttribute("SubType", _))
        node.representation.foreach(g.putAttribute("Representation", _))
        g.putDataset("Transform", shaped(IArray.genericWrapArray(node.transform).toArray, node.shape))
        node.domain.foreach: domain =>
          val d = g.putGroup("Domain")
          d.putDataset("Grid", Array(if domain.grid then 1 else 0))
          d.putDataset("Size", domain.size.toArray)
          d.putDataset("Mapping", shaped(domain.mapping.toArray, Vector(4, 4))): Unit
      if file.chains.nonEmpty then
        val chains = root.putGroup("TransformChain")
        file.chains.zipWithIndex.foreach((chain, i) => chains.putDataset(i.toString, Array(chain.mkString("/"))): Unit)

  /** jHDF infers dataset shape from nested Java arrays; build them from C-order data. */
  private def shaped(values: Array[Double], shape: Vector[Int]): AnyRef =
    shape match
      case Vector(_)          => values
      case Vector(r, c)       => Array.tabulate(r, c)((i, j) => values(i * c + j))
      case Vector(a, b, c, d) => Array.tabulate(a, b, c, d)((i, j, k, l) => values(((i * b + j) * c + k) * d + l))
      case _                  => values

  private def viaTemp(prefix: String)(write: WritableGroup => Unit): Either[TransformIoError, Array[Byte]] =
    try
      val path: Path = Files.createTempFile(s"scalafim-$prefix-", ".h5")
      try
        val out = HdfFile.write(path)
        try write(out)
        finally out.close()
        Right(Files.readAllBytes(path))
      finally Files.deleteIfExists(path): Unit
    catch case NonFatal(error) => Left(TransformIoError.Malformed("HDF5 write", Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))
