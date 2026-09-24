package scalafim.transform.itk

import io.jhdf.HdfFile
import io.jhdf.api.{Dataset, Group}
import scalafim.transform.TransformIoError

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** Reads ITK HDF5 transform files (jHDF, JVM only) into the platform-neutral [[ItkHdf5File]] model. Accepts the
  * misspelled `TranformParameters`/`TranformFixedParameters` dataset names that some released files (e.g.
  * TemplateFlow's) contain, and float or double datasets.
  */
object ItkHdf5Container:
  private val ParameterNames = Vector("TransformParameters", "TranformParameters")
  private val FixedParameterNames = Vector("TransformFixedParameters", "TranformFixedParameters")

  def read(bytes: Array[Byte]): Either[TransformIoError, ItkHdf5File] =
    try
      val hdf = HdfFile.fromBytes(bytes)
      try
        hdf.getByPath("/TransformGroup") match
          case group: Group => components(group).map(ItkHdf5File(_))
          case _            => Left(malformed("missing /TransformGroup"))
      finally hdf.close()
    catch case NonFatal(error) => Left(malformed(Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))

  private def components(group: Group): Either[TransformIoError, Vector[ItkHdf5Component]] =
    val children = group.getChildren.asScala.toVector
    val indexed = children.map((name, node) => (name.toIntOption, name, node))
    if indexed.exists(_._1.isEmpty) then Left(malformed(s"TransformGroup has non-numeric children ${indexed.filter(_._1.isEmpty).map(_._2).mkString(",")}"))
    else
      val ordered = indexed.map((i, name, node) => (i.get, name, node)).sortBy(_._1)
      if ordered.isEmpty then Left(malformed("TransformGroup is empty"))
      else if ordered.map(_._1) != ordered.indices.toVector then Left(malformed(s"TransformGroup indices must be 0..n-1, got ${ordered.map(_._1).mkString(",")}"))
      else
        ordered.foldLeft[Either[TransformIoError, Vector[ItkHdf5Component]]](Right(Vector.empty)): (acc, entry) =>
          val (index, name, node) = entry
          acc.flatMap: done =>
            node match
              case child: Group =>
                for
                  kind <- string(child, "TransformType", index)
                  parameters <- numbers(child, ParameterNames)
                  fixed <- numbers(child, FixedParameterNames)
                yield done :+ ItkHdf5Component(index, kind, parameters, fixed)
              case _ => Left(malformed(s"TransformGroup/$name is not a group"))

  private def string(group: Group, name: String, index: Int): Either[TransformIoError, String] =
    group.getChild(name) match
      case dataset: Dataset =>
        dataset.getData match
          case value: String if value.nonEmpty                                    => Right(value)
          case values: Array[String] if values.length == 1 && values(0).nonEmpty => Right(values(0))
          case _                                                                   => Left(malformed(s"/TransformGroup/$index/$name is not one non-empty string"))
      case _ => Left(malformed(s"missing /TransformGroup/$index/$name"))

  private def numbers(group: Group, names: Vector[String]): Either[TransformIoError, IArray[Double]] =
    names.iterator.map(group.getChild).collectFirst { case d: Dataset => d } match
      case None => Right(IArray.empty[Double])
      case Some(dataset) =>
        dataset.getDataFlat match
          case v: Array[Double] => Right(IArray.unsafeFromArray(v))
          case v: Array[Float]  => Right(IArray.unsafeFromArray(v.map(_.toDouble)))
          case v: Array[Long]   => Right(IArray.unsafeFromArray(v.map(_.toDouble)))
          case v: Array[Int]    => Right(IArray.unsafeFromArray(v.map(_.toDouble)))
          case other            => Left(malformed(s"${dataset.getPath} is not numeric (${other.getClass.getSimpleName})"))

  private def malformed(reason: String): TransformIoError =
    TransformIoError.Malformed("ITK HDF5", reason)
