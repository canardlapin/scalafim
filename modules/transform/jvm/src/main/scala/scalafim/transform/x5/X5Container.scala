package scalafim.transform.x5

import io.jhdf.HdfFile
import io.jhdf.api.{Attribute, Dataset, Group, Node}
import scalafim.transform.TransformIoError

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** Reads X5 files (jHDF, JVM only) into the platform-neutral [[X5File]] model. */
object X5Container:
  def read(bytes: Array[Byte]): Either[TransformIoError, X5File] =
    try
      val hdf = HdfFile.fromBytes(bytes)
      try
        if attribute(hdf, "Format").forall(_ != "X5") then Left(malformed("the root Format attribute is not X5"))
        else
          hdf.getByPath("/TransformGroup") match
            case group: Group =>
              for
                nodes <- group.getChildren.asScala.toVector.sortBy(_._1.toIntOption.getOrElse(Int.MaxValue)).foldLeft[Either[TransformIoError, Vector[X5Node]]](Right(Vector.empty)): (acc, entry) =>
                  acc.flatMap(done => node(entry._1, entry._2).map(done :+ _))
                chains <- chains(hdf)
              yield X5File(nodes, chains)
            case _ => Left(malformed("missing /TransformGroup"))
      finally hdf.close()
    catch case NonFatal(error) => Left(malformed(Option(error.getMessage).getOrElse(error.getClass.getSimpleName)))

  private def node(name: String, value: Node): Either[TransformIoError, X5Node] =
    (name.toIntOption, value) match
      case (Some(index), group: Group) =>
        for
          kind <- attribute(group, "Type").toRight(malformed(s"node $index has no Type"))
          transform <- dataset(group, "Transform").toRight(malformed(s"node $index has no Transform"))
          values <- numbers(transform)
          domain <- group.getChild("Domain") match
            case d: Group =>
              for
                size <- dataset(d, "Size").toRight(malformed("Domain has no Size")).flatMap(numbers)
                mapping <- dataset(d, "Mapping").toRight(malformed("Domain has no Mapping")).flatMap(numbers)
                grid = dataset(d, "Grid").forall(flag)
              yield Some(X5Domain(grid, IArray.genericWrapArray(size).toVector.map(_.toInt), IArray.genericWrapArray(mapping).toVector))
            case _ => Right(None)
        yield X5Node(index, kind, attribute(group, "SubType"), attribute(group, "Representation"), transform.getDimensions.toVector, values, domain)
      case _ => Left(malformed(s"TransformGroup child '$name' is not a numbered group"))

  private def chains(hdf: HdfFile): Either[TransformIoError, Vector[Vector[Int]]] =
    hdf.getChild("TransformChain") match
      case group: Group =>
        group.getChildren.asScala.toVector.sortBy(_._1.toIntOption.getOrElse(Int.MaxValue)).foldLeft[Either[TransformIoError, Vector[Vector[Int]]]](Right(Vector.empty)): (acc, entry) =>
          acc.flatMap: done =>
            entry._2 match
              case d: Dataset =>
                val text = d.getData match
                  case s: String        => s
                  case a: Array[String] => a.headOption.getOrElse("")
                  case other            => other.toString
                val indices = text.split("/").toVector.map(_.trim.toIntOption)
                if indices.exists(_.isEmpty) then Left(malformed(s"TransformChain entry '$text' is not a list of node indices"))
                else Right(done :+ indices.flatten)
              case _ => Left(malformed("TransformChain entries must be datasets"))
      case _ => Right(Vector.empty)

  private def attribute(node: Node, name: String): Option[String] =
    Option(node.getAttribute(name)).map((a: Attribute) =>
      a.getData match
        case s: String        => s
        case a: Array[String] => a.headOption.getOrElse("")
        case other            => other.toString
    )

  /** Scalar or one-element flag dataset (h5py writes Domain/Grid as a scalar). */
  private def flag(dataset: Dataset): Boolean =
    dataset.getData match
      case n: java.lang.Number   => n.doubleValue != 0.0
      case b: java.lang.Boolean  => b.booleanValue
      case a: Array[?]           => a.headOption.exists(v => v.toString != "0" && v.toString != "false")
      case other                 => other.toString != "0"

  private def dataset(group: Group, name: String): Option[Dataset] =
    group.getChild(name) match
      case d: Dataset => Some(d)
      case _          => None

  private def numbers(dataset: Dataset): Either[TransformIoError, IArray[Double]] =
    dataset.getDataFlat match
      case v: Array[Double] => Right(IArray.unsafeFromArray(v))
      case v: Array[Float]  => Right(IArray.unsafeFromArray(v.map(_.toDouble)))
      case v: Array[Long]   => Right(IArray.unsafeFromArray(v.map(_.toDouble)))
      case v: Array[Int]    => Right(IArray.unsafeFromArray(v.map(_.toDouble)))
      case v: Array[Byte]   => Right(IArray.unsafeFromArray(v.map(_.toDouble)))
      case v: Array[Boolean] => Right(IArray.unsafeFromArray(v.map(b => if b then 1.0 else 0.0)))
      case other            => Left(malformed(s"${dataset.getPath} is not numeric (${other.getClass.getSimpleName})"))

  private def malformed(reason: String): TransformIoError =
    TransformIoError.Malformed("X5", reason)
