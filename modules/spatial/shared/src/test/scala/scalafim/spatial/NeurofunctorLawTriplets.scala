package scalafim.spatial

import scalafim.image.{SampleSpaces, SpatialPoint}
import scalafim.image.world.SubjectId
import scalafim.spatial.fixtures.NeurofunctorLawFixtures

/** Loads the embedded neurofunctor law fixtures (see `tools/r-parity/generate_neurofunctor_law_fixtures.R`). */
object NeurofunctorLawTriplets:
  /** One encoded value from a neurofunctor law triplet cell (`name=kind:payload`). */
  enum LawValue:
    case Dense(rows: Int, cols: Int, values: Vector[Double])
    case Sparse(rows: Int, cols: Int, rowIndices: Vector[Int], colIndices: Vector[Int], values: Vector[Double])
    case Scalar(value: Double)
    case IntValue(value: Int)
    case Bool(value: Boolean)
    case Ids(ids: Vector[String])
    case Routes(routes: Vector[Vector[String]])
    case Points(points: Vector[SpatialPoint])
    case Rows(rows: Vector[Int])
    case Text(value: String)
    case Error(kind: String)

    /** Row-major dense values of a matrix-valued cell. */
    def denseRows: Vector[Vector[Double]] =
      this match
        case Dense(rows, cols, values) =>
          Vector.tabulate(rows)(row => values.slice(row * cols, (row + 1) * cols))
        case Sparse(rows, cols, rowIndices, colIndices, values) =>
          val out = Array.fill(rows, cols)(0.0)
          rowIndices.indices.foreach(i => out(rowIndices(i))(colIndices(i)) += values(i))
          out.toVector.map(_.toVector)
        case other =>
          throw new IllegalArgumentException(s"$other is not a matrix")

  object LawValue:
    def parse(cell: String): LawValue =
      val colon = cell.indexOf(':')
      require(colon > 0, s"malformed triplet value '$cell'")
      val kind = cell.substring(0, colon)
      val payload = cell.substring(colon + 1)
      kind match
        case "dense" =>
          val (rows, cols, body) = shaped(payload)
          val values = if body.isEmpty then Vector.empty else body.split(",").toVector.map(_.toDouble)
          require(values.length == rows * cols, s"dense value has ${values.length} entries, expected ${rows * cols}")
          Dense(rows, cols, values)
        case "sparse" =>
          val (rows, cols, body) = shaped(payload)
          val entries = if body.isEmpty then Vector.empty else body.split(";").toVector.map(_.split(",").toVector)
          require(entries.forall(_.length == 3), s"malformed sparse entries in '$cell'")
          Sparse(rows, cols, entries.map(_(0).toInt), entries.map(_(1).toInt), entries.map(_(2).toDouble))
        case "scalar" => Scalar(payload.toDouble)
        case "int" => IntValue(payload.toInt)
        case "bool" =>
          require(payload == "TRUE" || payload == "FALSE", s"malformed boolean '$payload'")
          Bool(payload == "TRUE")
        case "ids" => Ids(if payload.isEmpty then Vector.empty else payload.split("\\|").toVector)
        case "routes" => Routes(payload.split("/").toVector.map(_.split("\\|").toVector))
        case "points" =>
          Points(payload.split(";").toVector.map { point =>
            val xyz = point.split(",").toVector.map(_.toDouble)
            require(xyz.length == 3, s"malformed point '$point'")
            SpatialPoint(xyz(0), xyz(1), xyz(2))
          })
        case "rows" => Rows(if payload.isEmpty then Vector.empty else payload.split(",").toVector.map(_.toInt))
        case "text" => Text(payload)
        case "error" => Error(payload)
        case other => throw new IllegalArgumentException(s"unknown triplet value kind '$other'")

    private def shaped(payload: String): (Int, Int, String) =
      val colon = payload.indexOf(':')
      require(colon > 0, s"malformed matrix payload '$payload'")
      val shape = payload.substring(0, colon).split("x")
      (shape(0).toInt, shape(1).toInt, payload.substring(colon + 1))

  enum TripletStatus:
    case Exact
    case Deviation(key: String)

  /** An (input, operation, expected output) triplet exported from neurofunctor. */
  final case class LawTriplet(
    id: String,
    law: String,
    operation: String,
    graph: String,
    args: Map[String, String],
    input: Map[String, LawValue],
    expected: Map[String, LawValue],
    tolerance: Double,
    status: TripletStatus,
    reason: String
  ):
    def arg(name: String): String =
      args.getOrElse(name, throw new NoSuchElementException(s"$id has no argument $name"))

    def in(name: String): LawValue =
      input.getOrElse(name, throw new NoSuchElementException(s"$id has no input $name"))

    def out(name: String): LawValue =
      expected.getOrElse(name, throw new NoSuchElementException(s"$id has no expected $name"))

  /** A neurofunctor fixture graph rebuilt as a ScalaFIM [[SpatialGraph]]; domain and morphism ids are the fixture
    * names.
    */
  final case class LawGraph(name: String, domains: Map[String, Domain], graph: SpatialGraph):
    def domain(name: String): Domain =
      domains.getOrElse(name, throw new NoSuchElementException(s"graph ${this.name} has no domain $name"))

  private def rows(lines: Vector[String], header: String): Vector[Vector[String]] =
    require(lines.headOption.contains(header), s"unexpected fixture header ${lines.headOption}")
    lines.drop(1).map(_.split("\t", -1).toVector)

  private def fields(cell: String): Map[String, String] =
    if cell.isEmpty then Map.empty
    else
      cell.split(" ").toVector.map { field =>
        val eq = field.indexOf('=')
        require(eq > 0, s"malformed field '$field'")
        field.substring(0, eq) -> field.substring(eq + 1)
      }.toMap

  lazy val triplets: Vector[LawTriplet] =
    rows(NeurofunctorLawFixtures.triplets, "id\tlaw\toperation\tgraph\targs\tinput\texpected\ttolerance\tstatus\treason").map {
      cells =>
        require(cells.length == 10, s"triplet row has ${cells.length} cells")
        val status =
          if cells(8) == "exact" then TripletStatus.Exact
          else if cells(8).startsWith("deviation:") then TripletStatus.Deviation(cells(8).stripPrefix("deviation:"))
          else throw new IllegalArgumentException(s"unknown triplet status ${cells(8)}")
        LawTriplet(
          id = cells(0),
          law = cells(1),
          operation = cells(2),
          graph = cells(3),
          args = fields(cells(4)),
          input = fields(cells(5)).view.mapValues(LawValue.parse).toMap,
          expected = fields(cells(6)).view.mapValues(LawValue.parse).toMap,
          tolerance = cells(7).toDouble,
          status = status,
          reason = cells(9)
        )
    }

  /** `"key": value` pairs of the flat manifest object, values without quotes. */
  lazy val manifest: Map[String, String] =
    NeurofunctorLawFixtures.manifest.flatMap { line =>
      val trimmed = line.trim.stripSuffix(",")
      val colon = trimmed.indexOf("\":")
      if trimmed.startsWith("\"") && colon > 0 then
        Some(trimmed.substring(1, colon) -> trimmed.substring(colon + 2).trim.stripPrefix("\"").stripSuffix("\""))
      else None
    }.toMap

  private def affine(cell: String) =
    ProviderAffines.fromRowMajor(cell.split(",").toVector.map(_.toDouble))

  private def value[A](result: Either[SpatialError, A]): A =
    result.fold(error => throw new IllegalStateException(error.message), identity)

  lazy val graphs: Map[String, LawGraph] =
    val subject = value(SubjectId("toy").asSpatial)
    val domainRows = rows(NeurofunctorLawFixtures.domains, "graph\tdomain\tdims\taffine")
    val edgeRows = rows(NeurofunctorLawFixtures.edges, "graph\tedge\tsource\ttarget\tkind\tcost\tinverse\tquality\tpullback")
    domainRows.map(_(0)).distinct.map { graphName =>
      val domains =
        domainRows.filter(_(0) == graphName).map { cells =>
          val dims = cells(2).split("x").toVector.map(_.toInt)
          val geometry = value(SamplingGeometry.volume(SampleSpaces(dims, affine = Some(affine(cells(3))))))
          cells(1) -> value(Domain.build(value(DomainId(cells(1))), SpaceRef.Volume(subject, None, value(Modality(s"$graphName-${cells(1)}"))), geometry))
        }.toMap
      val morphisms =
        edgeRows.filter(_(0) == graphName).map { cells =>
          require(cells(4) == "affine", s"unsupported fixture edge kind ${cells(4)}")
          val inverse =
            cells(6) match
              case "exact" =>
                require(cells(7) == "1", s"exact inverse ${cells(1)} must have quality 1, not ${cells(7)}")
                Inverse.Exact("analytic")
              case other => throw new IllegalArgumentException(s"unsupported fixture inverse type $other")
          val source = domains(cells(2))
          val target = domains(cells(3))
          value(
            Morphism.build(
              value(MorphismId(cells(1))),
              source.id,
              target.id,
              MorphismKind.Affine3D,
              RouteTag.Anatomical,
              cells(5).toDouble,
              inverse,
              value(CoordinateMap.affine(source, target, affine(cells(8))))
            )
          )
        }
      graphName -> LawGraph(graphName, domains, value(SpatialGraph.build(domains.values.toVector.sortBy(_.id.value), morphisms)))
    }.toMap
