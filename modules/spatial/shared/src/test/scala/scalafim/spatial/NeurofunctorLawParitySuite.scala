package scalafim.spatial

import scalafim.image.SpatialPoint
import scalafim.image.world.TemplateName
import scalafim.spatial.NeurofunctorLawTriplets.*

/** Replays every neurofunctor law triplet (STP P7.06) through ScalaFIM's spatial API.
  *
  * The triplets come from `tools/r-parity/generate_neurofunctor_law_fixtures.R`, run against neurofunctor 0.1.0; the
  * manifest records the R and package versions and the neurofunctor source commit. They cover the identity,
  * composition, adjoint, ROI-restriction, round-trip and commutativity laws of neurofunctor's `test-functor-laws.R`,
  * the `projection_metrics` QC of `test-qc.R`, hybrid assembly (`compile_to_hybrid`, `compile_from_hybrid`, and a
  * block-diagonal oracle built from neurofunctor's per-part projectors) and `backproject`.
  *
  * A triplet marked `deviation:<key>` records a place where ScalaFIM deliberately differs from neurofunctor; the key
  * must be one of [[declaredDeviations]], and its replay compares everything the deviation leaves comparable. Where
  * the fixture can observe the difference, the replay asserts it, so a deviation that silently disappears fails;
  * `projection-metrics-rows` and `backproject-inverse-setting` are not observable in values (the generator samples
  * every row; forward-first routing picks the same route), and their replays assert full agreement instead.
  */
class NeurofunctorLawParitySuite extends munit.FunSuite:

  /** Every deliberate difference from neurofunctor that a triplet may declare. */
  private val declaredDeviations: Map[String, String] =
    Map(
      "all-paths-order" -> "allPaths ranks routes by cost (then length, then ids) before capping at maxPaths",
      "projection-metrics-rows" -> "projection metrics summarise every row rather than a random sample of 1000",
      "backproject-inverse-setting" -> "backprojection compiles with the view's own inverse setting",
      "trilinear-boundary" ->
        "trilinear sampling renormalises the in-grid corners of a point within one voxel of the grid and reports fractional row coverage",
      "roi-shape" -> "ROI operators return only the ROI rows, in ROI order, instead of a full-height operator with empty rows"
    )

  private val triplets = NeurofunctorLawTriplets.triplets

  private def value[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def apiValue[A](result: Either[FieldApiError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def linValue[A](result: Either[LinearMapError, A]): A =
    result.fold(error => fail(error.getMessage), identity)

  private def matrix(cell: LawValue): DoubleMatrix =
    DoubleMatrix.fromRows(cell.denseRows)

  private def rowsOf(m: DoubleMatrix): Vector[Vector[Double]] =
    Vector.tabulate(m.rows)(row => Vector.tabulate(m.cols)(col => m(row, col)))

  private def eye(n: Int): DoubleMatrix =
    DoubleMatrix.fromRows(Vector.tabulate(n)(row => Vector.tabulate(n)(col => if row == col then 1.0 else 0.0)))

  private def denseOf(operator: SpatialOperator): Vector[Vector[Double]] =
    rowsOf(linValue(operator.forward(eye(operator.cols))))

  private def assertMatrix(label: String, actual: Vector[Vector[Double]], expected: Vector[Vector[Double]], tol: Double): Unit =
    assertEquals((actual.length, actual.headOption.fold(0)(_.length)), (expected.length, expected.headOption.fold(0)(_.length)), clue = label)
    actual.flatten.zip(expected.flatten).zipWithIndex.foreach { case ((a, e), i) =>
      assertEqualsDouble(a, e, tol, clue = s"$label entry $i")
    }

  private def assertMatrix(label: String, actual: DoubleMatrix, expected: LawValue, tol: Double): Unit =
    assertMatrix(label, rowsOf(actual), expected.denseRows, tol)

  private def scalar(cell: LawValue): Double =
    cell match
      case LawValue.Scalar(v) => v
      case other => fail(s"expected a scalar, got $other")

  private def int(cell: LawValue): Int =
    cell match
      case LawValue.IntValue(v) => v
      case other => fail(s"expected an integer, got $other")

  private def bool(cell: LawValue): Boolean =
    cell match
      case LawValue.Bool(v) => v
      case other => fail(s"expected a boolean, got $other")

  private def ids(cell: LawValue): Vector[String] =
    cell match
      case LawValue.Ids(v) => v
      case other => fail(s"expected morphism ids, got $other")

  private def points(cell: LawValue): Vector[SpatialPoint] =
    cell match
      case LawValue.Points(v) => v
      case other => fail(s"expected points, got $other")

  private def rowList(text: String): Vector[Int] =
    LawValue.parse(text) match
      case LawValue.Rows(v) => v
      case other => fail(s"expected rows, got $other")

  private def compile(g: LawGraph, source: String, target: String, allowInverses: Boolean = false, roi: Option[Vector[Int]] = None): SpatialOperator =
    value(OperatorCompiler.compile(g.graph, CompileRequest(g.domain(source).id, g.domain(target).id, roi = roi, allowInverses = allowInverses)))

  private def assertPoints(label: String, actual: Vector[SpatialPoint], expected: Vector[SpatialPoint], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = label)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assertEqualsDouble(a.x, e.x, tol, clue = s"$label point $i x")
      assertEqualsDouble(a.y, e.y, tol, clue = s"$label point $i y")
      assertEqualsDouble(a.z, e.z, tol, clue = s"$label point $i z")
    }

  /** Checks the stored R triplets with [[SpatialQc.tripletFixtureLaw]]: same sparsity pattern, values within `tol`. */
  private def assertTriplets(label: String, operator: SpatialOperator, expected: LawValue, tol: Double): Unit =
    expected match
      case LawValue.Sparse(rows, cols, r, c, x) =>
        val fixture = value(TripletFixture.zeroBased(rows, cols, r, c, x))
        val check = value(SpatialQc.tripletFixtureLaw(operator, fixture, value(QcTolerance(tol))))
        assert(check.passed, clue = s"$label: $check")
      case other => fail(s"$label: expected sparse triplets, got $other")

  private def hybrid(g: LawGraph, name: String, parts: Vector[(String, String)]): Domain =
    val geometry = value(SamplingGeometry.hybrid(parts.map((part, domain) => value(PartName(part)) -> g.domain(domain))))
    value(Domain.build(value(DomainId(name)), SpaceRef.Template(value(TemplateName(name).asSpatial), None, TemplateKind.Hybrid), geometry))

  private def partList(text: String): Vector[(String, String)] =
    text.split(",").toVector.map { part =>
      val colon = part.indexOf(':')
      part.substring(0, colon) -> part.substring(colon + 1)
    }

  /** neurofunctor `projection_metrics` summaries, computed from dense row sums and stored weights. */
  private final case class RMetrics(
    nnz: Int,
    coverage: Double,
    mean: Double,
    min: Double,
    max: Double,
    fracGtHalf: Double,
    fracNearOne: Double,
    wMin: Double,
    wMax: Double
  )

  private def rMetrics(dense: Vector[Vector[Double]]): RMetrics =
    val sums = dense.map(_.sum)
    val weights = dense.flatten.filter(_ != 0.0)
    val n = sums.length.toDouble
    RMetrics(
      nnz = weights.length,
      coverage = sums.count(_ > 0.5) / n,
      mean = sums.sum / n,
      min = sums.min,
      max = sums.max,
      fracGtHalf = sums.count(_ > 0.5) / n,
      fracNearOne = sums.count(s => math.abs(s - 1.0) < 0.1) / n,
      wMin = weights.min,
      wMax = weights.max
    )

  private def assertRMetrics(label: String, actual: RMetrics, t: LawTriplet): Unit =
    val tol = t.tolerance
    assertEquals(actual.nnz, int(t.out("nnz")), clue = label)
    assertEqualsDouble(actual.coverage, scalar(t.out("coverage")), tol, clue = s"$label coverage")
    assertEqualsDouble(actual.mean, scalar(t.out("mean")), tol, clue = s"$label mean")
    assertEqualsDouble(actual.min, scalar(t.out("min")), tol, clue = s"$label min")
    assertEqualsDouble(actual.max, scalar(t.out("max")), tol, clue = s"$label max")
    assertEqualsDouble(actual.fracGtHalf, scalar(t.out("frac_gt_half")), tol, clue = s"$label frac_gt_half")
    assertEqualsDouble(actual.fracNearOne, scalar(t.out("frac_near_one")), tol, clue = s"$label frac_near_one")
    assertEqualsDouble(actual.wMin, scalar(t.out("w_min")), tol, clue = s"$label w_min")
    assertEqualsDouble(actual.wMax, scalar(t.out("w_max")), tol, clue = s"$label w_max")

  // ---------------------------------------------------------------------------
  // Replays, one per operation
  // ---------------------------------------------------------------------------

  private def replay(t: LawTriplet): Unit =
    val g = NeurofunctorLawTriplets.graphs(t.graph)
    val tol = t.tolerance
    t.operation match
      case "compile" =>
        val operator = compile(g, t.arg("source"), t.arg("target"))
        t.expected.get("path").foreach(path => assertEquals(operator.path.ids.map(_.value), ids(path)))
        assertTriplets(t.id, operator, t.out("matrix"), tol)
        assertMatrix(s"${t.id} matrix", denseOf(operator), t.out("matrix").denseRows, tol)
        assertMatrix(s"${t.id} forward", linValue(operator.forward(matrix(t.in("x")))), t.out("forward"), tol)
        assertEqualsDouble(operator.qc.coverage.fraction, scalar(t.out("coverage")), tol)
        if t.law == "identity" then
          val check = value(SpatialQc.identityLaw(operator, matrix(t.in("x")), value(QcTolerance(tol))))
          assert(check.passed, clue = check)

      case "compose" =>
        val direct = compile(g, t.arg("source"), t.arg("target"))
        val first = compile(g, t.arg("source"), t.arg("mid"))
        val second = compile(g, t.arg("mid"), t.arg("target"))
        val x = matrix(t.in("x"))
        val directOut = linValue(direct.forward(x))
        val composedOut = linValue(second.forward(linValue(first.forward(x))))
        assertMatrix(s"${t.id} direct", directOut, t.out("direct"), tol)
        assertMatrix(s"${t.id} composed", composedOut, t.out("composed"), tol)
        val composedMatrix = rowsOf(linValue(second.forward(linValue(first.forward(eye(first.cols))))))
        assertMatrix(s"${t.id} composed matrix", composedMatrix, t.out("composed_matrix").denseRows, tol)
        val gap = rowsOf(directOut).flatten.zip(rowsOf(composedOut).flatten).map((a, b) => math.abs(a - b)).max
        assertEquals(gap < 1e-12, bool(t.out("law_holds")), clue = s"${t.id} composition gap $gap")

      case "adjoint" =>
        val operator = compile(g, t.arg("source"), t.arg("target"))
        val x = matrix(t.in("x"))
        val y = matrix(t.in("y"))
        assertMatrix(s"${t.id} forward", linValue(operator.forward(x)), t.out("forward"), tol)
        assertMatrix(s"${t.id} adjoint", linValue(operator.map.transposeApplyTo(y)), t.out("adjoint"), tol)
        val check = value(SpatialQc.adjointLaw(operator, x, y, value(QcTolerance(tol))))
        assert(check.passed, clue = check)
        assertEqualsDouble(check.observed, scalar(t.out("lhs")), tol)
        assertEqualsDouble(check.expected, scalar(t.out("rhs")), tol)

      case "compile-roi" =>
        val roi = rowList(t.arg("roi"))
        val restricted = compile(g, t.arg("source"), t.arg("target"), roi = Some(roi))
        val full = compile(g, t.arg("source"), t.arg("target"))
        val x = matrix(t.in("x"))
        assertEquals(restricted.rows, roi.length)
        // the declared difference: neurofunctor's ROI operator keeps the full target height
        assertEquals(int(t.out("n_rows")), full.rows)
        assertNotEquals(restricted.rows, int(t.out("n_rows")))
        assertEquals(restricted.qc.coverage.targetRows, roi)
        assertMatrix(s"${t.id} forward", linValue(restricted.forward(x)), t.out("forward"), tol)
        assertMatrix(s"${t.id} full rows", linValue(full.forward(x)).selectRows(roi), t.out("full_forward"), tol)
        assertEqualsDouble(restricted.qc.coverage.fraction, scalar(t.out("coverage")), tol)
        val check = value(SpatialQc.roiRestrictionLaw(full, restricted, roi, x, value(QcTolerance(tol))))
        assert(check.passed, clue = check)

      case "round-trip-operator" =>
        val roi = rowList(t.arg("roi"))
        val there = compile(g, t.arg("source"), t.arg("via"))
        val back = compile(g, t.arg("via"), t.arg("source"), allowInverses = true, roi = Some(roi))
        assertEquals(there.path.ids.map(_.value), ids(t.out("there")))
        assertEquals(back.path.ids.map(_.value), ids(t.out("back")))
        assert(back.path.usedInverses)
        assertMatrix(s"${t.id} back matrix", denseOf(back), t.out("back_matrix").denseRows, tol)
        val roundTrip = linValue(back.forward(linValue(there.forward(matrix(t.in("x"))))))
        assertMatrix(s"${t.id} round trip", roundTrip, t.out("round_trip"), tol)

      case "field-reexpress" =>
        given SpatialGraph = g.graph
        given FieldRuntime = LazyFieldRuntime(g.graph)
        val root = Field.fromMatrix(g.domain(t.arg("root")).id, matrix(t.in("x")), "law")
        val view = apiValue(root.to(g.domain(t.arg("via"))))
        val back = apiValue(view.to(g.domain(t.arg("root")), allowInverses = true))
        assertMatrix(s"${t.id} view", apiValue(view.value), t.out("view"), tol)
        assertMatrix(s"${t.id} round trip", apiValue(back.value), t.out("round_trip"), tol)

      case "round-trip-coordinates" =>
        val a = g.domain(t.arg("a")).id
        val b = g.domain(t.arg("b")).id
        val probes = points(t.in("probes"))
        val trip = value(SpatialQc.roundTrip(g.graph, a, b, probes))
        assertEquals(trip.first.map(_.value), ids(t.out("path")))
        assertEquals(trip.usedInverses, bool(t.out("used_inverses")))
        assertEqualsDouble(trip.maxDistance, scalar(t.out("max")), tol)
        assertEqualsDouble(trip.rmsDistance, scalar(t.out("rms")), tol)
        val there = value(g.graph.path(a, b, allowInverses = true))
        val back = value(g.graph.path(b, a, allowInverses = true))
        val loop = value(MorphismPath.build(there.morphisms ++ back.morphisms, there.usedInverses || back.usedInverses))
        assertPoints(s"${t.id} pulled", probes.map(p => value(loop.pullback(p))), points(t.out("pulled")), tol)

      case "commutativity" =>
        val source = g.domain(t.arg("source")).id
        val target = g.domain(t.arg("target")).id
        val probes = points(t.in("probes"))
        val count = int(t.out("routes"))
        val routes = (1 to count).toVector.map { i =>
          (ids(t.out(s"r${i}_path")), scalar(t.out(s"r${i}_cost")), points(t.out(s"r${i}_pulled")),
            scalar(t.out(s"r${i}_max_vs_cheapest")), scalar(t.out(s"r${i}_rms_vs_cheapest")))
        }
        val cheapest = routes(int(t.out("cheapest")))
        val scala = value(g.graph.allPaths(source, target, maxPaths = t.arg("max_paths").toInt))
        assertEquals(scala.map(_.ids.map(_.value)).toSet, routes.map(_._1).toSet)
        routes.foreach { (path, cost, pulled, _, _) =>
          val route = scala.find(_.ids.map(_.value) == path).getOrElse(fail(s"missing route $path"))
          assertEqualsDouble(route.cost, cost, tol)
          assertPoints(s"${t.id} ${path.mkString("|")}", probes.map(p => value(route.pullback(p))), pulled, tol)
        }
        val differences = value(SpatialQc.commutativity(g.graph, source, target, probes, maxPaths = t.arg("max_paths").toInt))
        assertEquals(differences.length, count - 1)
        assert(differences.forall(_.first.map(_.value) == cheapest._1), clue = differences)
        routes.filterNot(_._1 == cheapest._1).foreach { (path, _, _, maxDistance, rms) =>
          val difference = differences.find(_.second.map(_.value) == path).getOrElse(fail(s"no difference for $path"))
          assertEqualsDouble(difference.maxDistance, maxDistance, tol)
          assertEqualsDouble(difference.rmsDistance, rms, tol)
        }
        val firstListed = value(MorphismPath.build(scala.find(_.ids.map(_.value) == routes.head._1).get.morphisms))
        val pair = value(SpatialQc.commutes(scala.head, firstListed, probes))
        assertEqualsDouble(pair.maxDistance, routes.head._4, tol)

      case "all-paths" =>
        val source = g.domain(t.arg("source")).id
        val target = g.domain(t.arg("target")).id
        val rRoutes =
          t.out("uncapped") match
            case LawValue.Routes(routes) => routes
            case other => fail(s"expected routes, got $other")
        assert(rRoutes.length > 1)
        val scalaAll = value(g.graph.allPaths(source, target))
        assertEquals(scalaAll.map(_.ids.map(_.value)).toSet, rRoutes.toSet)
        val capped = value(g.graph.allPaths(source, target, maxPaths = t.arg("max_paths").toInt))
        assertEquals(capped.length, 1)
        val cheapest = scalaAll.minBy(_.cost)
        assertEquals(capped.head.ids, cheapest.ids)
        // the declared deviation: neurofunctor keeps its first-found route, which is not the cheapest here
        assertNotEquals(capped.head.ids.map(_.value), ids(t.out("capped")))
        assertEquals(rRoutes.head, ids(t.out("capped")))

      case "projection-metrics" if t.status == TripletStatus.Deviation("projection-metrics-rows") =>
        val operator = compile(g, t.arg("source"), t.arg("target"))
        val metrics = value(SpatialQc.projectionMetrics(operator))
        assertEquals(int(t.out("sample_n")), int(t.out("n_target")), clue = "the generator must sample every row")
        assertEquals(metrics.targetRows, int(t.out("n_target")))
        assertEquals(metrics.sourceColumns, int(t.out("n_source")))
        assertEqualsDouble(metrics.nnzPerRow.get, scalar(t.out("nnz_per_row")), tol)
        assertEqualsDouble(metrics.coverage, scalar(t.out("coverage")), tol)
        assertEqualsDouble(metrics.rowSumSummary.mean, scalar(t.out("mean")), tol)
        assertEqualsDouble(metrics.rowSumSummary.min, scalar(t.out("min")), tol)
        assertEqualsDouble(metrics.rowSumSummary.max, scalar(t.out("max")), tol)
        assertEqualsDouble(metrics.rowSumSummary.fractionAboveHalf, scalar(t.out("frac_gt_half")), tol)
        assertEqualsDouble(metrics.rowSumSummary.fractionNearOne, scalar(t.out("frac_near_one")), tol)
        metrics.weights match
          case WeightInspection.Stored(summary) =>
            assertEquals(summary.entries, int(t.out("nnz")))
            assertEquals(summary.anyNegative, bool(t.out("any_negative")))
            assertEquals(summary.anyNonFinite, bool(t.out("any_na")))
            assertEqualsDouble(summary.min.get, scalar(t.out("w_min")), tol)
            assertEqualsDouble(summary.max.get, scalar(t.out("w_max")), tol)
          case other => fail(s"expected stored weights, got $other")
        assertRMetrics(t.id, rMetrics(denseOf(operator)), t)

      case "projection-metrics" if t.status == TripletStatus.Deviation("trilinear-boundary") =>
        val operator = compile(g, t.arg("source"), t.arg("target"))
        val scalaRows = denseOf(operator)
        val rRows = t.out("matrix").denseRows
        val dropped = rRows.indices.filter(row => rRows(row).forall(_ == 0.0)).toSet
        assert(dropped.nonEmpty, clue = "the boundary fixture must exercise dropped rows")
        scalaRows.indices.foreach { row =>
          if dropped(row) then
            // declared difference: a partially covered row, renormalised to sum to one
            // x = 3.5 on a 4-voxel axis: one in-grid corner of weight 0.5, renormalised to 1
            assertEqualsDouble(operator.qc.coverage.rowCoverage(row), 0.5, tol, clue = s"row $row coverage")
            val stored = scalaRows(row).filter(_ != 0.0)
            assertEquals(stored.length, 1, clue = s"row $row")
            assertEqualsDouble(stored.head, 1.0, tol, clue = s"row $row")
          else assertMatrix(s"${t.id} row $row", Vector(scalaRows(row)), Vector(rRows(row)), tol)
        }
        // with the renormalised rows emptied again, every neurofunctor metric is reproduced exactly
        val asR = scalaRows.zipWithIndex.map((row, i) => if dropped(i) then row.map(_ => 0.0) else row)
        val emptied = rMetrics(asR)
        assertRMetrics(t.id, emptied, t)
        assertEqualsDouble(emptied.nnz.toDouble / asR.length, scalar(t.out("nnz_per_row")), tol)
        assertEquals(int(t.out("sample_n")), int(t.out("n_target")), clue = "the generator must sample every row")
        assert(!bool(t.out("any_na")) && !bool(t.out("any_negative")))
        val metrics = value(SpatialQc.projectionMetrics(operator))
        assertEquals(metrics.targetRows, int(t.out("n_target")))
        assertEquals(metrics.sourceColumns, int(t.out("n_source")))
        metrics.weights match
          case WeightInspection.Stored(summary) =>
            assert(!summary.anyNonFinite && !summary.anyNegative)
            assertEquals(summary.entries, int(t.out("nnz")) + dropped.size)
          case other => fail(s"expected stored weights, got $other")
        assertEqualsDouble(metrics.coverage, 1.0, tol)
        assert(metrics.coverage > scalar(t.out("coverage")))

      case "to-hybrid" =>
        val target = hybrid(g, s"${t.id}-target", partList(t.arg("parts")))
        val op = value(HybridOperator.toHybrid(g.graph, g.domain(t.arg("source")).id, target))
        replayHybrid(t, op)

      case "from-hybrid" =>
        val source = hybrid(g, s"${t.id}-source", partList(t.arg("parts")))
        val op = value(HybridOperator.fromHybrid(g.graph, source, g.domain(t.arg("target")).id))
        replayHybrid(t, op)

      case "block-diagonal" =>
        val pairs = partList(t.arg("parts")).map((part, route) => (part, route.split(">")(0), route.split(">")(1)))
        val source = hybrid(g, s"${t.id}-source", pairs.map((part, from, _) => part -> from))
        val target = hybrid(g, s"${t.id}-target", pairs.map((part, _, to) => part -> to))
        val op = value(HybridOperator.blockDiagonal(g.graph, source, target))
        replayHybrid(t, op)

      case "backproject" =>
        given SpatialGraph = g.graph
        given FieldRuntime = LazyFieldRuntime(g.graph)
        val allowInverses = t.arg("method") == "inverse_ok"
        val root = Field.fromMatrix(g.domain(t.arg("root")).id, matrix(t.in("root")), "law")
        val view = apiValue(root.to(g.domain(t.arg("view")), allowInverses = allowInverses))
        val viewData = apiValue(view.value)
        assertMatrix(s"${t.id} view", viewData, t.out("view"), tol)
        val back = apiValue(view.backproject(matrix(t.out("view"))))
        assertMatrix(s"${t.id} backprojected", back.data, t.out("backprojected"), tol)
        assertEquals(back.field.domain, g.domain(t.arg("root")).id)
        assertEquals(back.operator.provenance.allowInverses, allowInverses)
        t.expected.get("path").foreach { path =>
          assertEquals(back.operator.path.ids.map(_.value), ids(path))
          assertEquals(back.operator.provenance.usedInverses, ids(path).exists(_.endsWith(":inverse")))
        }

      case "view" =>
        given SpatialGraph = g.graph
        val allowInverses = t.arg("method") == "inverse_ok"
        val root = Field.fromMatrix(g.domain(t.arg("root")).id, matrix(t.in("root")), "law")
        val view = root.to(g.domain(t.arg("view")), allowInverses = allowInverses)
        t.out("view") match
          case LawValue.Error("nopath") =>
            assertEquals(
              view.left.toOption,
              Some(FieldApiError.Spatial(SpatialError.NoPath(g.domain(t.arg("root")).id, g.domain(t.arg("view")).id)))
            )
          case other => fail(s"${t.id}: unexpected expectation $other")

      case other =>
        fail(s"${t.id}: no replay for operation '$other' with status ${t.status}")

  private def replayHybrid(t: LawTriplet, op: HybridOperator): Unit =
    val tol = t.tolerance
    val dense = rowsOf(linValue(op.forward(eye(op.cols))))
    assertMatrix(s"${t.id} matrix", dense, t.out("matrix").denseRows, tol)
    assertMatrix(s"${t.id} forward", linValue(op.forward(matrix(t.in("x")))), t.out("forward"), tol)
    assertMatrix(s"${t.id} adjoint", linValue(op.adjoint(matrix(t.in("y")))), t.out("adjoint"), tol)
    t.expected.get("coverage").foreach(coverage => assertEqualsDouble(op.meanPartCoverage, scalar(coverage), tol))
    val metrics = value(SpatialQc.projectionMetrics(op))
    assertEquals((metrics.targetRows, metrics.sourceColumns), (op.rows, op.cols))
    metrics.weights match
      case WeightInspection.Stored(summary) =>
        val expectedEntries =
          t.out("matrix") match
            case LawValue.Sparse(_, _, _, _, values) => values.count(_ != 0.0)
            case other => fail(s"expected sparse triplets, got $other")
        assertEquals(summary.entries, expectedEntries)
      case other => fail(s"expected stored part weights, got $other")

  // ---------------------------------------------------------------------------
  // Tests
  // ---------------------------------------------------------------------------

  triplets.foreach { t =>
    val tag = t.status match
      case TripletStatus.Exact => ""
      case TripletStatus.Deviation(key) => s" [declared deviation: $key]"
    test(s"${t.id} ${t.law}: ${t.operation}$tag"):
      replay(t)
  }

  test("the manifest records the R toolchain, the neurofunctor commit and zero-based indices"):
    val manifest = NeurofunctorLawTriplets.manifest
    assertEquals(manifest("neurofunctor_version"), "0.1.0")
    assert(manifest("neurofunctor_source_commit").matches("[0-9a-f]{40}"), clue = manifest)
    assertEquals(manifest("neurofunctor_source_dirty"), "false")
    assert(manifest("r_version").startsWith("R version "), clue = manifest)
    assert(manifest.contains("neurotransform_version") && manifest.contains("matrix_version") && manifest.contains("igraph_version"))
    assertEquals(manifest("index_origin"), "0")
    assertEquals(manifest("triplets").toInt, triplets.length)

  test("triplet ids are unique and every law family is covered"):
    assertEquals(triplets.map(_.id).distinct.length, triplets.length)
    assertEquals(
      triplets.map(_.law).toSet,
      Set("identity", "operator", "composition", "adjoint", "roi-restriction", "round-trip", "commutativity",
        "projection-metrics", "hybrid", "backprojection")
    )

  test("every deviation is declared, explained, and used"):
    val used = triplets.collect { case LawTriplet(id, _, _, _, _, _, _, _, TripletStatus.Deviation(key), reason) =>
      assert(declaredDeviations.contains(key), clue = s"$id declares unknown deviation $key")
      assert(reason.trim.length > 40, clue = s"$id must explain its deviation")
      key
    }.toSet
    assertEquals(used, declaredDeviations.keySet)
    triplets.filter(_.status == TripletStatus.Exact).foreach(t => assert(t.reason.isEmpty, clue = t.id))

  test("tolerances are explicit and tight"):
    triplets.foreach { t =>
      assert(t.tolerance >= 0.0 && t.tolerance <= 1e-10, clue = s"${t.id} tolerance ${t.tolerance}")
    }
