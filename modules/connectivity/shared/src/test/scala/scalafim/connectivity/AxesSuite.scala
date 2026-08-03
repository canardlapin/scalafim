package scalafim.connectivity

import gale.linalg.{DMat, Matrix}

class AxesSuite extends munit.FunSuite:

  test("opaque ids and sample periods validate construction boundaries") {
    assertEquals(NodeId(" node-1 ").toOption.map(_.value), Some("node-1"))
    assert(NodeId("bad node").isLeft)
    assert(SystemId("visual.cortex").isRight)
    assert(RunId("").isLeft)
    assert(SampleIndex(-1).isLeft)
    assertEquals(SampleIndex(0).toOption.map(_.value), Some(0))
    assert(SamplePeriod.fromSeconds(0.0).isLeft)
    assert(SamplePeriod.fromHz(Double.PositiveInfinity).isLeft)
    assertEquals(SamplePeriod.fromHz(2.0).toOption.map(_.seconds), Some(0.5))
  }

  test("NodeAxis rejects mismatched labels and duplicate ids") {
    val n1 = NodeId.unsafe("n1")
    val n2 = NodeId.unsafe("n2")

    val axis = NodeAxis.fromIdsAndLabels(Vector(n1, n2), Vector("left", "right")).toOption.get

    assertEquals(axis.size, 2)
    assertEquals(axis.labels, Vector("left", "right"))
    assertEquals(axis.indexOf(n2), Some(1))
    assertEquals(axis.ids, Vector(n1, n2))
    assertEquals(axis.nodes.map(_.id), axis.ids)
    assert(NodeAxis.fromIdsAndLabels(Vector(n1, n2), Vector("only-one")).isLeft)
    assert(NodeAxis.from(Vector(NodeSpec(n1, "a"), NodeSpec(n1, "b"))).isLeft)
  }

  test("NodeAxis compatibility separates coordinates metadata and scientific provenance") {
    val n1 = NodeId.unsafe("n1")
    val n2 = NodeId.unsafe("n2")
    val version1 = NodeAxisProvenance.unsafeDeclared("schaefer-400", Some("1.0"))
    val version2 = NodeAxisProvenance.unsafeDeclared("schaefer-400", Some("2.0"))
    val nodes = Vector(NodeSpec(n1, "left"), NodeSpec(n2, "right"))
    val original = NodeAxis.from(nodes, version1).toOption.get
    val relabeled = NodeAxis.from(Vector(NodeSpec(n1, "L"), NodeSpec(n2, "R")), version1).toOption.get
    val reordered = NodeAxis.from(nodes.reverse, version1).toOption.get
    val revised = NodeAxis.from(nodes, version2).toOption.get

    assert(original.sameKeyOrderAs(relabeled))
    assert(original.sameKeySetAs(relabeled))
    assert(!original.sameMetadataAs(relabeled))
    assert(original.sameScientificBasisAs(relabeled))
    assert(!original.sameKeyOrderAs(reordered))
    assert(original.sameKeySetAs(reordered))
    assert(original.sameIdentityAs(revised), "legacy identity remains NodeSpec equality")
    assert(!original.sameScientificBasisAs(revised))
    assertEquals(original.sameIdsAs(relabeled), original.sameKeyOrderAs(relabeled))
  }

  test("NodeAxis provenance validates declarations and treats legacy axes explicitly") {
    assert(NodeAxisProvenance.declared("bad basis").isLeft)
    assert(NodeAxisProvenance.declared("schaefer", Some("bad version")).isLeft)
    val declared = NodeAxisProvenance.declared("schaefer", Some("2018.1")).toOption.get
    val same = NodeAxisProvenance.declared("schaefer", Some("2018.1")).toOption.get
    val other = NodeAxisProvenance.declared("schaefer", Some("2018.2")).toOption.get

    assertEquals(declared.description, "schaefer@2018.1")
    assert(declared.compatibleWith(same))
    assert(!declared.compatibleWith(other))
    assert(NodeAxisProvenance.Unspecified.compatibleWith(NodeAxisProvenance.Unspecified))
    assert(!NodeAxisProvenance.Unspecified.compatibleWith(declared))
  }

  test("NodeAxis.generated validates prefixes without throwing") {
    val axis = NodeAxis.generated(2, prefix = "parcel").toOption.get

    assertEquals(axis.ids.map(_.value), Vector("parcel-1", "parcel-2"))
    assert(NodeAxis.generated(2, prefix = "bad prefix").isLeft)
    assert(NodeAxis.generated(0, prefix = "parcel").isLeft)
  }

  test("TimeAxis, FrameWeights, and NuisanceMatrix enforce sample alignment") {
    val time = TimeAxis.fromSeconds(3, 0.8).toOption.get
    val matrix = GaleTestMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0)))

    assert(TimeAxis.fromSeconds(0, 1.0).isLeft)
    assert(FrameWeights.from(Vector(1.0, 0.0), time).isLeft)
    assert(FrameWeights.from(Vector(1.0, -0.1, 1.0), time).isLeft)
    assert(FrameWeights.from(Vector(0.0, 0.0, 0.0), time).isLeft)
    assertEquals(FrameWeights.uniform(time).length, 3)
    assert(NuisanceMatrix.from(matrix, time, Vector("motion-x")).isLeft)
    assert(NuisanceMatrix.from(matrix, time, Vector("motion-x", "motion-y")).isRight)
  }

  test("ParcelTimeSeries rejects shape mismatch empty matrices and non-finite data") {
    val axis = NodeAxis.generated(2).toOption.get
    val time = TimeAxis.fromSeconds(2, 1.0).toOption.get
    val ok = GaleTestMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val badRows = GaleTestMatrix.fromRows(Vector(Vector(1.0, 2.0)))
    val badFinite = GaleTestMatrix.fromRows(Vector(Vector(1.0, Double.NaN), Vector(3.0, 4.0)))

    assert(ParcelTimeSeries.from(ok, axis, time).isRight)
    assert(ParcelTimeSeries.from(badRows, axis, time).isLeft)
    assert(ParcelTimeSeries.from(Matrix.zeros(0, 2), axis, time).isLeft)
    assert(ParcelTimeSeries.from(badFinite, axis, time).swap.toOption.exists(_.message.contains("not finite")))
  }

  test("ParcelTimeSeries and NuisanceMatrix copy validated matrix inputs") {
    val axis = NodeAxis.generated(2).toOption.get
    val time = TimeAxis.fromSeconds(2, 1.0).toOption.get
    val seriesSource = GaleTestMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val nuisanceSource = GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(0.0)))
    val series = ParcelTimeSeries.from(seriesSource, axis, time).toOption.get
    val nuisance = NuisanceMatrix.from(nuisanceSource, time, Vector("motion-x")).toOption.get

    assert(!(series.values eq seriesSource))
    assert(!(nuisance.values eq nuisanceSource))
    assertEqualsDouble(series.values(0, 0), 1.0, 1e-12)
    assertEqualsDouble(nuisance.values(0, 0), 1.0, 1e-12)
  }

  test("MultiRunTimeSeries requires unique run ids and shared node-axis identity") {
    val axis = NodeAxis.generated(2).toOption.get
    val otherAxis = NodeAxis.generated(2, prefix = "other").toOption.get
    val relabeledAxis = NodeAxis.fromIdsAndLabels(axis.ids, Vector("left", "right")).toOption.get
    val provenanceAxis = NodeAxis.from(axis.nodes, NodeAxisProvenance.unsafeDeclared("atlas", Some("1"))).toOption.get
    val time = TimeAxis.fromSeconds(2, 1.0).toOption.get
    val data = GaleTestMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val run1 = RunTimeSeries(RunId.unsafe("run-1"), ParcelTimeSeries.from(data, axis, time).toOption.get)
    val run2 = RunTimeSeries(RunId.unsafe("run-2"), ParcelTimeSeries.from(data, axis, time).toOption.get)
    val other = RunTimeSeries(RunId.unsafe("run-3"), ParcelTimeSeries.from(data, otherAxis, time).toOption.get)
    val relabeled = RunTimeSeries(RunId.unsafe("run-4"), ParcelTimeSeries.from(data, relabeledAxis, time).toOption.get)
    val reprovenanced = RunTimeSeries(RunId.unsafe("run-5"), ParcelTimeSeries.from(data, provenanceAxis, time).toOption.get)

    assertEquals(MultiRunTimeSeries.from(Vector(run1, run2)).toOption.map(_.size), Some(2))
    assert(MultiRunTimeSeries.from(Vector(run1, run1)).isLeft)
    assert(MultiRunTimeSeries.from(Vector(run1, other)).isLeft)
    assert(MultiRunTimeSeries.from(Vector(run1, relabeled)).isLeft)
    assert(MultiRunTimeSeries.from(Vector(run1, reprovenanced)).isLeft)
  }
