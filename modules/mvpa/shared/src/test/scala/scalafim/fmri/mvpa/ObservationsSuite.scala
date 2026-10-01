package scalafim.fmri.mvpa

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import gale.sparse.Sparse
import multivar.core.{OperatorRepresentation, SpaceRole, ValueId, ValueIdentity}
import scala.compiletime.testing.typeCheckErrors
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class ObservationsSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(
      namespace: String,
      role: SpaceRole,
      keys: Vector[String],
      basis: String = "native",
      units: String = "percent-signal-change",
      scale: String = "unscaled",
      lineage: Vector[String] = Vector("source:fixture-v1")
  ): AxisRef[String] =
    right(AxisRef.fromStableKeys(namespace, role, keys, basis, units, scale, lineage))

  private def valueId(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))

  private def source(value: String): EvidenceSource =
    val id = SourceId.unsafe(value)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$value-root"), id)))

  private final class Counted(matrix: Option[DMat]) extends DoubleLinearOperator:
    val rows: Int = matrix.fold(3)(_.rows)
    val cols: Int = matrix.fold(2)(_.cols)
    var reads = 0

    def applyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      matrix match
        case Some(value) => value.applyTo(input, output)
        case None        => throw new IllegalStateException("poison evidence read")

    override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
      reads += 1
      matrix match
        case Some(value) => value.transposeApplyTo(input, output)
        case None        => throw new IllegalStateException("poison evidence read")

  test("dense and operator realizations share scientific identity without hidden materialization"):
    val samples = axis("trials", SpaceRole.Samples, Vector("trial-91", "trial-7", "trial-32"), units = "trial")
    val neural = axis("brain", SpaceRole.Observed, Vector("voxel-8", "voxel-3"))
    val raw = DMat.dense(3, 2, Vector(1.0, 2.0, 3.0, 5.0, 7.0, 11.0))
    val sparseBuilder = Sparse.coo(3, 2)
    for row <- 0 until raw.rows; column <- 0 until raw.cols do
      sparseBuilder.add(row, column, raw(row, column))
    val sparseValues = sparseBuilder.toCSR()
    val counted = new Counted(Some(raw))
    val revision = valueId("bold-source-revision-v1")
    val evidenceSource = source("fixture-bold")
    val dense = right(Observations.fromDense(samples, neural, raw, revision, evidenceSource))
    val sparse = right(Observations.fromSparse(samples, neural, sparseValues, revision, evidenceSource))
    val operator = right(Observations.fromOperator(samples, neural, counted, revision, evidenceSource))

    assertEquals(dense.identity, operator.identity)
    assertEquals(dense.identity, sparse.identity)
    assertEquals(dense.representation, OperatorRepresentation.Dense)
    assertEquals(sparse.representation, OperatorRepresentation.Sparse)
    assertEquals(operator.representation, OperatorRepresentation.MatrixFree)
    assertEquals(counted.reads, 0)
    val actual = right(operator.patterns(DMat.eye(2)))
    val sparseActual = right(sparse.patterns(DMat.eye(2)))
    for row <- 0 until raw.rows; column <- 0 until raw.cols do
      assertEqualsDouble(actual(row, column), raw(row, column), 1e-12)
      assertEqualsDouble(sparseActual(row, column), raw(row, column), 1e-12)
    assertEquals(counted.reads, 2)

    val revised = right(Observations.fromDense(samples, neural, raw, valueId("bold-source-revision-v2"), evidenceSource))
    val relocated = right(Observations.fromDense(samples, neural, raw, revision, source("fixture-bold-copy")))
    assert(dense.identity != revised.identity)
    assert(dense.identity != relocated.identity)

  test("complete decoded descriptors fail before a poison source is read"):
    val samples = axis("trials", SpaceRole.Samples, Vector("a", "b", "c"), units = "trial")
    val neural = axis("brain", SpaceRole.Observed, Vector("u", "v"))
    val poison = new Counted(None)
    val evidenceSource = source("poison-bold")
    val variants = Vector(
      axis("brain", SpaceRole.Observed, Vector("v", "u")),
      axis("brain", SpaceRole.Observed, Vector("u", "v"), basis = "aligned"),
      axis("brain", SpaceRole.Observed, Vector("u", "v"), units = "raw-signal"),
      axis("brain", SpaceRole.Observed, Vector("u", "v"), scale = "training-zscore"),
      axis("brain", SpaceRole.Observed, Vector("u", "v"), lineage = Vector("source:fixture-v2"))
    )
    variants.foreach: declared =>
      assert(
        Observations
          .decode(
            samples,
            neural,
            samples.toRecord,
            declared.toRecord,
            poison,
            valueId("poison-v1"),
            evidenceSource
          )
          .isLeft
      )
    assertEquals(poison.reads, 0)

  test("multiresponse targets retain both sample and target-feature identity"):
    val samples = axis("trials", SpaceRole.Samples, Vector("a", "b", "c"), units = "trial")
    val neural = axis("brain", SpaceRole.Observed, Vector("u", "v"))
    val features = axis("stimulus", SpaceRole.Observed, Vector("texture", "shape"), units = "contrast")
    val patterns = right(
      Observations.fromDense(
        samples,
        neural,
        DMat.dense(3, 2, Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)),
        valueId("patterns-v1"),
        source("patterns")
      )
    )
    val targets = right(
      MultiResponse.fromDense(
        samples,
        features,
        DMat.dense(3, 2, Vector(0.1, 1.0, 0.2, 0.0, 0.3, -1.0)),
        valueId("targets-v1"),
        source("targets")
      )
    )
    val supervised = right(MultiResponseSupervised(patterns, targets))
    assertEquals(supervised.target.featureAxis, features.descriptor)
    assertEquals(targets.toRecord.rows.stableKeys, Vector("a", "b", "c"))
    assertEquals(targets.toRecord.columns.stableKeys, Vector("texture", "shape"))
    assertEquals(right(targets.targets(DMat.eye(2))).rows, 3)

    val reversed = axis("stimulus", SpaceRole.Observed, Vector("shape", "texture"), units = "contrast")
    assert(
      MultiResponse
        .decode(
          samples,
          features,
          samples.toRecord,
          reversed.toRecord,
          DMat.dense(3, 2, Vector.fill(6)(0.0)),
          valueId("targets-v2"),
          source("targets-v2")
        )
        .isLeft
    )

  test("response provenance must declare the bound source"):
    val expected = SourceId.unsafe("expected-source")
    val other = SourceId.unsafe("other-source")
    val provenance = Provenance.source(ProvenanceId.unsafe("other-root"), other)
    assertEquals(
      EvidenceSource(expected, provenance).left.toOption.map(_.message),
      Some("invalid evidence source: response provenance does not contain source expected-source")
    )

  test("nominal sample and feature boundaries reject invalid supervision at compile time"):
    val errors = typeCheckErrors("""import scalafim.fmri.mvpa.*
import multivar.core.*
def invalid[S <: SemanticSpace,T <: SemanticSpace,N <: SemanticSpace,F <: SemanticSpace](
  observations: Observations[S,N], target: MultiResponse[T,F]
    ) = MultiResponseSupervised(observations, target)
""")
    assertEquals(errors.length, 1)
    assert(errors.head.message.contains("MultiResponse[T, F]"))
    assert(errors.head.message.contains("Required: scalafim.fmri.mvpa.MultiResponse[S,"))
