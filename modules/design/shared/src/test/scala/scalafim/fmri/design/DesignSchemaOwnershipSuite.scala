package scalafim.fmri.design

import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.linalg.Mat

class DesignSchemaOwnershipSuite extends munit.FunSuite:
  private val rows = RowLayout(
    Vector.fill(4)(RunIndex.unsafeOneBased(1)),
    Vector.tabulate(4)(index => Seconds(index.toDouble)),
    Vector.tabulate(4)(index => ScanIndex.unsafeOneBased(index + 1))
  )
  private val columns = Vector(
    StructuralColumn.fromOrigin(1, StructuralColumnOrigin.Sampled(ModulatorId.unsafe("x"), ColumnRole.Covariate, RunScope.Global), "x").toOption.get,
    StructuralColumn.fromOrigin(2, StructuralColumnOrigin.Intercept(RunScope.Global), "intercept").toOption.get
  )

  test("compiled schema owns its input and exported matrix storage") {
    val input = Mat.fromRows(Vector.tabulate(4)(index => Vector(index.toDouble, 1.0)))
    val schema = DesignSchema.validated(input, rows, columns).toOption.get
    val fingerprint = schema.fingerprint
    input.data(0) = 42.0
    schema.matrix.data(1) = 99.0

    assertEqualsDouble(schema.matrix(0, 0), 0.0, 0.0)
    assertEqualsDouble(schema.matrix(0, 1), 1.0, 0.0)
    assertEquals(schema.fingerprint, fingerprint)
    assert(schema.validate.isRight)
  }

  test("relabeling and combining schemas cannot reopen matrix ownership") {
    val schema = DesignSchema.validated(Mat.fromRows(Vector.tabulate(4)(index => Vector(index.toDouble, 1.0))), rows, columns).toOption.get
    val renamed = schema.withRenderedLabels(Vector("display x", "display intercept")).toOption.get
    renamed.matrix.data(0) = 42.0
    assertEqualsDouble(renamed.matrix(0, 0), 0.0, 0.0)
    assertEquals(renamed.fingerprint, schema.fingerprint)
    assert(renamed.validate.isRight)
    val extra = StructuralColumn.fromOrigin(1, StructuralColumnOrigin.Sampled(ModulatorId.unsafe("z"), ColumnRole.Covariate, RunScope.Global), "z").toOption.get
    val right = DesignSchema.validated(Mat.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(0.0), Vector(1.0))), rows, Vector(extra)).toOption.get
    val combined = DesignSchema.combine(schema, right).toOption.get
    combined.matrix.data(0) = 42.0
    assertEqualsDouble(combined.matrix(0, 0), 0.0, 0.0)
    assert(combined.validate.isRight)
  }

  test("runwise slices retain the matrix bound to their coefficient axis") {
    val schema = DesignSchema.validated(Mat.fromRows(Vector.tabulate(4)(index => Vector(index.toDouble, 1.0))), rows, columns).toOption.get
    val slice = schema.runwiseSlice(RunIndex.unsafeOneBased(1)).toOption.get
    slice.matrix.data(0) = 42.0
    assertEqualsDouble(slice.matrix(0, 0), 0.0, 0.0)
    assertEquals(slice.axis.designFingerprint, schema.fingerprint)
  }
