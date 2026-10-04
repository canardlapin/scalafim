package scalafim.fmri.hrf

import scalafim.fmri.hrf.fixtures.HrfRParityFixtures
import scalafim.fmri.hrf.linalg.{Mat, Vec}

/** Characterizes the current public storage behavior before a future storage
  * migration. These assertions intentionally describe observed compatibility,
  * rather than prescribing a new ownership contract.
  */
class HrfStorageCharacterizationSuite extends munit.FunSuite:

  test("unsafe Mat and Vec retain caller arrays and public data remains mutable"):
    val matrixStorage = Array(1.0, 2.0, 3.0, 4.0)
    val vectorStorage = Array(5.0, 6.0)
    val matrix = Mat.unsafe(2, 2, matrixStorage)
    val vector = Vec.unsafe(vectorStorage)

    matrixStorage(0) = 11.0
    vectorStorage(1) = 16.0
    assertEqualsDouble(matrix(0, 0), 11.0, 0.0)
    assertEqualsDouble(vector(1), 16.0, 0.0)

    matrix.data(3) = 44.0
    vector.data(0) = 55.0
    assertEqualsDouble(matrixStorage(3), 44.0, 0.0)
    assertEqualsDouble(vectorStorage(0), 55.0, 0.0)

  test("Mat and Vec case-class equality observes array identity"):
    val matrixStorage = Array(1.0, 2.0, 3.0, 4.0)
    val vectorStorage = Array(5.0, 6.0)

    assertEquals(Mat.unsafe(2, 2, matrixStorage), Mat.unsafe(2, 2, matrixStorage))
    assertNotEquals(Mat.unsafe(2, 2, matrixStorage), Mat.unsafe(2, 2, matrixStorage.clone))
    assertEquals(Vec.unsafe(vectorStorage), Vec.unsafe(vectorStorage))
    assertNotEquals(Vec.unsafe(vectorStorage), Vec.unsafe(vectorStorage.clone))

  test("Mat row col updated and Vec toArray detach their storage"):
    val matrixStorage = Array(1.0, 2.0, 3.0, 4.0)
    val vectorStorage = Array(5.0, 6.0)
    val matrix = Mat.unsafe(2, 2, matrixStorage)
    val vector = Vec.unsafe(vectorStorage)
    val row = matrix.row(0)
    val col = matrix.col(1)
    val updated = matrix.updated(1, 0, 30.0)
    val copied = vector.toArray

    matrixStorage(0) = 10.0
    matrixStorage(1) = 20.0
    matrixStorage(2) = 30.0
    vectorStorage(0) = 50.0
    row.data(1) = 200.0
    col.data(0) = 300.0
    copied(1) = 600.0

    assertEquals(row.data.toVector, Vector(1.0, 200.0))
    assertEquals(col.data.toVector, Vector(300.0, 4.0))
    assertEquals(updated.data.toVector, Vector(1.0, 2.0, 30.0, 4.0))
    assertEquals(copied.toVector, Vector(5.0, 600.0))
    assertEquals(matrix.data.toVector, Vector(10.0, 20.0, 30.0, 4.0))
    assertEquals(vector.data.toVector, Vector(50.0, 6.0))

  test("SPMG1 eval retains its R parity value at a representative lag"):
    val fixture = HrfRParityFixtures.kernel("spmg1")
    val index = fixture.times.indexOf(5.0)
    assert(index >= 0, "the trusted SPMG1 fixture must include a five-second lag")

    val actual = Hrfs.SPMG1(Lag(fixture.times(index))).data(0)
    assertEqualsDouble(actual, fixture.at(index, 0), 1e-9)
