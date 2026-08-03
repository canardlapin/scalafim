package scalafim.image

import image4s.apply
import munit.FunSuite

final class Image4sAdmissionSuite extends FunSuite:
  test("legacy scalar volume enters image4s with logical and affine parity"):
    val shape = Vector(2, 3, 4)
    val affine =
      DMat.fromRows(
        Vector(
          Vector(0.0, -2.0, 0.0, 11.0),
          Vector(3.0, 0.0, 0.0, -7.0),
          Vector(0.0, 0.0, 4.0, 5.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val space = NeuroSpace(shape, trans = Some(affine))
    val values = Array.ofDim[Double](shape.product)
    var k = 0
    while k < shape(2) do
      var j = 0
      while j < shape(1) do
        var i = 0
        while i < shape(0) do
          values(i + shape(0) * (j + shape(1) * k)) =
            100.0 * i.toDouble + 10.0 * j.toDouble + k.toDouble
          i += 1
        j += 1
      k += 1

    val legacy =
      NeuroVol.fromLinear[Double](values, space, "admission-volume")
    val imported =
      Image4sInterop
        .canonicalizeScalarVolume(legacy)
        .fold(error => fail(error.message), identity)

    assertEquals(
      imported.transfer,
      Image4sStorageTransfer.CanonicalizedLegacy
    )
    imported.sampled.fold(
      _ => fail("expected a D3 sampled image"),
      d3 =>
        val ranked =
          d3.value
            .requireDataRank[3]
            .fold(error => fail(error.message), identity)
        assertEquals(ranked.logicalShape, shape)
        assertEquals(ranked.grid.indexToFrame.rowMajor, affine.toRows.flatten)
        assertEquals(ranked(1, 2, 3), 123.0)
        assertEquals(ranked(0, 1, 2), 12.0)
    )
