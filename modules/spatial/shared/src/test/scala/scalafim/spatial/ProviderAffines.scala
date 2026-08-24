package scalafim.spatial

import image4s.geometry.{Affine, D3}

private[spatial] object ProviderAffines:
  val identity: Affine[D3] =
    Affine.identity[D3]

  def fromRows(rows: Vector[Vector[Double]]): Affine[D3] =
    require(rows.length == 4 && rows.forall(_.length == 4), "D3 affine fixture must be 4x4")
    fromRowMajor(rows.flatten)

  def fromRowMajor(values: IterableOnce[Double]): Affine[D3] =
    Affine
      .fromRowMajor[D3](values)
      .fold(error => throw new IllegalArgumentException(error.message), value => value)
