package scalafim.surface.view

/** Reference interpolation in original-triangle barycentric coordinates.
  * Nonfinite samples invalidate only locations where they contribute. Clipping
  * and raster edge tolerance can leave tiny negative weights; clamp those to
  * zero and renormalize to retain constant fields exactly.
  */
object SurfaceScalarInterpolation:
  def value(a: Double, b: Double, c: Double, wa: Double, wb: Double, wc: Double): Double =
    require(wa.isFinite && wb.isFinite && wc.isFinite &&
      wa >= -1e-10 && wb >= -1e-10 && wc >= -1e-10 &&
      math.abs(wa + wb + wc - 1.0) <= 1e-9, "expected normalized barycentric weights")
    val x = math.max(0.0, wa)
    val y = math.max(0.0, wb)
    val z = math.max(0.0, wc)
    if (x > 0.0 && !a.isFinite) || (y > 0.0 && !b.isFinite) || (z > 0.0 && !c.isFinite) then Double.NaN
    else
      val av = if x == 0.0 then 0.0 else a
      val bv = if y == 0.0 then 0.0 else b
      val cv = if z == 0.0 then 0.0 else c
      val anchor = if x > 0.0 then av else if y > 0.0 then bv else cv
      val constant = (x == 0.0 || av == anchor) && (y == 0.0 || bv == anchor) && (z == 0.0 || cv == anchor)
      val direct = (x * av + y * bv + z * cv) / (x + y + z)
      val scale = math.max(math.abs(av), math.max(math.abs(bv), math.abs(cv)))
      if constant then anchor
      else if direct.isFinite then direct
      else if scale == 0.0 then 0.0
      else
        val normalized = (x * (av / scale) + y * (bv / scale) + z * (cv / scale)) / (x + y + z)
        math.max(-1.0, math.min(1.0, normalized)) * scale
