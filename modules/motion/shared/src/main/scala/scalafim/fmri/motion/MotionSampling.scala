package scalafim.fmri.motion

import ravel.NDArray
import ravel.Rank

private[motion] object MotionSampling:
  final case class VoxelMap(ax: Array[Double], ay: Array[Double], az: Array[Double], c: Array[Double])

  def voxelMap(
      nx: Int,
      ny: Int,
      nz: Int,
      zpad: Int,
      px: Double,
      py: Double,
      pz: Double,
      pose: RigidPose
  ): VoxelMap =
    val nxp = nx + 2 * zpad
    val nyp = ny + 2 * zpad
    val nzp = nz + 2 * zpad
    val cx = 0.5 * (nxp - 1.0)
    val cy = 0.5 * (nyp - 1.0)
    val cz = 0.5 * (nzp - 1.0)
    val centerW = Array(cx * px, cy * py, cz * pz)
    val trans = Array(pose.tx, pose.ty, pose.tz)
    val invp = Array(1.0 / px, 1.0 / py, 1.0 / pz)
    val r = rotationMatrix(pose.rx, pose.ry, pose.rz)

    val ax = Array.ofDim[Double](3)
    val ay = Array.ofDim[Double](3)
    val az = Array.ofDim[Double](3)
    val c = Array.ofDim[Double](3)
    var row = 0
    while row < 3 do
      val r0 = row
      val r1 = row + 3
      val r2 = row + 6
      ax(row) = r(r0) * px * invp(row)
      ay(row) = r(r1) * py * invp(row)
      az(row) = r(r2) * pz * invp(row)
      val base =
        (-r(r0) * (centerW(0) + trans(0)) -
          r(r1) * (centerW(1) + trans(1)) -
          r(r2) * (centerW(2) + trans(2))) *
          invp(row) +
          (if row == 0 then cx else if row == 1 then cy else cz)
      c(row) = base + (ax(row) + ay(row) + az(row)) * zpad.toDouble - zpad.toDouble
      row += 1

    VoxelMap(ax, ay, az, c)

  inline def sourceX(map: VoxelMap, i: Int, j: Int, k: Int): Double =
    map.ax(0) * i + map.ay(0) * j + map.az(0) * k + map.c(0)

  inline def sourceY(map: VoxelMap, i: Int, j: Int, k: Int): Double =
    map.ax(1) * i + map.ay(1) * j + map.az(1) * k + map.c(1)

  inline def sourceZ(map: VoxelMap, i: Int, j: Int, k: Int): Double =
    map.ax(2) * i + map.ay(2) * j + map.az(2) * k + map.c(2)

  def rotationMatrix(rx: Double, ry: Double, rz: Double): Array[Double] =
    val cx = math.cos(rx)
    val sx = math.sin(rx)
    val cy = math.cos(ry)
    val sy = math.sin(ry)
    val cz = math.cos(rz)
    val sz = math.sin(rz)
    Array(
      cz * cy,
      cz * sy * sx - sz * cx,
      cz * sy * cx + sz * sx,
      sz * cy,
      sz * sy * sx + cz * cx,
      sz * sy * cx - cz * sx,
      -sy,
      cy * sx,
      cy * cx
    )

  def trilinear(
      data: NDArray[Double, Rank[4]],
      nx: Int,
      ny: Int,
      nz: Int,
      t: Int,
      x: Double,
      y: Double,
      z: Double,
      zeroPad: Boolean
  ): Double =
    val x0 = math.floor(x).toInt
    val y0 = math.floor(y).toInt
    val z0 = math.floor(z).toInt
    val x1 = x0 + 1
    val y1 = y0 + 1
    val z1 = z0 + 1
    val ax = x - x0
    val ay = y - y0
    val az = z - z0

    inline def sample(i0: Int, j0: Int, k0: Int): Double =
      var ii = i0
      var jj = j0
      var kk = k0
      if zeroPad then
        if ii < 0 || ii >= nx || jj < 0 || jj >= ny || kk < 0 || kk >= nz then 0.0
        else data(ii, jj, kk, t)
      else
        if ii < 0 then ii = 0 else if ii >= nx then ii = nx - 1
        if jj < 0 then jj = 0 else if jj >= ny then jj = ny - 1
        if kk < 0 then kk = 0 else if kk >= nz then kk = nz - 1
        data(ii, jj, kk, t)

    val c000 = sample(x0, y0, z0)
    val c100 = sample(x1, y0, z0)
    val c010 = sample(x0, y1, z0)
    val c110 = sample(x1, y1, z0)
    val c001 = sample(x0, y0, z1)
    val c101 = sample(x1, y0, z1)
    val c011 = sample(x0, y1, z1)
    val c111 = sample(x1, y1, z1)

    val c00 = c000 * (1.0 - ax) + c100 * ax
    val c10 = c010 * (1.0 - ax) + c110 * ax
    val c01 = c001 * (1.0 - ax) + c101 * ax
    val c11 = c011 * (1.0 - ax) + c111 * ax
    val c0 = c00 * (1.0 - ay) + c10 * ay
    val c1 = c01 * (1.0 - ay) + c11 * ay
    c0 * (1.0 - az) + c1 * az
