package scalafim.image


object ConnComp:

  enum Connectivity:
    case Connect6, Connect18, Connect26

  private val offsets6: Vector[(Int, Int, Int)] =
    Vector(
      (-1, 0, 0), (1, 0, 0),
      (0, -1, 0), (0, 1, 0),
      (0, 0, -1), (0, 0, 1)
    )

  private val offsets18: Vector[(Int, Int, Int)] =
    offsets6 ++ Vector(
      (-1, -1, 0), (-1, 1, 0), (1, -1, 0), (1, 1, 0),
      (-1, 0, -1), (-1, 0, 1), (1, 0, -1), (1, 0, 1),
      (0, -1, -1), (0, -1, 1), (0, 1, -1), (0, 1, 1)
    )

  private val offsets26: Vector[(Int, Int, Int)] =
    val buf = Vector.newBuilder[(Int, Int, Int)]
    var dx = -1
    while dx <= 1 do
      var dy = -1
      while dy <= 1 do
        var dz = -1
        while dz <= 1 do
          if !(dx == 0 && dy == 0 && dz == 0) then buf += ((dx, dy, dz))
          dz += 1
        dy += 1
      dx += 1
    buf.result()

  private def offsets(conn: Connectivity): Vector[(Int, Int, Int)] =
    conn match
      case Connectivity.Connect6 => offsets6
      case Connectivity.Connect18 => offsets18
      case Connectivity.Connect26 => offsets26

  /** Connected components for a 3D boolean mask.
    *
    * Returns (indexVol, sizeVol) where:
    *  - indexVol: consecutive cluster ids (0 for background), ordered by decreasing size.
    *  - sizeVol: voxelwise cluster sizes (0 for background).
    */
  def connComp3D(
    mask: NeuroVol[Boolean],
    connectivity: Connectivity = Connectivity.Connect26,
    label: String = ""
  ): (NeuroVol[Int], NeuroVol[Int]) =
    val sp = mask.space
    val dims = sp.spatialDims
    val spatialNels = dims.product
    val activeIdx = Mask.indices(mask)
    if activeIdx.size == 0 then
      val zeros = PrimitiveBuffers.fillConst[Int](spatialNels, 0)
      val zvol = NeuroVol.fromLinear[Int](zeros, sp, label)
      (zvol, zvol)
    else
      val labels = PrimitiveBuffers.fillConst[Int](spatialNels, 0)
      val nodes = Array.ofDim[Int](activeIdx.size + 1) // 1-based provisional labels

      def find(i0: Int): Int =
        var root = i0
        while nodes(root) != root do root = nodes(root)
        var i = i0
        while nodes(i) != root do
          val parent = nodes(i)
          nodes(i) = root
          i = parent
        root

      val hood = offsets(connectivity)

      var nextLabel = 1
      var p = 0
      while p < activeIdx.size do
        val lin = activeIdx(p)
        val g = Indexing.indexToGrid3D(dims, lin)
        val x0 = g(0); val y0 = g(1); val z0 = g(2)

        var minLab = Int.MaxValue
        val nbuf = Array.newBuilder[Int]
        var h = 0
        while h < hood.length do
          val (dx, dy, dz) = hood(h)
          val x = x0 + dx
          val y = y0 + dy
          val z = z0 + dz
          if x >= 0 && x < dims(0) && y >= 0 && y < dims(1) && z >= 0 && z < dims(2) then
            val nlin = Indexing.gridToIndex3D(dims, x, y, z)
            val lab = labels(nlin)
            if lab != 0 then
              nbuf += lab
              if lab < minLab then minLab = lab
          h += 1

        val neighborLabs = nbuf.result()
        if neighborLabs.isEmpty then
          nodes(nextLabel) = nextLabel
          labels(lin) = nextLabel
        else
          labels(lin) = minLab
          nodes(nextLabel) = minLab
          val uniq = neighborLabs.distinct
          var u = 0
          val rootMin = find(minLab)
          while u < uniq.length do
            val r = find(uniq(u))
            nodes(r) = rootMin
            u += 1

        nextLabel += 1
        p += 1

      // Second pass: resolve labels, count cluster sizes
      val counts = scala.collection.mutable.Map.empty[Int, Int]
      p = 0
      while p < activeIdx.size do
        val lin = activeIdx(p)
        val root = find(labels(lin))
        labels(lin) = root
        counts.update(root, counts.getOrElse(root, 0) + 1)
        p += 1

      val sortedRoots =
        counts.toVector.sortBy { case (lab, size) => (-size, lab) }
      val rootToNew = sortedRoots.zipWithIndex.map { case ((lab, _), i) => lab -> (i + 1) }.toMap

      val idxOut = PrimitiveBuffers.fillConst[Int](spatialNels, 0)
      val sizeOut = PrimitiveBuffers.fillConst[Int](spatialNels, 0)

      p = 0
      while p < activeIdx.size do
        val lin = activeIdx(p)
        val root = labels(lin)
        val nid = rootToNew(root)
        val sz = counts(root)
        idxOut(lin) = nid
        sizeOut(lin) = sz
        p += 1

      (NeuroVol.fromLinear[Int](idxOut, sp, label), NeuroVol.fromLinear[Int](sizeOut, sp, label))
