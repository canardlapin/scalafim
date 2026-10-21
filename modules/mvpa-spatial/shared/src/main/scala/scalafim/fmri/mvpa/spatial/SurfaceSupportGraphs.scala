package scalafim.fmri.mvpa.spatial

import mvpa4s.locus.SupportWeightPolicy
import mvpa4s.AxisRef
import mvpa4s.pattern.*
import neurosurf4s.surface.{TriangleMesh, VertexId}

/** Geometry adapters build only supplied anatomical adjacency. Spatially close
  * vertices on opposing sulcal banks are never linked by Euclidean proximity.
  */
object SurfaceSupportGraphs:
  def surface[P](axis: AxisRef[P], mesh: TriangleMesh, orderedVertices: Vector[VertexId],
      coordinateUnits: String, weights: SupportWeightPolicy = SupportWeightPolicy.UnitWeights): Either[SpatialSupportError, SupportGraph] =
    if orderedVertices.size != axis.size || orderedVertices.distinct.size != orderedVertices.size ||
        orderedVertices.exists(v => v.index >= mesh.vertexCount) || coordinateUnits.trim.isEmpty then
      Left(SpatialSupportError.Invalid("surface mapping/coordinate units are invalid"))
    else
      val selected = orderedVertices.map(_.index).zipWithIndex.toMap
      val edges = scala.collection.mutable.HashSet.empty[(Int, Int)]
      var face = 0
      while face < mesh.faceCount do
        val offset = face * 3
        var corner = 0
        while corner < 3 do
          val a = mesh.faceIndices(offset + corner)
          val b = mesh.faceIndices(offset + (corner + 1) % 3)
          for left <- selected.get(a); right <- selected.get(b) do
            edges += ((math.min(left, right), math.max(left, right)))
          corner += 1
        face += 1
      val weighted = edges.toVector.sorted.map: (a, b) =>
        val left = mesh.vertex(orderedVertices(a))
        val right = mesh.vertex(orderedVertices(b))
        val length = math.hypot(math.hypot(left.x - right.x, left.y - right.y), left.z - right.z)
        val weight = weights match
          case SupportWeightPolicy.UnitWeights => 1.0
          case SupportWeightPolicy.InversePhysicalLength => 1.0 / length
        SupportEdge(a, b, weight)
      SupportGraph(axis.descriptor, weighted, SupportTopology.SurfaceTriangles(mesh.topologyIdentity.toString), units(weights, coordinateUnits))

  private def units(policy: SupportWeightPolicy, coordinateUnits: String): String = policy match
    case SupportWeightPolicy.UnitWeights => "dimensionless"
    case SupportWeightPolicy.InversePhysicalLength => s"1/$coordinateUnits"
