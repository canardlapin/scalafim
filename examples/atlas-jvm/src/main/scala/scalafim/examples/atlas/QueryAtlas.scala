package scalafim.examples.atlas

import scalafim.atlas.*

final case class QueryExampleRow(
  input: Point3D,
  label: Option[String],
  regionId: Option[RegionId],
  distanceMm: Option[Double]
)

object QueryAtlas:
  def exactToyQuery(point: Point3D): QueryExampleRow =
    fromHit(AtlasQuery.exact(AtlasExampleData.atlas(), point, fromSpace = SpaceId.MNI152))

  def radiusToyQuery(point: Point3D, radiusMm: Double): Vector[QueryExampleRow] =
    AtlasQuery
      .query(AtlasExampleData.atlas(), Vector(point), radiusMm = radiusMm, fromSpace = SpaceId.MNI152)
      .map(fromHit)

  private def fromHit(hit: QueryHit): QueryExampleRow =
    QueryExampleRow(hit.input, hit.label, hit.id, hit.distanceMm)

@main def queryToyAtlas(): Unit =
  val rows = QueryAtlas.radiusToyQuery(Point3D(0.0, 0.0, 0.0), radiusMm = 3.0)
  rows.foreach { row =>
    println(s"${row.input} -> ${row.regionId.map(_.value).getOrElse(0)} ${row.label.getOrElse("background")}")
  }
