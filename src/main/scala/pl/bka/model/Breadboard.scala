package pl.bka.model

sealed trait BreadboardTrack {
  val length: Int
}
case class Vertical(upper: Boolean, index: Int, length: Int) extends BreadboardTrack
case class Horizontal(upper: Boolean, left: Boolean, index: Boolean, length: Int) extends BreadboardTrack

case class Hole(track: BreadboardTrack, index: Int)

case class Breadboard(
                     logical: Map[LegId, BreadboardTrack],
                     physical: Map[LegId, Hole]
                     )


object Breadboard {
  def fromDiagram(diagram: Diagram): Breadboard = {
    val connsTracks: Map[Connection, BreadboardTrack] = diagram.connections.map(conn => (conn, Vertical(true, conn.id, 5))).toMap
    val logical: Map[LegId, BreadboardTrack] = diagram.connectionsLegs.flatMap {
      case (conn, legs) => legs.map((_, connsTracks(conn)))
    }
    Breadboard(logical, Map.empty[LegId, Hole])
  }
}