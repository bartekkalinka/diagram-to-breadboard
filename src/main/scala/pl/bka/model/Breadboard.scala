package pl.bka.model

sealed trait BreadboardTrack
case class LogicalTrack(index: Int) extends BreadboardTrack
case class Vertical(upper: Boolean, index: Int, length: Int) extends BreadboardTrack
case class Horizontal(upper: Boolean, left: Boolean, index: Int, length: Int) extends BreadboardTrack

case class Hole(track: BreadboardTrack, index: Int)

case class Breadboard(
                     logical: Map[LegId, LogicalTrack],
                     physical: Map[LegId, Hole]
                     )


object Breadboard {
  def fromDiagram(diagram: Diagram): Breadboard = {
    val connsTracks: Map[Connection, LogicalTrack] = diagram.connections.map(conn => (conn, LogicalTrack(conn.id))).toMap
    val logical: Map[LegId, LogicalTrack] = diagram.connectionsLegs.flatMap {
      case (conn, legs) => legs.map((_, connsTracks(conn)))
    }
    Breadboard(logical, Map.empty[LegId, Hole])
  }
}