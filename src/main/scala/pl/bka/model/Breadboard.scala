package pl.bka.model

sealed trait BreadboardTrack
case class LogicalTrack(index: Int) extends BreadboardTrack
sealed trait PhysicalTrack extends BreadboardTrack {
  val length: Int
}
case class Vertical(upper: Boolean, index: Int, length: Int) extends PhysicalTrack
case class Horizontal(upper: Boolean, left: Boolean, index: Int, length: Int) extends PhysicalTrack

case class Hole(track: PhysicalTrack, index: Int)

case class Breadboard(
                     logical: Map[LegId, LogicalTrack],
                     physical: Map[LegId, Hole]
                     )


object Breadboard {
  def fromDiagram(diagram: Diagram): Breadboard = {
    def logical: Map[LegId, LogicalTrack] = {
      val connsTracks: Map[Connection, LogicalTrack] = diagram.connections.map(conn => (conn, LogicalTrack(conn.id))).toMap
      diagram.connectionsLegs.flatMap {
        case (conn, legs) => legs.map((_, connsTracks(conn)))
      }
    }
    def physical(logical: Map[LegId, LogicalTrack]): Map[LegId, Hole] = ???
    Breadboard(logical, Map.empty[LegId, Hole])
  }
}