package pl.bka.model

case class TrackIndex(index: Int)

sealed trait BreadboardTrack
case class LogicalTrack(index: TrackIndex) extends BreadboardTrack
sealed trait PhysicalTrack extends BreadboardTrack {
  val length: Int
}
case class Vertical(upper: Boolean, index: TrackIndex, length: Int = 5) extends PhysicalTrack
case class Horizontal(upper: Boolean, left: Boolean, index: TrackIndex, length: Int = 25) extends PhysicalTrack

case class Logical(tracks: Seq[LogicalTrack], connections: Map[LegId, TrackIndex])
case class Hole(trackIndex: TrackIndex, holeIndex: Int)
case class Physical(tracks: Seq[PhysicalTrack], connections: Map[LegId, Hole])
case class Breadboard(
                     logical: Logical,
                     physical: Physical
                     )


object Breadboard {
  def fromDiagram(diagram: Diagram): Breadboard = {
    def toLogical: Logical = {
      val tracks: Seq[LogicalTrack] = diagram.connections.map(conn => LogicalTrack(TrackIndex(conn.id))).toList
      val map = diagram.connectionsLegs.flatMap {
        case (conn, legs) => legs.map((_, TrackIndex(conn.id)))
      }
      Logical(tracks, map)
    }
    def toPhysical(logical: Logical): Physical = {
      val tracks: Seq[PhysicalTrack] = logical.tracks.map(t => Vertical(upper = true, t.index))
      val map: Map[LegId, Hole] = logical.connections.toSeq.groupBy(_._2).mapValues(v => v.map(_._1)).flatMap {
        case (trackIndex, legIds) => legIds.zipWithIndex.map { case (legId, holeIndex) => (legId, Hole(trackIndex, holeIndex)) }
      }
      Physical(tracks, map)
    }
    val logical = toLogical
    Breadboard(logical, toPhysical(logical))
  }
}