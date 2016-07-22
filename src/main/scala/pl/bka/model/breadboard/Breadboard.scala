package pl.bka.model.breadboard

import pl.bka.model._

case class Logical(tracks: Seq[Track], connections: Map[LegId, TrackIndex]) {
  def prettyPrint: Seq[String] = Seq(
    s"""   logical: tracks cnt: ${tracks.length} conns: ${connections.map { case (l, i) => l.prettyPrint + "-conn" + i.index }}"""
  )
}
case class Hole(trackIndex: TrackIndex, holeIndex: Int)
case class Physical(tracks: Seq[Track], connections: Map[LegId, Hole]) {
  def prettyPrint: Seq[String] = Seq(
    s"""   physical tracks: $tracks""",
    s"""   physical conns: ${connections.map { case (l, Hole(t, h)) => l.prettyPrint + "-track" + t.index + "/hole" + h }}"""
  )
  def draw(): Unit = tracks.foreach(_.draw(10))
}
case class Breadboard(
                     logical: Logical,
                     physical: Physical
                     ) {
  def prettyPrint: Seq[String] = logical.prettyPrint ++ physical.prettyPrint
}

object Breadboard {
  def fromDiagram(diagram: Diagram): Breadboard = {
    def toLogical: Logical = {
      val tracks: Seq[Track] = diagram.connections.map(conn => Vertical(upper = true, TrackIndex(conn.id))).toList
      val map = diagram.connectionsLegs.flatMap {
        case (conn, legs) => legs.map((_, TrackIndex(conn.id)))
      }
      Logical(tracks, map)
    }
    def toPhysical(logical: Logical): Physical = {
      val tracks: Seq[Track] = logical.tracks.map(t => Vertical(upper = true, t.index))
      val mapTrackLegs: Map[TrackIndex, Seq[LegId]] = logical.connections.toSeq.groupBy(_._2).mapValues(v => v.map(_._1))
      val mapLegHole: Map[LegId, Hole] = mapTrackLegs.flatMap {
        case (trackIndex, legIds) => legIds.zipWithIndex.map { case (legId, holeIndex) => (legId, Hole(trackIndex, holeIndex)) }
      }
      Physical(tracks, mapLegHole)
    }
    val logical = toLogical
    Breadboard(logical, toPhysical(logical))
  }
}