package pl.bka.model

import pl.bka.DomOutput

case class TrackIndex(index: Int)

sealed trait BreadboardTrack
case class LogicalTrack(index: TrackIndex) extends BreadboardTrack
sealed trait PhysicalTrack extends BreadboardTrack {
  val length: Int
  def draw(position: Int): Unit
}
case class Vertical(upper: Boolean, index: TrackIndex, length: Int = 5) extends PhysicalTrack {
  def draw(position: Int): Unit = {
    val ctx = DomOutput.renderer
    ctx.strokeStyle = "#000000"
    ctx.lineWidth = 2
    ctx.beginPath()
    ctx.moveTo(index.index * 15 + 30, position)
    ctx.lineTo(index.index * 15 + 30, position + 125)
    ctx.stroke()
  }
}
case class Horizontal(upper: Boolean, left: Boolean, index: TrackIndex, length: Int = 25) extends PhysicalTrack {
  def draw(position: Int): Unit = () //TODO
}

case class Logical(tracks: Seq[LogicalTrack], connections: Map[LegId, TrackIndex]) {
  def prettyPrint: Seq[String] = Seq(
    s"""   logical: tracks cnt: ${tracks.length} conns: ${connections.map { case (l, i) => l.prettyPrint + "-conn" + i.index }}"""
  )
}
case class Hole(trackIndex: TrackIndex, holeIndex: Int)
case class Physical(tracks: Seq[PhysicalTrack], connections: Map[LegId, Hole]) {
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