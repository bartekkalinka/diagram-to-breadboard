package pl.bka.model.breadboard

import pl.bka.model._

case class Logical(tracks: Seq[Track], connections: Map[LegId, TrackIndex]) {
  def prettyPrint: Seq[String] = Seq(
    s"""   logical: tracks cnt: ${tracks.length} conns: ${connections.map { case (l, i) => l.prettyPrint + "-conn" + i.index }}"""
  )
}
case class VerticalPosition(position: Int)
case class Hole(trackIndex: TrackIndex, holeIndex: VerticalPosition)
case class Physical(tracks: Seq[Track], connections: Map[LegId, Hole]) {
  def sortedConnections: Seq[(LegId, Hole)] = connections.toSeq.groupBy(_._1.cName).toSeq.flatMap {
    case (_, legHolePairs) => legHolePairs.sortBy(_._1.leg.name)
  }
  def prettyPrint: Seq[String] = Seq(
    s"""   physical tracks: $tracks""",
    s"""   physical conns: ${sortedConnections.map { case (l, Hole(t, h)) => l.prettyPrint + "-track" + t.index + "/hole" + h.position }}"""
  )
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
      def insertComponent(cName: ComponentName, mapLegHole: Map[LegId, Hole],
                          freePositions: Map[TrackIndex, Seq[VerticalPosition]]): (Map[LegId, Hole], Map[TrackIndex, Seq[VerticalPosition]]) = {
        val compLegs: Seq[LegId] = diagram.componentsLegs(cName)
        val minPositions: Seq[VerticalPosition] = compLegs.map { legId =>
          val track = logical.connections(legId)
          freePositions(track).minBy(_.position)
        }
        val targetPosition: VerticalPosition = minPositions.maxBy(_.position)
        val legHoleDelta: Map[LegId, Hole] = compLegs.map { legId => (legId, Hole(logical.connections(legId), targetPosition)) }.toMap
        val newMapLegHole: Map[LegId, Hole] = mapLegHole ++ legHoleDelta
        val newFreePositions: Map[TrackIndex, Seq[VerticalPosition]] = legHoleDelta.toSeq.foldLeft(freePositions) { case (fps, (legId, hole)) =>
          val track = logical.connections(legId)
          val newFps = fps(track).filterNot(_ == hole.holeIndex)
          fps.updated(track, newFps)
        }
        (newMapLegHole, newFreePositions)
      }
      val initialFreePositions: Map[TrackIndex, Seq[VerticalPosition]] = tracks.map { track =>
        (track.index, List.tabulate(track.length)(VerticalPosition))
      }.toMap
      val (mapLegHole, _) = diagram.components.map(_.name).foldLeft((Map[LegId, Hole](), initialFreePositions)) {
        case ((mLegHole, freePositions), cName) => insertComponent(cName, mLegHole, freePositions)
      }
      Physical(tracks, mapLegHole)
    }
    val logical = toLogical
    Breadboard(logical, toPhysical(logical))
  }
}