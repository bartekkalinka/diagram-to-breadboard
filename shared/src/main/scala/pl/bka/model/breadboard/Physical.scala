package pl.bka.model.breadboard

import pl.bka.model.{Component, ComponentName, Diagram, LegId}

case class VerticalPosition(position: Int)
case class Hole(trackIndex: TrackIndex, holeIndex: VerticalPosition)
case class Physical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, Hole]) {
  def sortedConnections: Seq[(LegId, Hole)] = connections.toSeq.groupBy(_._1.cName).toSeq.flatMap {
    case (_, legHolePairs) => legHolePairs.sortBy(_._1.leg.name)
  }
  def prettyPrint: Seq[String] = Seq(
    s"""   physical tracks: $tracks""",
    s"""   physical conns: ${sortedConnections.map { case (l, Hole(t, h)) => l.prettyPrint + "-track" + t.index + "/hole" + h.position }}"""
  )
}

object Physical {
  def apply(logical: Logical): Physical = {
    val tracks: Seq[Track] = logical.tracks.collect { case t: Vertical => t }
    def insertComponent(cName: ComponentName, mapLegHole: Map[LegId, Hole],
                        freePositions: Map[TrackIndex, Seq[VerticalPosition]]): (Map[LegId, Hole], Map[TrackIndex, Seq[VerticalPosition]]) = {
      val compLegs: Seq[LegId] = logical.componentsLegs(cName)
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
    val (mapLegHole, _) = logical.components.map(_.name).foldLeft((Map[LegId, Hole](), initialFreePositions)) {
      case ((mLegHole, freePositions), cName) => insertComponent(cName, mLegHole, freePositions)
    }
    Physical(logical.components, tracks, mapLegHole)
  }
}

