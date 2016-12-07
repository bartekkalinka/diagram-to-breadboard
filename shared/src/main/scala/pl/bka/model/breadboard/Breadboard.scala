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
  def toLogical(diagram: Diagram): (Diagram, Logical) = {
    val transistors = diagram.components.filter(_.cType.isInstanceOf[Transistor])
    val transistorsLegs: Seq[LegId] = transistors.flatMap { t =>
      t.legs.map { leg => LegId(t.name, leg) }
    }
    val (tracks: Seq[Vertical], transistorMap: Seq[(LegId, TrackIndex)]) = transistorsLegs.zipWithIndex.map {
      case (legId, index) =>
        (
          Vertical(upper = true, TrackIndex(index), diagram.legsConnections(legId)),
          (legId, TrackIndex(index))
          )
    }.unzip
    def calcCables: (Seq[Component], Map[LegId, TrackIndex]) = {
      val connectionTracks = tracks.groupBy(_.diagramConnection)
      connectionTracks.toSeq.foldLeft((Seq[Component](), Map[LegId, TrackIndex]())) {
        case ((comps, legsMap), (connection, tracksGroup)) =>
          if(tracksGroup.length > 1) {
            val (cables, cableLegs) = tracksGroup.zipWithIndex.map { case (track, i) =>
              val cName = s"${connection.id}-$i"
              val component = Component(cName, Cable(""))
              val legs = component.legs.map(leg => (LegId(ComponentName(cName), leg), track.index))
              (component, legs)
            }.unzip
            (comps ++ cables, legsMap ++ cableLegs.flatten.toMap)
          }
          else {
            (comps ++ Seq[Component](), legsMap ++ Map[LegId, TrackIndex]())
          }
      }
    }
    val (cables, cablesLegs) = calcCables
    val map: Map[LegId, TrackIndex] = transistorMap.toMap ++ cablesLegs
    val extDiagram: Diagram = diagram.copy(components = diagram.components ++ cables)
    (extDiagram, Logical(tracks, map))
  }

  def toPhysical(diagram: Diagram, logical: Logical): Physical = {
    val tracks: Seq[Track] = logical.tracks.collect { case t: Vertical => t }
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

  def fromDiagram(diagram: Diagram): Breadboard = {
    val (extDiagram, logical) = toLogical(diagram)
    Breadboard(logical, toPhysical(extDiagram, logical))
  }
}