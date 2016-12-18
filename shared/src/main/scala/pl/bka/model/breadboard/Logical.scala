package pl.bka.model.breadboard

import pl.bka.model._

case class Logical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, TrackIndex]) extends Container {
  def prettyPrint: Seq[String] = Seq(
    s"""   logical: tracks cnt: ${tracks.length} conns: ${connections.map { case (l, i) => l.prettyPrint + "-conn" + i.index }}"""
  )
}

object Logical {
  private def transistorsToTracks(diagram: Diagram): (Seq[Vertical], Seq[(LegId, TrackIndex)]) = {
    val transistors = diagram.components.filter(_.cType.isInstanceOf[Transistor])
    val transistorsLegs: Seq[LegId] = transistors.flatMap { t =>
      t.legs.map { leg => LegId(t.name, leg) }
    }
    val (vertical: Seq[Vertical], transistorMap: Seq[(LegId, TrackIndex)]) = transistorsLegs.zipWithIndex.map {
      case (legId, index) =>
        (
          Vertical(upper = true, TrackIndex(index), diagram.legsConnections(legId)),
          (legId, TrackIndex(index))
          )
    }.unzip
    (vertical, transistorMap)
  }

  private def calcCables(vertical: Seq[Vertical]): (Seq[Component], Map[LegId, TrackIndex]) = {
    def addCable(connection: Connection)(prev: Track, next: Track): (Component, Seq[(LegId, TrackIndex)]) = {
      val cName = s"cable-${connection.id.fold(identity, identity)}-${prev.index.index}-${next.index.index}"
      val cable = Component(cName, Cable(""))
      val legs = Seq(
        (LegId(ComponentName(cName), cable.legs.head), prev.index),
        (LegId(ComponentName(cName), cable.legs(1)), next.index)
      )
      (cable, legs)
    }
    def connectTracksWithCables(comps: Seq[Component], legsMap: Map[LegId, TrackIndex],
                                connection: Connection, tracksGroup: Seq[Vertical]): (Seq[Component], Map[LegId, TrackIndex]) = {
      val (cables, cableLegs) = tracksGroup.init.zip(tracksGroup.tail).map((addCable(connection) _).tupled).unzip
      (comps ++ cables, legsMap ++ cableLegs.flatten.toMap)
    }
    val connectionTracks = vertical.groupBy(_.diagramConnection)
    connectionTracks.toSeq.foldLeft((Seq[Component](), Map[LegId, TrackIndex]())) {
      case ((comps, legsMap), (connection, tracksGroup)) =>
        if(tracksGroup.length > 1) connectTracksWithCables(comps, legsMap, connection, tracksGroup) else (comps, legsMap)
    }
  }

  def apply(diagram: Diagram): Logical = {
    val (vertical, transistorMap) = transistorsToTracks(diagram)
    val (cables, cablesLegs) = calcCables(vertical)
    val map: Map[LegId, TrackIndex] = transistorMap.toMap ++ cablesLegs
    val extComponents = diagram.components ++ cables
    val horizontal = Seq(
      Horizontal(upper = true, left = true, index = TrackIndex(0), power = Power.Plus),
      Horizontal(upper = true, left = true, index = TrackIndex(1), power = Power.GND)
    )
    Logical(extComponents, vertical ++ horizontal, map)
  }
}

