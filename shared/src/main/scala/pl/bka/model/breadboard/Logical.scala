package pl.bka.model.breadboard

import pl.bka.model.Power.PowerConnection
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
          Vertical(upper = true, TrackIndex(horizontal = false, index), diagram.legsConnections(legId)),
          (legId, TrackIndex(horizontal = false, index))
          )
    }.unzip
    (vertical, transistorMap)
  }

  private def calcRegularConnectionCables(vertical: Seq[Vertical]): (Seq[Component], Map[LegId, TrackIndex]) = {
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
    val connectionTracks = vertical.filter(_.diagramConnection.id.isLeft).groupBy(_.diagramConnection)
    connectionTracks.toSeq.foldLeft((Seq[Component](), Map[LegId, TrackIndex]())) {
      case ((comps, legsMap), (connection, tracksGroup)) =>
        if(tracksGroup.length > 1) connectTracksWithCables(comps, legsMap, connection, tracksGroup) else (comps, legsMap)
    }
  }

  private def calcPowerCables(vertical: Seq[Vertical],
                              horizontalMap: Map[PowerConnection, Horizontal]): (Seq[Component], Map[LegId, TrackIndex]) = {
    val powerConnectionTracks = vertical.filter(v => v.diagramConnection.id.isRight)
    val (cables, legs) = powerConnectionTracks.map { track =>
      val cName = s"cable-${track.diagramConnection.id.fold(identity, identity)}-${track.index.index}"
      val cable = Component(cName, Cable(""))
      val Right(power) = track.diagramConnection.id
      val legs = Seq(
        (LegId(ComponentName(cName), cable.legs.head), track.index),
        (LegId(ComponentName(cName), cable.legs(1)), horizontalMap(power).index)
      )
      (cable, legs)
    }.unzip
    (cables, legs.flatten.toMap)
  }

  private def horizontalTracks: (Seq[Horizontal], Map[PowerConnection, Horizontal]) = {
    val horizontal = Seq(
      Horizontal(upper = true, left = true, index = TrackIndex(horizontal = true, 0), power = Power.Plus),
      Horizontal(upper = true, left = true, index = TrackIndex(horizontal = true, 1), power = Power.GND)
    )
    val horizontalMap: Map[PowerConnection, Horizontal] = Map(Power.Plus -> horizontal.head, Power.GND -> horizontal(1))
    (horizontal, horizontalMap)
  }

  private def otherToTracks(diagram: Diagram, vertical: Seq[Vertical]): Map[LegId, TrackIndex] = {
    val other = diagram.components.filterNot(_.cType.isInstanceOf[Transistor])
    val otherLegs: Seq[LegId] = other.flatMap { t =>
      t.legs.map { leg => LegId(t.name, leg) }
    }
    val verticalByConnection = vertical.groupBy(_.diagramConnection)
    otherLegs.map { legId =>
      val conn = diagram.legsConnections(legId)
      (legId, verticalByConnection(conn).head.index)
    }.toMap
  }

  def apply(diagram: Diagram): Logical = {
    val (vertical, transistorMap) = transistorsToTracks(diagram)
    val (regularCables, regularCablesLegs) = calcRegularConnectionCables(vertical)
    val (horizontal, horizontalMap) = horizontalTracks
    val (powerCables, powerCablesLegs) = calcPowerCables(vertical, horizontalMap)
    val otherLegs = otherToTracks(diagram, vertical)
    val map: Map[LegId, TrackIndex] = transistorMap.toMap ++ regularCablesLegs ++ powerCablesLegs ++ otherLegs
    val extComponents = diagram.components ++ regularCables ++ powerCables
    Logical(extComponents, vertical ++ horizontal, map)
  }
}

