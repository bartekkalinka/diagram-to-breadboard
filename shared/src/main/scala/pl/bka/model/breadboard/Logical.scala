package pl.bka.model.breadboard

import pl.bka.model.Power.PowerConnection
import pl.bka.model._
import scala.collection.mutable

case class Logical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, TrackIndex]) extends Container {
  def prettyPrint: Seq[String] = Seq(
    s"""   logical: tracks cnt: ${tracks.length} conns: ${connections.map { case (l, i) => l.prettyPrint + "-conn" + i.index }}"""
  )
}

object Logical {
  def apply(diagram: Diagram): Logical = {
    val (vertical, transistorMap) = transistorsToTracks(diagram)
    val (otherLegs, extVertical) = otherToTracks(diagram, vertical)
    val (regularCables, regularCablesLegs) = calcRegularConnectionCables(extVertical)
    val (horizontal, horizontalMap) = horizontalTracks
    val (powerCables, powerCablesLegs) = calcPowerCables(extVertical, horizontalMap)
    val map: Map[LegId, TrackIndex] = transistorMap.toMap ++ regularCablesLegs ++ powerCablesLegs ++ otherLegs
    val extComponents = diagram.components ++ regularCables ++ powerCables
    Logical(extComponents, extVertical ++ horizontal, map)
  }

  private def transistorsToTracks(diagram: Diagram): (Seq[Vertical], Seq[(LegId, TrackIndex)]) = {
    val transistors = diagram.components.filter(_.cType.isInstanceOf[Transistor])
    val transistorsLegs: Seq[LegId] = transistors.flatMap { t =>
      t.legs.map { leg => LegId(t.name, leg) }
    }
    val (vertical: Seq[Vertical], transistorMap: Seq[(LegId, TrackIndex)]) = transistorsLegs.zipWithIndex.map {
      case (legId, index) =>
        (
          Vertical(upper = true, TrackIndex(horizontal = false, index), diagram.legsConnections(legId), freeSpace = Tracks.verticalTrackLength - 1),
          (legId, TrackIndex(horizontal = false, index))
          )
    }.unzip
    (vertical, transistorMap)
  }

  private def otherToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Map[LegId, TrackIndex], Seq[Vertical]) = {
    val other = diagram.components.filterNot(_.cType.isInstanceOf[Transistor])
    val legs = other.flatMap(c => c.legs.map(leg => LegId(c.name, leg)))
    var verticalByConnection = mutable.Map(vertical.groupBy(_.diagramConnection).mapValues(_.map(_.index)).toSeq: _*)
    val verticalByIndex = mutable.Map(vertical.groupBy(_.index).mapValues(_.head).toSeq: _*)
    val legsMap = mutable.Map.empty[LegId, TrackIndex]
    legs.foreach { legId =>
      val conn = diagram.legsConnections(legId)
      val possibleTracks = verticalByConnection(conn).map(verticalByIndex)
      val finalTrackIndex = possibleTracks.find(_.freeSpace > 1) match {
        case Some(possible) =>
          verticalByIndex.update(possible.index, possible.copy(freeSpace = possible.freeSpace - 1))
          possible.index
        case None =>
          val newTrackIndex = TrackIndex(horizontal = false, index = verticalByIndex.keys.toSeq.length)
          val newTrack = Vertical(
            upper = true,
            index = newTrackIndex,
            diagramConnection = conn
          )
          verticalByIndex += newTrackIndex -> newTrack
          verticalByConnection += conn -> (verticalByConnection(conn) :+ newTrackIndex)
          newTrackIndex
      }
      legsMap += (legId -> finalTrackIndex)
    }
    (Map(legsMap.toSeq: _*), verticalByIndex.values.toSeq)
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
    def connect2TracksWithCables(comps: Seq[Component], legsMap: Map[LegId, TrackIndex],
                                connection: Connection, tracksGroup: Seq[Vertical]): (Seq[Component], Map[LegId, TrackIndex]) = {
      val (cable, cableLegs) = addCable(connection)(tracksGroup.head, tracksGroup(1))
      (comps :+ cable, legsMap ++ cableLegs.toMap)
    }
    val connectionTracks = vertical.filter(_.diagramConnection.id.isLeft).groupBy(_.diagramConnection)
    connectionTracks.toSeq.foldLeft((Seq[Component](), Map[LegId, TrackIndex]())) {
      case ((comps, legsMap), (connection, tracksGroup)) =>
        val sortedGroup = tracksGroup.sortBy(_.index.index)
        sortedGroup.length match {
          case 1 => (comps, legsMap)
          case 2 => connect2TracksWithCables(comps, legsMap, connection, sortedGroup)
          case _ => throw new TrackGroupsOfLengthLargerThanTwoAreNotSupported
        }
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
}

