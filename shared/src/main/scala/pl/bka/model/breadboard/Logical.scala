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
          Vertical(upper = true, TrackIndex(horizontal = false, index), diagram.legsConnections(legId), freeSpace = Tracks.verticalTrackLength - 1),
          (legId, TrackIndex(horizontal = false, index))
          )
    }.unzip
    (vertical, transistorMap)
  }

  private def addLegToTrackWithLimit(legId: LegId, trackIndex: TrackIndex, vertical: Map[TrackIndex, Vertical]): (TrackIndex, Map[TrackIndex, Vertical]) = {
    val track = vertical(trackIndex)
    if(track.freeSpace > 1) { //leaving one space for connection cable
      (trackIndex, vertical + (trackIndex -> track.copy(freeSpace = track.freeSpace - 1)))
    } else {
      val newTrackIndex = TrackIndex(horizontal = false, index = vertical.keys.toSeq.length)
      val newTrack = Vertical(
        upper = true,
        index = newTrackIndex,
        diagramConnection = track.diagramConnection
      )
      (newTrackIndex, vertical + (newTrackIndex -> newTrack))
    }
  }

  private def otherToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Map[LegId, TrackIndex], Seq[Vertical]) = {
    val other = diagram.components.filterNot(_.cType.isInstanceOf[Transistor])
    val otherLegs: Seq[(Component, Seq[LegId])] = other.map { t =>
      (t, t.legs.map { leg => LegId(t.name, leg) })
    }
    val verticalByConnection = vertical.groupBy(_.diagramConnection)
    val verticalByIndex = vertical.groupBy(_.index).mapValues(_.head)
    val (finalLegsMap, finalVerticals) =
      otherLegs.foldLeft((Seq[(LegId, TrackIndex)](), verticalByIndex)) { case ((legsMap, verticalsMap), (_, legs)) =>
        val (_, newLegs, newVerticalsMap) =
        legs.foldLeft((0, Seq[(LegId, TrackIndex)](), verticalsMap)) { case ((minTrackIndex, legsToTracks, innerVerticalsMap), legId) =>
          val conn = diagram.legsConnections(legId)
          val possibleTracks = verticalByConnection(conn)
          val track = possibleTracks.find(_.index.index >= minTrackIndex).getOrElse(throw new NoPossibilityOfMappingLegsConnectionsToConsecutiveTracks)
          val (trackIndex, newInnerVerticalsMap) = addLegToTrackWithLimit(legId, track.index, innerVerticalsMap)
          (track.index.index, legsToTracks :+ (legId, trackIndex), newInnerVerticalsMap)
        }
        val sortedLegs = newLegs.map(_._1).zip(newLegs.map(_._2).sortBy(_.index))
        (legsMap ++ sortedLegs, newVerticalsMap)
      }
    (finalLegsMap.toMap, finalVerticals.values.toSeq)
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
}

