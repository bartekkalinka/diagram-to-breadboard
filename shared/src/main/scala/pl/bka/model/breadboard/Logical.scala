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

  private def calcRegularConnectionCables(vertical: Map[TrackIndex, Vertical]): (Seq[Component], Map[LegId, TrackIndex], Map[TrackIndex, Vertical]) = {
    def addCable(connection: Connection)(prev: Track, next: Track,
                                         currVertical: Map[TrackIndex, Vertical]): (Component, Seq[(LegId, TrackIndex)], Map[TrackIndex, Vertical]) = {
      val cName = s"cable-${connection.id.fold(identity, identity)}-${prev.index.index}-${next.index.index}"
      val cable = Component(cName, Cable(""))
      val legId1 = LegId(ComponentName(cName), cable.legs.head)
      val (trackIndex1, nextVertical) = addLegToTrackWithLimit(legId1, prev.index, currVertical)
      val legId2 = LegId(ComponentName(cName), cable.legs(1))
      val (trackIndex2, finalVertical) = addLegToTrackWithLimit(legId2, next.index, nextVertical)
      val legs = Seq(
          (legId1, trackIndex1),
          (legId2, trackIndex2)
        )
      (cable, legs, finalVertical)
    }
    def connectTracksWithCables(comps: Seq[Component], legsMap: Map[LegId, TrackIndex],
                                connection: Connection, tracksGroup: Seq[Vertical],
                                currVertical: Map[TrackIndex, Vertical]): (Seq[Component], Map[LegId, TrackIndex], Map[TrackIndex, Vertical]) = {
      val (verticl, cablezz) =
        tracksGroup.init.zip(tracksGroup.tail).foldLeft(
          (currVertical, Seq[(Component, Seq[(LegId, TrackIndex)])]())
        ) { case ((vert, cablez), (prev, next)) =>
          val (cable, legs, nextVert) = addCable(connection)(prev, next, vert)
          (nextVert, cablez :+ (cable, legs))
      }
      val (cables, cableLegs) = cablezz.unzip
      (comps ++ cables, legsMap ++ cableLegs.flatten.toMap, verticl)
    }
    val connectionTracks = vertical.values.toSeq.filter(_.diagramConnection.id.isLeft).groupBy(_.diagramConnection)
    connectionTracks.toSeq.foldLeft((Seq[Component](), Map[LegId, TrackIndex](), vertical)) {
      case ((comps, legsMap, currVertical), (connection, tracksGroup)) =>
        if(tracksGroup.length > 1) connectTracksWithCables(comps, legsMap, connection, tracksGroup, currVertical) else (comps, legsMap, currVertical)
    }
  }

  private def calcPowerCables(vertical: Map[TrackIndex, Vertical],
                              horizontalMap: Map[PowerConnection, Horizontal]): (Seq[Component], Map[LegId, TrackIndex], Map[TrackIndex, Vertical]) = {
    val powerConnectionTracks = vertical.values.toSeq.filter(v => v.diagramConnection.id.isRight)
    val (verticl, cablezz) = powerConnectionTracks.foldLeft(
      (vertical, Seq[(Component, Seq[(LegId, TrackIndex)])]())
    ) { case ((vert, cablez), track) =>
      val cName = s"cable-${track.diagramConnection.id.fold(identity, identity)}-${track.index.index}"
      val cable = Component(cName, Cable(""))
      val Right(power) = track.diagramConnection.id
      val legId1 = LegId(ComponentName(cName), cable.legs.head)
      val (trackIndex1, nextVert) = addLegToTrackWithLimit(legId1, track.index, vert)
      val legs = Seq(
        (legId1, trackIndex1),
        (LegId(ComponentName(cName), cable.legs(1)), horizontalMap(power).index)
      )
      (nextVert, cablez :+ (cable, legs))
    }
    val (cables, cableLegs) = cablezz.unzip
    (cables, cableLegs.flatten.toMap, verticl)
  }

  private def horizontalTracks: (Seq[Horizontal], Map[PowerConnection, Horizontal]) = {
    val horizontal = Seq(
      Horizontal(upper = true, left = true, index = TrackIndex(horizontal = true, 0), power = Power.Plus),
      Horizontal(upper = true, left = true, index = TrackIndex(horizontal = true, 1), power = Power.GND)
    )
    val horizontalMap: Map[PowerConnection, Horizontal] = Map(Power.Plus -> horizontal.head, Power.GND -> horizontal(1))
    (horizontal, horizontalMap)
  }

  private def addLegToTrackWithLimit(legId: LegId, trackIndex: TrackIndex, vertical: Map[TrackIndex, Vertical]): (TrackIndex, Map[TrackIndex, Vertical]) = {
    val track = vertical(trackIndex)
    if(track.freeSpace > 0) {
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

  private def verticalByIndex(vertical: Seq[Vertical]): Map[TrackIndex, Vertical] = vertical.groupBy(_.index).mapValues(_.head)

  private def otherToTracks(diagram: Diagram, verticals: Map[TrackIndex, Vertical]): (Map[LegId, TrackIndex], Map[TrackIndex, Vertical]) = {
    val other = diagram.components.filterNot(_.cType.isInstanceOf[Transistor])
    val otherLegs: Seq[(Component, Seq[LegId])] = other.map { t =>
      (t, t.legs.map { leg => LegId(t.name, leg) })
    }
    val verticalByConnection = verticals.values.toSeq.groupBy(_.diagramConnection)
    val (finalLegsMap, finalVerticals) = otherLegs.foldLeft((Seq[(LegId, TrackIndex)](), verticals)) { case ((legsMap, verticalsMap), (component, legs)) =>
      val (_, newLegs, newVerticalsMap) = legs.foldLeft((0, Seq[(LegId, TrackIndex)](), verticalsMap)) { case ((minTrackIndex, legsToTracks, innerVerticalsMap), legId) =>
        val conn = diagram.legsConnections(legId)
        val possibleTracks = verticalByConnection(conn)
        val track = possibleTracks.find(_.index.index >= minTrackIndex).getOrElse(throw new NoPossibilityOfMappingLegsConnectionsToConsecutiveTracks)
        val (trackIndex, newInnerVerticalsMap) = addLegToTrackWithLimit(legId, track.index, innerVerticalsMap)
        (track.index.index, legsToTracks :+ (legId, trackIndex), newInnerVerticalsMap)
      }
      (legsMap ++ newLegs, newVerticalsMap)
    }
    (finalLegsMap.toMap, finalVerticals)
  }

  def apply(diagram: Diagram): Logical = {
    val (vertical, transistorMap) = transistorsToTracks(diagram)
    val (regularCables, regularCablesLegs, vertical2) = calcRegularConnectionCables(verticalByIndex(vertical))
    val (horizontal, horizontalMap) = horizontalTracks
    val (powerCables, powerCablesLegs, vertical3) = calcPowerCables(vertical2, horizontalMap)
    val (otherLegs, vertical4) = otherToTracks(diagram, vertical3)
    val map: Map[LegId, TrackIndex] = transistorMap.toMap ++ regularCablesLegs ++ powerCablesLegs ++ otherLegs
    val extComponents = diagram.components ++ regularCables ++ powerCables
    Logical(extComponents, vertical4.values.toSeq ++ horizontal, map)
  }
}

