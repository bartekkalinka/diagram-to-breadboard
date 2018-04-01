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
    val (extVertical, componentsLegs) =
      Seq(icsToTracks _, transistorsToTracks _, otherToTracks _)
        .foldLeft((Seq.empty[Vertical], Map.empty[LegId, TrackIndex])) { case ((vertical, legs), componentsToTracks) =>
          val (newVertical, newLegs) = componentsToTracks(diagram, vertical)
          (newVertical, legs ++ newLegs)
        }
    println(s"11111111111111 ${extVertical.toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
    val (regularCables, regularCablesLegs) = calcRegularConnectionCables(extVertical)
    val (horizontal, horizontalMap) = horizontalTracks
    val (powerCables, powerCablesLegs) = calcPowerCables(extVertical, horizontalMap)
    val map: Map[LegId, TrackIndex] = componentsLegs ++ regularCablesLegs ++ powerCablesLegs
    val extComponents = diagram.components ++ regularCables ++ powerCables
    Logical(extComponents, extVertical ++ horizontal, map)
  }

  private def icsToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex]) = {
    val ics = diagram.components.filter(_.cType.isInstanceOf[IC])
    var startingIndex = vertical.count(_.upper)
    val (allNewVertical, allLegs) = ics.map { ic =>
      val halfLength = ic.legs.length / 2
      val legsWithUpper = ic.legs.take(halfLength).map(l => (LegId(ic.name, l), true)).zipWithIndex ++
        ic.legs.drop(halfLength).map(l => (LegId(ic.name, l), false)).zipWithIndex
      val (newVertical, icsLegs) = legsWithUpper.map {
        case ((legId, upper), relativeIndex) =>
          val index = if(upper) relativeIndex + startingIndex else -Breadboard.maxVerticalTracks + relativeIndex
          (
            Vertical(upper = upper, TrackIndex(horizontal = false, index), diagram.legsConnections(legId), freeSpace = Tracks.verticalTrackLength - 1),
            (legId, TrackIndex(horizontal = false, index))
          )
      }.unzip
      startingIndex += halfLength
      (newVertical, icsLegs)
    }.reduce((vl1, vl2) => (vl1._1 ++ vl2._1, vl1._2 ++ vl2._2))
    println(s"222222222222222 ${(vertical ++ allNewVertical).toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
    (vertical ++ allNewVertical, allLegs.toMap)
  }

  private def transistorsToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex]) = {
    val transistors = diagram.components.filter(_.cType.isInstanceOf[Transistor])
    val legs: Seq[LegId] = transistors.flatMap { t =>
      t.legs.map { leg => LegId(t.name, leg) }
    }
    val startingIndex = vertical.count(_.upper)
    val (newVertical, transistorsLegs) = legs.zipWithIndex.map {
      case (legId, relativeIndex) =>
        val index = relativeIndex + startingIndex
        (
          Vertical(upper = true, TrackIndex(horizontal = false, index), diagram.legsConnections(legId), freeSpace = Tracks.verticalTrackLength - 1),
          (legId, TrackIndex(horizontal = false, index))
        )
    }.unzip
    println(s"3333333333333333 ${(vertical ++ newVertical).toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
    (vertical ++ newVertical, transistorsLegs.toMap)
  }

  private def otherToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex]) = {
    val other = diagram.components.filterNot(_.cType.isInstanceOf[Transistor])
    val legs = other.flatMap(c => c.legs.map(leg => LegId(c.name, leg)))
    var trackIndexByConnection = mutable.Map(vertical.groupBy(_.diagramConnection).mapValues(_.map(_.index)).toSeq: _*)
    def getTrackIndexByConnection(conn: Connection): Seq[TrackIndex] =
      trackIndexByConnection.getOrElse(conn, Seq.empty[TrackIndex])
    val verticalByIndex = mutable.Map(vertical.groupBy(_.index).mapValues(_.head).toSeq: _*)
    val legsMap = mutable.Map.empty[LegId, TrackIndex]
    legs.foreach { legId =>
      val conn = diagram.legsConnections(legId)
      val possibleTracks = getTrackIndexByConnection(conn).map(verticalByIndex)
      val finalTrackIndex = possibleTracks.find(_.freeSpace > 1) match {
        case Some(possible) =>
          verticalByIndex.update(possible.index, possible.copy(freeSpace = possible.freeSpace - 1))
          possible.index
        case None =>
          val newTrackIndex = TrackIndex(horizontal = false, index = verticalByIndex.values.toSeq.count(_.upper))
          val newTrack = Vertical(
            upper = true,
            index = newTrackIndex,
            diagramConnection = conn
          )
          verticalByIndex += newTrackIndex -> newTrack
          trackIndexByConnection += conn -> (getTrackIndexByConnection(conn) :+ newTrackIndex)
          newTrackIndex
      }
      legsMap += (legId -> finalTrackIndex)
    }
    println(s"4444444444444 ${verticalByIndex.values.toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
    (verticalByIndex.values.toSeq, Map(legsMap.toSeq: _*))
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

