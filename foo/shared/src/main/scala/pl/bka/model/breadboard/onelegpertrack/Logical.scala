package pl.bka.model.breadboard.onelegpertrack
import pl.bka.model.Power.PowerConnection
import pl.bka.model._
import pl.bka.model.breadboard._
import pl.bka.PrettyPrint._

import scala.collection.mutable

case class Logical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, TrackIndex]) extends Container {
  def prettyPrint: Seq[String] = Seq(
    s"""   logical: tracks cnt: ${tracks.length} conns: ${connections.map { case (l, i) => l.prettyPrint + "-conn" + i.index }}"""
  )
}

object Logical {
  val minSpaceBetweenLegs = 3

  def apply(diagram: Diagram): Logical = {
    val (extVertical, componentsLegs) =
      Seq(icsToTracks _, transistorsToTracks _, otherToTracks _)
        .foldLeft((Seq.empty[Vertical], Map.empty[LegId, TrackIndex])) { case ((vertical, legs), componentsToTracks) =>
          val (newVertical, newLegs) = componentsToTracks(diagram, vertical)
          (newVertical, legs ++ newLegs)
        }
    println(s"------------ tracks after components ------------ ${extVertical.toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
    println(s"------------ legs after components -------------")
    componentsLegs.prettyPrint
    val (regularCables, regularCablesLegs) = calcRegularConnectionCables(extVertical)
    val (horizontal, horizontalMap) = horizontalTracks
    val (powerCables, powerCablesLegs) = calcPowerCables(extVertical, horizontalMap)
    val (unionCables, unionCablesLegs) = powerUnionCables(horizontalMap)
    val map: Map[LegId, TrackIndex] = componentsLegs ++ regularCablesLegs ++ powerCablesLegs ++ unionCablesLegs
    val extComponents = diagram.components ++ regularCables ++ powerCables ++ unionCables
    Logical(extComponents, extVertical ++ horizontal, map)
  }

  //debug utility
  private def tracksLegsQuantities(legs: Map[LegId, TrackIndex]): Map[TrackIndex, Int] =
    legs.toSeq.map { case (l, t) => (t, l) }.groupBy(_._1).mapValues(_.length)

  private def icsToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex]) = {
    val ics = diagram.components.filter(_.cType.isInstanceOf[IC])
    var startingIndex = vertical.count(_.upper)
    val (allNewVertical, allLegs) = ics.map { ic =>
      val halfLength = ic.legs.length / 2
      val dividedLegs = ic.legs.take(halfLength).map(l => (LegId(ic.name, l), true)).zipWithIndex ++
        ic.legs.drop(halfLength).map(l => (LegId(ic.name, l), false)).zipWithIndex
      val (newVertical, icsLegs) = dividedLegs.map {
        case ((legId, upper), relativeIndex) =>
          val index = if(upper) relativeIndex + startingIndex else -Breadboard.maxVerticalTracks + relativeIndex
          (
            Vertical(TrackIndex(horizontal = false, index), diagram.legsConnections(legId), freeSpace = Tracks.verticalTrackLength - 1, freeSpaceForLegs = 0),
            (legId, TrackIndex(horizontal = false, index))
          )
      }.unzip
      startingIndex += halfLength
      (newVertical, icsLegs)
    }.reduceOption((vl1, vl2) => (vl1._1 ++ vl2._1, vl1._2 ++ vl2._2))
      .getOrElse((Seq.empty[Vertical], Seq.empty[(LegId, TrackIndex)]))
    println(s"------------ tracks after ICs ------------ ${(vertical ++ allNewVertical).toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
    println(s"------------ legs after ICs ------------- ")
    allLegs.toMap.prettyPrint
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
          Vertical(TrackIndex(horizontal = false, index), diagram.legsConnections(legId), freeSpace = Tracks.verticalTrackLength - 1, freeSpaceForLegs = 0),
          (legId, TrackIndex(horizontal = false, index))
        )
    }.unzip
    println(s"------------ tracks after transistors ------------ ${(vertical ++ newVertical).toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
    (vertical ++ newVertical, transistorsLegs.toMap)
  }

  private def otherToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex]) = {
    val other = diagram.components.filterNot(c => c.cType.isInstanceOf[Transistor] || c.cType.isInstanceOf[IC])
    val groupsBy3 = other.zipWithIndex.groupBy { case (comp, i) => i / 3 }.values.toSeq.map(_.map(_._1))
    var nextTrackIndex: Int = vertical.count(_.upper)
    var newTracks = mutable.ArrayBuffer.empty[Vertical]
    var newLegsMap = mutable.Map.empty[LegId, TrackIndex]
    groupsBy3.foreach { group =>
      def getLegId(component: Component, legIndex: Int): LegId = LegId(component.name, component.legs(legIndex))
      val legIds = Seq(
        getLegId(group.head, 0),
        getLegId(group(1), 0),
        getLegId(group(2), 0),
        getLegId(group.head, 1),
        getLegId(group(1), 1),
        getLegId(group(2), 1)
      )
      legIds.zipWithIndex.foreach { case (legId, i) =>
        val newTrackIndex = TrackIndex(false, nextTrackIndex + i)
        val newTrack =
          Vertical(newTrackIndex, diagram.legsConnections(legId))
        newTracks += newTrack.copy(freeSpace = newTrack.freeSpace - 1).copy(freeSpaceForLegs = newTrack.freeSpaceForLegs - 1)
        newLegsMap.put(legId, newTrackIndex)
      }
      nextTrackIndex += 6
    }
    (vertical ++ newTracks.toVector, newLegsMap.toMap)
  }

//
//  private def otherToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex]) = {
//    val other = diagram.components.filterNot(c => c.cType.isInstanceOf[Transistor] || c.cType.isInstanceOf[IC])
//    val legs = other.flatMap(c => c.legs.map(leg => LegId(c.name, leg))).sortBy(_.leg.name)
//    var trackIndexByConnection = mutable.Map(vertical.groupBy(_.diagramConnection).mapValues(_.map(_.index)).toSeq: _*)
//    def getTrackIndexByConnection(conn: Connection): Seq[TrackIndex] =
//      trackIndexByConnection.getOrElse(conn, Seq.empty[TrackIndex])
//    val verticalByIndex = mutable.Map(vertical.groupBy(_.index).mapValues(_.head).toSeq: _*)
//    val legsMap = mutable.Map.empty[LegId, TrackIndex]
//    legs.foreach { legId =>
//      val conn = diagram.legsConnections(legId)
//      val possibleTracks = getTrackIndexByConnection(conn).map(verticalByIndex)
//      var possibleTrackOpt: Option[Vertical] = None
//      while(possibleTrackOpt.isEmpty) {
//        possibleTrackOpt =
//          if (legId.leg.name == Leg.secondLeg) {
//            val firstLegTrackIndex = legsMap(LegId(legId.cName, Leg(Leg.firstLeg)))
//            possibleTracks.find(tr => tr.freeSpaceForLegs > 0 && tr.index.index >= firstLegTrackIndex.index + minSpaceBetweenLegs)
//          } else {
//            possibleTracks.find(_.freeSpaceForLegs > 0)
//          }
//        if(possibleTrackOpt.isEmpty) {
//          val newTrackIndex = TrackIndex(horizontal = false, index = verticalByIndex.values.toSeq.count(_.upper))
//          val newTrack = Vertical(
//            index = newTrackIndex,
//            diagramConnection = conn
//          )
//          verticalByIndex += newTrackIndex -> newTrack
//          trackIndexByConnection += conn -> (getTrackIndexByConnection(conn) :+ newTrackIndex)
//        }
//      }
//      val finalTrack = possibleTrackOpt.get
//      verticalByIndex.update(finalTrack.index, finalTrack.copy(freeSpace = finalTrack.freeSpace - 1).copy(freeSpaceForLegs = finalTrack.freeSpaceForLegs - 1))
//      legsMap += (legId -> finalTrack.index)
//    }
//    println(s"------------ tracks after other components ------------ ${verticalByIndex.values.toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
//    (verticalByIndex.values.toSeq, Map(legsMap.toSeq: _*))
//  }

  private def calcRegularConnectionCables(vertical: Seq[Vertical]): (Seq[Component], Map[LegId, TrackIndex]) = {
    def addCable(connection: Connection)(prev: Track, next: Track): (Component, Seq[(LegId, TrackIndex)]) = {
      val cName = s"cable-${connection.id.fold(identity, identity)}-${prev.index.index}-${next.index.index}"
      val cable = Component(cName, Cable("", CableType.ConnCable))
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
    def connectManyTracksWithCables(comps: Seq[Component], legsMap: Map[LegId, TrackIndex],
                                    connection: Connection, tracksGroup: Seq[Vertical]): (Seq[Component], Map[LegId, TrackIndex]) = {
      val modifiedTracks = mutable.ArrayBuffer[Vertical](tracksGroup: _*)
      val newCables = mutable.ArrayBuffer.empty[Component]
      val newLegsMap = mutable.Map.empty[LegId, TrackIndex]
      val connectedTracks = mutable.ArrayBuffer[Int](0)
      for(i <- 1 until tracksGroup.length) {
        val nextTrack = modifiedTracks(i)
        val sortedTracks = connectedTracks.map(j => (j, modifiedTracks(j))).sortBy(tr => (- tr._2.freeSpace, tr._1))
        sortedTracks.headOption.foreach {
          case (j, prevTrack) =>
            if(prevTrack.freeSpace <= 0) {
              throw new TooManyCables
            }
            val (cable, legs) = addCable(connection)(prevTrack, nextTrack)
            newCables += cable
            newLegsMap ++= legs
            modifiedTracks.update(j, prevTrack.copy(freeSpace = prevTrack.freeSpace - 1))
            modifiedTracks.update(i, nextTrack.copy(freeSpace = nextTrack.freeSpace - 1))
            connectedTracks += i
        }
      }
      (comps ++ newCables, legsMap ++ newLegsMap)
    }
    val connectionTracks = vertical.filter(_.diagramConnection.id.isLeft).groupBy(_.diagramConnection)
    connectionTracks.toSeq.foldLeft((Seq[Component](), Map[LegId, TrackIndex]())) {
      case ((comps, legsMap), (connection, tracksGroup)) =>
        val sortedGroup = tracksGroup.sortBy(_.index.index)
        sortedGroup.length match {
          case 1 => (comps, legsMap)
          case 2 => connect2TracksWithCables(comps, legsMap, connection, sortedGroup)
          case _ => connectManyTracksWithCables(comps, legsMap, connection, tracksGroup)
        }
    }
  }

  private def calcPowerCables(vertical: Seq[Vertical],
                              horizontalMap: Map[(Boolean, PowerConnection), Horizontal]): (Seq[Component], Map[LegId, TrackIndex]) = {
    val powerConnectionTracks = vertical.filter(v => v.diagramConnection.id.isRight)
    val (cables, legs) = powerConnectionTracks.map { track =>
      val cName = s"cable-${track.diagramConnection.id.fold(identity, identity)}-${track.index.index}"
      val cable = Component(cName, Cable("", CableType.PowerCable))
      val Right(power) = track.diagramConnection.id
      val legs = Seq(
        (LegId(ComponentName(cName), cable.legs.head), track.index),
        (LegId(ComponentName(cName), cable.legs(1)), horizontalMap((track.upper, power)).index)
      )
      (cable, legs)
    }.unzip
    (cables, legs.flatten.toMap)
  }

  private def powerUnionCables(horizontalMap: Map[(Boolean, PowerConnection), Horizontal]): (Seq[Component], Map[LegId, TrackIndex]) = {
    val (comps, legs) = Seq(
      {
        val cName = "cable-plus-union"
        val cable = Component(cName, Cable("", CableType.UnionCable))
        (cable, Map(LegId(ComponentName(cName), cable.legs.head) -> horizontalMap((true, Power.Plus)).index, LegId(ComponentName(cName), cable.legs(1)) -> horizontalMap((false, Power.Plus)).index))
      },
      {
        val cName = "cable-gnd-union"
        val cable = Component(cName, Cable("", CableType.UnionCable))
        (cable, Map(LegId(ComponentName(cName), cable.legs.head) -> horizontalMap((true, Power.GND)).index, LegId(ComponentName(cName), cable.legs(1)) -> horizontalMap((false, Power.GND)).index))
      }
    ).unzip
    (comps, legs.reduce(_ ++ _))
  }

  private def horizontalTracks: (Seq[Horizontal], Map[(Boolean, PowerConnection), Horizontal]) = {
    val horizontal = Seq(
      Horizontal(left = true, index = TrackIndex(horizontal = true, 0), power = Power.Plus),
      Horizontal(left = true, index = TrackIndex(horizontal = true, 1), power = Power.GND),
      Horizontal(left = true, index = TrackIndex(horizontal = true, -2), power = Power.Plus),
      Horizontal(left = true, index = TrackIndex(horizontal = true, -1), power = Power.GND)
    )
    val horizontalMap: Map[(Boolean, PowerConnection), Horizontal] = Map(
      (true ,Power.Plus) -> horizontal.head,
      (true, Power.GND) -> horizontal(1),
      (false ,Power.Plus) -> horizontal(2),
      (false, Power.GND) -> horizontal(3)
    )
    (horizontal, horizontalMap)
  }
}
