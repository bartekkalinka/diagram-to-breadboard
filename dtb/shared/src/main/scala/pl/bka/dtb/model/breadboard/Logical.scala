package pl.bka.dtb.model.breadboard

import pl.bka.dtb.model.Power.PowerConnection
import pl.bka.dtb.model._
import pl.bka.dtb.PrettyPrint._

import scala.collection.mutable

case class Group3Index(index: Int)

case class Logical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, TrackIndex], group3Order: Map[ComponentName, Group3Index]) extends Container

object Logical {
  val minSpaceBetweenLegs = 3

  def apply(diagram: Diagram): Logical = {
    val (vertical, componentsLegs, group3Order) =
      Seq(icsToTracks _, transistorsToTracks _, otherToTracks _)
        .foldLeft((Seq.empty[Vertical], Map.empty[LegId, TrackIndex], Map.empty[ComponentName, Group3Index])) { case ((currVertical, legs, currGroup3Order), componentsToTracks) =>
          val (newVertical, newLegs, newGroup3Order) = componentsToTracks(diagram, currVertical)
          (newVertical, legs ++ newLegs, currGroup3Order ++ newGroup3Order)
        }
    println(s"------------ tracks after components ------------ ${vertical.toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}") //TODO -> PrettyPrint
    println(s"------------ legs after components -------------")
    componentsLegs.prettyPrint
    val (regularCables, regularCablesLegs) = calcRegularConnectionCables(vertical)
    val (horizontal, horizontalMap) = horizontalTracks
    val (powerCables, powerCablesLegs) = calcPowerCables(vertical, horizontalMap)
    val (unionCables, unionCablesLegs) = powerUnionCables(horizontalMap)
    val map: Map[LegId, TrackIndex] = componentsLegs ++ regularCablesLegs ++ powerCablesLegs ++ unionCablesLegs
    val components = diagram.components ++ regularCables ++ powerCables ++ unionCables
    Logical(components, vertical ++ horizontal, map, group3Order)
  }

  def fillInEmptyVertical(vertical: Seq[Track]): Seq[Track] = {
    def fillOneSide(upper: Boolean): Seq[Track] = {
      val side = vertical.filter(_.upper == upper)
      val maxIndex = nextVerticalTrackIndex(side, upper) - 1
      val verticalByIndex = side.groupBy(_.index.index).mapValues(_.head)
      (Breadboard.sideStartIndex(upper) to maxIndex).map { i =>
        verticalByIndex.getOrElse(i, Vertical(TrackIndex(horizontal = false, i), Connection(Left(-1))))
      }
    }
    fillOneSide(true) ++ fillOneSide(false)
  }

  private def icsToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex], Map[ComponentName, Group3Index]) = {
    val ics = diagram.components.filter(_.cType.isInstanceOf[IC])
    var startingIndex = nextVerticalTrackIndex(vertical)
    val (allNewVertical, allLegs) = ics.map { ic =>
      val halfLength = ic.legs.length / 2
      val dividedLegs = ic.legs.take(halfLength).map(l => (LegId(ic.name, l), true)).zipWithIndex ++
        ic.legs.drop(halfLength).map(l => (LegId(ic.name, l), false)).zipWithIndex
      val (newVertical, icsLegs) = dividedLegs.map {
        case ((legId, upper), relativeIndex) =>
          //TODO better starting index for !upper
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
    (vertical ++ allNewVertical, allLegs.toMap, Map.empty[ComponentName, Group3Index])
  }

  private def nextVerticalTrackIndex(vertical: Seq[Track], upper: Boolean = true): Int =
    if(vertical.exists(_.upper == upper)) {
      vertical.filter(_.upper == upper).map(_.index.index).max + 1
    } else {
      Breadboard.sideStartIndex(upper)
    }

  private def transistorsToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex], Map[ComponentName, Group3Index]) = {
    val transistors = diagram.components.filter(_.cType.isInstanceOf[Transistor])
    val nextTrackIndices = mutable.Map(false -> nextVerticalTrackIndex(vertical, upper = false), true -> nextVerticalTrackIndex(vertical, true))
    var currSide = true
    val newTracks = mutable.ArrayBuffer.empty[Vertical]
    val newLegsMap = mutable.Map.empty[LegId, TrackIndex]
    transistors.foreach { transistor =>
      val legIds = transistor.legs.map { leg => LegId(transistor.name, leg) }
      legIds.zipWithIndex.foreach { case (legId, relativeIndex) =>
        val index = nextTrackIndices(currSide) + relativeIndex
        newTracks += Vertical(TrackIndex(horizontal = false, index), diagram.legsConnections(legId), freeSpace = Tracks.verticalTrackLength - 1, freeSpaceForLegs = 0)
        newLegsMap.put(legId, TrackIndex(horizontal = false, index))
      }
      nextTrackIndices.put(currSide, nextTrackIndices(currSide) + (legIds.length + 1))
      currSide = !currSide
    }
    println(s"------------ tracks after transistors ------------ ${(vertical ++ newTracks.toList).toList.map(v => (v.upper, v.index.index, v.diagramConnection.id))}")
    (vertical ++ newTracks.toVector, newLegsMap.toMap, Map.empty[ComponentName, Group3Index])
  }

  private def otherToTracks(diagram: Diagram, vertical: Seq[Vertical]): (Seq[Vertical], Map[LegId, TrackIndex], Map[ComponentName, Group3Index]) = {
    val other = diagram.components.filterNot(c => c.cType.isInstanceOf[Transistor] || c.cType.isInstanceOf[IC])
    val groupsBy3 = other.zipWithIndex.groupBy { case (_, i) => i / 3 }.values.toSeq.map(_.map(_._1))
    val nextTrackIndices = mutable.Map(
      false -> (nextVerticalTrackIndex(vertical, upper = false) + 1),
      true -> (nextVerticalTrackIndex(vertical, true) + 1))
    var currSide = true
    val newTracks = mutable.ArrayBuffer.empty[Vertical]
    val newLegsMap = mutable.Map.empty[LegId, TrackIndex]
    val newGroup3Order = mutable.Map.empty[ComponentName, Group3Index]
    groupsBy3.foreach { group =>
      def getLegId(component: Component, legIndex: Int): LegId = LegId(component.name, component.legs(legIndex))
      val (legIds, group3OrderSeq) = (for(j <- 0 to 1; i <- 0 to 2) yield group.lift(i).map((_, i, j)))
        .flatten
        .map { case (comp, compIndex, legIndex) => (getLegId(comp, legIndex), (comp.name, Group3Index(compIndex)))}
        .unzip
      legIds.zipWithIndex.foreach { case (legId, i) =>
        val newTrackIndex = TrackIndex(horizontal = false, nextTrackIndices(currSide) + i * 2)
        val newTrack =
          Vertical(newTrackIndex, diagram.legsConnections(legId))
        newTracks += newTrack.copy(freeSpace = newTrack.freeSpace - 1).copy(freeSpaceForLegs = newTrack.freeSpaceForLegs - 1)
        newLegsMap.put(legId, newTrackIndex)
      }
      nextTrackIndices.put(currSide, nextTrackIndices(currSide) + legIds.length * 2)
      newGroup3Order ++= group3OrderSeq
      currSide = !currSide
    }
    (vertical ++ newTracks.toVector, newLegsMap.toMap, newGroup3Order.toMap)
  }

  private def calcRegularConnectionCables(vertical: Seq[Vertical]): (Seq[Component], Map[LegId, TrackIndex]) = {
    def addCable(connection: Connection)(prev: Track, next: Track): (Component, Seq[(LegId, TrackIndex)]) = {
      val cName = s"cable-${connection.id.fold(identity, identity)}-${prev.index.index}-${next.index.index}"
      val cable = Component(cName, Cable(CableType.ConnCable))
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
      val cable = Component(cName, Cable(CableType.PowerCable))
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
        val cable = Component(cName, Cable(CableType.UnionCable))
        (cable, Map(LegId(ComponentName(cName), cable.legs.head) -> horizontalMap((true, Power.Plus)).index, LegId(ComponentName(cName), cable.legs(1)) -> horizontalMap((false, Power.Plus)).index))
      },
      {
        val cName = "cable-gnd-union"
        val cable = Component(cName, Cable(CableType.UnionCable))
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
