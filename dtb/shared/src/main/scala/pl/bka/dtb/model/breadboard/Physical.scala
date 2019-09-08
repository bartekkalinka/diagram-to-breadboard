package pl.bka.dtb.model.breadboard

import pl.bka.dtb.model._
import pl.bka.dtb.PrettyPrint._

case class TrackPosition(position: Int)
case class Hole(trackIndex: TrackIndex, holeIndex: TrackPosition)
case class Physical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, Hole]) extends Container {
  def sortedConnections: Seq[(LegId, Hole)] = connections.toSeq.groupBy(_._1.cName).toSeq.flatMap {
    case (_, legHolePairs) => legHolePairs.sortBy(_._1.leg.name)
  }

  //this is for testing purposes
  //so it doesn't use Vertical.diagramConnection attribute on purpose
  def toDiagram: Diagram = {
    val compsByName = this.componentsByName
    val cableConnections: Seq[(ComponentName, TrackIndex)] = this.connections.toSeq
      .filter { case (legId, _) => compsByName(legId.cName).cType.isInstanceOf[Cable] }
      .map {case (legId, Hole(trackIndex, _)) => (legId.cName, trackIndex)}
    println("---- cable connections ----")
    cableConnections.prettyPrint
    val rawTrackConns: Seq[(TrackIndex, TrackIndex)] =
      cableConnections.groupBy(_._1).values.map { legs => (legs.head._2, legs(1)._2) }.toSeq
    println("----- raw track connections -----")
    rawTrackConns.prettyPrint
    val rawTrackConnsBothWays = rawTrackConns ++ rawTrackConns.map { case (a, b) => (b, a) }
    println("----- raw track connections both ways -----")
    rawTrackConnsBothWays.sortBy(_._1.index).prettyPrint
    val trackConns: Map[TrackIndex, Seq[TrackIndex]] = rawTrackConnsBothWays.groupBy(_._1).mapValues(_.map(_._2))
    println("----- track connections seqs -----")
    trackConns.prettyPrint
    def pullConnectionTrackGroup(index: TrackIndex): Seq[TrackIndex] = {
      val visited = scala.collection.mutable.Map.empty[TrackIndex, Unit]
      def pull(index: TrackIndex): Seq[TrackIndex] = {
        visited.put(index, ())
        index +: trackConns.get(index).map(children => children.filter(child => visited.get(child).isEmpty).flatMap(pull)).getOrElse(Seq[TrackIndex]())
      }
      pull(index)
    }
    def groupTracksByConnection(toTraverse: Seq[TrackIndex], acc: Seq[Seq[TrackIndex]]): Seq[Seq[TrackIndex]] =
      if(toTraverse.nonEmpty) {
        val conn = pullConnectionTrackGroup(toTraverse.head)
        groupTracksByConnection(toTraverse.diff(conn), acc :+ conn)
      }
      else acc
    val connectionByTrack: Map[TrackIndex, Connection] =
      groupTracksByConnection(this.tracks.map(_.trackIndex), Seq[Seq[TrackIndex]]())
        .zipWithIndex.flatMap { case (trks, i) => trks.map((_, Connection(Left(i)))) }.toMap
    println("----- connection by track -----")
    connectionByTrack.prettyPrint
    val legsConnections: Map[LegId, Connection] = this.connections.toSeq
      .filterNot { case (legId, _) => compsByName(legId.cName).cType.isInstanceOf[Cable] }
      .map { case (legId, Hole(trackIndex, _)) => (legId, connectionByTrack(trackIndex)) }.toMap
    Diagram(this.noCables, legsConnections)
  }

  def tracksWithFillIns: Seq[Track] = {
    val vertical = tracks.filter(_.trackIndex.tpe == VerticalType)
    val verticalWithFillIns = Logical.fillInEmptyVertical(vertical)
    tracks.filter(_.trackIndex.tpe == HorizontalType) ++ verticalWithFillIns
  }

  def horizontalTrackLength(upper: Boolean) =
    Seq(tracksWithFillIns.count(t => t.upper == upper && t.trackIndex.tpe == VerticalType), Tracks.horizontalTrackLength).max
}

object Physical {
  private def insertComponent(logical: Logical)(cName: ComponentName, legsInsertions: Map[LegId, Hole],
                                                freePositions: Map[TrackIndex, Seq[TrackPosition]]): (Map[LegId, Hole], Map[TrackIndex, Seq[TrackPosition]]) = {
    val compLegs: Seq[LegId] = logical.componentsLegs(cName)
    val component = logical.componentsByName(cName)
    val compType = component.cType
    val minPositions: Seq[TrackPosition] = compLegs.map { legId =>
      val trackIndex = logical.connections(legId)
      freePositions(trackIndex).minBy(_.position)
    }
    val targetPositions: Seq[TrackPosition] = compType match {
      case _: Transistor =>
        Seq.tabulate(compLegs.length)(_ => TrackPosition(Tracks.verticalTrackLength - 1))
      case c: Cable if c.tpe == CableType.PowerCable =>
        val otherTrack = logical.connections(compLegs.head)
        Seq(minPositions.head, TrackPosition(otherTrack.verticalLocationIndex))
      case _: IC =>
        val halfLength = component.legs.length / 2
        (component.legs.take(halfLength).map((_, true)) ++ component.legs.drop(halfLength).map((_, false)))
          .map(_ => TrackPosition(Tracks.verticalTrackLength - 1))
      case _: Cable =>
        minPositions
      case _ =>
        val group3OrderIndex = logical.group3Order.get(cName).map(_.index).getOrElse(0)
        Seq.tabulate(compLegs.length)(_ => TrackPosition(Tracks.verticalTrackLength - group3OrderIndex - 1))
    }
    val newLegsInsertions: Map[LegId, Hole] =
      compLegs.zipWithIndex.map { case (legId, i) =>
        (legId, Hole(logical.connections(legId), targetPositions(i)))
      }.toMap
    val updatedLegsInsertions: Map[LegId, Hole] = legsInsertions ++ newLegsInsertions
    val updatedFreePositions: Map[TrackIndex, Seq[TrackPosition]] = newLegsInsertions.toSeq.foldLeft(freePositions) { case (fps, (legId, hole)) =>
      val track = logical.connections(legId)
      val newFps = fps(track).filterNot(_ == hole.holeIndex)
      fps.updated(track, newFps)
    }
    (updatedLegsInsertions, updatedFreePositions)
  }

  def apply(logical: Logical): Physical = {
    val tracks: Seq[Track] = logical.tracks
    val initialFreePositions: Map[TrackIndex, Seq[TrackPosition]] = tracks.map { track =>
      (track.trackIndex, List.tabulate(track.length)(TrackPosition))
    }.toMap
    val sortedComponents = logical.components.sortBy(_.cType.physicalInsertOrder)
    val (legsInsertions, _) = sortedComponents.map(_.name).foldLeft((Map[LegId, Hole](), initialFreePositions)) {
      case ((mLegHole, freePositions), cName) => insertComponent(logical)(cName, mLegHole, freePositions)
    }
    Physical(logical.components, tracks, legsInsertions)
  }
}

