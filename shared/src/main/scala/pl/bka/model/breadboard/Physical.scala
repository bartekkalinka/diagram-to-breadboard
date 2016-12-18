package pl.bka.model.breadboard

import pl.bka.model._

case class TrackPosition(position: Int)
case class Hole(trackIndex: TrackIndex, holeIndex: TrackPosition)
case class Physical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, Hole]) extends Container {
  def sortedConnections: Seq[(LegId, Hole)] = connections.toSeq.groupBy(_._1.cName).toSeq.flatMap {
    case (_, legHolePairs) => legHolePairs.sortBy(_._1.leg.name)
  }
  def prettyPrint: Seq[String] = Seq(
    s"""   physical tracks: $tracks""",
    s"""   physical conns: ${sortedConnections.map { case (l, Hole(t, h)) => l.prettyPrint + "-track" + t.index + "/hole" + h.position }}"""
  )

  //this is for testing purposes
  //so it doesn't use Vertical.diagramConnection attribute on purpose
  def toDiagram: Diagram = {
    val compsByName = this.componentsByName
    val cableConnections: Seq[(ComponentName, TrackIndex, String)] = this.connections.toSeq
      .filter { case (legId, _) => compsByName(legId.cName).cType.isInstanceOf[Cable] }
      .map {case (legId, Hole(index, _)) => (legId.cName, index, legId.leg.name)}
    val rawTrackConns: Seq[(TrackIndex, TrackIndex)] = cableConnections.groupBy(_._1).values.map { legs =>
      val sortedLegs = legs.sortBy(_._3)
      (legs.head._2, legs(1)._2)
    }.toSeq
    val trackConns: Map[TrackIndex, Seq[TrackIndex]] = rawTrackConns.groupBy(_._1).mapValues(_.map(_._2))
    def pullConnection(index: TrackIndex): Seq[TrackIndex] =
      index +: trackConns.get(index).map(children => children.flatMap(pullConnection)).getOrElse(Seq[TrackIndex]())
    def connections(toTraverse: Seq[TrackIndex], acc: Seq[Seq[TrackIndex]]): Seq[Seq[TrackIndex]] =
      if(toTraverse.nonEmpty) {
        val conn = pullConnection(toTraverse.head)
        connections(toTraverse.diff(conn), acc :+ conn)
      }
      else acc
    val connectionByTrack: Map[TrackIndex, Connection] =
      connections(this.tracks.map(_.index), Seq[Seq[TrackIndex]]())
        .zipWithIndex.flatMap { case (tracks, i) => tracks.map((_, Connection(Left(i)))) }.toMap
    val legsConnections: Map[LegId, Connection] = this.connections.toSeq
      .filterNot { case (legId, _) => compsByName(legId.cName).cType.isInstanceOf[Cable] }
      .map { case (legId, Hole(trackIndex, _)) => (legId, connectionByTrack(trackIndex)) }.toMap
    Diagram(this.noCables, legsConnections)
  }
}

object Physical {
  def insertComponent(logical: Logical)(cName: ComponentName, allLegsInsertions: Map[LegId, Hole],
                                        freePositions: Map[TrackIndex, Seq[TrackPosition]]): (Map[LegId, Hole], Map[TrackIndex, Seq[TrackPosition]]) = {
    val compLegs: Seq[LegId] = logical.componentsLegs(cName)
    val minPositions: Seq[TrackPosition] = compLegs.map { legId =>
      val track = logical.connections(legId)
      freePositions(track).minBy(_.position)
    }
    val targetPosition: TrackPosition = minPositions.maxBy(_.position)
    val compLegsInsertions: Map[LegId, Hole] = compLegs.map { legId => (legId, Hole(logical.connections(legId), targetPosition)) }.toMap
    val newAllLegsInsertions: Map[LegId, Hole] = allLegsInsertions ++ compLegsInsertions
    val newFreePositions: Map[TrackIndex, Seq[TrackPosition]] = compLegsInsertions.toSeq.foldLeft(freePositions) { case (fps, (legId, hole)) =>
      val track = logical.connections(legId)
      val newFps = fps(track).filterNot(_ == hole.holeIndex)
      fps.updated(track, newFps)
    }
    (newAllLegsInsertions, newFreePositions)
  }

  def apply(logical: Logical): Physical = {
    val tracks: Seq[Track] = logical.tracks
    val initialFreePositions: Map[TrackIndex, Seq[TrackPosition]] = tracks.map { track =>
      (track.index, List.tabulate(track.length)(TrackPosition))
    }.toMap
    val sortedComponents = logical.components.sortBy(_.cType.physicalInsertOrder)
    val (legsInsertions, _) = sortedComponents.map(_.name).foldLeft((Map[LegId, Hole](), initialFreePositions)) {
      case ((mLegHole, freePositions), cName) => insertComponent(logical)(cName, mLegHole, freePositions)
    }
    Physical(logical.components, tracks, legsInsertions)
  }
}

