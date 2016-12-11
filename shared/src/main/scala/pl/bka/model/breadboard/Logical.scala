package pl.bka.model.breadboard

import pl.bka.model._

case class Logical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, TrackIndex]) extends Container {
  def prettyPrint: Seq[String] = Seq(
    s"""   logical: tracks cnt: ${tracks.length} conns: ${connections.map { case (l, i) => l.prettyPrint + "-conn" + i.index }}"""
  )
}

object Logical {
  def apply(diagram: Diagram): Logical = {
    val transistors = diagram.components.filter(_.cType.isInstanceOf[Transistor])
    val transistorsLegs: Seq[LegId] = transistors.flatMap { t =>
      t.legs.map { leg => LegId(t.name, leg) }
    }
    val (tracks: Seq[Vertical], transistorMap: Seq[(LegId, TrackIndex)]) = transistorsLegs.zipWithIndex.map {
      case (legId, index) =>
        (
          Vertical(upper = true, TrackIndex(index), diagram.legsConnections(legId)),
          (legId, TrackIndex(index))
          )
    }.unzip
    def calcCables: (Seq[Component], Map[LegId, TrackIndex]) = {
      val connectionTracks = tracks.groupBy(_.diagramConnection)
      connectionTracks.toSeq.foldLeft((Seq[Component](), Map[LegId, TrackIndex]())) {
        case ((comps, legsMap), (connection, tracksGroup)) =>
          if(tracksGroup.length > 1) {
            val (cables, cableLegs) = tracksGroup.init.zip(tracksGroup.tail).map { case (prev, next) =>
              val cName = s"cable-${connection.id.fold(identity, identity)}-${prev.index.index}-${next.index.index}"
              val cable = Component(cName, Cable(""))
              val legs = Seq(
                (LegId(ComponentName(cName), cable.legs.head), prev.index),
                (LegId(ComponentName(cName), cable.legs(1)), next.index)
              )
              (cable, legs)
            }.unzip
            (comps ++ cables, legsMap ++ cableLegs.flatten.toMap)
          }
          else {
            (comps ++ Seq[Component](), legsMap ++ Map[LegId, TrackIndex]())
          }
      }
    }
    val (cables, cablesLegs) = calcCables
    val map: Map[LegId, TrackIndex] = transistorMap.toMap ++ cablesLegs
    val extComponents = diagram.components ++ cables
    Logical(extComponents, tracks, map)
  }
}

