package pl.bka

import pl.bka.model.{ComponentName, Connection, LegId}
import pl.bka.model.breadboard.TrackIndex

trait PrettyPrint[A] {
  def prettyPrint(a: A): Unit
}

object PrettyPrint {
  implicit val legsPrettyPrint = new PrettyPrint[Map[LegId, TrackIndex]] {
    override def prettyPrint(legs: Map[LegId, TrackIndex]): Unit = {
      legs.toSeq.sortBy(l => (l._1.cName.value, l._1.leg.name)).foreach { case (LegId(cName, leg), TrackIndex(horizontal, index)) =>
        println(s"  ${cName.value} ${leg.name} -> $horizontal $index")
      }
    }
  }

  implicit val cableConnectionsPrettyPrint = new PrettyPrint[Seq[(ComponentName, TrackIndex)]] {
    override def prettyPrint(cableConnections: Seq[(ComponentName, TrackIndex)]): Unit = {
      cableConnections.foreach { case (cname, trackIndex) => println(s"  ${cname.value} -> ${trackIndex.horizontal} ${trackIndex.index}")}
    }
  }

  implicit val trackConnectionsPrettyPrint = new PrettyPrint[Seq[(TrackIndex, TrackIndex)]] {
    override def prettyPrint(trackConnections: Seq[(TrackIndex, TrackIndex)]): Unit = {
      trackConnections.foreach { case (from, to) => println(s"  ${from.horizontal} ${from.index} -> ${to.horizontal} ${to.index}")}
    }
  }

  implicit val connectionByTrackPrettyPrint = new PrettyPrint[Map[TrackIndex, Connection]] {
    override def prettyPrint(connectionByTrack: Map[TrackIndex, Connection]): Unit = {
      connectionByTrack.toSeq.sortBy(tr => (tr._1.horizontal, tr._1.index)).foreach { case (TrackIndex(horizontal, index), Connection(id)) =>
        println(s"  $horizontal $index -> $id")}
    }
  }

  implicit val trackConnectionsSeqPrettyPrint = new PrettyPrint[Map[TrackIndex, Seq[TrackIndex]]] {
    override def prettyPrint(trackConnections: Map[TrackIndex, Seq[TrackIndex]]): Unit = {
      trackConnections.toSeq.sortBy(tr => (tr._1.horizontal, tr._1.index)).foreach { case (TrackIndex(horizontal, index), tracks) =>
        println(s"  $horizontal $index -> ${tracks.toList.map(tr => (tr.horizontal, tr.index))}")}
    }
  }



  implicit class PrettyPrintOps[A: PrettyPrint](a: A) {
    def prettyPrint = implicitly[PrettyPrint[A]].prettyPrint(a)
  }
}
