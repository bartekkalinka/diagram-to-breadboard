package pl.bka

import pl.bka.model.LegId
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

  implicit class PrettyPrintOps[A: PrettyPrint](a: A) {
    def prettyPrint = implicitly[PrettyPrint[A]].prettyPrint(a)
  }
}
