package pl.bka.model.breadboard.onelegpertrack
import pl.bka.model.{Component, Container, Diagram, LegId}
import pl.bka.model.breadboard.{Track, TrackIndex}

case class Logical(components: Seq[Component], tracks: Seq[Track], connections: Map[LegId, TrackIndex]) extends Container {
  def prettyPrint: Seq[String] = Seq(
    s"""   logical: tracks cnt: ${tracks.length} conns: ${connections.map { case (l, i) => l.prettyPrint + "-conn" + i.index }}"""
  )
}

object Logical {
  def apply(diagram: Diagram): Logical = ???
}
