package pl.bka.model

sealed trait BreadboardTrack {
  val length: Int
}
case class Vertical(upper: Boolean, index: Int, length: Int) extends BreadboardTrack
case class Horizontal(upper: Boolean, left: Boolean, index: Boolean, length: Int) extends BreadboardTrack

case class Hole(track: BreadboardTrack, index: Int)

case class Breadboard(
                     logical: Map[LegId, BreadboardTrack],
                     physical: Map[LegId, Hole]
                     )


