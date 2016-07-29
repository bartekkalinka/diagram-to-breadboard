package pl.bka.model.breadboard

case class TrackIndex(index: Int)

sealed trait Track {
  val upper: Boolean
  val index: TrackIndex
  val length: Int
}

case class Vertical(upper: Boolean, index: TrackIndex, length: Int = 5) extends Track

case class Horizontal(upper: Boolean, left: Boolean, index: TrackIndex, length: Int = 25)


