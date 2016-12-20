package pl.bka.model.breadboard

import pl.bka.model.Connection
import pl.bka.model.Power._

case class TrackIndex(horizontal: Boolean, index: Int)

sealed trait Track {
  val upper: Boolean
  val index: TrackIndex
  val length: Int
}

case class Vertical(upper: Boolean, index: TrackIndex, diagramConnection: Connection, length: Int = Tracks.verticalTrackLength) extends Track

case class Horizontal(upper: Boolean, left: Boolean, index: TrackIndex,
                      power: PowerConnection, length: Int = Tracks.horizontalTrackLength) extends Track

object Tracks {
  val verticalTrackLength = 5
  val horizontalTrackLength = 25
}

