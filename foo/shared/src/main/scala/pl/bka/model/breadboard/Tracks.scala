package pl.bka.model.breadboard

import pl.bka.model.Connection
import pl.bka.model.Power._

case class TrackIndex(horizontal: Boolean, index: Int) {
  def upper: Boolean = index >= 0
  def verticalLocationIndex: Int = if(upper) index else index + Breadboard.maxVerticalTracks
  def order: Int = (if(horizontal) 1000000 else 0) + index
  def label: String =
    if(upper) {
      "U" + "%02d".format(index)
    } else {
      "D" + "%02d".format(-index)
    }
}

sealed trait Track {
  val index: TrackIndex
  val length: Int
  def upper: Boolean = index.upper
}

case class Vertical(index: TrackIndex, diagramConnection: Connection,
                    length: Int = Tracks.verticalTrackLength, freeSpace: Int = Tracks.verticalTrackLength, freeSpaceForLegs: Int = Tracks.verticalTrackLegsNumber) extends Track

case class Horizontal(left: Boolean, index: TrackIndex,
                      power: PowerConnection, length: Int = Tracks.horizontalTrackLength) extends Track

object Tracks {
  val verticalTrackLength = 5
  val verticalTrackLegsNumber = 1
  val horizontalTrackLength = 25
}

