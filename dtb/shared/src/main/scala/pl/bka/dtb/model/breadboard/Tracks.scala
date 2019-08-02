package pl.bka.dtb.model.breadboard

import pl.bka.dtb.model.Connection
import pl.bka.dtb.model.Power._

sealed trait TrackType
case object Horizontal extends TrackType
case object Vertical extends TrackType
case object OutOfBoard extends TrackType

object TrackType {
  implicit val trackTypeOrdering: Ordering[TrackType] = implicitly[Ordering[Int]].on {
    case Horizontal => 0
    case Vertical => 1
    case OutOfBoard => 2
  }
}

case class TrackIndex(tpe: TrackType, /*horizontal: Boolean, */ index: Int) {
  def upper: Boolean = index >= 0
  def verticalLocationIndex: Int = if(upper) index else index + Breadboard.maxVerticalTracks
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

