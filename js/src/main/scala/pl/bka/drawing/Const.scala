package pl.bka.drawing

import pl.bka.drawing.BoardDrawing.upperVerticalTracksVerticalOffset
import pl.bka.model.breadboard.Tracks

trait Const {
  val tracksStep = 40
  val holeStep = tracksStep
  val verticalTrackLength = tracksStep * (Tracks.verticalTrackLength - 1)
  val horizontalTrackLength = tracksStep * (Tracks.horizontalTrackLength - 1)
  val upperHorizontalTracksVerticalOffset = tracksStep
  val tracksHorizontalOffset = 2 * tracksStep
  val upperVerticalTracksVerticalOffset = upperHorizontalTracksVerticalOffset + 2 * tracksStep
  val upperVerticalTrackEnd = upperVerticalTracksVerticalOffset + verticalTrackLength
  val bottomVerticalTracksVerticalOffset = upperVerticalTrackEnd + (1 + Tracks.verticalTrackLength) * tracksStep
  val bottomHorizontalTracksVerticalOffset = bottomVerticalTracksVerticalOffset + 2 * tracksStep
  val holeRadius = 5
  val transistorBodyRadius = 15
  val transistorLegsSpread = 3
  val resistorLegsSpread = 3
  val resistorBodySize = (30, 10)
  val fontSize = 8
  val font = s"${fontSize}px Arial"
  val shortCableArcRadiusFactor = 0.75
  val longCableArcRadiusFactor = 3
}

