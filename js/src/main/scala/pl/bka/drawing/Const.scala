package pl.bka.drawing

import pl.bka.model.breadboard.Tracks

trait Const {
  val tracksStep = 30
  val holeStep = tracksStep
  val verticalTrackLength = tracksStep * (Tracks.verticalTrackLength - 1)
  val horizontalTrackLength = tracksStep * (Tracks.horizontalTrackLength - 1)
  val horizontalTracksVerticalOffset = tracksStep
  val tracksHorizontalOffset = 2 * tracksStep
  val verticalTracksVerticalOffset = horizontalTracksVerticalOffset + 2 * tracksStep
  val holeRadius = 5
  val transistorBodyRadius = 12
  val transistorLegsSpread = 3
  val resistorLegsSpread = 3
  val resistorBodySize = (20, 10)
  val fontSize = 8
  val font = s"${fontSize}px Arial"
  val cableArcRadiusFactor = 0.75
}

