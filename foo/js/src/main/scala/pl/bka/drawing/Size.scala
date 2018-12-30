package pl.bka.drawing

import pl.bka.model.breadboard.Tracks

class Size(zoomFactor: Double) {
  def zoom(pixels: Int): Int = (pixels * zoomFactor).round.toInt
  val tracksStep = zoom(40)
  val holeStep = tracksStep
  val verticalTrackLength = tracksStep * (Tracks.verticalTrackLength - 1)
  val horizontalTrackLength = tracksStep * (Tracks.horizontalTrackLength - 1)
  val upperHorizontalTracksVerticalOffset = tracksStep
  val tracksHorizontalOffset = 2 * tracksStep
  val upperVerticalTracksVerticalOffset = upperHorizontalTracksVerticalOffset + 2 * tracksStep
  val upperVerticalTrackEnd = upperVerticalTracksVerticalOffset + verticalTrackLength
  val bottomVerticalTracksVerticalOffset = upperVerticalTrackEnd + (1 + Tracks.verticalTrackLength) * tracksStep
  val bottomHorizontalTracksVerticalOffset = bottomVerticalTracksVerticalOffset + 2 * tracksStep
  val holeRadius = zoom(5)
  val transistorBodyRadius = zoom(pixels = 10)
  val transistorLegsSpread = zoom(3)
  val resistorLegsSpread = zoom(3)
  val resistorBodySize = (30, 10)
  val capacitorSize = (6, 25)
  val diodeBodySize = (35, 10)
  val fontSize = 8
  val font = s"${fontSize}px Arial"
  val trackIndexFontSize = 12
  val trackIndexFont = s"${trackIndexFontSize}px Arial"
  val icFontSize = 12
  val icFont = s"${icFontSize}px Arial"
  val shortCableArcRadiusFactor = 0.75
  val longCableArcRadiusFactor = 3
  val selectionDistance = zoom(20)
  val selectionMarkSize = zoom(20)
  val arrowLength = zoom(12)
  val arrowHeadWidth = zoom(5)
  val arrowHeadFontSize = 10
  val arrowHeadFont = s"${arrowHeadFontSize} Arial"
}

