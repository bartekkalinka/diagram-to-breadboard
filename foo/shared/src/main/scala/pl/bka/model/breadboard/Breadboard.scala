package pl.bka.model.breadboard

import pl.bka.model._
import pl.bka.model.breadboard.onelegpertrack.Logical

case class Breadboard(logical: Logical, physical: Physical)

object Breadboard {
  val maxVerticalTracks = 62

  def apply(diagram: Diagram): Breadboard = {
    val logical = Logical(diagram)
    Breadboard(logical, Physical(logical))
  }
}