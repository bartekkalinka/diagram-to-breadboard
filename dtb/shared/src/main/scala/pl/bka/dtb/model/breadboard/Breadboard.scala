package pl.bka.dtb.model.breadboard

import pl.bka.dtb.model._

case class Breadboard(logical: Logical, physical: Physical)

object Breadboard {
  val maxVerticalTracks = 5000
  def sideStartIndex(upper: Boolean): Int = if(upper) 0 else -Breadboard.maxVerticalTracks

  def apply(diagram: Diagram): Breadboard = {
    val logical = Logical(diagram)
    Breadboard(logical, Physical(logical))
  }
}