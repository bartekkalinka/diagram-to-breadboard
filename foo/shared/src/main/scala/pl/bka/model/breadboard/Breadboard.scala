package pl.bka.model.breadboard

import pl.bka.model._
import pl.bka.model.breadboard.minspace.Logical

case class Breadboard(logical: Logical,
                     physical: Physical
                     ) {
  def prettyPrint: Seq[String] = logical.prettyPrint ++ physical.prettyPrint
}

object Breadboard {
  val maxVerticalTracks = 50

  def apply(diagram: Diagram): Breadboard = {
    val logical = Logical(diagram)
    Breadboard(logical, Physical(logical))
  }
}