package pl.bka.model.breadboard

import pl.bka.model._

case class Breadboard(logical: Logical,
                     physical: Physical
                     ) {
  def prettyPrint: Seq[String] = logical.prettyPrint ++ physical.prettyPrint
}

object Breadboard {
  def apply(diagram: Diagram): Breadboard = {
    val logical = Logical(diagram)
    Breadboard(logical, Physical(logical))
  }
}