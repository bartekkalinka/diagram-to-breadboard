package pl.bka.model.breadboard

import pl.bka.model._

case class Breadboard(extDiagram: Diagram,
                     logical: Logical,
                     physical: Physical
                     ) {
  def prettyPrint: Seq[String] = logical.prettyPrint ++ physical.prettyPrint
}

object Breadboard {
  def apply(diagram: Diagram): Breadboard = {
    val (extDiagram, logical) = Logical(diagram)
    Breadboard(extDiagram, logical, Physical(extDiagram, logical))
  }
}