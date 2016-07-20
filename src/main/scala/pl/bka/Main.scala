package pl.bka

import scala.scalajs.js.JSApp
import pl.bka.model.{Diagram, Breadboard}

object Main extends JSApp {
  def main(): Unit = {
    println(s"avtDog: ${Diagram.prettyPrint(Diagrams.example)}")
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard.fromDiagram(diagram)
        println(s"exampleBoard: ${exampleBoard.prettyPrint}")
      case _ => ()
    }
  }
}