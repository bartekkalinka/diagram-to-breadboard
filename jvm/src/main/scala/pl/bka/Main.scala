package pl.bka

import pl.bka.model.Diagram
import pl.bka.model.breadboard.Breadboard

object Main {
  def main(args: Array[String]): Unit = {
    println(s"avtDog: ${Diagram.prettyPrint(Diagrams.example)}")
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard.fromDiagram(diagram)
        println(s"exampleBoard: ${exampleBoard.prettyPrint}")
      case _ => ()
    }
  }
}