package pl.bka

import pl.bka.model.Diagram
import pl.bka.model.breadboard.Breadboard

object Main {
  def main(args: Array[String]): Unit = {
    println(s"avtDog: ${Diagram.prettyPrint(Diagrams.example)}")
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard(diagram)
        println(s"------------ example board ------------ ${exampleBoard.prettyPrint}")
      case _ => ()
    }
  }
}