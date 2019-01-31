package pl.bka

import pl.bka.model.Diagram
import pl.bka.model.breadboard.Breadboard
import pl.bka.PrettyPrint._

object Main {
  def main(args: Array[String]): Unit = {
    println(s"example: ${Diagram.prettyPrint(Diagrams.roll3)}")
    Diagrams.roll3 match {
      case Right(diagram) =>
        val exampleBoard = Breadboard(diagram)
        println("--------- example board ----------")
        exampleBoard.prettyPrint
      case _ => ()
    }
  }
}