package pl.bka

import pl.bka.model.Breadboard

object Main {
  def main(args: Array[String]): Unit = {
    println(s"avtDog: ${Diagrams.example}")
    Diagrams.example match {
      case Right(diagram) =>
        val exampleBoard = Breadboard.fromDiagram(diagram)
        println(s"exampleBoard: $exampleBoard")
      case _ => ()
    }
  }
}