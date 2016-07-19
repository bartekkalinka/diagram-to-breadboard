package pl.bka

import pl.bka.model.Breadboard

object Main {
  def main(args: Array[String]): Unit = {
    println(s"avtDog: ${Diagrams.avtDog}")
    Diagrams.avtDog match {
      case Right(diagram) =>
        val avtDogBoard = Breadboard.fromDiagram(diagram)
        println(s"avtDogBoard: $avtDogBoard")
      case _ => ()
    }
  }
}