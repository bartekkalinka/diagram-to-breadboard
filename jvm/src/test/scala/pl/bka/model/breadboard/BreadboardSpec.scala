package pl.bka.model.breadboard

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers, PropSpec}
import pl.bka.model._
import pl.bka.model.Power._

class BreadboardSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  val testInput1 = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B"))
    ),
    Map(("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(1), ("Tr549B.1", "2") -> Right(GND),
      ("Tr549B.2", "0") -> Right(Plus), ("Tr549B.2", "1") -> Left(1), ("Tr549B.2", "2") -> Right(GND))
  )

  val testInputs = Table("input items", testInput1)

  def testDiagram(testInput: Either[Fail, Diagram]): Diagram = testInput match { case Right(d) => d; case _ => fail() }

  property("each diagram should be a valid diagram") {
    forAll(testInputs) { testInput =>
      val diagram = testInput match { case Right(d) => Some(d); case _ => None }
      diagram shouldBe defined
    }
  }

  //TODO all connections are preserved after mapping

  //TODO transistor legs order is maintained

  property("each leg is inserted into some physical hole of logical track") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard.fromDiagram(diagram)
      board.physical.connections.foreach { case (legId, Hole(trackIndex, holeIndex)) =>
        board.logical.connections(legId) shouldBe trackIndex
      }
      board.physical.connections.keys.toSeq shouldBe diagram.legsConnections.keys.toSeq
    }
  }

  property("one hole holds not more than one leg") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard.fromDiagram(diagram)
      board.physical.connections.groupBy(_._2).toSeq.filter(_._2.toSeq.length > 1).toList shouldBe List()
    }
  }

  property("each component leg should be on same hole level inside the track") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard.fromDiagram(diagram)
      board.physical.connections.toSeq.groupBy(_._1.cName).mapValues(_.map { case (_, Hole(_, holeIndex)) => holeIndex }.distinct).foreach {
        case (cName, holeIndices) => holeIndices.length shouldBe 1
      }
    }
  }
}

