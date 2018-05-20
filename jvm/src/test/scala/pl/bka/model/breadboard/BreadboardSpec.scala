package pl.bka.model.breadboard

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers, PropSpec}
import pl.bka.model._
import pl.bka.model.Power._

class BreadboardSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  //transistors only
  val testInput1 = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B"))
    ),
    Map(("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(1), ("Tr549B.1", "2") -> Right(GND),
      ("Tr549B.2", "0") -> Right(Plus), ("Tr549B.2", "1") -> Left(1), ("Tr549B.2", "2") -> Right(GND))
  )

  //transistors + resistors
  val testInput2 = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B")),
      Component("R220-1", Resistor("220K")),
      Component("R220-2", Resistor("220K")),
      Component("R220-3", Resistor("220K")),
      Component("R220-4", Resistor("220K")),
      Component("R220-5", Resistor("220K")),
      Component("R220-6", Resistor("220K"))
    ),
    Map(("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(0), ("Tr549B.1", "2") -> Right(GND),
      ("Tr549B.2", "0") -> Right(Plus), ("Tr549B.2", "1") -> Left(0), ("Tr549B.2", "2") -> Right(GND),
      ("R220-1", "0") -> Right(GND), ("R220-1", "1") -> Left(0),
      ("R220-2", "0") -> Right(Plus), ("R220-2", "1") -> Left(0),
      ("R220-3", "0") -> Left(0), ("R220-3", "1") -> Right(GND),
      ("R220-4", "0") -> Right(GND), ("R220-4", "1") -> Left(0),
      ("R220-5", "0") -> Left(0), ("R220-5", "1") -> Right(Plus),
      ("R220-6", "0") -> Right(GND), ("R220-6", "1") -> Left(0))
  )

  //resistors with a connection outside of transistors reach
  val testInput3 = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("R220-1", Resistor("220K"))
    ),
    Map(("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(0), ("Tr549B.1", "2") -> Right(GND),
      ("R220-1", "0") -> Right(GND), ("R220-1", "1") -> Left(1))
  )

  //simple diagram with IC
  val testInput4 = Diagram(
    List(
      Component("549B-1", Transistor("549B")),
      Component("549B-2", Transistor("549B")),
      Component("R220-1", Resistor("220K")),
      Component("R220-2", Resistor("220K")),
      Component("082-1", IC("082", 8))
    ),
    Map(("549B-1", "0") -> Right(Plus), ("549B-1", "1") -> Left(0), ("549B-1", "2") -> Right(GND),
      ("549B-2", "0") -> Right(Plus), ("549B-2", "1") -> Left(0), ("549B-2", "2") -> Right(GND),
      ("R220-1", "0") -> Right(GND), ("R220-1", "1") -> Left(0),
      ("R220-2", "0") -> Right(Plus), ("R220-2", "1") -> Left(0),
      ("082-1", "0") -> Right(Plus), ("082-1", "1") -> Left(1),
      ("082-1", "2") -> Left(1), ("082-1", "3") -> Right(GND),
      ("082-1", "4") -> Right(GND), ("082-1", "5") -> Left(2),
      ("082-1", "6") -> Right(GND), ("082-1", "7") -> Right(Plus)
    )
  )

  val testInputs = Table("input items", testInput1, testInput2, testInput3, testInput4)

  def testDiagram(testInput: Either[Fail, Diagram]): Diagram = testInput match { case Right(d) => d; case _ => fail() }

  property("each diagram should be a valid diagram") {
    forAll(testInputs) { testInput =>
      val diagram = testInput match { case Right(d) => Some(d); case _ => None }
      diagram shouldBe defined
    }
  }

  property("physical breadboard layout preserves diagram connections") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard(diagram)
      board.physical.toDiagram.isEquivalentTo(diagram) shouldBe true
    }
  }

  property("transistor legs are placed into consecutive logical tracks") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard(diagram)
      val transistors = diagram.components.filter(_.cType.isInstanceOf[Transistor])
      val physicalConns = board.physical.connections.toSeq.groupBy(_._1.cName)
      transistors.foreach { transistor =>
        val transConns = physicalConns(transistor.name)
        val legsTracks = transConns.map { case (legId, hole) => (legId.leg.name.toInt, hole.trackIndex.index) }.sortBy(_._1)
        val (legs, indices) = legsTracks.map { case (legName, index) => (legName, index - legsTracks.head._2)}.unzip
        legs shouldBe indices
      }
    }
  }

  property("breadboard's extended diagram without cables should be original diagram") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard(diagram)
      diagram.components shouldBe board.physical.noCables
    }
  }

  property("each leg is inserted into some physical hole of its logical track") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard(diagram)
      board.physical.connections.foreach { case (legId, Hole(trackIndex, holeIndex)) =>
        board.logical.connections(legId) shouldBe trackIndex
      }
      board.physical.connections.keys.toSet shouldBe board.logical.componentsLegs.values.toSeq.flatten.toSet
    }
  }

  property("one hole holds not more than one leg") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard(diagram)
      board.physical.connections.groupBy(_._2).toSeq.filter(_._2.toSeq.length > 1).toList shouldBe List()
    }
  }

  property("each transistor's leg should be on same hole level inside the track") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard(diagram)
      board.physical.connections.toSeq.filter { conn =>
        board.logical.componentsByName(conn._1.cName).cType.isInstanceOf[Transistor]
      }.groupBy(_._1.cName).mapValues(_.map { case (_, Hole(_, holeIndex)) => holeIndex }.distinct).foreach {
        case (cName, holeIndices) => holeIndices.length shouldBe 1
      }
    }
  }
}

