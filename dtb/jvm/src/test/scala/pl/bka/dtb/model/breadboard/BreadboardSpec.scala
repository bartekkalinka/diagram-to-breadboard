package pl.bka.dtb.model.breadboard

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import pl.bka.dtb.model.Power._
import pl.bka.dtb.model._

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
      ("R220-1", Leg.firstLeg) -> Right(GND), ("R220-1", Leg.secondLeg) -> Left(0),
      ("R220-2", Leg.firstLeg) -> Right(Plus), ("R220-2", Leg.secondLeg) -> Left(0),
      ("R220-3", Leg.firstLeg) -> Left(0), ("R220-3", Leg.secondLeg) -> Right(GND),
      ("R220-4", Leg.firstLeg) -> Right(GND), ("R220-4", Leg.secondLeg) -> Left(0),
      ("R220-5", Leg.firstLeg) -> Left(0), ("R220-5", Leg.secondLeg) -> Right(Plus),
      ("R220-6", Leg.firstLeg) -> Right(GND), ("R220-6", Leg.secondLeg) -> Left(0))
  )

  //resistors with a connection outside of transistors reach
  val testInput3 = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("R220-1", Resistor("220K"))
    ),
    Map(("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(0), ("Tr549B.1", "2") -> Right(GND),
      ("R220-1", Leg.firstLeg) -> Right(GND), ("R220-1", Leg.secondLeg) -> Left(1))
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
      ("R220-1", Leg.firstLeg) -> Right(GND), ("R220-1", Leg.secondLeg) -> Left(0),
      ("R220-2", Leg.firstLeg) -> Right(Plus), ("R220-2", Leg.secondLeg) -> Left(0),
      ("082-1", "0") -> Right(Plus), ("082-1", "1") -> Left(1),
      ("082-1", "2") -> Left(1), ("082-1", "3") -> Right(GND),
      ("082-1", "4") -> Right(GND), ("082-1", "5") -> Left(2),
      ("082-1", "6") -> Right(GND), ("082-1", "7") -> Right(Plus)
    )
  )

  //3-roll
  val testInput5 = Diagram(
    List(
      Component("diode", Diode("???")),
      Component("R470K-1", Resistor("470K")),
      Component("R22K-1", Resistor("22K")),
      Component("R470K-2", Resistor("470K")),
      Component("R22K-2", Resistor("22K")),
      Component("R470K-3", Resistor("470K")),
      Component("R22K-3", Resistor("22K")),
      Component("Tr-1", Transistor("minus")),
      Component("Tr-2", Transistor("minus")),
      Component("Tr-3", Transistor("minus")),
      Component("hairy-1", Capacitor(bipolar = true)),
      Component("hairy-2", Capacitor(bipolar = true)),
      Component("hairy-3", Capacitor(bipolar = true)),
      Component("cap-4", Capacitor(bipolar = true))
    ),
    Map(
      ("diode", Leg.cathode) -> Left(1), ("diode", Leg.anode) -> Right(Plus),
      ("R470K-1", Leg.firstLeg) -> Left(1), ("R470K-1", Leg.secondLeg) -> Left(3),
      ("R22K-1", Leg.firstLeg) -> Left(1), ("R22K-1", Leg.secondLeg) -> Left(2),
      ("R470K-2", Leg.firstLeg) -> Left(1), ("R470K-2", Leg.secondLeg) -> Left(7),
      ("R22K-2", Leg.firstLeg) -> Left(1), ("R22K-2", Leg.secondLeg) -> Left(6),
      ("R470K-3", Leg.firstLeg) -> Left(1), ("R470K-3", Leg.secondLeg) -> Left(5),
      ("R22K-3", Leg.firstLeg) -> Left(1), ("R22K-3", Leg.secondLeg) -> Left(4),
      ("Tr-1", "0") -> Left(2), ("Tr-1", "1") -> Left(3), ("Tr-1", "2") -> Right(GND),
      ("Tr-2", "0") -> Left(6), ("Tr-2", "1") -> Left(7), ("Tr-2", "2") -> Right(GND),
      ("Tr-3", "0") -> Left(4), ("Tr-3", "1") -> Left(5), ("Tr-3", "2") -> Right(GND),
      ("hairy-1", Leg.capMinus) -> Left(7), ("hairy-1", Leg.capPlus) -> Left(2),
      ("hairy-2", Leg.capMinus) -> Left(3), ("hairy-2", Leg.capPlus) -> Left(4),
      ("hairy-3", Leg.capMinus) -> Left(5), ("hairy-3", Leg.capPlus) -> Left(6),
      ("cap-4", Leg.capMinus) -> Right(GND), ("cap-4", Leg.capPlus) -> Left(1)
    )
  )

  val testInputs = Table("input items", testInput1, testInput2, testInput3, testInput4, testInput5)

  def testDiagram(testInput: Either[Fail, Diagram]): Diagram = testInput match { case Right(d) => d; case _ => fail() }

  property("each diagram should be a valid diagram") {
    forAll(testInputs) { testInput =>
      val diagram = testInput.toOption
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

