package pl.bka.dtb.model.breadboard

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}
import pl.bka.dtb.model.Power._
import pl.bka.dtb.model._

class BreadboardSpec extends PropSpec with TableDrivenPropertyChecks with Matchers {
  //transistors only
  val testInput1 = Diagram(
   """
     |t.Tr549B-1 plus 1 gnd
     |t.Tr549B-2 plus 1 gnd
   """.stripMargin
  )

  //transistors + resistors
  val testInput2 = Diagram(
    """
      |t.Tr549B-1 plus 0 gnd
      |t.Tr549B-2 plus 0 gnd
      |r.R220-1 gnd 0
      |r.R220-2 plus 0
      |r.R220-3 0 gnd
      |r.R220-4 gnd 0
      |r.R220-5 0 plus
      |r.R220-6 gnd 0
    """.stripMargin
  )

  //resistors with a connection outside of transistors reach
  val testInput3 = Diagram(
    """
      |t.Tr549B-1 plus 0 gnd
      |r.R220-1 gnd 1
    """.stripMargin
  )

  //simple diagram with IC
  val testInput4 = Diagram(
    """
      |t.549B-1 plus 0 gnd
      |t.549B-2 plus 0 gnd
      |r.R220-1 gnd 0
      |r.R220-2 plus 0
      |i.082-1 plus 1 1 gnd gnd 2 gnd plus
    """.stripMargin
  )

  //3-roll
  val testInput5 = Diagram(
    """
      |d.diode band.1 plus
      |r.R470K-1 1 3
      |r.R22K-1 1 2
      |r.R470K-2 1 7
      |r.R22K-2 1 6
      |r.R470K-3 1 5
      |r.R22K-3 1 4
      |t.Tr-1 2 3 gnd
      |t.Tr-2 6 7 gnd
      |t.Tr-3 4 5 gnd
      |bc.hairy-1 -7 +2
      |bc.hairy-2 -3 +4
      |bc.hairy-3 -5 +6
      |bc.cap-4 -gnd +1
    """.stripMargin
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

