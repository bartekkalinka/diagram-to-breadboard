package pl.bka.model.breadboard

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.model._

class BreadboardSpec extends FlatSpec with Matchers {
  val example = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("+9V", VoltageSource(9))
    ),
    Map(("Tr549B.1", "0") -> 0, ("Tr549B.1", "1") -> 1, ("Tr549B.1", "2") -> 2,
      ("+9V", "0") -> 0, ("+9V", "1") -> 1)
  )
  val diagram = example match { case Right(d) => d; case _ => fail()}

  "Breadboard" should "render correct tracks length" in {
    val board = Breadboard.fromDiagram(diagram)
    val connectionsNumber = 3
    board.logical.tracks.length shouldBe connectionsNumber
    board.logical.connections.values.toSeq.distinct.length shouldBe connectionsNumber
    board.physical.tracks.length shouldBe connectionsNumber
    board.physical.connections.values.toSeq.map(_.trackIndex).distinct.length shouldBe connectionsNumber
  }

  it should "map connections to logical tracks" in {
    val board = Breadboard.fromDiagram(diagram)
    val connectionsLegs = diagram.connectionsLegs
    board.logical.connections.foreach { case (legId, trackIndex) => connectionsLegs(Connection(trackIndex.index)) should contain (legId) }
  }

  it should "map each leg to some physical hole of logical track" in {
    val board = Breadboard.fromDiagram(diagram)
    board.physical.connections.foreach { case (legId, Hole(trackIndex, holeIndex)) =>
      board.logical.connections(legId) shouldBe trackIndex
    }
    board.physical.connections.keys.toSeq shouldBe diagram.legsConnections.keys.toSeq
  }

  it should "map each component leg to same hole level inside the track" in {
    val board = Breadboard.fromDiagram(diagram)
    board.physical.connections.toSeq.groupBy(_._1.cName).mapValues(_.map { case (_, Hole(_, holeIndex)) => holeIndex }.distinct).foreach {
      case (cName, holeIndices) => holeIndices.length shouldBe 1
    }
  }
}

