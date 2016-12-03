package pl.bka.model.breadboard

import org.scalatest.{FlatSpec, Matchers}
import pl.bka.model._
import pl.bka.model.Power._

class BreadboardSpec extends FlatSpec with Matchers {
  val example = Diagram(
    List(
      Component("Tr549B.1", Transistor("549B")),
      Component("Tr549B.2", Transistor("549B"))
    ),
    Map(("Tr549B.1", "0") -> Right(Plus), ("Tr549B.1", "1") -> Left(1), ("Tr549B.1", "2") -> Right(GND),
      ("Tr549B.2", "0") -> Right(Plus), ("Tr549B.2", "1") -> Left(1), ("Tr549B.2", "2") -> Right(GND))
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
    val connectionsIndices = diagram.connections.groupBy(identity).toSeq.map { case (k, v) => (k.initialTrackIndex, v.head) }.toMap
    board.logical.connections.foreach { case (legId, trackIndex) => connectionsLegs(connectionsIndices(trackIndex.index)) should contain (legId) }
  }

  it should "map each leg to some physical hole of logical track" in {
    val board = Breadboard.fromDiagram(diagram)
    board.physical.connections.foreach { case (legId, Hole(trackIndex, holeIndex)) =>
      board.logical.connections(legId) shouldBe trackIndex
    }
    board.physical.connections.keys.toSeq shouldBe diagram.legsConnections.keys.toSeq
  }

  it should "not map more than one leg into one hole" in {
    val board = Breadboard.fromDiagram(diagram)
    board.physical.connections.groupBy(_._2).toSeq.filter(_._2.toSeq.length > 1).toList shouldBe List()
  }

  it should "map each component leg to same hole level inside the track" in {
    val board = Breadboard.fromDiagram(diagram)
    board.physical.connections.toSeq.groupBy(_._1.cName).mapValues(_.map { case (_, Hole(_, holeIndex)) => holeIndex }.distinct).foreach {
      case (cName, holeIndices) => holeIndices.length shouldBe 1
    }
  }
}

