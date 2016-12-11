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

  def physicalToDiagram(physical: Physical): Diagram = {
    val compsByName = physical.componentsByName
    val cableConnections: Seq[(ComponentName, TrackIndex, String)] = physical.connections.toSeq
      .filter { case (legId, _) => compsByName(legId.cName).cType.isInstanceOf[Cable] }
      .map {case (legId, Hole(index, _)) => (legId.cName, index, legId.leg.name)}
    val rawTrackConns: Seq[(TrackIndex, TrackIndex)] = cableConnections.groupBy(_._1).values.map { legs =>
      val sortedLegs = legs.sortBy(_._3)
      (legs.head._2, legs(1)._2)
    }.toSeq
    val trackConns: Map[TrackIndex, Seq[TrackIndex]] = rawTrackConns.groupBy(_._1).mapValues(_.map(_._2))
    def pullConnection(index: TrackIndex): Seq[TrackIndex] =
      index +: trackConns.get(index).map(children => children.flatMap(pullConnection)).getOrElse(Seq[TrackIndex]())
    def connections(toTraverse: Seq[TrackIndex], acc: Seq[Seq[TrackIndex]]): Seq[Seq[TrackIndex]] =
      if(toTraverse.nonEmpty) {
        val conn = pullConnection(toTraverse.head)
        connections(toTraverse.diff(conn), acc :+ conn)
      }
      else acc
    val connectionByTrack: Map[TrackIndex, Connection] =
      connections(physical.tracks.map(_.index), Seq[Seq[TrackIndex]]())
        .zipWithIndex.flatMap { case (tracks, i) => tracks.map((_, Connection(Left(i)))) }.toMap
    val legsConnections: Map[LegId, Connection] = physical.connections.toSeq
      .filterNot { case (legId, _) => compsByName(legId.cName).cType.isInstanceOf[Cable] }
      .map { case (legId, Hole(trackIndex, _)) => (legId, connectionByTrack(trackIndex)) }.toMap
    Diagram(physical.noCables, legsConnections)
  }

  property("each diagram should be a valid diagram") {
    forAll(testInputs) { testInput =>
      val diagram = testInput match { case Right(d) => Some(d); case _ => None }
      diagram shouldBe defined
    }
  }

  //TODO all connections are preserved after mapping

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

  property("each component leg should be on same hole level inside the track") {
    forAll(testInputs) { testInput =>
      val diagram = testDiagram(testInput)
      val board = Breadboard(diagram)
      board.physical.connections.toSeq.groupBy(_._1.cName).mapValues(_.map { case (_, Hole(_, holeIndex)) => holeIndex }.distinct).foreach {
        case (cName, holeIndices) => holeIndices.length shouldBe 1
      }
    }
  }
}

