package pl.bka.dtb

import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

object Diagrams {
  val roll3 = Diagram(
    """
      |d.diode band.1 plus
      |r.R470K-1 1 3
      |r.22K-1 1 2
      |r.R470K-2 1 7
      |r.22K-2 1 6
      |r.R470K-3 1 5
      |r.22K-3 1 4
      |t.Tr-1 2 3 gnd
      |t.Tr-2 6 7 gnd
      |t.Tr-3 4 5 gnd
      |bc.hairy-1 -7 +2
      |bc.hairy-2 -3 +4
      |bc.hairy-3 -5 +6
      |bc.cap-4 -gnd +1
      |n.node-1 3
      |n.node-2 5
      |n.node-3 7
    """.stripMargin
  )

  val roll4 = Diagram(
    """
      |d.diode band.9 plus
      |r.R470K-1 9 4
      |r.R22K-1 9 7
      |r.R470K-2 9 2
      |r.R22K-2 9 6
      |r.R470K-3 9 3
      |r.R22K-3 9 8
      |r.R470K-4 9 1
      |r.R22K-4 9 5
      |t.Tr-1 5 1 gnd
      |t.Tr-2 8 3 gnd
      |t.Tr-3 6 2 gnd
      |t.Tr-4 7 4 gnd
      |bc.hairy-1 -1 +8
      |bc.hairy-2 -3 +6
      |bc.hairy-3 -2 +7
      |bc.hairy-4 -4 +5
      |bc.cap-5 -gnd +9
    """.stripMargin
  )
}
