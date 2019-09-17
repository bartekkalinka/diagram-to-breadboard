package pl.bka.dtb

import pl.bka.dtb.model.Power.{GND, Plus}
import pl.bka.dtb.model._

object Diagrams {
  val roll3 = Diagram(
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

  val doubleICs = Diagram(
    """
      |i.082-1 0 1 2 3 0 1 2 3
      |i.082-2 0 1 2 3 0 1 2 3
    """.stripMargin
  )


  //TODO check again
  val avDog = Diagram(
    """
      |d.diode band.11 plus
      |t.t-plus-1 2 3 4
      |t.t-plus-2 4 9 11
      |t.t-plus-3 9 9 11
      |t.t-plus-4 6 5 4
      |t.t-minus-1 9 10 14
      |t.t-minus-2 25 26 gnd
      |i.082 11 13 14 15 7 8 16 gnd
      |i.084 27 24 22 gnd 17 18 18 12 23 18 11 18 20 19
      |p.pot 30 22 18
      |n.node-1 29
      |n.output 1
      |bc.hairy -25 +28
      |c.poly 8 gnd
      |bc.cap-1 +11 -gnd
      |bc.cap-2 +17 -gnd
      |bc.cap-3 +19 -20
      |bc.cap-4 +12 -23
      |r.r-z 1 2
      |r.r-y 7 8
      |r.r-x-1 12 20
      |r.r-x-2 23 27
      |r.r-0-1 12 15
      |r.r-0-2 3 18
      |r.r-0-3 19 21
      |r.r-4-7k 14 18
      |r.r-10k-1 2 gnd
      |r.r-10k-2 6 gnd
      |r.r-10k-3 3 5
      |r.r-10k-4 10 13
      |r.r-10k-5 26 29
      |r.r-22k-1 11 17
      |r.r-22k-2 11 25
      |r.r-22k-3 17 gnd
      |r.r-22k-4 12 30
      |r.r-100k-1 5 7
      |r.r-100k-2 11 14
      |r.r-100k-3 21 24
      |r.r-100k-4 24 27
      |r.r-100k-5 24 28
      |r.r-220k-1 7 16
      |r.r-220k-2 16 18
    """.stripMargin
  )
}
