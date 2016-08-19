package pl.bka.model

sealed trait ComponentType {
  val legsCount: Int
  def initLegs: Seq[Leg] = List.tabulate(legsCount)(_.toString).map(Leg)
}
case class VoltageSource(voltage: Int, legsCount: Int = 2) extends ComponentType
case class IC(symbol: String, legsCount: Int) extends ComponentType
case class Resistor(ohms: Int, legsCount: Int = 2) extends ComponentType
case class Capacitor(capacitance: Int, electrolytic: Boolean, legsCount: Int = 2) extends ComponentType
case class Diode(symbol: String, legsCount: Int = 2) extends ComponentType
case class Transistor(symbol: String, legsCount: Int = 3) extends ComponentType

