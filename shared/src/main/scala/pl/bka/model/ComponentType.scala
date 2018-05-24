package pl.bka.model

sealed trait ComponentType {
  val legsCount: Int
  def physicalInsertOrder: Int = 0
  def initLegs: Seq[Leg] = List.tabulate(legsCount)(_.toString).map(Leg)
}
case class IC(symbol: String, legsCount: Int) extends ComponentType {
  override def physicalInsertOrder: Int = -1
}
case class Resistor(ohms: String, legsCount: Int = 2) extends ComponentType
case class Capacitor(capacitance: Int, bipolar: Boolean, legsCount: Int = 2) extends ComponentType
case class Diode(symbol: String, legsCount: Int = 2) extends ComponentType
case class Transistor(symbol: String, legsCount: Int = 3) extends ComponentType
case class Cable(symbol: String, legsCount: Int = 2) extends ComponentType {
  override def physicalInsertOrder: Int = -1
}


