package pl.bka.dtb.model

sealed trait ComponentType {
  val legsCount: Int
  def physicalInsertOrder: Int = 0
  def initLegs: Seq[Leg] = List.tabulate(legsCount)(_.toString).map(Leg(_))
}
case class IC(symbol: String, legsCount: Int) extends ComponentType {
  override def physicalInsertOrder: Int = -1
}
case class Resistor(ohms: String, legsCount: Int = 2) extends ComponentType
case class Capacitor(capacitance: Double, bipolar: Boolean, legsCount: Int = 2) extends ComponentType
case class Diode(symbol: String, legsCount: Int = 2) extends ComponentType
case class Transistor(symbol: String, legsCount: Int = 3) extends ComponentType
case class Cable(symbol: String, tpe: CableType.CableType, legsCount: Int = 2) extends ComponentType {
  override def physicalInsertOrder: Int = -1
}
object CableType {
  sealed trait CableType
  case object ConnCable extends CableType
  case object PowerCable extends CableType
  case object UnionCable extends CableType
}


