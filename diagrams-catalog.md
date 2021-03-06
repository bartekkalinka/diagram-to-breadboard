# Converted diagrams catalog

## 3-roll

<img src=./static/3roll.png width="300" height="300">

    val roll3 = Diagram(
    List(
      Component("diode", Diode("1N914")),
      Component("R470K-1", Resistor("470K")),
      Component("R22K-1", Resistor("22K")),
      Component("R470K-2", Resistor("470K")),
      Component("R22K-2", Resistor("22K")),
      Component("R470K-3", Resistor("470K")),
      Component("R22K-3", Resistor("22K")),
      Component("Tr-1", Transistor("minus")),
      Component("Tr-2", Transistor("minus")),
      Component("Tr-3", Transistor("minus")),
      Component("hairy-1", Capacitor(0d, bipolar = true)),
      Component("hairy-2", Capacitor(0d, bipolar = true)),
      Component("hairy-3", Capacitor(0d, bipolar = true)),
      Component("cap-4", Capacitor(0d, bipolar = true))
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
    ))

<img src=./static/3roll_breadboard.png width="840" height="500">

## 4-roll

<img src=./static/4roll.png width="400" height="400">

    val roll4 = Diagram(
    List(
      Component("diode", Diode("1N914")),
      Component("R470K-1", Resistor("470K")),
      Component("R22K-1", Resistor("22K")),
      Component("R470K-2", Resistor("470K")),
      Component("R22K-2", Resistor("22K")),
      Component("R470K-3", Resistor("470K")),
      Component("R22K-3", Resistor("22K")),
      Component("R470K-4", Resistor("470K")),
      Component("R22K-4", Resistor("22K")),
      Component("Tr-1", Transistor("minus")),
      Component("Tr-2", Transistor("minus")),
      Component("Tr-3", Transistor("minus")),
      Component("Tr-4", Transistor("minus")),
      Component("hairy-1", Capacitor(0d, bipolar = true)),
      Component("hairy-2", Capacitor(0d, bipolar = true)),
      Component("hairy-3", Capacitor(0d, bipolar = true)),
      Component("hairy-4", Capacitor(0d, bipolar = true)),
      Component("cap-5", Capacitor(0d, bipolar = true))
    ),
    Map(
      ("diode", Leg.cathode) -> Left(9), ("diode", Leg.anode) -> Right(Plus),
      ("R470K-1", Leg.firstLeg) -> Left(9), ("R470K-1", Leg.secondLeg) -> Left(4),
      ("R22K-1", Leg.firstLeg) -> Left(9), ("R22K-1", Leg.secondLeg) -> Left(7),
      ("R470K-2", Leg.firstLeg) -> Left(9), ("R470K-2", Leg.secondLeg) -> Left(2),
      ("R22K-2", Leg.firstLeg) -> Left(9), ("R22K-2", Leg.secondLeg) -> Left(6),
      ("R470K-3", Leg.firstLeg) -> Left(9), ("R470K-3", Leg.secondLeg) -> Left(3),
      ("R22K-3", Leg.firstLeg) -> Left(9), ("R22K-3", Leg.secondLeg) -> Left(8),
      ("R470K-4", Leg.firstLeg) -> Left(9), ("R470K-4", Leg.secondLeg) -> Left(1),
      ("R22K-4", Leg.firstLeg) -> Left(9), ("R22K-4", Leg.secondLeg) -> Left(5),
      ("Tr-1", "0") -> Left(5), ("Tr-1", "1") -> Left(1), ("Tr-1", "2") -> Right(GND),
      ("Tr-2", "0") -> Left(8), ("Tr-2", "1") -> Left(3), ("Tr-2", "2") -> Right(GND),
      ("Tr-3", "0") -> Left(6), ("Tr-3", "1") -> Left(2), ("Tr-3", "2") -> Right(GND),
      ("Tr-4", "0") -> Left(7), ("Tr-4", "1") -> Left(4), ("Tr-4", "2") -> Right(GND),
      ("hairy-1", Leg.capMinus) -> Left(1), ("hairy-1", Leg.capPlus) -> Left(8),
      ("hairy-2", Leg.capMinus) -> Left(3), ("hairy-2", Leg.capPlus) -> Left(6),
      ("hairy-3", Leg.capMinus) -> Left(2), ("hairy-3", Leg.capPlus) -> Left(7),
      ("hairy-4", Leg.capMinus) -> Left(4), ("hairy-4", Leg.capPlus) -> Left(5),
      ("cap-5", Leg.capMinus) -> Right(GND), ("cap-5", Leg.capPlus) -> Left(9)
    ))

<img src=./static/4roll_breadboard.png width="1100" height="500">
