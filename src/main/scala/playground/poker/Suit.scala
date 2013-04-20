package playground.poker

abstract sealed class Suit
case object Hearts extends Suit {
  override def toString() = "H"
}
case object Diamonds extends Suit {
  override def toString() = "D"
}
case object Spades extends Suit {
  override def toString() = "S"
}
case object Clubs extends Suit {
  override def toString() = "C"
}

object Suit {
  def parse(s: String): Option[Suit] = s.toUpperCase match {
    case "H" => Some(Hearts)
    case "D" => Some(Diamonds)
    case "S" => Some(Spades)
    case "C" => Some(Clubs)
  }

  def allExcept(suit: Suit): List[Suit] = List(Hearts, Diamonds, Spades, Clubs).filterNot(_ == suit)

  val All = Array(Hearts, Diamonds, Spades, Clubs)
}
