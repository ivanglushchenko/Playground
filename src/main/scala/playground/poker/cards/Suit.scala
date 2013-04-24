package playground.poker.cards

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
  def allExcept(suit: Suit): List[Suit] = List(Hearts, Diamonds, Spades, Clubs) filterNot (_ == suit)

  val All = Array(Hearts, Diamonds, Spades, Clubs)
}
