package playground.poker.cards

abstract sealed class Suit {
  val value: Int
}
case object Hearts extends Suit {
  override def toString() = "H"
  val value = 0
}
case object Diamonds extends Suit {
  override def toString() = "D"
  val value = 1
}
case object Spades extends Suit {
  override def toString() = "S"
  val value = 2
}
case object Clubs extends Suit {
  override def toString() = "C"
  val value = 3
}

object Suit {
  def allExcept(suit: Suit): List[Suit] = List(Hearts, Diamonds, Spades, Clubs) filterNot (_ == suit)

  val All: Array[Suit] = Array(Hearts, Diamonds, Spades, Clubs)
}
