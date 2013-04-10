package playground.poker

case class Card(rank: Rank, suit: Suit) {
  override def toString() = rank.toString + suit
}

object Card {
  def unapply(str: String): Option[Card] = (str(0).toString, str(1).toString) match {
    case (Rank(r), Suit(s)) => Some(Card(r, s))
    case _ => None
  }
}
