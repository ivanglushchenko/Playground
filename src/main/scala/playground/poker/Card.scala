package playground.poker

case class Card(val rank: Rank, val suit: Suit) {
  override def toString() = rank.toString + suit
}

object Card {
  def parse(str: String): Option[Card] = ReplInput parseCard str

  //  (str(str.size - 2).toString, str(str.size - 1).toString) match {
  //  case (Rank(r), Suit(s)) => Some(new Card(r, s))
  //  case _ => None
  //}
}