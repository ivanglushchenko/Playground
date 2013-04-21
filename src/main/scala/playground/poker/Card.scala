package playground.poker

case class Card(val rank: Rank, val suit: Suit) {
  override def toString() = rank.toString + suit
}

object Card {
  def parse(str: String): Option[Card] = ReplInput parseCard str
}