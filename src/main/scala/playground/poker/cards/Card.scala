package playground.poker.cards

case class Card(val rank: Rank, val suit: Suit) {
  override def toString() = rank.toString + suit
}