package playground.poker

case class Deck(val cards: List[Card]) {
  def -(card: Card) = Deck(cards.filterNot(_ == card))
  def -(hand: Hand) = Deck(cards.filterNot(c => hand.cards.contains(c)))
}

object Deck {
  val Full52 = Deck((for { r <- Rank.All; s <- Suit.All } yield Card(r, s)).toList)
}
