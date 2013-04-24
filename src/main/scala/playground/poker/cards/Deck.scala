package playground.poker.cards

case class Deck(val cards: List[Card]) {
  def -(card: Card) = Deck(cards filterNot (_ == card))
  def -(exc: List[Card]) = Deck(cards filterNot (c => exc.contains(c)))
}

object Deck {
  val Full52 = Deck((for { r <- Rank.All; s <- Suit.All } yield Card(r, s)).toList)
}
