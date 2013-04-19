package playground.poker

trait Environment {
  val deck: Deck
  val hands: Map[String, Hand]
  val openCards: List[Card]
  def +(hand: (String, Hand)): Environment
  def +(card: Card): Environment

  def myHand = hands("")
  def myFullHand = extend(hands(""))
  def extend(hand: Hand) = new Hand(hand.cards ::: openCards)
  def extHands() = hands.toList.map(t => (t._1, extend(t._2)))
}

object Environment {
  def apply(): Environment = new EnvironmentImpl(Deck.Full52, Map(), List())

  private class EnvironmentImpl(val deck: Deck, val hands: Map[String, Hand], val openCards: List[Card]) extends Environment {
    def +(hand: (String, Hand)): Environment = new EnvironmentImpl(deck - hand._2, hands + hand, openCards)
    def +(card: Card): Environment = new EnvironmentImpl(deck - card, hands, card :: openCards)
  }
}

