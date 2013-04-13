package playground.poker

trait Environment {
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
  def apply(): Environment = new EnvironmentImpl(Map(), List())

  private class EnvironmentImpl(val hands: Map[String, Hand], val openCards: List[Card]) extends Environment {
    def +(hand: (String, Hand)): Environment = new EnvironmentImpl(hands + hand, openCards)
    def +(card: Card): Environment = new EnvironmentImpl(hands, card :: openCards)
  }
}

