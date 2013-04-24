package playground.poker

import playground.poker.cards._

trait Environment {
  val deck: Deck
  val hands: Map[String, Hand]
  val openCards: List[Card]
  def +(hand: (String, Hand)): Environment
  def +(cards: List[Card]): Environment

  def myHand = if (hands contains "") hands("") else Hand(List())
  def myFullHand = extend(myHand)
  def extend(hand: Hand) = new Hand(hand.cards ::: openCards)
  def extHands() = hands.toList.map(t => (t._1, extend(t._2)))
}

object Environment {
  def apply(): Environment = new EnvironmentImpl(Map(), List())

  private class EnvironmentImpl(val hands: Map[String, Hand], val openCards: List[Card]) extends Environment {
    val deck = Deck.Full52 - (hands.flatMap(_._2.cards).toList ::: openCards)
    def +(hand: (String, Hand)): Environment = new EnvironmentImpl(hands + hand, openCards)
    def +(cards: List[Card]): Environment = new EnvironmentImpl(hands, cards ::: openCards)
  }
}

