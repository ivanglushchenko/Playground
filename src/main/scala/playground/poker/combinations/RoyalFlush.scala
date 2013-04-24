package playground.poker.combinations

import playground.poker.cards._

case class RoyalFlush() extends Combination {
  override def toString = "Royal flush"

  val value = 9
  type CombinationType = RoyalFlush
  def compareWithSameKind(c: RoyalFlush): Int = 0
}

object RoyalFlush extends CombinationFactory {
  override def toString = "Royal flush"

  def get(hand: Hand): Option[Combination] = hand.straight match {
    case Some(s) if s._2 == true && s._1 == Ace => Some(RoyalFlush())
    case _ => None
  }
}