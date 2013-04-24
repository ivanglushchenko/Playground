package playground.poker.combinations

import playground.poker.cards._

case class OnePair(rank: Rank) extends Combination {
  override def toString = "Pair of " + rank

  val value = 1
  type CombinationType = OnePair
  def compareWithSameKind(c: OnePair): Int = rank.compare(c.rank)
}

object OnePair extends CombinationFactory {
  override def toString = "One pair"

  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 2), _*) => Some(OnePair(r))
    case _ => None
  }
}