package playground.poker.combinations

import playground.poker.cards._

case class ThreeOfKind(rank: Rank) extends Combination {
  override def toString = "Three " + rank

  val value = 3
  type CombinationType = ThreeOfKind
  def compareWithSameKind(c: ThreeOfKind): Int = rank.compare(c.rank)
}

object ThreeOfKind extends CombinationFactory {
  override def toString = "Three"

  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 3), _*) => Some(ThreeOfKind(r))
    case _ => None
  }
}