package playground.poker.combinations

import playground.poker.cards._

case class FourOfKind(rank: Rank) extends Combination {
  override def toString = "Four " + rank

  val value = 7
  type CombinationType = FourOfKind
  def compareWithSameKind(c: FourOfKind): Int = rank.compare(c.rank)
}

object FourOfKind extends CombinationFactory {
  override def toString = "Four"

  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 4), _*) => Some(FourOfKind(r))
    case _ => None
  }
}