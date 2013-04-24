package playground.poker.combinations

import playground.poker.cards._

case class TwoPairs(rank1: Rank, rank2: Rank) extends Combination {
  override def toString = "Two pairs " + rank1 + " and " + rank2

  val value = 2
  type CombinationType = TwoPairs
  def compareWithSameKind(c: TwoPairs): Int = compareRanks(List((rank1, c.rank1), (rank2, c.rank2)))
}

object TwoPairs extends CombinationFactory {
  override def toString = "Two pairs"

  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r1, 2), (r2, 2), _*) => Some(TwoPairs(r1, r2))
    case _ => None
  }
}