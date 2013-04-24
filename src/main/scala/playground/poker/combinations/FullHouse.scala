package playground.poker.combinations

import playground.poker.cards._

case class FullHouse(rank1: Rank, rank2: Rank) extends Combination {
  override def toString = "Full house with " + rank1 + " and " + rank2

  val value = 6
  type CombinationType = FullHouse
  def compareWithSameKind(c: FullHouse): Int = compareRanks(List((rank1, c.rank1), (rank2, c.rank2)))
}

case object FullHouse extends CombinationFactory {
  override def toString = "Full house"

  def get(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r1, 3), (r2, i), _*) if i >= 2 => Some(FullHouse(r1, r2))
    case _ => None
  }
}