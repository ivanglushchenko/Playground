package playground.poker.combinations

import playground.poker.cards._

case class HighCard(rank: Rank) extends Combination {
  override def toString = "High " + rank

  val value = 0
  type CombinationType = HighCard
  def compareWithSameKind(c: HighCard): Int = rank.compare(c.rank)
}

object HighCard extends CombinationFactory {
  override def toString = "High card"

  def get(hand: Hand): Option[Combination] = Some(HighCard(hand.ranks.head._1))
}