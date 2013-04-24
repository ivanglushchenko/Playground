package playground.poker.combinations

import playground.poker.cards._

case class Flush(rank: Rank) extends Combination {
  override def toString = "Flush " + rank

  val value = 5
  type CombinationType = Flush
  def compareWithSameKind(c: Flush): Int = rank.compare(c.rank)
}

case object Flush extends CombinationFactory {
  override def toString = "Flush"

  def get(hand: Hand): Option[Combination] = hand.suits match {
    case List((s, i), _*) if i >= 5 => Some(Flush(hand.sortedCards.filter(_.suit == s).head.rank))
    case _ => None
  }
}