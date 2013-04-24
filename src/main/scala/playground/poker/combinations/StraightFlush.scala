package playground.poker.combinations

import playground.poker.cards._

case class StraightFlush(rank: Rank) extends Combination {
  override def toString = "Straight flush " + rank

  val value = 8
  type CombinationType = StraightFlush
  def compareWithSameKind(c: StraightFlush): Int = rank.compare(c.rank)
}

object StraightFlush extends CombinationFactory {
  override def toString = "Straight flush"
  def get(hand: Hand): Option[Combination] = hand.straight match {
    case Some(s) if s._2 == true => Some(StraightFlush(s._1))
    case _ => None
  }
}