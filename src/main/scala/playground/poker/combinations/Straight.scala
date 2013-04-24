package playground.poker.combinations

import playground.poker.cards._

case class Straight(rank: Rank) extends Combination {
  override def toString = "Straight " + rank

  val value = 4
  type CombinationType = Straight
  def compareWithSameKind(c: Straight): Int = rank.compare(c.rank)
}

object Straight extends CombinationFactory {
  override def toString = "Straight"

  def get(hand: Hand): Option[Combination] = hand.straight match {
    case Some(r) => Some(Straight(r._1))
    case None => None
  }
}