package playground.poker.combinations

import playground.poker.cards._

trait Combination extends Ordered[Combination] {
  val value: Int
  type CombinationType <: Combination
  def compareWithSameKind(c: CombinationType): Int

  def compare(that: Combination) =
    if (value == that.value) compareWithSameKind(that.asInstanceOf[CombinationType])
    else value.compare(that.value)

  def compareRanks(ranks: List[(Rank, Rank)]): Int = ranks match {
    case hd :: tl => hd._1.compare(hd._2) match {
      case 0 => compareRanks(tl)
      case i => i
    }
    case _ => 0
  }
}

object Combination {
  val factories = List(RoyalFlush, StraightFlush, FourOfKind, FullHouse, Flush, Straight, ThreeOfKind, TwoPairs, OnePair, HighCard)

  def best(hand: Hand) = {
    def loop(list: List[CombinationFactory]): Combination = list match {
      case hd :: tl => hd.get(hand) match {
        case Some(c) => c
        case None => loop(tl)
      }
      case Nil => throw new Exception
    }
    loop(factories)
  }
}

trait CombinationFactory {
  def get(hand: Hand): Option[Combination]
}