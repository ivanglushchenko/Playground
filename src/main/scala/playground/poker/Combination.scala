package playground.poker

trait Combination extends Ordered[Combination] {
  val value: Int
  type CombinationType <: Combination
  def compareWithSameKind(c: CombinationType): Int

  def compare(that: Combination) =
    if (value == that.value) compareWithSameKind(that.asInstanceOf[CombinationType])
    else value.compare(that.value)
}

trait CombinationExtractor {
  def unapply(hand: Hand): Option[Combination]
}

object Combination {
  val extractors = List(/*RoyalFlush, StraightFlush, FourOfKind, FullHouse, Flush, Straight, ThreeOfKind, TwoPairs, */OnePair)

  def best(hand: Hand) = {
    def loop(list: List[CombinationExtractor]): Option[Combination] = list match {
      case hd :: tl => hd.unapply(hand) match {
        case Some(c) => Some(c)
        case None => loop(tl)
      }
      case Nil => None
    }
    loop(extractors)
  }
}

case class HighCard(rank: Rank) extends Combination {
  override def toString = "High " + rank

  val value = 0
  type CombinationType = HighCard

  /*
  def compare(that: Combination) = that match {
    case c: HighCard => rank.compare(c.rank)
    case c => value.compare(c.value)
  }
  */

  def compareWithSameKind(c: HighCard): Int = rank.compare(c.rank)
}

case class OnePair(rank: Rank) extends Combination {
  override def toString = "Pair of " + rank

  val value = 1
  type CombinationType = OnePair

  /*
  def compare(that: Combination) = that match {
    case c: OnePair => rank.compare(c.rank)
    case c => value.compare(c.value)
  }
  */

  def compareWithSameKind(c: OnePair): Int = rank.compare(c.rank)
}

object OnePair extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 2), _*) => Some(OnePair(r))
    case _ => None
  }
}
/*
case class TwoPairs(rank1: Rank, rank2: Rank) extends Combination {
  override def toString = "Two pairs " + rank1 + " and " + rank2

  val value = 2
  def compare(that: Combination) = that match {
    case c: TwoPairs => rank1.compare(c.rank1)
    case c => value.compare(c.value)
  }
}

object TwoPairs extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r1, 2), (r2, 2), _*) => Some(TwoPairs(r1, r2))
    case _ => None
  }
}

case class ThreeOfKind(rank: Rank) extends Combination {
  override def toString = "Three " + rank

  val value = 3
  def compare(that: Combination) = that match {
    case c: ThreeOfKind => rank.compare(c.rank)
    case c => value.compare(c.value)
  }
}

object ThreeOfKind extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 3), _*) => Some(ThreeOfKind(r))
    case _ => None
  }
}

case class Straight(rank: Rank) extends Combination {
  override def toString = "Straight " + rank

  val value = 4
  def compare(that: Combination) = that match {
    case c: Straight => rank.compare(c.rank)
    case c => value.compare(c.value)
  }
}

object Straight extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = hand.diffs match {
    case List(1, 1, 1, 1) => Some(Straight(hand.highCards.head.rank))
    case _ => None
  }
}

case class Flush(rank: Rank) extends Combination {
  override def toString = "Flush"

  val value = 5
  def compare(that: Combination) = that match {
    case c: Flush => rank.compare(c.rank)
    case c => value.compare(c.value)
  }
}

case object Flush extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = hand.suits match {
    case List((r, i), _*) if i >= 4 => Some(Flush(hand.highCards.head.rank))
    case _ => None
  }
}

case class FullHouse(rank1: Rank, rank2: Rank) extends Combination {
  override def toString = "Full house with " + rank1 + " and " + rank2

  val value = 6
  def compare(that: Combination) = that match {
    case c: FullHouse => rank1.compare(c.rank1)
    case c => value.compare(c.value)
  }
}

case object FullHouse extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r1, 3), (r2, 2), _*) => Some(FullHouse(r1, r2))
    case _ => None
  }
}

case class FourOfKind(rank: Rank) extends Combination {
  override def toString = "Four " + rank

  val value = 7
  def compare(that: Combination) = that match {
    case c: FourOfKind => rank.compare(c.rank)
    case c => value.compare(c.value)
  }
}

object FourOfKind extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = hand.ranks match {
    case List((r, 4), _*) => Some(FourOfKind(r))
    case _ => None
  }
}

case class StraightFlush(rank: Rank) extends Combination {
  override def toString = "Straight flush " + rank

  val value = 8
  def compare(that: Combination) = that match {
    case c: StraightFlush => rank.compare(c.rank)
    case c => value.compare(c.value)
  }
}

object StraightFlush extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = (hand.suits, hand.diffs) match {
    case (List((r, i), _*), List(1, 1, 1, 1)) if i >= 4 => Some(StraightFlush(hand.highCards.head.rank))
    case _ => None
  }
}

case class RoyalFlush() extends Combination {
  override def toString = "Royal flush"

  val value = 9
  def compare(that: Combination) = that match {
    case c: RoyalFlush => 0
    case c => value.compare(c.value)
  }
}

object RoyalFlush extends CombinationExtractor {
  def unapply(hand: Hand): Option[Combination] = (hand.ranks, hand.suits, hand.diffs) match {
    case (List((Ace, _), _*), List((_, i), _*), List(1, 1, 1, 1)) if i >= 4 => Some(RoyalFlush())
    case _ => None
  }
}
*/